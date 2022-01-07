# pylint: disable=too-many-lines

import itertools
import logging
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, List, Mapping, Optional, Sequence, Set, Tuple, Type, Union

import librflxlang as lang

from rflx import expression as expr, model
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail, warn
from rflx.identifier import ID, StrID
from rflx.integration import Integration
from rflx.model import declaration as decl, statement as stmt
from rflx.specification.const import RESERVED_WORDS

from . import style
from .cache import Cache

log = logging.getLogger(__name__)
STDIN = Path("<stdin>")


def node_location(node: lang.RFLXNode, filename: Path, end_location: bool = False) -> Location:
    start = node.token_start.sloc_range
    end = node.token_end.sloc_range
    return Location(
        start=(start.start.line, start.start.column)
        if not end_location
        else (end.end.line, end.end.column),
        source=filename,
        end=(end.end.line, end.end.column),
    )


def type_location(identifier: ID, node: lang.RFLXNode) -> Location:
    """
    Create a location object.

    The location object covers the area from the start of an identifier to the end of a node.
    """
    assert identifier.location is not None
    assert identifier.location.source is not None
    return Location(
        identifier.location.start,
        identifier.location.source,
        node_location(node, identifier.location.source).end,
    )


def diagnostics_to_error(
    diagnostics: List[lang.Diagnostic], error: RecordFluxError, filename: Path
) -> bool:
    """Append langkit diagnostics to RecordFlux error. Return True if error occured."""

    if len(diagnostics) == 0:
        return False

    for diag in diagnostics:
        loc = diag.sloc_range
        error.extend(
            [
                (
                    diag.message,
                    Subsystem.PARSER,
                    Severity.ERROR,
                    Location(
                        start=(loc.start.line, loc.start.column),
                        source=filename,
                        end=(loc.end.line, loc.end.column),
                    ),
                )
            ],
        )
    return True


def create_description(description: lang.Description = None) -> Optional[str]:
    if description:
        assert isinstance(description.text, str)
        return description.text.split('"')[1]
    return None


def create_transition(transition: lang.Transition, filename: Path) -> model.Transition:
    if transition.kind_name not in ("Transition", "ConditionalTransition"):
        raise NotImplementedError(f"Transition kind {transition.kind_name} unsupported")
    target = create_id(transition.f_target, filename)
    condition: expr.Expr = expr.TRUE
    description = create_description(transition.f_description)
    if isinstance(transition, lang.ConditionalTransition):
        condition = create_bool_expression(transition.f_condition, filename)
    return model.Transition(target, condition, description, node_location(transition, filename))


def create_reset(reset: lang.Statement, filename: Path) -> stmt.Statement:
    assert isinstance(reset, lang.Reset)
    return stmt.Reset(
        create_id(reset.f_identifier, filename),
        {
            create_id(c.f_identifier, filename): create_expression(c.f_expression, filename)
            for c in reset.f_associations
        },
        location=node_location(reset, filename),
    )


def create_assignment(assignment: lang.Statement, filename: Path) -> stmt.Statement:
    assert isinstance(assignment, lang.Assignment)
    return stmt.Assignment(
        create_id(assignment.f_identifier, filename),
        create_expression(assignment.f_expression, filename),
        location=node_location(assignment, filename),
    )


def create_attribute_statement(expression: lang.Statement, filename: Path) -> stmt.Statement:
    assert isinstance(expression, lang.AttributeStatement)
    attrs = {
        "Append": stmt.Append,
        "Extend": stmt.Extend,
        "Read": stmt.Read,
        "Write": stmt.Write,
    }
    constructor = attrs[expression.f_attr.text]

    return constructor(
        create_id(expression.f_identifier, filename),
        create_expression(expression.f_expression, filename),
        location=node_location(expression, filename),
    )


def create_statement(statement: lang.Statement, filename: Path) -> stmt.Statement:
    handlers = {
        "Reset": create_reset,
        "Assignment": create_assignment,
        "AttributeStatement": create_attribute_statement,
    }
    return handlers[statement.kind_name](statement, filename)


def create_state(state: lang.State, package: ID, filename: Path) -> model.State:
    location = node_location(state, filename)
    identifier = create_id(state.f_identifier, filename)
    if isinstance(state.f_body, lang.NullStateBody):
        return model.State(identifier)
    assert isinstance(state.f_body, lang.StateBody)
    if state.f_identifier.text != state.f_body.f_end_identifier.text:
        fail(
            "inconsistent state identifier: "
            f"{state.f_identifier.text} /= {state.f_body.f_end_identifier.text}",
            Subsystem.PARSER,
            Severity.ERROR,
            location,
        )
    transitions = []
    for t in state.f_body.f_conditional_transitions:
        transitions.append(create_transition(t, filename))
    transitions.append(create_transition(state.f_body.f_final_transition, filename))
    exception_transition = (
        create_transition(state.f_body.f_exception_transition, filename)
        if state.f_body.f_exception_transition
        else None
    )
    actions = []
    for a in state.f_body.f_actions:
        actions.append(create_statement(a, filename))
    declarations = []
    for d in state.f_body.f_declarations:
        declarations.append(create_declaration(d, package, filename))
    description = create_description(state.f_description)
    return model.State(
        identifier=identifier,
        transitions=transitions,
        exception_transition=exception_transition,
        actions=actions,
        declarations=declarations,
        description=description,
        location=node_location(state, filename),
    )


def __check_session_identifier(session: lang.SessionDecl, filename: Path) -> None:
    if session.f_identifier.text != session.f_end_identifier.text:
        fail(
            "inconsistent session identifier: "
            f"{session.f_identifier.text} /= {session.f_end_identifier.text}",
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(session, filename),
        )


def create_unproven_session(
    session: lang.SessionDecl,
    package: ID,
    filename: Path,
    types: Sequence[model.Type] = None,
) -> model.UnprovenSession:
    __check_session_identifier(session, filename)

    return model.UnprovenSession(
        package * create_id(session.f_identifier, filename),
        create_id(session.f_aspects.f_initial, filename),
        create_id(session.f_aspects.f_final, filename),
        [create_state(s, package, filename) for s in session.f_states],
        [create_declaration(d, package, filename) for d in session.f_declarations],
        [create_formal_declaration(p, package, filename) for p in session.f_parameters],
        types or [],
        node_location(session, filename),
    )


def create_session(
    session: lang.SessionDecl,
    package: ID,
    filename: Path,
    types: Sequence[model.Type] = None,
) -> model.Session:
    return create_unproven_session(session, package, filename, types).proven()


def create_id(identifier: lang.AbstractID, filename: Path) -> ID:
    if isinstance(identifier, lang.UnqualifiedID):
        if identifier.text.lower() in RESERVED_WORDS:
            fail(
                f'reserved word "{identifier.text}" used as identifier',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(identifier, filename),
            )
        return ID(identifier.text, location=node_location(identifier, filename))
    if isinstance(identifier, lang.ID):
        name = ID(identifier.f_name.text, location=node_location(identifier.f_name, filename))
        if identifier.f_package:
            return (
                ID(
                    identifier.f_package.text,
                    location=node_location(identifier.f_package, filename),
                )
                * name
            )
        return name

    raise NotImplementedError(f"Invalid ID: {identifier.text}")


def create_sequence(
    identifier: ID,
    _parameters: lang.Parameters,
    sequence: lang.TypeDef,
    types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> model.Type:
    assert isinstance(sequence, lang.SequenceTypeDef)
    element_identifier = model.qualified_type_identifier(
        create_id(sequence.f_element_type, filename), identifier.parent
    )

    try:
        element_type = next(t for t in types if element_identifier == t.identifier)
    except StopIteration:
        fail(
            f'undefined element type "{element_identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            element_identifier.location,
        )

    return model.Sequence(identifier, element_type, type_location(identifier, sequence))


def create_numeric_literal(expression: lang.Expr, filename: Path) -> expr.Expr:
    location = node_location(expression, filename)
    num = expression.text.split("#")
    if len(num) == 1:
        return expr.Number(int(num[0]), location=location)
    if len(num) == 3:
        base = int(num[0])
        return expr.Number(int(num[1], base), base=base, location=location)
    raise NotImplementedError(f"Invalid numeric literal: {expression.text}")


OPERATIONS: Dict[str, Type[expr.BinExpr]] = {
    "OpIn": expr.In,
    "OpNotin": expr.NotIn,
    "OpEq": expr.Equal,
    "OpNeq": expr.NotEqual,
}


def create_binop(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    loc = node_location(expression, filename)
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename),
            create_expression(expression.f_right, filename),
            location=loc,
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename),
            create_expression(expression.f_right, filename),
            location=loc,
        )

    left = create_math_expression(expression.f_left, filename)
    right = create_math_expression(expression.f_right, filename)
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](left, right, location=loc)
    if expression.f_op.kind_name in MATH_COMPARISONS:
        return MATH_COMPARISONS[expression.f_op.kind_name](left, right, location=loc)

    raise NotImplementedError(f"Invalid BinOp {expression.f_op.kind_name} => {expression.text}")


MATH_OPERATIONS: Dict[str, Union[Type[expr.BinExpr], Type[expr.AssExpr]]] = {
    "OpPow": expr.Pow,
    "OpAdd": expr.Add,
    "OpSub": expr.Sub,
    "OpMul": expr.Mul,
    "OpDiv": expr.Div,
    "OpMod": expr.Mod,
}


def create_math_binop(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](
            create_math_expression(expression.f_left, filename),
            create_math_expression(expression.f_right, filename),
            location=node_location(expression, filename),
        )
    raise NotImplementedError(
        f"Invalid math BinOp {expression.f_op.kind_name} => {expression.text}"
    )


MATH_COMPARISONS: Dict[str, Type[expr.Relation]] = {
    "OpLt": expr.Less,
    "OpGt": expr.Greater,
    "OpLe": expr.LessEqual,
    "OpGe": expr.GreaterEqual,
}

BOOLEAN_OPERATIONS = {
    "OpAnd": expr.And,
    "OpOr": expr.Or,
}


def create_bool_binop(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    if expression.f_op.kind_name in MATH_COMPARISONS:
        return MATH_COMPARISONS[expression.f_op.kind_name](
            create_math_expression(expression.f_left, filename),
            create_math_expression(expression.f_right, filename),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_bool_expression(expression.f_left, filename),
            create_bool_expression(expression.f_right, filename),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename),
            create_expression(expression.f_right, filename),
            location=node_location(expression, filename),
        )
    raise NotImplementedError(
        f"Invalid bool BinOp {expression.f_op.kind_name} => {expression.text}"
    )


def create_paren_bool_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_bool_expression(expression.f_data, filename)


def create_paren_math_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_math_expression(expression.f_data, filename)


def create_paren_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_expression(expression.f_data, filename)


def create_variable(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Variable)
    location = node_location(expression, filename)
    if expression.f_identifier.text.lower() in ("true", "false"):
        return expr.Variable(create_id(expression.f_identifier, filename), location=location)
    return expr.Variable(create_id(expression.f_identifier, filename), location=location)


def create_math_attribute(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Attribute)
    inner = create_expression(expression.f_expression, filename)
    if expression.f_kind.kind_name == "AttrLast":
        return expr.Last(inner)
    if expression.f_kind.kind_name == "AttrFirst":
        return expr.First(inner)
    if expression.f_kind.kind_name == "AttrSize":
        return expr.Size(inner)
    raise NotImplementedError(
        f"Invalid math attribute: {expression.f_kind.kind_name} => {expression.text}"
    )


def create_attribute(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Attribute)
    inner = create_expression(expression.f_expression, filename)
    if expression.f_kind.kind_name == "AttrLast":
        return expr.Last(inner)
    if expression.f_kind.kind_name == "AttrFirst":
        return expr.First(inner)
    if expression.f_kind.kind_name == "AttrSize":
        return expr.Size(inner)
    if expression.f_kind.kind_name == "AttrValidChecksum":
        return expr.ValidChecksum(inner)
    if expression.f_kind.kind_name == "AttrHasData":
        return expr.HasData(inner)
    if expression.f_kind.kind_name == "AttrHead":
        return expr.Head(inner)
    if expression.f_kind.kind_name == "AttrOpaque":
        return expr.Opaque(inner)
    if expression.f_kind.kind_name == "AttrPresent":
        return expr.Present(inner)
    if expression.f_kind.kind_name == "AttrValid":
        return expr.Valid(inner)
    raise NotImplementedError(
        f"Invalid attribute: {expression.f_kind.kind_name} => {expression.text}"
    )


def create_sequence_aggregate(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.SequenceAggregate)
    return expr.Aggregate(
        *[create_math_expression(v, filename) for v in expression.f_values],
        location=node_location(expression, filename),
    )


def create_string_literal(expression: lang.Expr, filename: Path) -> expr.Expr:
    return expr.String(
        expression.text.split('"')[1],
        location=node_location(expression, filename),
    )


def create_call(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Call)
    return expr.Call(
        create_id(expression.f_identifier, filename),
        [create_expression(a, filename) for a in expression.f_arguments],
        location=node_location(expression, filename),
    )


def create_quantified_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.QuantifiedExpression)
    param_id = create_id(expression.f_parameter_identifier, filename)
    iterable = create_expression(expression.f_iterable, filename)
    predicate = create_expression(expression.f_predicate, filename)
    location = node_location(expression, filename)
    if expression.f_operation.kind_name == "QuantifierAll":
        return expr.ForAllIn(param_id, iterable, predicate, location)
    if expression.f_operation.kind_name == "QuantifierSome":
        return expr.ForSomeIn(param_id, iterable, predicate, location)

    raise NotImplementedError(f"Invalid quantified: {expression.f_operation.text}")


def create_binding(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Binding)
    bindings: Mapping[Union[str, ID], expr.Expr] = {
        create_id(b.f_identifier, filename): create_expression(b.f_expression, filename)
        for b in expression.f_bindings
    }
    return expr.Binding(
        create_expression(expression.f_expression, filename),
        bindings,
        node_location(expression, filename),
    )


def create_variable_decl(
    declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    assert isinstance(declaration, lang.VariableDecl)
    initializer = (
        create_expression(declaration.f_initializer, filename)
        if declaration.f_initializer
        else None
    )
    return decl.VariableDeclaration(
        create_id(declaration.f_identifier, filename),
        model.qualified_type_identifier(
            create_id(declaration.f_type_identifier, filename), package
        ),
        initializer,
        location=node_location(declaration, filename),
    )


def create_private_type_decl(
    declaration: lang.FormalDecl, package: ID, filename: Path
) -> decl.FormalDeclaration:
    assert isinstance(declaration, lang.FormalPrivateTypeDecl)
    return decl.TypeDeclaration(
        model.Private(
            model.qualified_type_identifier(create_id(declaration.f_identifier, filename), package),
            location=node_location(declaration, filename),
        )
    )


def create_channel_decl(
    declaration: lang.FormalDecl,
    _package: ID,
    filename: Path,
) -> decl.FormalDeclaration:
    assert isinstance(declaration, lang.FormalChannelDecl)
    readable = False
    writable = False
    for p in declaration.f_parameters:
        if p.kind_name == "Readable":
            readable = True
        elif p.kind_name == "Writable":
            writable = True
        else:
            raise NotImplementedError(f"channel parameter: {p.kind_name}")
    return decl.ChannelDeclaration(
        create_id(declaration.f_identifier, filename),
        readable=readable,
        writable=writable,
        location=node_location(declaration, filename),
    )


def create_renaming_decl(
    declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    assert isinstance(declaration, lang.RenamingDecl)
    selected = create_expression(declaration.f_expression, filename)
    assert isinstance(selected, expr.Selected)
    return decl.RenamingDeclaration(
        create_id(declaration.f_identifier, filename),
        model.qualified_type_identifier(
            create_id(declaration.f_type_identifier, filename), package
        ),
        selected,
        location=node_location(declaration, filename),
    )


def create_function_decl(
    declaration: lang.FormalDecl,
    package: ID,
    filename: Path,
) -> decl.FormalDeclaration:
    assert isinstance(declaration, lang.FormalFunctionDecl)
    arguments = []
    if declaration.f_parameters:
        for p in declaration.f_parameters.f_parameters:
            arguments.append(
                decl.Argument(
                    create_id(p.f_identifier, filename),
                    model.qualified_type_identifier(
                        create_id(p.f_type_identifier, filename), package
                    ),
                )
            )
    return decl.FunctionDeclaration(
        create_id(declaration.f_identifier, filename),
        arguments,
        model.qualified_type_identifier(
            create_id(declaration.f_return_type_identifier, filename), package
        ),
        location=node_location(declaration, filename),
    )


def create_negation(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Negation)
    math_expr = create_math_expression(expression.f_data, filename)
    assert isinstance(math_expr, expr.Number)
    return expr.Number(-math_expr.value, math_expr.base, node_location(expression, filename))


def create_concatenation(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Concatenation)
    left = create_expression(expression.f_left, filename)
    right = create_expression(expression.f_right, filename)
    assert isinstance(left, expr.Aggregate)
    assert isinstance(right, expr.Aggregate)
    return expr.Aggregate(
        *(left.elements + right.elements), location=node_location(expression, filename)
    )


def create_comprehension(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Comprehension)
    condition = (
        create_bool_expression(expression.f_condition, filename)
        if expression.f_condition
        else expr.TRUE
    )
    return expr.Comprehension(
        create_id(expression.f_iterator, filename),
        create_expression(expression.f_sequence, filename),
        create_expression(expression.f_selector, filename),
        condition,
        node_location(expression, filename),
    )


def create_selected(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.SelectNode)
    return expr.Selected(
        create_expression(expression.f_expression, filename),
        create_id(expression.f_selector, filename),
        location=node_location(expression, filename),
    )


def create_conversion(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Conversion)
    return expr.Conversion(
        create_id(expression.f_target_identifier, filename),
        create_expression(expression.f_argument, filename),
        location=node_location(expression, filename),
    )


def create_message_aggregate(expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.MessageAggregate)
    values: Mapping[StrID, expr.Expr] = {}
    if isinstance(expression.f_values, lang.NullMessageAggregate):
        values = {}
    elif isinstance(expression.f_values, lang.MessageAggregateAssociations):
        values = {
            create_id(c.f_identifier, filename): create_expression(c.f_expression, filename)
            for c in expression.f_values.f_associations
        }
    else:
        raise NotImplementedError(f"invalid message field: {type(expression.f_values)}")

    return expr.MessageAggregate(
        create_id(expression.f_identifier, filename),
        values,
        location=node_location(expression, filename),
    )


EXPRESSION_MAP = {
    "NumericLiteral": create_numeric_literal,
    "ParenExpression": create_paren_expression,
    "Variable": create_variable,
    "Attribute": create_attribute,
    "SequenceAggregate": create_sequence_aggregate,
    "StringLiteral": create_string_literal,
    "Call": create_call,
    "QuantifiedExpression": create_quantified_expression,
    "Binding": create_binding,
    "Negation": create_negation,
    "Concatenation": create_concatenation,
    "Comprehension": create_comprehension,
    "SelectNode": create_selected,
    "Conversion": create_conversion,
    "MessageAggregate": create_message_aggregate,
    "BinOp": create_binop,
}


def create_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    return EXPRESSION_MAP[expression.kind_name](expression, filename)


def create_declaration(
    declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    handlers = {
        "VariableDecl": create_variable_decl,
        "RenamingDecl": create_renaming_decl,
    }
    return handlers[declaration.kind_name](declaration, package, filename)


def create_formal_declaration(
    declaration: lang.FormalDecl, package: ID, filename: Path
) -> decl.FormalDeclaration:
    handlers = {
        "FormalChannelDecl": create_channel_decl,
        "FormalFunctionDecl": create_function_decl,
        "FormalPrivateTypeDecl": create_private_type_decl,
    }
    return handlers[declaration.kind_name](declaration, package, filename)


def create_math_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    handlers = {
        "NumericLiteral": create_numeric_literal,
        "BinOp": create_math_binop,
        "ParenExpression": create_paren_math_expression,
        "Variable": create_variable,
        "Call": create_call,
        "Binding": create_binding,
        "Negation": create_negation,
        "Attribute": create_math_attribute,
        "SelectNode": create_selected,
        "SequenceAggregate": create_sequence_aggregate,
    }
    return handlers[expression.kind_name](expression, filename)


def create_bool_expression(expression: lang.Expr, filename: Path) -> expr.Expr:
    handlers = {
        "BinOp": create_bool_binop,
        "ParenExpression": create_paren_bool_expression,
        "Attribute": create_attribute,
        "Call": create_call,
        "Variable": create_variable,
        "QuantifiedExpression": create_quantified_expression,
        "Binding": create_binding,
        "SelectNode": create_selected,
    }
    return handlers[expression.kind_name](expression, filename)


def create_modular(
    identifier: ID,
    _parameters: lang.Parameters,
    modular: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> model.Type:
    assert isinstance(modular, lang.ModularTypeDef)
    return model.ModularInteger(
        identifier,
        create_math_expression(modular.f_mod, filename),
        type_location(identifier, modular),
    )


def create_range(
    identifier: ID,
    _parameters: lang.Parameters,
    rangetype: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> model.Type:
    assert isinstance(rangetype, lang.RangeTypeDef)
    if rangetype.f_size.f_identifier.text != "Size":
        fail(
            f"invalid aspect {rangetype.f_size.f_identifier.text} for range type {identifier}",
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(rangetype, filename),
        )
    size = create_math_expression(rangetype.f_size.f_value, filename)
    return model.RangeInteger(
        identifier,
        create_math_expression(rangetype.f_first, filename),
        create_math_expression(rangetype.f_last, filename),
        size,
        type_location(identifier, rangetype),
    )


def create_null_message(
    identifier: ID,
    _parameters: lang.Parameters,
    message: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    _filename: Path,
) -> model.Type:
    assert isinstance(message, lang.NullMessageTypeDef)
    return model.Message(identifier, [], {}, location=type_location(identifier, message))


def create_message(
    identifier: ID,
    parameters: lang.Parameters,
    message: lang.TypeDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    workers: int,
    cache: Cache,
    filename: Path,
) -> model.Type:
    # pylint: disable = too-many-arguments, too-many-locals

    assert isinstance(message, lang.MessageTypeDef)
    error = RecordFluxError()
    fields = message.f_message_fields

    field_types, message_arguments = create_message_types(
        error, identifier, parameters, fields, types, filename
    )
    structure = create_message_structure(error, fields, filename)
    checksum_aspects, byte_order_aspect = parse_aspects(message.f_aspects, filename)
    try:
        result = create_proven_message(
            model.UnprovenMessage(
                identifier,
                structure,
                field_types,
                checksum_aspects,
                byte_order_aspect,
                type_location(identifier, message),
            ).merged(message_arguments),
            skip_verification,
            workers,
            cache,
        )
    except RecordFluxError as e:
        error.extend(e)
    error.propagate()
    return result


def create_message_types(
    error: RecordFluxError,
    identifier: ID,
    parameters: lang.Parameters,
    fields: lang.MessageFields,
    types: Sequence[model.Type],
    filename: Path,
) -> Tuple[Mapping[model.Field, model.Type], Dict[ID, Dict[ID, expr.Expr]]]:
    def get_parameters(param: lang.Parameters) -> Optional[lang.ParameterList]:
        if not param:
            return None
        assert isinstance(param.f_parameters, lang.ParameterList)
        return param.f_parameters

    field_types: Dict[model.Field, model.Type] = {}
    message_arguments: Dict[ID, Dict[ID, expr.Expr]] = defaultdict(dict)

    for field_identifier, type_identifier, type_arguments in itertools.chain(
        (
            (parameter.f_identifier, parameter.f_type_identifier, [])
            for parameter in get_parameters(parameters) or []
        ),
        (
            (field.f_identifier, field.f_type_identifier, field.f_type_arguments)
            for field in fields.f_fields
        ),
    ):
        qualified_type_identifier = model.qualified_type_identifier(
            create_id(type_identifier, filename), identifier.parent
        )
        field_type = next((t for t in types if t.identifier == qualified_type_identifier), None)
        if field_type:
            field = model.Field(create_id(field_identifier, filename))
            if field not in field_types:
                if isinstance(field_type, model.Message):
                    assert isinstance(type_arguments, lang.TypeArgumentList)
                    message_arguments[qualified_type_identifier] = create_message_arguments(
                        error,
                        field_type,
                        type_arguments,
                        qualified_type_identifier.location,
                        filename,
                    )
                field_types[field] = field_type
            else:
                error.extend(
                    [
                        (
                            f'name conflict for "{field.identifier}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            node_location(field_identifier, filename),
                        ),
                        (
                            "conflicting name",
                            Subsystem.PARSER,
                            Severity.INFO,
                            [f.identifier for f in field_types if f == field][0].location,
                        ),
                    ]
                )
        else:
            error.extend(
                [
                    (
                        f'undefined type "{qualified_type_identifier}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        qualified_type_identifier.location,
                    )
                ]
            )

    return (field_types, message_arguments)


def create_message_arguments(
    error: RecordFluxError,
    message: model.Message,
    type_arguments: lang.TypeArgumentList,
    field_type_location: Optional[Location],
    filename: Path,
) -> Dict[ID, expr.Expr]:
    result = {}
    argument_errors = RecordFluxError()

    for param, arg in itertools.zip_longest(message.parameters, type_arguments):
        if param:
            param_id = param.identifier
        if arg:
            arg_id = create_id(arg.f_identifier, filename)
            arg_expression = create_expression(arg.f_expression, filename)
        if arg and param and arg_id == param_id:
            result[arg_id] = arg_expression
        if not param or (arg and arg_id != param_id):
            argument_errors.extend(
                [
                    (
                        f'unexpected argument "{arg_id}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(arg, filename),
                    ),
                    (
                        "expected no argument"
                        if not param
                        else f'expected argument for parameter "{param_id}"',
                        Subsystem.PARSER,
                        Severity.INFO,
                        node_location(arg, filename),
                    ),
                ]
            )
        if not arg:
            argument_errors.extend(
                [
                    (
                        "missing argument",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        field_type_location,
                    ),
                    (
                        f'expected argument for parameter "{param_id}"',
                        Subsystem.PARSER,
                        Severity.INFO,
                        param_id.location,
                    ),
                ]
            )

    error.extend(argument_errors)

    return result


def create_message_structure(
    error: RecordFluxError, fields: lang.MessageFields, filename: Path
) -> List[model.Link]:
    def extract_aspect(aspects: lang.AspectList) -> Tuple[expr.Expr, expr.Expr]:
        size: expr.Expr = expr.UNDEFINED
        first: expr.Expr = expr.UNDEFINED
        for aspect in aspects:
            if aspect.f_identifier.text == "Size":
                size = create_math_expression(aspect.f_value, filename)
            elif aspect.f_identifier.text == "First":
                first = create_math_expression(aspect.f_value, filename)
            else:
                error.extend(
                    [
                        (
                            f'invalid aspect "{aspect.f_identifier.text}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            node_location(aspect.f_identifier, filename),
                        )
                    ],
                )
        return size, first

    def extract_then(
        then: lang.ThenNode,
    ) -> Tuple[model.Field, expr.Expr, expr.Expr, expr.Expr, Location]:
        target = (
            model.FINAL
            if then.f_target.text == "null"
            else model.Field(create_id(then.f_target, filename))
        )
        condition = (
            create_bool_expression(then.f_condition, filename) if then.f_condition else expr.TRUE
        )
        size, first = extract_aspect(then.f_aspects)
        return target, condition, size, first, node_location(then, filename)

    structure: List[model.Link] = []

    if fields.f_initial_field:
        structure.append(
            model.Link(
                model.INITIAL,
                *extract_then(fields.f_initial_field.f_then),
            )
        )
    else:
        structure.append(
            model.Link(
                model.INITIAL,
                model.Field(create_id(fields.f_fields[0].f_identifier, filename)),
                location=node_location(fields.f_fields[0].f_identifier, filename),
            )
        )

    for i, field in enumerate(fields.f_fields):
        source_node = (
            model.Field(create_id(field.f_identifier, filename))
            if field.f_identifier
            else model.INITIAL
        )
        field_identifier = create_id(field.f_identifier, filename)
        if field.f_identifier.text.lower() == "message":
            error.extend(
                [
                    (
                        'reserved word "Message" used as identifier',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        field_identifier.location,
                    )
                ],
            )
            continue

        if len(field.f_thens) == 0:
            target_id = (
                create_id(fields.f_fields[i + 1].f_identifier, filename)
                if i + 1 < len(fields.f_fields)
                else None
            )
            target_node = model.Field(target_id) if target_id else model.FINAL
            structure.append(
                model.Link(
                    source_node,
                    target_node,
                    location=node_location(field, filename, end_location=True),
                )
            )

        for then in field.f_thens:
            if then.f_target.kind_name != "NullID" and not any(
                then.f_target.text == c.f_identifier.text for c in fields.f_fields
            ):
                error.extend(
                    [
                        (
                            f'undefined field "{then.f_target.text}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            node_location(then.f_target, filename) if then.f_target else None,
                        )
                    ],
                )
                continue
            structure.append(model.Link(source_node, *extract_then(then)))

        merge_field_aspects(error, field_identifier, structure, *extract_aspect(field.f_aspects))
        merge_field_condition(
            field_identifier,
            structure,
            create_bool_expression(field.f_condition, filename) if field.f_condition else expr.TRUE,
        )

    return structure


def merge_field_aspects(
    error: RecordFluxError,
    field_identifier: ID,
    structure: Sequence[model.Link],
    size: expr.Expr,
    first: expr.Expr,
) -> None:
    if first != expr.UNDEFINED or size != expr.UNDEFINED:
        for l in (l for l in structure if l.target.identifier == field_identifier):
            if first != expr.UNDEFINED:
                if l.first == expr.UNDEFINED:
                    l.first = first
                else:
                    error.extend(
                        [
                            (
                                f'first aspect of field "{field_identifier}"'
                                " conflicts with previous"
                                " specification",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                first.location,
                            ),
                            (
                                "previous specification of first",
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.first.location,
                            ),
                        ],
                    )

            if size != expr.UNDEFINED:
                if l.size == expr.UNDEFINED:
                    l.size = size
                else:
                    error.extend(
                        [
                            (
                                f'size aspect of field "{field_identifier}" conflicts with'
                                " previous specification",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                size.location,
                            ),
                            (
                                "previous specification of size",
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.size.location,
                            ),
                        ],
                    )


def merge_field_condition(
    field_identifier: ID,
    structure: Sequence[model.Link],
    condition: expr.Expr,
) -> None:
    if condition != expr.TRUE:
        for l in (l for l in structure if l.source.identifier == field_identifier):
            l.condition = (
                expr.And(condition, l.condition, location=l.condition.location)
                if l.condition != expr.TRUE
                else condition
            )


def parse_aspects(
    aspects: lang.MessageAspectList, filename: Path
) -> Tuple[Mapping[ID, Sequence[expr.Expr]], Optional[model.ByteOrder]]:
    checksum_result = {}
    byte_order_result = None
    for aspect in aspects:
        if isinstance(aspect, lang.ChecksumAspect):
            for assoc in aspect.f_associations:
                exprs = []
                for value in assoc.f_covered_fields:
                    if isinstance(value, lang.ChecksumVal):
                        exprs.append(create_math_expression(value.f_data, filename))
                    elif isinstance(value, lang.ChecksumValueRange):
                        exprs.append(
                            expr.ValueRange(
                                create_math_expression(value.f_first, filename),
                                create_math_expression(value.f_last, filename),
                            )
                        )
                    else:
                        raise NotImplementedError(f"Invalid checksum association {value.kind_name}")
                checksum_result[create_id(assoc.f_identifier, filename)] = exprs
        if isinstance(aspect, lang.ByteOrderAspect):
            if isinstance(aspect.f_byte_order, lang.ByteOrderTypeLoworderfirst):
                byte_order_result = model.ByteOrder.LOW_ORDER_FIRST
            else:
                byte_order_result = model.ByteOrder.HIGH_ORDER_FIRST
    return checksum_result, byte_order_result


def create_derived_message(
    identifier: ID,
    _parameters: lang.Parameters,
    derivation: lang.TypeDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    workers: int,
    cache: Cache,
    filename: Path,
) -> model.Type:
    # pylint: disable=too-many-arguments
    assert isinstance(derivation, lang.TypeDerivationDef)
    base_id = create_id(derivation.f_base, filename)
    base_name = model.qualified_type_identifier(base_id, identifier.parent)

    base_types: Sequence[model.Type] = [t for t in types if t.identifier == base_name]

    if not base_types:
        fail(
            f'undefined base message "{base_name}" in derived message',
            Subsystem.PARSER,
            Severity.ERROR,
            base_name.location,
        )

    base_messages: Sequence[model.Message] = [t for t in base_types if isinstance(t, model.Message)]

    if not base_messages:
        error = RecordFluxError()
        error.extend(
            [
                (
                    f'illegal derivation "{identifier}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    identifier.location,
                ),
                (
                    f'invalid base message type "{base_name}"',
                    Subsystem.PARSER,
                    Severity.INFO,
                    base_types[0].identifier.location,
                ),
            ],
        )
        error.propagate()

    return create_proven_message(
        model.UnprovenDerivedMessage(
            identifier, base_messages[0], location=type_location(identifier, derivation)
        ).merged(),
        skip_verification,
        workers,
        cache,
    )


def create_enumeration(
    identifier: ID,
    _parameters: lang.Parameters,
    enumeration: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> model.Type:
    assert isinstance(enumeration, lang.EnumerationTypeDef)
    literals: List[Tuple[StrID, expr.Number]] = []
    error = RecordFluxError()

    def create_aspects(aspects: lang.AspectList) -> Tuple[expr.Expr, bool]:
        always_valid = False
        size = None
        for a in aspects:
            if a.f_identifier.text == "Size":
                size = create_math_expression(a.f_value, filename)
            if a.f_identifier.text == "Always_Valid":
                if a.f_value:
                    av_expr = create_bool_expression(a.f_value, filename)
                    if av_expr == expr.Variable("True"):
                        always_valid = True
                    elif av_expr == expr.Variable("False"):
                        always_valid = False
                    else:
                        error.extend(
                            [
                                (
                                    f"invalid Always_Valid expression: {av_expr}",
                                    Subsystem.PARSER,
                                    Severity.ERROR,
                                    node_location(a.f_value, filename),
                                )
                            ],
                        )
                else:
                    always_valid = True
        if not size:
            error.extend(
                [
                    (
                        f'no size set for "{identifier}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        identifier.location,
                    )
                ],
            )
        error.propagate()
        assert size
        return size, always_valid

    if isinstance(enumeration.f_elements, lang.NamedEnumerationDef):
        for e in enumeration.f_elements.f_elements:
            element_identifier = create_id(e.f_identifier, filename)
            value = create_math_expression(e.f_literal, filename)
            assert isinstance(value, expr.Number)
            literals.append((element_identifier, value))
    elif isinstance(enumeration.f_elements, lang.PositionalEnumerationDef):
        literals = [
            (create_id(e, filename), expr.Number(i))
            for i, e in enumerate(enumeration.f_elements.f_elements)
        ]
    else:
        raise NotImplementedError(
            f"Enumeration kind {enumeration.f_elements.kind_name} unsupported"
        )

    size, always_valid = create_aspects(enumeration.f_aspects)

    return model.Enumeration(
        identifier, literals, size, always_valid, location=type_location(identifier, enumeration)
    )


def create_proven_message(
    unproven_message: model.UnprovenMessage, skip_verification: bool, workers: int, cache: Cache
) -> model.Message:
    proven_message = unproven_message.proven(
        skip_verification or cache.is_verified(unproven_message), workers
    )

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(
    refinement: lang.RefinementDecl, package: ID, types: Sequence[model.Type], filename: Path
) -> model.Refinement:
    messages = {t.identifier: t for t in types if isinstance(t, model.Message)}

    pdu = model.qualified_type_identifier(create_id(refinement.f_pdu, filename), package)
    if pdu not in messages:
        fail(
            f'undefined type "{pdu}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(refinement, filename),
        )

    sdu = model.qualified_type_identifier(create_id(refinement.f_sdu, filename), package)
    if sdu not in messages:
        fail(
            f'undefined type "{sdu}" in refinement of "{pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            sdu.location,
        )

    if refinement.f_condition:
        condition = create_bool_expression(refinement.f_condition, filename)
    else:
        condition = expr.TRUE

    return model.Refinement(
        package,
        messages[pdu],
        model.Field(create_id(refinement.f_field, filename)),
        messages[sdu],
        condition,
        node_location(refinement, filename),
    )


def check_naming(error: RecordFluxError, package: lang.PackageNode, name: Path) -> None:
    identifier = package.f_identifier.text
    if identifier.startswith("RFLX"):
        error.extend(
            [
                (
                    f'illegal prefix "RFLX" in package identifier "{identifier}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(package.f_identifier, name),
                )
            ],
        )
    if identifier != package.f_end_identifier.text:
        error.extend(
            [
                (
                    f'inconsistent package identifier "{package.f_end_identifier.text}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(package.f_end_identifier, name),
                ),
                (
                    f'previous identifier was "{identifier}"',
                    Subsystem.PARSER,
                    Severity.INFO,
                    node_location(package.f_identifier, name),
                ),
            ],
        )
    if name != STDIN:
        expected_filename = f"{identifier.lower()}.rflx"
        if name.name != expected_filename:
            error.extend(
                [
                    (
                        f'file name does not match unit name "{identifier}",'
                        f' should be "{expected_filename}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(package.f_identifier, name),
                    )
                ],
            )
    for t in package.f_declarations:
        if isinstance(t, lang.TypeDecl) and model.is_builtin_type(
            create_id(t.f_identifier, name).name
        ):
            error.extend(
                [
                    (
                        f'illegal redefinition of built-in type "{t.f_identifier.text}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        node_location(t, name),
                    )
                ],
            )


@dataclass(frozen=True)
class SpecificationNode:
    filename: Path
    spec: lang.Specification
    withed_files: List[str]


class Parser:
    def __init__(
        self,
        skip_verification: bool = False,
        cached: bool = False,
        workers: int = 1,
        integration_files_dir: Optional[Path] = None,
    ) -> None:
        if skip_verification:
            warn("model verification skipped", Subsystem.MODEL)
        self.skip_verification = skip_verification
        self.__workers = workers
        self.__specifications: OrderedDict[str, SpecificationNode] = OrderedDict()
        self.__types: List[model.Type] = [
            *model.BUILTIN_TYPES.values(),
            *model.INTERNAL_TYPES.values(),
        ]
        self.__sessions: List[model.Session] = []
        self.__integration: Integration = Integration(integration_files_dir)
        self.__cache = Cache(not skip_verification and cached)

    def __convert_unit(
        self,
        error: RecordFluxError,
        spec: lang.Specification,
        filename: Path,
        transitions: List[ID] = None,
    ) -> None:
        transitions = transitions or []
        withed_files = []

        check_naming(error, spec.f_package_declaration, filename)
        packagefile = f"{spec.f_package_declaration.f_identifier.text.lower()}.rflx"
        for context in spec.f_context_clause:
            item = create_id(context.f_item, filename)
            if item in transitions:
                error.extend(
                    [
                        (
                            f'dependency cycle when including "{transitions[0]}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            transitions[0].location,
                        ),
                        *[
                            (
                                f'when including "{i}"',
                                Subsystem.PARSER,
                                Severity.INFO,
                                i.location,
                            )
                            for i in transitions[1:] + [item]
                        ],
                    ],
                )
                continue
            withed_file = filename.parent / f"{str(item).lower()}.rflx"
            withed_files.append(withed_file.name)
            if withed_file.name not in self.__specifications:
                error.extend(self.__parse_specfile(withed_file, transitions + [item]))

        if (
            packagefile in self.__specifications
            and filename != self.__specifications[packagefile].filename
        ):
            error.extend(
                [
                    (
                        "duplicate specification",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(spec.f_package_declaration.f_identifier, filename),
                    ),
                    (
                        "previous specification",
                        Subsystem.PARSER,
                        Severity.INFO,
                        node_location(
                            self.__specifications[
                                packagefile
                            ].spec.f_package_declaration.f_identifier,
                            self.__specifications[packagefile].filename,
                        ),
                    ),
                ],
            )
        self.__specifications[packagefile] = SpecificationNode(filename, spec, withed_files)

    def __parse_specfile(self, filename: Path, transitions: List[ID] = None) -> RecordFluxError:
        error = RecordFluxError()
        transitions = transitions or []

        log.info("Parsing %s", filename)
        unit = lang.AnalysisContext().get_from_file(str(filename))
        if diagnostics_to_error(unit.diagnostics, error, filename):
            return error
        self.__integration.load_integration_file(filename, error)
        if unit.root:
            assert isinstance(unit.root, lang.Specification)
            self.__convert_unit(error, unit.root, filename, transitions)
        return error

    def __sort_specs_topologically(self) -> None:
        """(Reverse) Topologically sort specifications using Kahn's algorithm."""

        result: List[str] = []
        incoming: Dict[str, Set[str]] = {f: set() for f in self.__specifications.keys()}
        for filename, spec_node in self.__specifications.items():
            for d in spec_node.withed_files:
                if d in incoming:
                    incoming[d].add(filename)

        specs = [f for f, i in incoming.items() if len(i) == 0]
        visited = set(specs)

        while specs:
            s = specs.pop(0)
            result.insert(0, s)
            for e in self.__specifications[s].withed_files:
                visited.add(e)
                if e in incoming and incoming[e] <= visited:
                    specs.append(e)

        self.__specifications = OrderedDict((f, self.__specifications[f]) for f in result)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        for f in specfiles:
            error.extend(self.__parse_specfile(f))
            error.extend(style.check(f))
        self.__sort_specs_topologically()
        error.propagate()

    def parse_string(
        self,
        string: str,
        rule: str = lang.GrammarRule.main_rule_rule,
    ) -> None:
        error = RecordFluxError()
        unit = lang.AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)
        if not diagnostics_to_error(unit.diagnostics, error, STDIN):
            assert isinstance(unit.root, lang.Specification)
            self.__convert_unit(error, unit.root, STDIN)
            self.__sort_specs_topologically()
        error.propagate()

    def create_model(self) -> model.Model:
        error = RecordFluxError()
        for spec_node in self.__specifications.values():
            self.__evaluate_specification(error, spec_node.spec, spec_node.filename)
        try:
            result = model.Model(self.__types, self.__sessions)
            self.__integration.validate(result, error)
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()
        return result

    def get_integration(self) -> Integration:
        return self.__integration

    @property
    def specifications(self) -> Dict[str, lang.Specification]:
        return {
            spec_node.spec.f_package_declaration.f_identifier.text: spec_node.spec
            for spec_node in self.__specifications.values()
        }

    def __evaluate_specification(
        self, error: RecordFluxError, spec: lang.Specification, filename: Path
    ) -> None:
        handlers: Dict[
            str,
            Callable[
                [ID, lang.Parameters, lang.TypeDef, Sequence[model.Type], bool, int, Cache, Path],
                model.Type,
            ],
        ] = {
            "SequenceTypeDef": create_sequence,
            "ModularTypeDef": create_modular,
            "RangeTypeDef": create_range,
            "MessageTypeDef": create_message,
            "NullMessageTypeDef": create_null_message,
            "TypeDerivationDef": create_derived_message,
            "EnumerationTypeDef": create_enumeration,
        }
        log.info("Processing %s", spec.f_package_declaration.f_identifier.text)
        package_id = create_id(spec.f_package_declaration.f_identifier, filename)

        for t in spec.f_package_declaration.f_declarations:
            if isinstance(t, lang.TypeDecl):
                identifier = model.qualified_type_identifier(
                    create_id(t.f_identifier, filename), package_id
                )
                try:
                    if not t.f_definition.kind_name == "MessageTypeDef" and t.f_parameters:
                        error.extend(
                            [
                                (
                                    "only message types can be parameterized",
                                    Subsystem.PARSER,
                                    Severity.ERROR,
                                    node_location(t.f_parameters.f_parameters, filename),
                                )
                            ]
                        )
                    new_type = handlers[t.f_definition.kind_name](
                        identifier,
                        t.f_parameters,
                        t.f_definition,
                        self.__types,
                        self.skip_verification,
                        self.__workers,
                        self.__cache,
                        filename,
                    )
                    self.__types.append(new_type)
                    error.extend(new_type.error)
                except RecordFluxError as e:
                    error.extend(e)
            elif isinstance(t, lang.RefinementDecl):
                try:
                    new_type = create_refinement(t, package_id, self.__types, filename)
                    self.__types.append(new_type)
                    error.extend(new_type.error)
                except RecordFluxError as e:
                    error.extend(e)
            elif isinstance(t, lang.SessionDecl):
                try:
                    new_session = create_session(t, package_id, filename, self.__types)
                    self.__sessions.append(new_session)
                    error.extend(new_session.error)
                except RecordFluxError as e:
                    error.extend(e)
            else:
                raise NotImplementedError(f"Declaration kind {t.kind_name} unsupported")
