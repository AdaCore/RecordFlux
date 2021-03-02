# pylint: disable=too-many-lines

import logging
from collections import OrderedDict
from pathlib import Path
from typing import Callable, Dict, List, Mapping, Optional, Sequence, Tuple, Type, Union

from librflxlang import (
    AnalysisContext,
    ArrayTypeDef,
    Aspect,
    ChecksumAspect,
    Components,
    Description,
    Diagnostic,
    EnumerationTypeDef,
    Expr,
    FormalChannelDecl,
    FormalFunctionDecl,
    FormalPrivateTypeDecl,
    GrammarRule,
    MessageTypeDef,
    ModularTypeDef,
    NullID,
    PackageNode,
    RangeTypeDef,
    RefinementDecl,
    RenamingDecl,
    RFLXNode,
    SessionDecl,
    Specification,
    State,
    Statement,
    ThenNode,
    Transition,
    TypeDecl,
    TypeDerivationDef,
    VariableDecl,
)

import rflx.declaration as decl
import rflx.expression as expr
import rflx.model as model
import rflx.statement as stmt
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID, StrID
from rflx.specification.const import RESERVED_WORDS

from .cache import Cache

log = logging.getLogger(__name__)


def node_location(node: RFLXNode, filename: Path = None) -> Location:
    start = node.token_start.sloc_range
    end = node.token_end.sloc_range
    return Location(
        start=(start.start.line, start.start.column),
        source=Path(str(filename) if filename else "<stdin>"),
        end=(end.end.line, end.end.column),
    )


def type_location(identifier: ID, node: RFLXNode) -> Location:
    """
    Create a location object covering the area from the start of an identifier to the end of a node.
    """
    assert identifier.location is not None
    return Location(identifier.location.start, identifier.location.source, node_location(node).end)


def diagnostics_to_error(
    diagnostics: List[Diagnostic], error: RecordFluxError, specfile: Path = None
) -> bool:
    """
    Append langkit diagnostics to RecordFlux error. Return True if error occured.
    """

    if len(diagnostics) == 0:
        return False

    for diag in diagnostics:
        loc = diag.sloc_range
        error.append(
            diag.message,
            Subsystem.PARSER,
            Severity.ERROR,
            Location(
                start=(loc.start.line, loc.start.column),
                source=specfile,
                end=(loc.end.line, loc.end.column),
            ),
        )
    return True


def create_description(description: Description = None) -> Optional[str]:
    if description:
        return description.text.split('"')[1]
    return None


def create_transition(transition: Transition, filename: Path = None) -> model.Transition:
    if transition.kind_name not in ("Transition", "ConditionalTransition"):
        raise NotImplementedError(f"Transition kind {transition.kind_name} unsupported")
    target = create_id(transition.f_target, filename)
    condition: expr.Expr = expr.TRUE
    description = create_description(transition.f_description)
    if transition.kind_name == "ConditionalTransition":
        condition = create_bool_expression(transition.f_condition, filename)
    return model.Transition(target, condition, description, node_location(transition, filename))


def create_reset(
    reset: Statement, filename: Path = None, location: Location = None
) -> stmt.Statement:
    return stmt.Reset(create_id(reset.f_identifier, filename), location=location)


def create_assignment(
    assignment: Statement, filename: Path = None, location: Location = None
) -> stmt.Statement:
    return stmt.Assignment(
        create_id(assignment.f_identifier, filename),
        create_expression(assignment.f_expression, filename),
        location,
    )


def create_list_attribute(
    expression: Statement, filename: Path = None, location: Location = None
) -> stmt.Statement:
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
        location=location,
    )


def create_statement(statement: Statement, filename: Path = None) -> stmt.Statement:
    handlers: Dict[
        str, Callable[[Statement, Optional[Path], Optional[Location]], stmt.Statement]
    ] = {
        "Reset": create_reset,
        "Assignment": create_assignment,
        "ListAttribute": create_list_attribute,
    }
    return handlers[statement.kind_name](statement, filename, node_location(statement, filename))


def create_state(state: State, filename: Path = None) -> model.State:
    location = node_location(state, filename)
    identifier = create_id(state.f_identifier, filename)
    if state.f_body.kind_name == "NullStateBody":
        return model.State(identifier)
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
    actions = []
    for a in state.f_body.f_actions:
        actions.append(create_statement(a, filename))
    declarations = []
    for d in state.f_body.f_declarations:
        declarations.append(create_declaration(d, filename))
    description = create_description(state.f_description)
    return model.State(
        identifier=identifier,
        transitions=transitions,
        actions=actions,
        declarations=declarations,
        description=description,
        location=node_location(state, filename),
    )


def __check_session_identifier(session: SessionDecl, filename: Path = None) -> None:
    if session.f_identifier.text != session.f_end_identifier.text:
        fail(
            "inconsistent session identifier: "
            f"{session.f_identifier.text} /= {session.f_end_identifier.text}",
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(session, filename),
        )


def create_unproven_session(
    session: SessionDecl,
    package: ID,
    types: Sequence[model.Type] = None,
    filename: Path = None,
) -> model.UnprovenSession:
    __check_session_identifier(session, filename)
    return model.UnprovenSession(
        model.qualified_type_identifier(create_id(session.f_identifier, filename), package),
        create_id(session.f_aspects.f_initial, filename),
        create_id(session.f_aspects.f_final, filename),
        [create_state(s, filename) for s in session.f_states],
        [create_declaration(d, filename) for d in session.f_declarations],
        [create_formal_declaration(p, filename) for p in session.f_parameters],
        types or [],
        node_location(session, filename),
    )


def create_session(
    session: SessionDecl,
    package: ID,
    types: Sequence[model.Type] = None,
    filename: Path = None,
) -> model.Session:
    __check_session_identifier(session, filename)
    return model.Session(
        model.qualified_type_identifier(create_id(session.f_identifier, filename), package),
        create_id(session.f_aspects.f_initial, filename),
        create_id(session.f_aspects.f_final, filename),
        [create_state(s, filename) for s in session.f_states],
        [create_declaration(d, filename) for d in session.f_declarations],
        [create_formal_declaration(p, filename) for p in session.f_parameters],
        types or [],
        node_location(session, filename),
    )


def create_id(identifier: NullID, filename: Path = None) -> ID:
    if identifier.kind_name == "UnqualifiedID":
        if identifier.text.lower() in RESERVED_WORDS:
            fail(
                f'reserved word "{identifier.text}" used as identifier',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(identifier, filename),
            )
        return ID(identifier.text, location=node_location(identifier, filename))
    if identifier.kind_name == "ID":
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


def create_array(
    identifier: ID,
    array: ArrayTypeDef,
    types: Sequence[model.Type],
    _skip_verification: bool,
    _cache: Cache,
    filename: Path,
) -> model.Array:
    element_identifier = model.qualified_type_identifier(
        create_id(array.f_element_type, filename), identifier.parent
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

    return model.Array(identifier, element_type, type_location(identifier, array))


def create_numeric_literal(
    expression: Expr, _filename: Path = None, _package: ID = None, location: Location = None
) -> expr.Expr:
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


def create_binop(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    loc = node_location(expression, filename)
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename, package),
            create_expression(expression.f_right, filename, package),
            location=loc,
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename, package),
            create_expression(expression.f_right, filename, package),
            location=loc,
        )

    left = create_math_expression(expression.f_left, filename, package)
    right = create_math_expression(expression.f_right, filename, package)
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](left, right, location=loc)
    if expression.f_op.kind_name in MATH_COMPARISONS:
        return MATH_COMPARISONS[expression.f_op.kind_name](left, right, location=loc)

    raise NotImplementedError(f"Invalid BinOp {expression.f_op.kind_name} => {expression.text}")


MATH_OPERATIONS = {
    "OpPow": expr.Pow,
    "OpAdd": expr.Add,
    "OpSub": expr.Sub,
    "OpMul": expr.Mul,
    "OpDiv": expr.Div,
    "OpMod": expr.Mod,
}


def create_math_binop(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](
            create_math_expression(expression.f_left, filename, package),
            create_math_expression(expression.f_right, filename, package),
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


def create_bool_binop(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    if expression.f_op.kind_name in MATH_COMPARISONS:
        return MATH_COMPARISONS[expression.f_op.kind_name](
            create_math_expression(expression.f_left, filename, package),
            create_math_expression(expression.f_right, filename, package),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_bool_expression(expression.f_left, filename, package),
            create_bool_expression(expression.f_right, filename, package),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(expression.f_left, filename, package),
            create_expression(expression.f_right, filename, package),
            location=node_location(expression, filename),
        )
    raise NotImplementedError(
        f"Invalid bool BinOp {expression.f_op.kind_name} => {expression.text}"
    )


def create_paren_bool_expression(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    return create_bool_expression(expression.f_data, filename, package)


def create_paren_math_expression(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    return create_math_expression(expression.f_data, filename, package)


def create_paren_expression(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    return create_expression(expression.f_data, filename, package)


def create_variable(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    if expression.f_identifier.text.lower() in ("true", "false"):
        return expr.Variable(create_id(expression.f_identifier), location=location)
    if package:
        return expr.Variable(
            model.qualified_type_identifier(create_id(expression.f_identifier, filename), package),
            location=location,
        )
    return expr.Variable(create_id(expression.f_identifier, filename), location=location)


def create_math_attribute(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    inner = create_expression(expression.f_expression, filename, package)
    if expression.f_kind.kind_name == "AttrLast":
        return expr.Last(inner)
    if expression.f_kind.kind_name == "AttrFirst":
        return expr.First(inner)
    if expression.f_kind.kind_name == "AttrSize":
        return expr.Size(inner)
    raise NotImplementedError(
        f"Invalid math attribute: {expression.f_kind.kind_name} => {expression.text}"
    )


def create_attribute(
    expression: Expr, filename: Path = None, package: ID = None, _location: Location = None
) -> expr.Expr:
    inner = create_expression(expression.f_expression, filename, package)
    if expression.f_kind.kind_name == "AttrLast":
        return expr.Last(inner)
    if expression.f_kind.kind_name == "AttrFirst":
        return expr.First(inner)
    if expression.f_kind.kind_name == "AttrSize":
        return expr.Size(inner)
    if expression.f_kind.kind_name == "AttrValidChecksum":
        return expr.ValidChecksum(inner)
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


def create_array_aggregate(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    return expr.Aggregate(
        *[create_math_expression(v, filename, package) for v in expression.f_values],
        location=location,
    )


def create_string_literal(
    expression: Expr, _filename: Path = None, _package: ID = None, location: Location = None
) -> expr.Expr:
    return expr.String(
        expression.text.split('"')[1],
        location=location,
    )


def create_call(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    return expr.Call(
        create_id(expression.f_identifier, filename),
        [create_expression(a, filename, package) for a in expression.f_arguments],
        location=location,
    )


def create_quantified_expression(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    param_id = create_id(expression.f_parameter_identifier, filename)
    iterable = create_expression(expression.f_iterable, filename, package)
    predicate = create_expression(expression.f_predicate, filename, package)
    if expression.f_operation.kind_name == "QuantifierAll":
        return expr.ForAllIn(param_id, iterable, predicate, location)
    if expression.f_operation.kind_name == "QuantifierSome":
        return expr.ForSomeIn(param_id, iterable, predicate, location)

    raise NotImplementedError(f"Invalid quantified: {expression.f_operation.text}")


def create_binding(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    bindings: Mapping[Union[str, ID], expr.Expr] = {
        create_id(b.f_identifier, filename): create_expression(b.f_expression, filename, package)
        for b in expression.f_bindings
    }
    return expr.Binding(
        create_expression(expression.f_expression, filename, package), bindings, location
    )


def create_variable_decl(
    declaration: VariableDecl, filename: Path = None, location: Location = None
) -> decl.BasicDeclaration:
    initializer = (
        create_expression(declaration.f_initializer, filename)
        if declaration.f_initializer
        else None
    )
    return decl.VariableDeclaration(
        create_id(declaration.f_identifier, filename),
        model.qualified_type_identifier(create_id(declaration.f_type_identifier, filename)),
        initializer,
        location=location,
    )


def create_private_type_decl(
    declaration: FormalPrivateTypeDecl,
    filename: Path = None,
    _package: ID = None,
    location: Location = None,
) -> decl.FormalDeclaration:
    return decl.TypeDeclaration(
        model.Private(create_id(declaration.f_identifier, filename), location=location)
    )


def create_channel_decl(
    declaration: FormalChannelDecl,
    filename: Path = None,
    _package: ID = None,
    location: Location = None,
) -> decl.FormalDeclaration:
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
        location=location,
    )


def create_renaming_decl(
    declaration: RenamingDecl, filename: Path = None, location: Location = None
) -> decl.BasicDeclaration:
    selected = create_expression(declaration.f_expression, filename, None)
    assert isinstance(selected, expr.Selected)
    return decl.RenamingDeclaration(
        create_id(declaration.f_identifier, filename),
        model.qualified_type_identifier(create_id(declaration.f_type_identifier, filename), None),
        selected,
        location,
    )


def create_function_decl(
    declaration: FormalFunctionDecl,
    filename: Path = None,
    package: ID = None,
    location: Location = None,
) -> decl.FormalDeclaration:
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
        create_id(declaration.f_return_type_identifier, filename),
        location,
    )


def create_negation(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    expression = create_math_expression(expression.f_data, filename, package)
    assert isinstance(expression, expr.Number)
    return expr.Number(-expression.value, expression.base, location)


def create_concatenation(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    left = create_expression(expression.f_left, filename, package)
    right = create_expression(expression.f_right, filename, package)
    assert isinstance(left, expr.Aggregate)
    assert isinstance(right, expr.Aggregate)
    return expr.Aggregate(*(left.elements + right.elements), location=location)


def create_comprehension(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    condition = (
        create_bool_expression(expression.f_condition, filename, package)
        if expression.f_condition
        else expr.TRUE
    )
    return expr.Comprehension(
        create_id(expression.f_iterator, filename),
        create_expression(expression.f_array, filename, package),
        create_expression(expression.f_selector, filename, package),
        condition,
        location,
    )


def create_selected(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    return expr.Selected(
        create_expression(expression.f_expression, filename, package),
        create_id(expression.f_selector, filename),
        location=location,
    )


def create_conversion(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    return expr.Conversion(
        create_id(expression.f_target_identifier, filename),
        create_expression(expression.f_argument, filename, package),
        location=location,
    )


def create_message_aggregate(
    expression: Expr, filename: Path = None, package: ID = None, location: Location = None
) -> expr.Expr:
    values: Mapping[StrID, expr.Expr] = {}
    if expression.f_values.kind_name == "NullMessageAggregate":
        values = {}
    elif expression.f_values.kind_name == "MessageAggregateAssociations":
        values = {
            create_id(c.f_identifier, filename): create_expression(
                c.f_expression, filename, package
            )
            for c in expression.f_values.f_associations
        }
    else:
        raise NotImplementedError(f"invalid message component: {expression.f_values.kind_name}")

    return expr.MessageAggregate(
        create_id(expression.f_identifier, filename),
        values,
        location=location,
    )


EXPRESSION_MAP = {
    "NumericLiteral": create_numeric_literal,
    "ParenExpression": create_paren_expression,
    "Variable": create_variable,
    "Attribute": create_attribute,
    "ArrayAggregate": create_array_aggregate,
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


def create_expression(expression: Expr, filename: Path = None, package: ID = None) -> expr.Expr:

    location = node_location(expression, filename)
    return EXPRESSION_MAP[expression.kind_name](expression, filename, package, location)


def create_declaration(declaration: Expr, filename: Path = None) -> decl.BasicDeclaration:
    location = node_location(declaration, filename)
    handlers: Dict[str, Callable[..., decl.BasicDeclaration]] = {
        "VariableDecl": create_variable_decl,
        "RenamingDecl": create_renaming_decl,
    }
    return handlers[declaration.kind_name](declaration, filename, location)


def create_formal_declaration(
    declaration: Expr, filename: Path = None, package: ID = None
) -> decl.FormalDeclaration:
    location = node_location(declaration, filename)
    handlers: Dict[str, Callable[..., decl.FormalDeclaration]] = {
        "FormalChannelDecl": create_channel_decl,
        "FormalFunctionDecl": create_function_decl,
        "FormalPrivateTypeDecl": create_private_type_decl,
    }
    return handlers[declaration.kind_name](declaration, filename, package, location)


def create_math_expression(
    expression: Expr, filename: Path = None, package: ID = None
) -> expr.Expr:
    location = node_location(expression, filename)
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
        "ArrayAggregate": create_array_aggregate,
    }
    return handlers[expression.kind_name](expression, filename, package, location)


def create_bool_expression(
    expression: Expr, filename: Path = None, package: ID = None
) -> expr.Expr:
    location = node_location(expression, filename)
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
    return handlers[expression.kind_name](expression, filename, package, location)


def create_modular(
    identifier: ID,
    modular: ModularTypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _cache: Cache,
    filename: Path,
) -> model.ModularInteger:
    return model.ModularInteger(
        identifier,
        create_math_expression(modular.f_mod, filename, identifier.parent),
        type_location(identifier, modular),
    )


def create_range(
    identifier: ID,
    rangetype: RangeTypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _cache: Cache,
    filename: Path,
) -> model.RangeInteger:
    if rangetype.f_size.f_identifier.text != "Size":
        fail(
            f"invalid aspect {rangetype.f_size.f_identifier.text} for range type {identifier}",
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(rangetype, filename),
        )
    size = create_math_expression(rangetype.f_size.f_value, filename, identifier.parent)
    return model.RangeInteger(
        identifier,
        create_math_expression(rangetype.f_first, filename, identifier.parent),
        create_math_expression(rangetype.f_last, filename, identifier.parent),
        size,
        type_location(identifier, rangetype),
    )


def create_null_message(
    identifier: ID,
    message: MessageTypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _cache: Cache,
    _filename: Path,
) -> model.Message:
    return model.Message(identifier, [], {}, location=type_location(identifier, message))


def create_message(
    identifier: ID,
    message: MessageTypeDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    cache: Cache,
    filename: Path,
) -> model.Message:

    error = RecordFluxError()
    components = message.f_components

    field_types: Mapping[model.Field, model.Type] = create_message_types(
        identifier, components, types, filename
    )
    structure = create_message_structure(components, identifier.parent, error, filename)
    aspects = {
        ID("Checksum"): create_message_aspects(message.f_checksums, identifier.parent, filename)
    }

    try:
        result = create_proven_message(
            model.UnprovenMessage(
                identifier, structure, field_types, aspects, type_location(identifier, message)
            ).merged(),
            skip_verification,
            cache,
        )
    except RecordFluxError as e:
        error.extend(e)
    error.propagate()
    return result


def create_message_types(
    identifier: ID,
    components: Components,
    types: Sequence[model.Type],
    filename: Path = None,
) -> Mapping[model.Field, model.Type]:

    field_types: Dict[model.Field, model.Type] = {}

    for component in components.f_components:
        type_identifier = model.qualified_type_identifier(
            create_id(component.f_type_identifier, filename), identifier.parent
        )
        field_type = [t for t in types if t.identifier == type_identifier]
        if field_type:
            field_types[model.Field(create_id(component.f_identifier, filename))] = field_type[0]

    return field_types


def create_message_structure(
    components: Components, _package: ID, error: RecordFluxError, filename: Path = None
) -> List[model.Link]:
    # pylint: disable=too-many-branches, too-many-locals

    def extract_aspect(aspects: List[Aspect]) -> Tuple[expr.Expr, expr.Expr]:
        size: expr.Expr = expr.UNDEFINED
        first: expr.Expr = expr.UNDEFINED
        for aspect in aspects:
            if aspect.f_identifier.text == "Size":
                size = create_math_expression(aspect.f_value, filename)
            elif aspect.f_identifier.text == "First":
                first = create_math_expression(aspect.f_value, filename)
            else:
                error.append(
                    f'invalid aspect "{aspect.f_identifier.text}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(aspect.f_identifier, filename),
                )
        return size, first

    def extract_then(
        then: ThenNode,
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

    if components.f_initial_component:
        structure.append(
            model.Link(model.INITIAL, *extract_then(components.f_initial_component.f_then))
        )
    else:
        structure.append(
            model.Link(
                model.INITIAL,
                model.Field(create_id(components.f_components[0].f_identifier, filename)),
            )
        )

    for i, component in enumerate(components.f_components):
        source_node = (
            model.Field(create_id(component.f_identifier, filename))
            if component.f_identifier
            else model.INITIAL
        )
        component_identifier = create_id(component.f_identifier, filename)
        if component.f_identifier.text.lower() == "message":
            error.append(
                'reserved word "Message" used as identifier',
                Subsystem.PARSER,
                Severity.ERROR,
                component_identifier.location,
            )
            continue

        if len(component.f_thens) == 0:
            target_id = (
                create_id(components.f_components[i + 1].f_identifier, filename)
                if i + 1 < len(components.f_components)
                else None
            )
            target_node = model.Field(target_id) if target_id else model.FINAL
            structure.append(model.Link(source_node, target_node))

        condition = (
            create_bool_expression(component.f_condition, filename)
            if component.f_condition
            else expr.TRUE
        )
        size, first = extract_aspect(component.f_aspects)
        if first != expr.UNDEFINED or size != expr.UNDEFINED or condition != expr.TRUE:
            for l in (l for l in structure if l.target.identifier == component_identifier):
                if first != expr.UNDEFINED:
                    if l.first == expr.UNDEFINED:
                        l.first = first
                    else:
                        error.append(
                            f'first aspect of field "{component_identifier}"'
                            " conflicts with previous"
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            first.location,
                        )
                        error.append(
                            "previous specification of first",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.first.location,
                        )

                if size != expr.UNDEFINED:
                    if l.size == expr.UNDEFINED:
                        l.size = size
                    else:
                        error.append(
                            f'size aspect of field "{component_identifier}" conflicts with previous'
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            size.location,
                        )
                        error.append(
                            "previous specification of size",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.size.location,
                        )

                if condition != expr.TRUE:
                    l.condition = (
                        expr.And(condition, l.condition, location=l.condition.location)
                        if l.condition != expr.TRUE
                        else condition
                    )

        for then in component.f_thens:
            if then.f_target.kind_name != "NullID" and not any(
                then.f_target.text == c.f_identifier.text for c in components.f_components
            ):
                error.append(
                    f'undefined field "{then.f_target.text}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(then.f_target, filename) if then.f_target else None,
                )
                continue
            structure.append(model.Link(source_node, *extract_then(then)))

    return structure


def create_message_aspects(
    checksum: ChecksumAspect, _package: ID = None, filename: Path = None
) -> Mapping[ID, Sequence[expr.Expr]]:
    result = {}
    if checksum:
        for assoc in checksum.f_associations:
            exprs = []
            for value in assoc.f_covered_fields:
                if value.kind_name == "ChecksumVal":
                    exprs.append(create_math_expression(value.f_data, filename))
                elif value.kind_name == "ChecksumValueRange":
                    exprs.append(
                        expr.ValueRange(
                            create_math_expression(value.f_first, filename),
                            create_math_expression(value.f_last, filename),
                        )
                    )
                else:
                    raise NotImplementedError(f"Invalid checksum association {value.kind_name}")
            result[create_id(assoc.f_identifier, filename)] = exprs
    return result


def create_derived_message(
    identifier: ID,
    derivation: TypeDerivationDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    cache: Cache,
    filename: Path,
) -> model.Message:
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
        error.append(
            f'illegal derivation "{identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            identifier.location,
        )
        error.append(
            f'invalid base message type "{base_name}"',
            Subsystem.PARSER,
            Severity.INFO,
            base_types[0].identifier.location,
        )
        error.propagate()

    return create_proven_message(
        model.UnprovenDerivedMessage(
            identifier, base_messages[0], location=type_location(identifier, derivation)
        ).merged(),
        skip_verification,
        cache,
    )


def create_enumeration(
    identifier: ID,
    enumeration: EnumerationTypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _cache: Cache,
    filename: Path,
) -> model.Enumeration:
    literals: List[Tuple[StrID, expr.Number]] = []
    error = RecordFluxError()

    def create_aspects(aspects: List[Aspect]) -> Tuple[expr.Expr, bool]:
        always_valid = False
        size = None
        for a in aspects:
            if a.f_identifier.text == "Size":
                size = create_math_expression(a.f_value, filename, identifier.parent)
            if a.f_identifier.text == "Always_Valid":
                if a.f_value:
                    av_expr = create_bool_expression(a.f_value, filename)
                    if av_expr == expr.Variable("True"):
                        always_valid = True
                    elif av_expr == expr.Variable("False"):
                        always_valid = False
                    else:
                        error.append(
                            f"invalid Always_Valid expression: {av_expr}",
                            Subsystem.PARSER,
                            Severity.ERROR,
                            node_location(a.f_value, filename),
                        )
                else:
                    always_valid = True
        if not size:
            error.append(
                f'no size set for "{identifier}"',
                Subsystem.PARSER,
                Severity.ERROR,
                identifier.location,
            )
        error.propagate()
        assert size
        return size, always_valid

    if enumeration.f_elements.kind_name == "NamedEnumerationDef":
        for e in enumeration.f_elements.f_elements:
            element_identifier = create_id(e.f_identifier, filename)
            value = create_math_expression(e.f_literal, filename)
            assert isinstance(value, expr.Number)
            literals.append((element_identifier, value))
    elif enumeration.f_elements.kind_name == "PositionalEnumerationDef":
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
    unproven_message: model.UnprovenMessage, skip_verification: bool, cache: Cache
) -> model.Message:
    proven_message = unproven_message.proven(
        skip_verification or cache.is_verified(unproven_message)
    )

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(
    refinement: RefinementDecl, package: ID, types: Sequence[model.Type], filename: Path
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


def check_naming(
    error: RecordFluxError, package: PackageNode, _filename: Path, origname: Path = None
) -> None:
    name = origname or Path("<stdin>")
    identifier = package.f_identifier.text
    if identifier.startswith("RFLX"):
        error.append(
            f'illegal prefix "RFLX" in package identifier "{identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_identifier, name),
        )
    if identifier != package.f_end_identifier.text:
        error.append(
            f'inconsistent package identifier "{package.f_end_identifier.text}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_end_identifier, name),
        )
        error.append(
            f'previous identifier was "{identifier}"',
            Subsystem.PARSER,
            Severity.INFO,
            node_location(package.f_identifier, name),
        )
    if origname:
        expected_filename = f"{identifier.lower()}.rflx"
        if origname.name != expected_filename:
            error.append(
                f'file name does not match unit name "{identifier}",'
                f' should be "{expected_filename}"',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(package.f_identifier, origname),
            )
    for t in package.f_declarations:
        if isinstance(t, TypeDecl) and model.is_builtin_type(create_id(t.f_identifier, name).name):
            error.append(
                f'illegal redefinition of built-in type "{t.f_identifier.text}"',
                Subsystem.MODEL,
                Severity.ERROR,
                node_location(t, name),
            )


class Parser:
    def __init__(self, skip_verification: bool = False, cached: bool = False) -> None:
        self.skip_verification = skip_verification
        self.__specifications: OrderedDict[Path, Specification] = OrderedDict()
        self.__types: List[model.Type] = [
            *model.BUILTIN_TYPES.values(),
            *model.INTERNAL_TYPES.values(),
        ]
        self.__sessions: List[model.Session] = []
        self.__cache = Cache(cached)

    def __convert_unit(
        self, spec: Specification, specfile: Path = None, transitions: List[ID] = None
    ) -> RecordFluxError:
        transitions = transitions or []
        error = RecordFluxError()

        if spec:
            parent = specfile.parent if specfile else Path(".")
            filename = parent / Path(f"{spec.f_package_declaration.f_identifier.text.lower()}.rflx")
            self.__specifications[filename] = (specfile, spec)
            for context in spec.f_context_clause:
                item = create_id(context.f_item, specfile)
                if item in transitions:
                    error.append(
                        f'dependency cycle when including "{transitions[0]}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        transitions[0].location,
                    )
                    error.extend(
                        [
                            (
                                f'when including "{i}"',
                                Subsystem.PARSER,
                                Severity.INFO,
                                i.location,
                            )
                            for i in transitions[1:] + [item]
                        ]
                    )
                    continue
                withed_file = filename.parent / f"{str(item).lower()}.rflx"
                error.extend(self.__parse_specfile(withed_file, transitions + [item]))

        return error

    def __parse_specfile(self, specfile: Path, transitions: List[ID] = None) -> RecordFluxError:
        error = RecordFluxError()
        if specfile in self.__specifications:
            self.__specifications.move_to_end(specfile)
            return error

        transitions = transitions or []

        log.info("Parsing %s", specfile)
        unit = AnalysisContext().get_from_file(str(specfile))
        if diagnostics_to_error(unit.diagnostics, error, specfile):
            return error
        return self.__convert_unit(unit.root, specfile, transitions)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        for f in specfiles:
            error.extend(self.__parse_specfile(f))

        for f, (origname, spec) in self.__specifications.items():
            check_naming(error, spec.f_package_declaration, f, origname)
        error.propagate()

    def parse_string(
        self,
        string: str,
        rule: GrammarRule = GrammarRule.main_rule_rule,
    ) -> None:
        error = RecordFluxError()
        unit = AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)
        if not diagnostics_to_error(unit.diagnostics, error):
            error = self.__convert_unit(unit.root)
            for f, (origname, spec) in self.__specifications.items():
                check_naming(error, spec.f_package_declaration, f, origname)
        error.propagate()

    def create_model(self) -> model.Model:
        error = RecordFluxError()
        for origname, specification in reversed(self.__specifications.values()):
            try:
                self.__evaluate_specification(specification, origname)
            except RecordFluxError as e:
                error.extend(e)
        try:
            result = model.Model(self.__types, self.__sessions)
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()
        return result

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {
            spec.f_package_declaration.f_identifier.text: spec
            for _, spec in self.__specifications.values()
        }

    def __evaluate_specification(self, spec: Specification, filename: Path) -> None:
        handlers = {
            "ArrayTypeDef": create_array,
            "ModularTypeDef": create_modular,
            "RangeTypeDef": create_range,
            "MessageTypeDef": create_message,
            "NullMessageTypeDef": create_null_message,
            "TypeDerivationDef": create_derived_message,
            "EnumerationTypeDef": create_enumeration,
        }
        log.info("Processing %s", spec.f_package_declaration.f_identifier.text)
        error = RecordFluxError()
        package_id = create_id(spec.f_package_declaration.f_identifier, filename)

        for t in spec.f_package_declaration.f_declarations:
            if isinstance(t, TypeDecl):
                identifier = model.qualified_type_identifier(
                    create_id(t.f_identifier, filename), package_id
                )
                try:
                    new_type = handlers[t.f_definition.kind_name](
                        identifier,
                        t.f_definition,
                        self.__types,
                        self.skip_verification,
                        self.__cache,
                        filename,
                    )
                    self.__types.append(new_type)
                    error.extend(new_type.error)
                except RecordFluxError as e:
                    error.extend(e)
            else:
                if t.kind_name == "RefinementDecl":
                    try:
                        new_type = create_refinement(t, package_id, self.__types, filename)
                        self.__types.append(new_type)
                        error.extend(new_type.error)
                    except RecordFluxError as e:
                        error.extend(e)
                elif t.kind_name == "SessionDecl":
                    new_session = create_session(t, package_id, self.__types, filename)
                    self.__sessions.append(new_session)
                    error.extend(new_session.error)
                else:
                    raise NotImplementedError(f"Declaration kind {t.kind_name} unsupported")
        error.propagate()
