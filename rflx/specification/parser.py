# pylint: disable=too-many-lines

from __future__ import annotations

import itertools
import logging
import textwrap
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, List, Mapping, Optional, Sequence, Set, Tuple, Type, Union

import librflxlang as lang

from rflx import expression as expr, model
from rflx.common import STDIN, unique
from rflx.error import Location, RecordFluxError, Severity, Subsystem, warn
from rflx.identifier import ID, StrID
from rflx.integration import Integration
from rflx.model import declaration as decl, statement as stmt
from rflx.specification.const import RESERVED_WORDS

from . import style
from .cache import Cache

log = logging.getLogger(__name__)


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


def create_transition(
    error: RecordFluxError, transition: lang.Transition, filename: Path
) -> model.Transition:
    if transition.kind_name not in ("Transition", "ConditionalTransition"):
        raise NotImplementedError(f"Transition kind {transition.kind_name} unsupported")
    target = create_id_or_null(error, transition.f_target, filename)
    condition: expr.Expr = expr.TRUE
    description = create_description(transition.f_description)
    if isinstance(transition, lang.ConditionalTransition):
        condition = create_bool_expression(error, transition.f_condition, filename)
    return model.Transition(target, condition, description, node_location(transition, filename))


def create_reset(error: RecordFluxError, reset: lang.Statement, filename: Path) -> stmt.Statement:
    assert isinstance(reset, lang.Reset)
    return stmt.Reset(
        create_id(error, reset.f_identifier, filename),
        {
            create_id(error, c.f_identifier, filename): create_expression(
                error, c.f_expression, filename
            )
            for c in reset.f_associations
        },
        location=node_location(reset, filename),
    )


def create_assignment(
    error: RecordFluxError, assignment: lang.Statement, filename: Path
) -> stmt.Statement:
    assert isinstance(assignment, lang.Assignment)
    return stmt.VariableAssignment(
        create_id(error, assignment.f_identifier, filename),
        create_expression(error, assignment.f_expression, filename),
        location=node_location(assignment, filename),
    )


def create_message_field_assignment(
    error: RecordFluxError, assignment: lang.Statement, filename: Path
) -> stmt.Statement:
    assert isinstance(assignment, lang.MessageFieldAssignment)
    return stmt.MessageFieldAssignment(
        create_id(error, assignment.f_message, filename),
        create_id(error, assignment.f_field, filename),
        create_expression(error, assignment.f_expression, filename),
        location=node_location(assignment, filename),
    )


def create_attribute_statement(
    error: RecordFluxError, expression: lang.Statement, filename: Path
) -> stmt.Statement:
    assert isinstance(expression, lang.AttributeStatement)
    attrs = {
        "Append": stmt.Append,
        "Extend": stmt.Extend,
        "Read": stmt.Read,
        "Write": stmt.Write,
    }
    constructor = attrs[expression.f_attr.text]

    return constructor(
        create_id(error, expression.f_identifier, filename),
        create_expression(error, expression.f_expression, filename),
        location=node_location(expression, filename),
    )


def create_statement(
    error: RecordFluxError, statement: lang.Statement, filename: Path
) -> stmt.Statement:
    handlers = {
        "Reset": create_reset,
        "Assignment": create_assignment,
        "MessageFieldAssignment": create_message_field_assignment,
        "AttributeStatement": create_attribute_statement,
    }
    return handlers[statement.kind_name](error, statement, filename)


def create_state(
    error: RecordFluxError, state: lang.State, package: ID, filename: Path
) -> model.State:
    location = node_location(state, filename)
    identifier = create_id(error, state.f_identifier, filename)
    assert isinstance(state.f_body, lang.StateBody)
    if state.f_identifier.text != state.f_body.f_end_identifier.text:
        error.extend(
            [
                (
                    "inconsistent state identifier: "
                    f"{state.f_identifier.text} /= {state.f_body.f_end_identifier.text}",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    location,
                )
            ]
        )
    transitions = []
    for t in state.f_body.f_conditional_transitions:
        transitions.append(create_transition(error, t, filename))
    transitions.append(create_transition(error, state.f_body.f_final_transition, filename))
    exception_transition = (
        create_transition(error, state.f_body.f_exception_transition, filename)
        if state.f_body.f_exception_transition
        else None
    )
    actions = []
    for a in state.f_body.f_actions:
        actions.append(create_statement(error, a, filename))
    declarations = []
    for d in state.f_body.f_declarations:
        declarations.append(create_declaration(error, d, package, filename))
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


def _check_session_identifier(
    error: RecordFluxError, session: lang.SessionDecl, filename: Path
) -> None:
    if session.f_identifier.text != session.f_end_identifier.text:
        error.extend(
            [
                (
                    "inconsistent session identifier: "
                    f"{session.f_identifier.text} /= {session.f_end_identifier.text}",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(session, filename),
                )
            ]
        )


def create_unproven_session(
    error: RecordFluxError,
    session: lang.SessionDecl,
    package: ID,
    filename: Path,
    types: Sequence[model.Type] = None,
) -> model.UnprovenSession:
    _check_session_identifier(error, session, filename)

    return model.UnprovenSession(
        package * create_id(error, session.f_identifier, filename),
        [create_state(error, s, package, filename) for s in session.f_states],
        [create_declaration(error, d, package, filename) for d in session.f_declarations],
        [create_formal_declaration(error, p, package, filename) for p in session.f_parameters],
        types or [],
        node_location(session, filename),
    )


def create_session(
    error: RecordFluxError,
    session: lang.SessionDecl,
    package: ID,
    filename: Path,
    types: Sequence[model.Type] = None,
) -> Optional[model.Session]:
    try:
        return create_unproven_session(error, session, package, filename, types).proven()
    except RecordFluxError as e:
        error.extend(e)

    return None


def create_id(error: RecordFluxError, identifier: lang.AbstractID, filename: Path) -> ID:
    if isinstance(identifier, lang.UnqualifiedID):
        if identifier.text.lower() in RESERVED_WORDS:
            error.extend(
                [
                    (
                        f'reserved word "{identifier.text}" used as identifier',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(identifier, filename),
                    )
                ]
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

    raise NotImplementedError(f"Invalid ID: {identifier.text} {type(identifier)}")


def create_id_or_null(error: RecordFluxError, identifier: lang.AbstractID, filename: Path) -> ID:
    if isinstance(identifier, lang.UnqualifiedID) and identifier.text.lower() == "null":
        return ID("null", location=node_location(identifier, filename))
    return create_id(error, identifier, filename)


def create_sequence(
    error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    sequence: lang.TypeDef,
    types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    assert isinstance(sequence, lang.SequenceTypeDef)
    element_identifier = model.internal_type_identifier(
        create_id(error, sequence.f_element_type, filename), identifier.parent
    )

    try:
        element_type = next(t for t in types if element_identifier == t.identifier)
    except StopIteration:
        error.extend(
            [
                (
                    f'undefined element type "{element_identifier}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    element_identifier.location,
                )
            ]
        )
        return None

    result = model.Sequence(identifier, element_type, type_location(identifier, sequence))
    error.extend(result.error)
    return result


def create_numeric_literal(
    _error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
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


def create_binop(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    loc = node_location(expression, filename)
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(error, expression.f_left, filename),
            create_expression(error, expression.f_right, filename),
            location=loc,
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_expression(error, expression.f_left, filename),
            create_expression(error, expression.f_right, filename),
            location=loc,
        )

    left = create_math_expression(error, expression.f_left, filename)
    right = create_math_expression(error, expression.f_right, filename)
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


def create_math_binop(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](
            create_math_expression(error, expression.f_left, filename),
            create_math_expression(error, expression.f_right, filename),
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


def create_bool_binop(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    if expression.f_op.kind_name in MATH_COMPARISONS:
        return MATH_COMPARISONS[expression.f_op.kind_name](
            create_math_expression(error, expression.f_left, filename),
            create_math_expression(error, expression.f_right, filename),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in BOOLEAN_OPERATIONS:
        return BOOLEAN_OPERATIONS[expression.f_op.kind_name](
            create_bool_expression(error, expression.f_left, filename),
            create_bool_expression(error, expression.f_right, filename),
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in OPERATIONS:
        return OPERATIONS[expression.f_op.kind_name](
            create_expression(error, expression.f_left, filename),
            create_expression(error, expression.f_right, filename),
            location=node_location(expression, filename),
        )
    raise NotImplementedError(
        f"Invalid bool BinOp {expression.f_op.kind_name} => {expression.text}"
    )


def create_paren_bool_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_bool_expression(error, expression.f_data, filename)


def create_paren_math_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_math_expression(error, expression.f_data, filename)


def create_paren_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_expression(error, expression.f_data, filename)


def create_variable(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Variable)
    location = node_location(expression, filename)
    if expression.f_identifier.text.lower() in ("true", "false"):
        return expr.Variable(create_id(error, expression.f_identifier, filename), location=location)
    return expr.Variable(create_id(error, expression.f_identifier, filename), location=location)


def create_math_attribute(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.Attribute)
    inner = create_expression(error, expression.f_expression, filename)
    if expression.f_kind.kind_name == "AttrLast":
        return expr.Last(inner)
    if expression.f_kind.kind_name == "AttrFirst":
        return expr.First(inner)
    if expression.f_kind.kind_name == "AttrSize":
        return expr.Size(inner)
    raise NotImplementedError(
        f"Invalid math attribute: {expression.f_kind.kind_name} => {expression.text}"
    )


def create_attribute(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Attribute)
    inner = create_expression(error, expression.f_expression, filename)
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


def create_sequence_aggregate(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.SequenceAggregate)
    return expr.Aggregate(
        *[create_math_expression(error, v, filename) for v in expression.f_values],
        location=node_location(expression, filename),
    )


def create_string_literal(
    _error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    return expr.String(
        expression.text.split('"')[1],
        location=node_location(expression, filename),
    )


def create_call(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Call)
    return expr.Call(
        create_id(error, expression.f_identifier, filename),
        [create_expression(error, a, filename) for a in expression.f_arguments],
        location=node_location(expression, filename),
    )


def create_quantified_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.QuantifiedExpression)
    param_id = create_id(error, expression.f_parameter_identifier, filename)
    iterable = create_expression(error, expression.f_iterable, filename)
    predicate = create_expression(error, expression.f_predicate, filename)
    location = node_location(expression, filename)
    if expression.f_operation.kind_name == "QuantifierAll":
        return expr.ForAllIn(param_id, iterable, predicate, location)
    if expression.f_operation.kind_name == "QuantifierSome":
        return expr.ForSomeIn(param_id, iterable, predicate, location)

    raise NotImplementedError(f"Invalid quantified: {expression.f_operation.text}")


def create_binding(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Binding)
    bindings: Mapping[Union[str, ID], expr.Expr] = {
        create_id(error, b.f_identifier, filename): create_expression(
            error, b.f_expression, filename
        )
        for b in expression.f_bindings
    }
    return expr.Binding(
        create_expression(error, expression.f_expression, filename),
        bindings,
        node_location(expression, filename),
    )


def create_variable_decl(
    error: RecordFluxError, declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    assert isinstance(declaration, lang.VariableDecl)
    initializer = (
        create_expression(error, declaration.f_initializer, filename)
        if declaration.f_initializer
        else None
    )
    return decl.VariableDeclaration(
        create_id(error, declaration.f_identifier, filename),
        model.internal_type_identifier(
            create_id(error, declaration.f_type_identifier, filename), package
        ),
        initializer,
        location=node_location(declaration, filename),
    )


def create_channel_decl(
    error: RecordFluxError,
    declaration: lang.FormalDecl,
    _package: ID,
    filename: Path,
) -> decl.FormalDeclaration:
    assert isinstance(declaration, lang.FormalChannelDecl)
    readable = False
    writable = False

    grouped = defaultdict(list)
    for p in declaration.f_parameters:
        grouped[p.kind_name].append(node_location(p, filename))

    for name, locations in grouped.items():
        check_duplicate_aspect(error, name, locations)
        if name == "Readable":
            readable = True
        elif name == "Writable":
            writable = True
        else:
            raise NotImplementedError(f"channel parameter: {name}")
    return decl.ChannelDeclaration(
        create_id(error, declaration.f_identifier, filename),
        readable=readable,
        writable=writable,
        location=node_location(declaration, filename),
    )


def create_renaming_decl(
    error: RecordFluxError, declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    assert isinstance(declaration, lang.RenamingDecl)
    selected = create_expression(error, declaration.f_expression, filename)
    assert isinstance(selected, expr.Selected)
    return decl.RenamingDeclaration(
        create_id(error, declaration.f_identifier, filename),
        model.internal_type_identifier(
            create_id(error, declaration.f_type_identifier, filename), package
        ),
        selected,
        location=node_location(declaration, filename),
    )


def create_function_decl(
    error: RecordFluxError,
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
                    create_id(error, p.f_identifier, filename),
                    model.internal_type_identifier(
                        create_id(error, p.f_type_identifier, filename), package
                    ),
                )
            )
    return decl.FunctionDeclaration(
        create_id(error, declaration.f_identifier, filename),
        arguments,
        model.internal_type_identifier(
            create_id(error, declaration.f_return_type_identifier, filename), package
        ),
        location=node_location(declaration, filename),
    )


def create_negation(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Negation)
    math_expr = create_math_expression(error, expression.f_data, filename)
    assert isinstance(math_expr, expr.Number)
    return expr.Number(-math_expr.value, math_expr.base, node_location(expression, filename))


def create_concatenation(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.Concatenation)
    left = create_expression(error, expression.f_left, filename)
    right = create_expression(error, expression.f_right, filename)
    assert isinstance(left, expr.Aggregate)
    assert isinstance(right, expr.Aggregate)
    return expr.Aggregate(
        *(left.elements + right.elements), location=node_location(expression, filename)
    )


def create_comprehension(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.Comprehension)
    condition = (
        create_bool_expression(error, expression.f_condition, filename)
        if expression.f_condition
        else expr.TRUE
    )
    return expr.Comprehension(
        create_id(error, expression.f_iterator, filename),
        create_expression(error, expression.f_sequence, filename),
        create_expression(error, expression.f_selector, filename),
        condition,
        node_location(expression, filename),
    )


def create_selected(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.SelectNode)
    return expr.Selected(
        create_expression(error, expression.f_expression, filename),
        create_id(error, expression.f_selector, filename),
        location=node_location(expression, filename),
    )


def create_case(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.CaseExpression)

    def create_choice(
        value: Union[lang.AbstractID, lang.Expr], filename: Path
    ) -> Union[ID, expr.Number]:
        if isinstance(value, lang.AbstractID):
            return create_id(error, value, filename)
        assert isinstance(value, lang.Expr)
        result = create_numeric_literal(error, value, filename)
        assert isinstance(result, expr.Number)
        return result

    choices: List[Tuple[List[Union[ID, expr.Number]], expr.Expr]] = [
        (
            [
                create_choice(s, filename)
                for s in c.f_selectors
                if isinstance(s, (lang.AbstractID, lang.Expr))
            ],
            create_expression(error, c.f_expression, filename),
        )
        for c in expression.f_choices
    ]

    return expr.CaseExpr(
        create_expression(error, expression.f_expression, filename),
        choices,
        location=node_location(expression, filename),
    )


def create_conversion(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Conversion)
    return expr.Conversion(
        create_id(error, expression.f_target_identifier, filename),
        create_expression(error, expression.f_argument, filename),
        location=node_location(expression, filename),
    )


def create_message_aggregate(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    assert isinstance(expression, lang.MessageAggregate)
    values: Mapping[StrID, expr.Expr] = {}
    if isinstance(expression.f_values, lang.NullMessageAggregate):
        values = {}
    elif isinstance(expression.f_values, lang.MessageAggregateAssociations):
        values = {
            create_id(error, c.f_identifier, filename): create_expression(
                error, c.f_expression, filename
            )
            for c in expression.f_values.f_associations
        }
    else:
        raise NotImplementedError(f"invalid message field: {type(expression.f_values)}")

    return expr.MessageAggregate(
        create_id(error, expression.f_identifier, filename),
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
    "CaseExpression": create_case,
}


def create_expression(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    return EXPRESSION_MAP[expression.kind_name](error, expression, filename)


def create_declaration(
    error: RecordFluxError, declaration: lang.LocalDecl, package: ID, filename: Path
) -> decl.BasicDeclaration:
    handlers = {
        "VariableDecl": create_variable_decl,
        "RenamingDecl": create_renaming_decl,
    }
    return handlers[declaration.kind_name](error, declaration, package, filename)


def create_formal_declaration(
    error: RecordFluxError, declaration: lang.FormalDecl, package: ID, filename: Path
) -> decl.FormalDeclaration:
    handlers = {
        "FormalChannelDecl": create_channel_decl,
        "FormalFunctionDecl": create_function_decl,
    }
    return handlers[declaration.kind_name](error, declaration, package, filename)


def create_math_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
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
    return handlers[expression.kind_name](error, expression, filename)


def create_bool_expression(
    error: RecordFluxError, expression: lang.Expr, filename: Path
) -> expr.Expr:
    handlers = {
        "BinOp": create_bool_binop,
        "ParenExpression": create_paren_bool_expression,
        "Attribute": create_attribute,
        "Call": create_call,
        "Variable": create_variable,
        "QuantifiedExpression": create_quantified_expression,
        "Binding": create_binding,
        "SelectNode": create_selected,
        "CaseExpression": create_case,
    }
    return handlers[expression.kind_name](error, expression, filename)


def create_modular(
    error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    modular: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    assert isinstance(modular, lang.ModularTypeDef)
    result = model.ModularInteger(
        identifier,
        create_math_expression(error, modular.f_mod, filename),
        type_location(identifier, modular),
    )
    error.extend(result.error)
    return result


def create_range(
    error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    rangetype: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    assert isinstance(rangetype, lang.RangeTypeDef)
    if rangetype.f_size.f_identifier.text != "Size":
        error.extend(
            [
                (
                    f"invalid aspect {rangetype.f_size.f_identifier.text} "
                    f"for range type {identifier}",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(rangetype, filename),
                )
            ]
        )
    size = create_math_expression(error, rangetype.f_size.f_value, filename)
    result = model.RangeInteger(
        identifier,
        create_math_expression(error, rangetype.f_first, filename),
        create_math_expression(error, rangetype.f_last, filename),
        size,
        type_location(identifier, rangetype),
    )
    error.extend(result.error)
    return result


def create_null_message(
    _error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    message: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    _filename: Path,
) -> Optional[model.Type]:
    assert isinstance(message, lang.NullMessageTypeDef)
    return model.Message(identifier, [], {}, location=type_location(identifier, message))


def create_message(
    error: RecordFluxError,
    identifier: ID,
    parameters: lang.Parameters,
    message: lang.TypeDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    workers: int,
    cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    # pylint: disable = too-many-arguments, too-many-locals

    assert isinstance(message, lang.MessageTypeDef)
    fields = message.f_message_fields

    field_types, message_arguments = create_message_types(
        error, identifier, parameters, fields, types, filename
    )
    structure = create_message_structure(error, fields, filename)
    checksum_aspects, byte_order_aspect = parse_aspects(error, message.f_aspects, filename)

    unproven_message = model.UnprovenMessage(
        identifier,
        structure,
        field_types,
        checksum_aspects,
        byte_order_aspect,
        type_location(identifier, message),
    ).merged(message_arguments)

    return create_proven_message(
        error,
        unproven_message,
        skip_verification,
        workers,
        cache,
    )


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
        qualified_type_identifier = model.internal_type_identifier(
            create_id(error, type_identifier, filename), identifier.parent
        )
        field_type = next((t for t in types if t.identifier == qualified_type_identifier), None)
        if field_type:
            field = model.Field(create_id(error, field_identifier, filename))
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
            arg_id = create_id(error, arg.f_identifier, filename)
            arg_expression = create_expression(error, arg.f_expression, filename)
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

        grouped = defaultdict(list)
        for aspect in aspects:
            grouped[aspect.f_identifier.text].append(
                (aspect, node_location(aspect.f_identifier, filename))
            )

        for name, locations in grouped.items():
            check_duplicate_aspect(error, name, [l for _, l in locations])
            aspect, location = locations[0]

            if name == "Size":
                size = create_math_expression(error, aspect.f_value, filename)
            elif name == "First":
                first = create_math_expression(error, aspect.f_value, filename)
            else:
                error.extend(
                    [
                        (
                            f'invalid aspect "{name}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            location,
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
            else model.Field(create_id(error, then.f_target, filename))
        )
        condition = (
            create_bool_expression(error, then.f_condition, filename)
            if then.f_condition
            else expr.TRUE
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
                model.Field(create_id(error, fields.f_fields[0].f_identifier, filename)),
                location=node_location(fields.f_fields[0].f_identifier, filename),
            )
        )

    for i, field in enumerate(fields.f_fields):
        source_node = (
            model.Field(create_id(error, field.f_identifier, filename))
            if field.f_identifier
            else model.INITIAL
        )
        field_identifier = create_id(error, field.f_identifier, filename)
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
                create_id(error, fields.f_fields[i + 1].f_identifier, filename)
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
            if then.f_target.text.lower() != "null" and not any(
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

        if error.check():
            continue

        merge_field_aspects(error, field_identifier, structure, *extract_aspect(field.f_aspects))
        merge_field_condition(
            field_identifier,
            structure,
            create_bool_expression(error, field.f_condition, filename)
            if field.f_condition
            else expr.TRUE,
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


def check_duplicate_aspect(error: RecordFluxError, name: str, locations: List[Location]) -> None:
    if len(locations) > 1:
        error.extend(
            [
                (
                    f'duplicate aspect "{name}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    locations[1],
                ),
                *[
                    (
                        "previous location",
                        Subsystem.PARSER,
                        Severity.INFO,
                        l,
                    )
                    for l in locations[:-1]
                ],
            ],
        )


def parse_aspects(
    error: RecordFluxError, aspects: lang.MessageAspectList, filename: Path
) -> Tuple[Mapping[ID, Sequence[expr.Expr]], Optional[model.ByteOrder]]:
    # pylint: disable=too-many-branches
    checksum_result = {}
    byte_order_result = None

    grouped = defaultdict(list)
    for aspect in aspects:
        if isinstance(aspect, lang.ByteOrderAspect):
            name = "Byte_Order"
        elif isinstance(aspect, lang.ChecksumAspect):
            name = "Checksum"
        else:
            raise NotImplementedError(f"Message aspect {type(aspect)} unsupported")
        grouped[name].append((aspect, node_location(aspect, filename)))

    for name, locations in grouped.items():
        check_duplicate_aspect(error, name, [l for _, l in locations])
        aspect, _ = locations[0]

        if isinstance(aspect, lang.ChecksumAspect):
            for assoc in aspect.f_associations:
                exprs = []
                for value in assoc.f_covered_fields:
                    if isinstance(value, lang.ChecksumVal):
                        exprs.append(create_math_expression(error, value.f_data, filename))
                    elif isinstance(value, lang.ChecksumValueRange):
                        exprs.append(
                            expr.ValueRange(
                                create_math_expression(error, value.f_first, filename),
                                create_math_expression(error, value.f_last, filename),
                            )
                        )
                    else:
                        raise NotImplementedError(f"Invalid checksum association {value.kind_name}")
                checksum_result[create_id(error, assoc.f_identifier, filename)] = exprs
        if isinstance(aspect, lang.ByteOrderAspect):
            if isinstance(aspect.f_byte_order, lang.ByteOrderTypeLoworderfirst):
                byte_order_result = model.ByteOrder.LOW_ORDER_FIRST
            else:
                byte_order_result = model.ByteOrder.HIGH_ORDER_FIRST

    return checksum_result, byte_order_result


def create_derived_message(
    error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    derivation: lang.TypeDef,
    types: Sequence[model.Type],
    skip_verification: bool,
    workers: int,
    cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    # pylint: disable=too-many-arguments
    assert isinstance(derivation, lang.TypeDerivationDef)
    base_id = create_id(error, derivation.f_base, filename)
    base_name = model.internal_type_identifier(base_id, identifier.parent)

    base_types: Sequence[model.Type] = [t for t in types if t.identifier == base_name]

    if not base_types:
        error.extend(
            [
                (
                    f'undefined base message "{base_name}" in derived message',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    base_name.location,
                )
            ]
        )
        return None

    base_messages: Sequence[model.Message] = [t for t in base_types if isinstance(t, model.Message)]

    if not base_messages:
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
        return None

    try:
        unproven_message = model.UnprovenDerivedMessage(
            identifier, base_messages[0], location=type_location(identifier, derivation)
        ).merged()
    except RecordFluxError as e:
        error.extend(e)
        return None

    return create_proven_message(
        error,
        unproven_message,
        skip_verification,
        workers,
        cache,
    )


def create_enumeration(
    error: RecordFluxError,
    identifier: ID,
    _parameters: lang.Parameters,
    enumeration: lang.TypeDef,
    _types: Sequence[model.Type],
    _skip_verification: bool,
    _workers: int,
    _cache: Cache,
    filename: Path,
) -> Optional[model.Type]:
    assert isinstance(enumeration, lang.EnumerationTypeDef)
    literals: List[Tuple[StrID, expr.Number]] = []

    def create_aspects(aspects: lang.AspectList) -> Optional[Tuple[expr.Expr, bool]]:
        always_valid = False
        size = None

        grouped = defaultdict(list)
        for aspect in aspects:
            grouped[aspect.f_identifier.text].append((aspect, node_location(aspect, filename)))

        for name, locations in grouped.items():
            check_duplicate_aspect(error, name, [l for _, l in locations])
            aspect, _ = locations[0]

            if aspect.f_identifier.text == "Size":
                size = create_math_expression(error, aspect.f_value, filename)
            if aspect.f_identifier.text == "Always_Valid":
                if aspect.f_value:
                    av_expr = create_bool_expression(error, aspect.f_value, filename)
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
                                    node_location(aspect.f_value, filename),
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
            return None

        assert size
        return size, always_valid

    if isinstance(enumeration.f_elements, lang.NamedEnumerationDef):
        for e in enumeration.f_elements.f_elements:
            element_identifier = create_id(error, e.f_identifier, filename)
            value = create_math_expression(error, e.f_literal, filename)
            assert isinstance(value, expr.Number)
            literals.append((element_identifier, value))
    elif isinstance(enumeration.f_elements, lang.PositionalEnumerationDef):
        literals = [
            (create_id(error, e, filename), expr.Number(i))
            for i, e in enumerate(enumeration.f_elements.f_elements)
        ]
    else:
        raise NotImplementedError(
            f"Enumeration kind {enumeration.f_elements.kind_name} unsupported"
        )

    aspects = create_aspects(enumeration.f_aspects)
    if aspects is None:
        return None

    size, always_valid = aspects

    result = model.Enumeration(
        identifier, literals, size, always_valid, location=type_location(identifier, enumeration)
    )
    error.extend(result.error)
    return result


def create_proven_message(
    error: RecordFluxError,
    unproven_message: model.UnprovenMessage,
    skip_verification: bool,
    workers: int,
    cache: Cache,
) -> Optional[model.Message]:
    try:
        proven_message = unproven_message.proven(
            skip_verification or cache.is_verified(unproven_message), workers
        )
    except RecordFluxError as e:
        error.extend(e)
        return None

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(
    error: RecordFluxError,
    refinement: lang.RefinementDecl,
    package: ID,
    types: Sequence[model.Type],
    filename: Path,
) -> Optional[model.Refinement]:
    messages = {t.identifier: t for t in types if isinstance(t, model.Message)}

    pdu = model.internal_type_identifier(create_id(error, refinement.f_pdu, filename), package)
    if pdu not in messages:
        error.extend(
            [
                (
                    f'undefined type "{pdu}" in refinement',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(refinement, filename),
                )
            ]
        )

    sdu = model.internal_type_identifier(create_id(error, refinement.f_sdu, filename), package)
    if sdu not in messages:
        error.extend(
            [
                (
                    f'undefined type "{sdu}" in refinement of "{pdu}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    sdu.location,
                )
            ]
        )

    if refinement.f_condition:
        condition = create_bool_expression(error, refinement.f_condition, filename)
    else:
        condition = expr.TRUE

    refinement_id = model.Field(create_id(error, refinement.f_field, filename))

    if error.check():
        return None

    result = model.Refinement(
        package,
        messages[pdu],
        refinement_id,
        messages[sdu],
        condition,
        node_location(refinement, filename),
    )
    error.extend(result.error)
    return result


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
        # https://github.com/Componolit/RecordFlux/issues/1208
        if isinstance(t, lang.TypeDecl) and model.is_builtin_type(
            create_id(error, t.f_identifier, name).name
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
class ContextClause:
    name: ID
    location: Location

    @property
    def withed_file(self) -> str:
        return f"{str(self.name).lower()}.rflx"


@dataclass(frozen=True)
class SpecificationFile:
    filename: Path
    spec: lang.Specification
    context_clauses: list[ContextClause]

    @staticmethod
    def create(
        error: RecordFluxError,
        spec: lang.Specification,
        filename: Path,
    ) -> SpecificationFile:
        check_naming(error, spec.f_package_declaration, filename)

        return SpecificationFile(
            filename,
            spec,
            [
                ContextClause(
                    create_id(error, context.f_item, filename),
                    node_location(context.f_item, filename),
                )
                for context in spec.f_context_clause
            ],
        )

    @property
    def package(self) -> ID:
        return ID(self.spec.f_package_declaration.f_identifier.text)


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
        self._workers = workers
        self._specifications: OrderedDict[ID, SpecificationFile] = OrderedDict()
        self._types: List[model.Type] = [
            *model.BUILTIN_TYPES.values(),
            *model.INTERNAL_TYPES.values(),
        ]
        self._sessions: List[model.Session] = []
        self._integration: Integration = Integration(integration_files_dir)
        self._cache = Cache(not skip_verification and cached)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        include_paths = []
        specifications = []

        for f in unique(specfiles):
            if f.parent.is_dir() and f.parent not in include_paths:
                include_paths.append(f.parent)

            spec = self._parse_file(error, f)
            if spec:
                specifications.append(spec)

            error.extend(style.check(f))

        _check_for_duplicate_specifications(
            error, [*self._specifications.values(), *specifications]
        )

        self._specifications.update({s.package: s for s in specifications})

        self._parse_withed_files(
            error, [c for s in specifications for c in s.context_clauses], include_paths
        )
        _check_for_dependency_cycles(error, specifications, self._specifications)

        error.propagate()

        self._specifications = _sort_specs_topologically(self._specifications)

    def parse_string(
        self,
        string: str,
        rule: str = lang.GrammarRule.main_rule_rule,
    ) -> None:
        error = RecordFluxError()
        specifications = []
        string = textwrap.dedent(string)
        unit = lang.AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)

        if not diagnostics_to_error(unit.diagnostics, error, STDIN):
            error.extend(style.check_string(string))
            assert isinstance(unit.root, lang.Specification)
            specifications.append(SpecificationFile.create(error, unit.root, STDIN))

        _check_for_duplicate_specifications(
            error, [*self._specifications.values(), *specifications]
        )

        error.propagate()

        self._specifications = _sort_specs_topologically(
            {**self._specifications, **{s.package: s for s in specifications}}
        )

    def create_model(self) -> model.Model:
        error = RecordFluxError()
        for spec_node in self._specifications.values():
            self._evaluate_specification(error, spec_node.spec, spec_node.filename)
        try:
            result = model.Model(self._types, self._sessions)
            self._integration.validate(result, error)
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()
        return result

    def get_integration(self) -> Integration:
        return self._integration

    @property
    def specifications(self) -> Dict[str, lang.Specification]:
        return {
            spec_node.spec.f_package_declaration.f_identifier.text: spec_node.spec
            for spec_node in self._specifications.values()
        }

    def _parse_file(self, error: RecordFluxError, filename: Path) -> Optional[SpecificationFile]:
        log.info("Parsing %s", filename)
        unit = lang.AnalysisContext().get_from_file(str(filename))

        if diagnostics_to_error(unit.diagnostics, error, filename) or not unit.root:
            return None

        assert isinstance(unit.root, lang.Specification)

        self._integration.load_integration_file(filename, error)

        return SpecificationFile.create(error, unit.root, filename)

    def _parse_withed_files(
        self,
        error: RecordFluxError,
        context_clauses: Sequence[ContextClause],
        include_paths: Sequence[Path],
    ) -> None:
        if not context_clauses:
            return

        nested_context_clauses: list[ContextClause] = []
        for context_clause in context_clauses:
            if context_clause.name in self._specifications:
                continue
            for path in include_paths:
                f = path / context_clause.withed_file
                if f.exists():
                    spec = self._parse_file(error, f)
                    if spec:
                        self._specifications[spec.package] = spec
                        nested_context_clauses.extend(
                            [
                                c
                                for c in spec.context_clauses
                                if c
                                not in [
                                    *context_clauses,
                                    *nested_context_clauses,
                                ]
                                and c.name not in self._specifications
                            ]
                        )
                    error.extend(style.check(f))
                    break
            else:
                error.extend(
                    [
                        (
                            f'cannot find specification "{context_clause.name}"',
                            Subsystem.PARSER,
                            Severity.ERROR,
                            context_clause.location,
                        )
                    ]
                )

        self._parse_withed_files(error, nested_context_clauses, include_paths)

    def _evaluate_specification(
        self, error: RecordFluxError, spec: lang.Specification, filename: Path
    ) -> None:
        handlers: Dict[
            str,
            Callable[
                [
                    RecordFluxError,
                    ID,
                    lang.Parameters,
                    lang.TypeDef,
                    Sequence[model.Type],
                    bool,
                    int,
                    Cache,
                    Path,
                ],
                Optional[model.Type],
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
        package_id = create_id(error, spec.f_package_declaration.f_identifier, filename)

        for t in spec.f_package_declaration.f_declarations:
            if isinstance(t, lang.TypeDecl):
                identifier = model.internal_type_identifier(
                    create_id(error, t.f_identifier, filename), package_id
                )
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
                    error,
                    identifier,
                    t.f_parameters,
                    t.f_definition,
                    self._types,
                    self.skip_verification,
                    self._workers,
                    self._cache,
                    filename,
                )
                if new_type is not None:
                    self._types.append(new_type)
            elif isinstance(t, lang.RefinementDecl):
                new_refinement = create_refinement(error, t, package_id, self._types, filename)
                if new_refinement is not None:
                    self._types.append(new_refinement)
            elif isinstance(t, lang.SessionDecl):
                new_session = create_session(error, t, package_id, filename, self._types)
                if new_session is not None:
                    self._sessions.append(new_session)
            else:
                raise NotImplementedError(f"Declaration kind {t.kind_name} unsupported")


def _check_for_duplicate_specifications(
    error: RecordFluxError, specifications: Sequence[SpecificationFile]
) -> None:
    for i, n1 in enumerate(specifications, start=1):
        for n2 in specifications[i:]:
            if n1.package == n2.package:
                error.extend(
                    [
                        (
                            "duplicate specification",
                            Subsystem.PARSER,
                            Severity.ERROR,
                            node_location(n2.spec.f_package_declaration.f_identifier, n2.filename),
                        ),
                        (
                            "previous specification",
                            Subsystem.PARSER,
                            Severity.INFO,
                            node_location(n1.spec.f_package_declaration.f_identifier, n1.filename),
                        ),
                    ],
                )


def _check_for_dependency_cycles(
    error: RecordFluxError,
    given_specs: list[SpecificationFile],
    specifications: Mapping[ID, SpecificationFile],
) -> None:
    for spec in given_specs:
        for p, c in [(c.name, c) for c in spec.context_clauses]:
            try:
                _check_for_dependency_cycle(p, c, [], specifications)
            except RecordFluxError as e:
                error.extend(e)


def _check_for_dependency_cycle(
    package: ID,
    context: ContextClause,
    visited: list[tuple[ID, ContextClause]],
    specifications: Mapping[ID, SpecificationFile],
) -> None:
    if package not in specifications:
        return  # ignore missing specifications

    visited_packages = [s for s, _ in visited]
    if package in visited_packages:
        idx = visited_packages.index(package)
        assert isinstance(context, ContextClause)
        cycle: list[ContextClause] = []
        for _, c in visited[idx:]:
            assert isinstance(c, ContextClause)
            cycle.append(c)
        if context not in cycle:
            cycle.append(context)
        error = RecordFluxError()
        error.extend(
            [
                (
                    f'dependency cycle when including "{cycle[0].name}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    cycle[0].location,
                ),
                *[
                    (
                        f'when including "{c.name}"',
                        Subsystem.PARSER,
                        Severity.INFO,
                        c.location,
                    )
                    for c in cycle[1:]
                ],
            ],
        )
        raise error

    for p, c in [(c.name, c) for c in specifications[package].context_clauses]:
        _check_for_dependency_cycle(p, c, [*visited, (package, context)], specifications)


def _sort_specs_topologically(
    specifications: Mapping[ID, SpecificationFile]
) -> OrderedDict[ID, SpecificationFile]:
    """(Reverse) Topologically sort specifications using Kahn's algorithm."""

    result: List[ID] = []
    incoming: Dict[ID, Set[ID]] = {f: set() for f in specifications.keys()}
    for package, spec_node in specifications.items():
        for c in spec_node.context_clauses:
            if c.name in incoming:
                incoming[c.name].add(package)

    specs = [f for f, i in incoming.items() if len(i) == 0]
    visited = set(specs)

    while specs:
        s = specs.pop(0)
        result.insert(0, s)
        for c in specifications[s].context_clauses:
            visited.add(c.name)
            if c.name in incoming and incoming[c.name] <= visited:
                specs.append(c.name)

    assert not (set(specifications) - visited), "dependency cycle"

    return OrderedDict((f, specifications[f]) for f in result)
