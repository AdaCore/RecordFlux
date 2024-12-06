from __future__ import annotations

import textwrap
from collections import OrderedDict, defaultdict
from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

from rflx import const, expr, lang, model, ty
from rflx.common import STDIN, unique
from rflx.const import RESERVED_WORDS
from rflx.error import fail
from rflx.identifier import ID, StrID
from rflx.integration import Integration
from rflx.model import AlwaysVerify, Cache, declaration as decl, statement as stmt
from rflx.rapidflux import (
    NO_LOCATION,
    Annotation,
    ErrorEntry,
    Location,
    RecordFluxError,
    Severity,
    logging,
    source_code,
)

from . import style


def node_location(
    node: lang.RFLXNode,
    filename: Path | None,
    end_location: bool = False,
) -> Location:
    assert node.token_start
    assert node.token_end
    start = node.token_start.sloc_range
    end = node.token_end.sloc_range
    return Location(
        start=(
            (start.start.line, start.start.column)
            if not end_location
            else (end.end.line, end.end.column)
        ),
        source=filename,
        end=(end.end.line, end.end.column),
    )


def type_location(identifier: ID, node: lang.RFLXNode) -> Location:
    """
    Create a location object.

    The location object covers the area from the start of an identifier to the end of a node.
    """
    return Location(
        identifier.location.start,
        identifier.location.source,
        node_location(node, identifier.location.source).end,
    )


def diagnostics_to_error(
    diagnostics: Sequence[lang.Diagnostic],
    error: RecordFluxError,
    filename: Path,
) -> bool:
    """Append langkit diagnostics to RecordFlux error. Return True if error occurred."""

    if len(diagnostics) == 0:
        return False

    for diag in diagnostics:
        loc = diag.sloc_range
        error.push(
            ErrorEntry(
                diag.message,
                Severity.ERROR,
                Location(
                    start=(loc.start.line, loc.start.column),
                    source=filename,
                    end=(loc.end.line, loc.end.column),
                ),
            ),
        )
    return True


def readable_name(name: str) -> str:
    """Convert entity name to a readable format, e.g. CaseExpression to 'case expression'."""

    words = []
    start_index = 0
    for i in range(1, len(name)):
        if name[i].isupper():
            words.append(name[start_index:i].lower())
            start_index = i
    words.append(name[start_index:].lower())
    return " ".join(words)


def validate_handler(
    error: RecordFluxError,
    context: str,
    entity: lang.RFLXNode,
    handlers: list[str],
    filename: Path,
) -> None:
    if entity.kind_name not in handlers:
        error.extend(
            [
                ErrorEntry(
                    f"{readable_name(entity.kind_name)} unsupported in {context} context",
                    Severity.ERROR,
                    node_location(entity, filename),
                ),
            ],
        )
        error.propagate()


def create_description(description: lang.Description | None = None) -> str | None:
    if description:
        assert isinstance(description.text, str)
        return description.text.split('"')[1]
    return None


def create_transition(
    error: RecordFluxError,
    transition: lang.Transition,
    filename: Path,
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
                error,
                c.f_expression,
                filename,
            )
            for c in reset.f_associations
        },
        location=node_location(reset, filename),
    )


def create_assignment(
    error: RecordFluxError,
    assignment: lang.Statement,
    filename: Path,
) -> stmt.Statement:
    assert isinstance(assignment, lang.Assignment)
    return stmt.VariableAssignment(
        create_id(error, assignment.f_identifier, filename),
        create_expression(error, assignment.f_expression, filename),
        location=node_location(assignment, filename),
    )


def create_message_field_assignment(
    error: RecordFluxError,
    assignment: lang.Statement,
    filename: Path,
) -> stmt.Statement:
    assert isinstance(assignment, lang.MessageFieldAssignment)
    return stmt.MessageFieldAssignment(
        create_id(error, assignment.f_message, filename),
        create_id(error, assignment.f_field, filename),
        create_expression(error, assignment.f_expression, filename),
        location=node_location(assignment, filename),
    )


def create_attribute_statement(
    error: RecordFluxError,
    expression: lang.Statement,
    filename: Path,
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
    error: RecordFluxError,
    statement: lang.Statement,
    filename: Path,
) -> stmt.Statement:
    handlers = {
        "Reset": create_reset,
        "Assignment": create_assignment,
        "MessageFieldAssignment": create_message_field_assignment,
        "AttributeStatement": create_attribute_statement,
    }
    validate_handler(error, "statement", statement, list(handlers.keys()), filename)
    return handlers[statement.kind_name](error, statement, filename)


def create_state(
    error: RecordFluxError,
    state: lang.State,
    package: ID,
    filename: Path,
) -> model.State:
    identifier = create_id(error, state.f_identifier, filename)
    assert isinstance(state.f_body, lang.StateBody)
    if state.f_identifier.text != state.f_body.f_end_identifier.text:
        error.extend(
            [
                ErrorEntry(
                    f'inconsistent state identifier "{state.f_body.f_end_identifier.text}"',
                    Severity.ERROR,
                    node_location(state.f_body.f_end_identifier, filename),
                    [
                        Annotation(
                            f'previous identifier was "{identifier}"',
                            Severity.NOTE,
                            node_location(state.f_identifier, filename),
                        ),
                    ],
                ),
            ],
        )
    transitions = [
        create_transition(error, t, filename) for t in state.f_body.f_conditional_transitions
    ]
    transitions.append(create_transition(error, state.f_body.f_final_transition, filename))
    exception_transition = (
        create_transition(error, state.f_body.f_exception_transition, filename)
        if state.f_body.f_exception_transition
        else None
    )
    actions = [create_statement(error, a, filename) for a in state.f_body.f_actions]
    declarations = [
        create_declaration(error, d, package, filename) for d in state.f_body.f_declarations
    ]
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


def _check_state_machine_identifier(
    error: RecordFluxError,
    state_machine: lang.StateMachineDecl | lang.SessionDecl,
    filename: Path,
) -> None:
    if state_machine.f_identifier.text != state_machine.f_end_identifier.text:
        error.extend(
            [
                ErrorEntry(
                    "inconsistent state machine identifier"
                    f' "{state_machine.f_end_identifier.text}"',
                    Severity.ERROR,
                    node_location(state_machine.f_end_identifier, filename),
                    [
                        Annotation(
                            f'previous identifier was "{state_machine.f_identifier.text}"',
                            Severity.NOTE,
                            node_location(state_machine.f_identifier, filename),
                        ),
                    ],
                ),
            ],
        )


def create_state_machine(
    error: RecordFluxError,
    state_machine: lang.StateMachineDecl,
    package: ID,
    filename: Path,
) -> model.UncheckedStateMachine:
    _check_state_machine_identifier(error, state_machine, filename)

    return model.UncheckedStateMachine(
        package * create_id(error, state_machine.f_identifier, filename),
        [create_state(error, s, package, filename) for s in state_machine.f_states],
        [create_declaration(error, d, package, filename) for d in state_machine.f_declarations],
        [
            create_formal_declaration(error, p, package, filename)
            for p in state_machine.f_parameters
        ],
        node_location(state_machine, filename),
    )


def add_error_for_deprecated_session(
    error: RecordFluxError,
    session: lang.SessionDecl,
    filename: Path,
) -> None:
    _check_state_machine_identifier(error, session, filename)

    error.extend(
        [
            ErrorEntry(
                '"session" keyword is deprecated',
                Severity.ERROR,
                node_location(session.f_session_keyword, filename),
                [
                    Annotation(
                        'use "machine" instead',
                        Severity.HELP,
                        node_location(session.f_session_keyword, filename),
                    ),
                ],
                generate_default_annotation=False,
            ),
        ],
    )


def create_id(error: RecordFluxError, identifier: lang.AbstractID, filename: Path) -> ID:
    if isinstance(identifier, lang.UnqualifiedID):
        if identifier.text.lower() in RESERVED_WORDS:
            error.push(
                ErrorEntry(
                    f'reserved word "{identifier.text}" used as identifier',
                    Severity.ERROR,
                    node_location(identifier, filename),
                ),
            )
        if identifier.text.upper().startswith("RFLX_"):
            error.extend(
                [
                    ErrorEntry(
                        f'illegal identifier "{identifier.text}"',
                        Severity.ERROR,
                        node_location(identifier, filename),
                    ),
                    ErrorEntry(
                        'identifiers starting with "RFLX_" are reserved for internal use',
                        Severity.NOTE,
                        node_location(identifier, filename),
                    ),
                ],
            )
        return ID(identifier.text, location=node_location(identifier, filename))
    if isinstance(identifier, lang.ID):
        name = ID(identifier.f_name.text, location=node_location(identifier.f_name, filename))
        if identifier.f_package:
            return (
                ID(
                    identifier.f_package.text,
                    location=node_location(identifier, filename),
                )
                * name
            )

        if name.parts[0].lower() in RESERVED_WORDS:
            error.push(
                ErrorEntry(
                    f'reserved word "{name}" used as identifier',
                    Severity.ERROR,
                    node_location(identifier, filename),
                ),
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
    sequence: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedSequence | None:
    assert isinstance(sequence, lang.SequenceTypeDef)
    element_identifier = model.internal_type_identifier(
        create_id(error, sequence.f_element_type, filename),
        identifier.parent,
    )
    return model.UncheckedSequence(
        identifier,
        element_identifier,
        type_location(identifier, sequence),
    )


def create_numeric_literal(
    _error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    location = node_location(expression, filename)
    num = expression.text.split("#")
    if len(num) == 1:
        return expr.Number(int(num[0]), location=location)
    if len(num) == 3:
        base = int(num[0])
        return expr.Number(int(num[1], base), base=base, location=location)
    raise NotImplementedError(f"Invalid numeric literal: {expression.text}")


OPERATIONS: Mapping[str, type[expr.BinExpr]] = {
    "OpIn": expr.In,
    "OpNotin": expr.NotIn,
    "OpEq": expr.Equal,
    "OpNeq": expr.NotEqual,
}


def create_binop(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)
    loc = node_location(expression, filename)
    if expression.f_right is None:
        error.push(  # type: ignore[unreachable]
            ErrorEntry(
                "missing right operand",
                Severity.ERROR,
                loc,
            ),
        )
        error.propagate()

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


MATH_OPERATIONS: Mapping[str, type[expr.BinExpr | expr.AssExpr]] = {
    "OpPow": expr.Pow,
    "OpAdd": expr.Add,
    "OpSub": expr.Sub,
    "OpMul": expr.Mul,
    "OpDiv": expr.Div,
    "OpMod": expr.Mod,
}


def create_math_binop(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.BinOp)

    if expression.f_left is None or expression.f_right is None:
        # TODO(eng/recordflux/RecordFlux#1526): This should be rejected by the parser already.
        fail(
            "empty subexpression",
            location=node_location(expression, filename),
        )
    if expression.f_op.kind_name in MATH_OPERATIONS:
        return MATH_OPERATIONS[expression.f_op.kind_name](
            create_math_expression(error, expression.f_left, filename),
            create_math_expression(error, expression.f_right, filename),
            location=node_location(expression, filename),
        )

    if expression.f_op.kind_name in [*OPERATIONS, *MATH_COMPARISONS, *BOOLEAN_OPERATIONS]:
        fail(
            "boolean expression in math context",
            location=node_location(expression, filename),
        )

    raise NotImplementedError(
        f"Invalid math BinOp {expression.f_op.kind_name} => {expression.text}",
    )


MATH_COMPARISONS: Mapping[str, type[expr.Relation]] = {
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

    if expression.f_left is None or expression.f_right is None:
        # TODO(eng/recordflux/RecordFlux#1526): This should be rejected by the parser already.
        fail(
            "empty subexpression",
            location=node_location(expression, filename),
        )

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

    if expression.f_op.kind_name in MATH_OPERATIONS:
        fail(
            "math expression in boolean context",
            location=node_location(expression, filename),
        )

    raise NotImplementedError(
        f"Invalid bool BinOp {expression.f_op.kind_name} => {expression.text}",
    )


def create_paren_bool_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_bool_expression(error, expression.f_data, filename)


def create_paren_math_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_math_expression(error, expression.f_data, filename)


def create_paren_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.ParenExpression)
    return create_expression(error, expression.f_data, filename)


def create_variable(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Variable)
    if expression.f_identifier.text.lower() in ("true", "false"):
        return expr.Literal(
            create_id(error, expression.f_identifier, filename),
            type_=ty.BOOLEAN,
        )
    return expr.Variable(create_id(error, expression.f_identifier, filename))


def create_math_attribute(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
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
        f"Invalid math attribute: {expression.f_kind.kind_name} => {expression.text}",
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
        f"Invalid attribute: {expression.f_kind.kind_name} => {expression.text}",
    )


def create_sequence_aggregate(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.SequenceAggregate)
    return expr.Aggregate(
        *[create_math_expression(error, v, filename) for v in expression.f_values],
        location=node_location(expression, filename),
    )


def create_string_literal(
    _error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    return expr.String(
        expression.text.split('"')[1],
        location=node_location(expression, filename),
    )


def create_call(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Call)
    return expr.Call(
        create_id(error, expression.f_identifier, filename),
        ty.UNDEFINED,
        [create_expression(error, a, filename) for a in expression.f_arguments],
        location=node_location(expression, filename),
    )


def create_quantified_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
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
    error.push(
        ErrorEntry(
            "bindings are not supported",
            Severity.ERROR,
            node_location(expression, filename),
        ),
    )
    return create_expression(error, expression.f_expression, filename)


def create_variable_decl(
    error: RecordFluxError,
    declaration: lang.LocalDecl,
    package: ID,
    filename: Path,
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
            create_id(error, declaration.f_type_identifier, filename),
            package,
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
    error: RecordFluxError,
    declaration: lang.LocalDecl,
    package: ID,
    filename: Path,
) -> decl.BasicDeclaration:
    assert isinstance(declaration, lang.RenamingDecl)
    selected = create_expression(error, declaration.f_expression, filename)
    assert isinstance(selected, expr.Selected)
    return decl.RenamingDeclaration(
        create_id(error, declaration.f_identifier, filename),
        model.internal_type_identifier(
            create_id(error, declaration.f_type_identifier, filename),
            package,
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
        arguments = [
            decl.Parameter(
                create_id(error, p.f_identifier, filename),
                model.internal_type_identifier(
                    create_id(error, p.f_type_identifier, filename),
                    package,
                ),
            )
            for p in declaration.f_parameters.f_parameters
        ]
    return decl.FunctionDeclaration(
        create_id(error, declaration.f_identifier, filename),
        arguments,
        model.internal_type_identifier(
            create_id(error, declaration.f_return_type_identifier, filename),
            package,
        ),
        location=node_location(declaration, filename),
    )


def create_negation(error: RecordFluxError, expression: lang.Expr, filename: Path) -> expr.Expr:
    assert isinstance(expression, lang.Negation)
    if expression.f_data is None:
        error.push(  # type: ignore[unreachable]
            ErrorEntry(
                "negation of non-expression",
                Severity.ERROR,
                node_location(expression, filename),
            ),
        )
        error.propagate()

    return expr.Neg(
        create_math_expression(error, expression.f_data, filename),
        node_location(expression, filename),
    )


def create_concatenation(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.Concatenation)
    left = create_expression(error, expression.f_left, filename)
    right = create_expression(error, expression.f_right, filename)
    assert isinstance(left, expr.Aggregate)
    assert isinstance(right, expr.Aggregate)
    return expr.Aggregate(
        *(left.elements + right.elements),
        location=node_location(expression, filename),
    )


def create_comprehension(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
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
        value: lang.AbstractID | lang.Expr,
        filename: Path,
    ) -> ID | expr.Number:
        if isinstance(value, lang.AbstractID):
            return create_id(error, value, filename)
        assert isinstance(value, lang.Expr)
        result = create_numeric_literal(error, value, filename)
        assert isinstance(result, expr.Number)
        return result

    choices: Sequence[tuple[Sequence[ID | expr.Number], expr.Expr]] = [
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
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
) -> expr.Expr:
    assert isinstance(expression, lang.MessageAggregate)
    values: Mapping[StrID, expr.Expr] = {}
    if isinstance(expression.f_values, lang.NullMessageAggregate):
        values = {}
    elif isinstance(expression.f_values, lang.MessageAggregateAssociations):
        values = {
            create_id(error, c.f_identifier, filename): create_expression(
                error,
                c.f_expression,
                filename,
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
    validate_handler(error, "expression", expression, list(EXPRESSION_MAP.keys()), filename)
    return EXPRESSION_MAP[expression.kind_name](error, expression, filename)


def create_declaration(
    error: RecordFluxError,
    declaration: lang.LocalDecl,
    package: ID,
    filename: Path,
) -> decl.BasicDeclaration:
    handlers = {
        "VariableDecl": create_variable_decl,
        "RenamingDecl": create_renaming_decl,
    }
    validate_handler(error, "declaration", declaration, list(handlers.keys()), filename)
    return handlers[declaration.kind_name](error, declaration, package, filename)


def create_formal_declaration(
    error: RecordFluxError,
    declaration: lang.FormalDecl,
    package: ID,
    filename: Path,
) -> decl.FormalDeclaration:
    handlers = {
        "FormalChannelDecl": create_channel_decl,
        "FormalFunctionDecl": create_function_decl,
    }
    validate_handler(error, "formal declaration", declaration, list(handlers.keys()), filename)
    return handlers[declaration.kind_name](error, declaration, package, filename)


def create_math_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
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
    validate_handler(error, "math expression", expression, list(handlers.keys()), filename)
    result = handlers[expression.kind_name](error, expression, filename)
    if result.type_ == ty.BOOLEAN:
        fail(
            "boolean expression in math context",
            location=node_location(expression, filename),
        )
    return result


def create_bool_expression(
    error: RecordFluxError,
    expression: lang.Expr,
    filename: Path,
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
    if expression.kind_name == "NumericLiteral":
        fail(
            "math expression in boolean context",
            location=node_location(expression, filename),
        )
    validate_handler(error, "boolean expression", expression, list(handlers.keys()), filename)
    return handlers[expression.kind_name](error, expression, filename)


def create_modular(
    error: RecordFluxError,
    identifier: ID,
    modular: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> None:
    assert isinstance(modular, lang.ModularTypeDef)
    modulus = create_math_expression(error, modular.f_mod, filename).simplified()
    assert isinstance(modulus, expr.Number)
    upper = int(modulus) - 1
    error.extend(
        [
            ErrorEntry(
                "modular integer types are not supported",
                Severity.ERROR,
                type_location(identifier, modular),
            ),
            ErrorEntry(
                f'use "type {identifier.name} is unsigned {upper.bit_length()}" instead',
                Severity.HELP,
                type_location(identifier, modular),
            ),
        ],
    )


def create_range(
    error: RecordFluxError,
    identifier: ID,
    rangetype: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedInteger | None:
    assert isinstance(rangetype, lang.RangeTypeDef)
    if rangetype.f_size.f_identifier.text != "Size":
        error.extend(
            [
                ErrorEntry(
                    f'invalid aspect "{rangetype.f_size.f_identifier.text}" '
                    f'for range type "{identifier}"',
                    Severity.ERROR,
                    node_location(rangetype, filename),
                ),
            ],
        )
        return None
    if rangetype.f_size.f_value is None:
        error.push(  # type: ignore[unreachable]
            ErrorEntry(
                '"Size" aspect has no value',
                Severity.ERROR,
                node_location(rangetype.f_size, filename),
            ),
        )
        return None

    size = create_math_expression(error, rangetype.f_size.f_value, filename)
    return model.UncheckedInteger(
        identifier,
        create_math_expression(error, rangetype.f_first, filename),
        create_math_expression(error, rangetype.f_last, filename),
        size,
        type_location(identifier, rangetype),
    )


def create_unsigned(
    error: RecordFluxError,
    identifier: ID,
    unsignedtype: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedUnsignedInteger | None:
    assert isinstance(unsignedtype, lang.UnsignedTypeDef)
    size = create_math_expression(error, unsignedtype.f_size, filename)
    return model.UncheckedUnsignedInteger(
        identifier,
        size,
        type_location(identifier, unsignedtype),
    )


def create_null_message(
    _error: RecordFluxError,
    identifier: ID,
    message: lang.TypeDef,
    _filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedMessage | None:
    assert isinstance(message, lang.NullMessageTypeDef)
    return model.UncheckedMessage(
        identifier,
        [],
        [],
        [],
        {},
        {},
        location=type_location(identifier, message),
    )


def create_message(
    error: RecordFluxError,
    identifier: ID,
    message: lang.TypeDef,
    filename: Path,
    parameters: lang.Parameters | None = None,
) -> model.UncheckedMessage | None:
    assert isinstance(message, lang.MessageTypeDef)
    fields = message.f_message_fields

    def get_parameters(param: lang.Parameters | None) -> lang.ParameterList | None:
        if not param:
            return None
        assert isinstance(param.f_parameters, lang.ParameterList)
        return param.f_parameters

    parameter_types = create_message_field_types(
        error,
        identifier,
        (
            (parameter.f_identifier, parameter.f_type_identifier, [])
            for parameter in get_parameters(parameters) or []
        ),
        filename,
    )
    field_types = create_message_field_types(
        error,
        identifier,
        (
            (field.f_identifier, field.f_type_identifier, field.f_type_arguments)
            for field in fields.f_fields
        ),
        filename,
    )
    structure = create_message_structure(error, fields, filename)
    checksum_aspects, byte_order_aspect = parse_aspects(error, message.f_aspects, filename)

    return model.UncheckedMessage(
        identifier,
        structure,
        parameter_types,
        field_types,
        checksum_aspects,
        byte_order_aspect,
        type_location(identifier, message),
    )


def create_message_field_types(
    error: RecordFluxError,
    identifier: ID,
    fields: Iterable[tuple[lang.AbstractID, lang.AbstractID, object]],
    filename: Path,
) -> list[tuple[model.Field, ID, list[tuple[ID, expr.Expr]]]]:
    result: list[tuple[model.Field, ID, list[tuple[ID, expr.Expr]]]] = []
    for field_identifier, type_identifier, type_arguments in fields:
        qualified_type_identifier = model.internal_type_identifier(
            create_id(error, type_identifier, filename),
            identifier.parent,
        )
        field = model.Field(create_id(error, field_identifier, filename))
        result.append(
            (
                field,
                qualified_type_identifier,
                (
                    create_message_arguments(error, type_arguments, filename)
                    if isinstance(type_arguments, lang.TypeArgumentList)
                    else []
                ),
            ),
        )
    return result


def create_message_arguments(
    error: RecordFluxError,
    type_arguments: lang.TypeArgumentList,
    filename: Path,
) -> list[tuple[ID, expr.Expr]]:
    return [
        (
            create_id(error, arg.f_identifier, filename),
            create_expression(error, arg.f_expression, filename),
        )
        for arg in type_arguments
    ]


def create_message_structure(
    error: RecordFluxError,
    fields: lang.MessageFields,
    filename: Path,
) -> list[model.Link]:
    def extract_aspect(aspects: lang.AspectList) -> tuple[expr.Expr, expr.Expr]:
        size: expr.Expr = expr.UNDEFINED
        first: expr.Expr = expr.UNDEFINED

        grouped = defaultdict(list)
        for aspect in aspects:
            grouped[aspect.f_identifier.text].append(
                (aspect, node_location(aspect.f_identifier, filename)),
            )

        for name, locations in grouped.items():
            check_duplicate_aspect(error, name, [l for _, l in locations])
            aspect, location = locations[0]

            if aspect.f_value is None:
                error.push(  # type: ignore[unreachable]
                    ErrorEntry(
                        f'"{name}" aspect has no value',
                        Severity.ERROR,
                        location,
                    ),
                )
                continue

            if name == "Size":
                size = create_math_expression(error, aspect.f_value, filename)
            elif name == "First":
                first = create_math_expression(error, aspect.f_value, filename)
            else:
                error.extend(
                    [
                        ErrorEntry(
                            f'invalid aspect "{name}"',
                            Severity.ERROR,
                            location,
                        ),
                    ],
                )
        return size, first

    def extract_then(
        then: lang.ThenNode,
    ) -> tuple[model.Field, expr.Expr, expr.Expr, expr.Expr, Location]:
        target = (
            model.FINAL
            if then.f_target.text == "null"
            else model.Field(create_id(error, then.f_target, filename))
        )
        condition = (
            create_bool_expression(error, then.f_condition, filename)
            if then.f_condition
            else expr.Literal(ID("True", node_location(then, filename)), type_=ty.BOOLEAN)
        )
        size, first = extract_aspect(then.f_aspects)
        return target, condition, size, first, node_location(then, filename)

    field_identifiers = {
        # Do not store errors in `error` object as this would result in duplicate error messages
        # from the invocation of create_message_types
        i: create_id(RecordFluxError(), field.f_identifier, filename)
        for i, field in enumerate(fields.f_fields)
    }

    if fields.f_initial_field:
        structure = [
            model.Link(model.INITIAL, *extract_then(then))
            for then in fields.f_initial_field.f_thens
        ]
    else:
        structure = [
            model.Link(
                model.INITIAL,
                model.Field(field_identifiers[0]),
                location=field_identifiers[0].location,
            ),
        ]

    for i, field in enumerate(fields.f_fields):
        source_node = model.Field(field_identifiers[i]) if field.f_identifier else model.INITIAL
        if field.f_identifier.text.lower() == "message":
            error.extend(
                [
                    ErrorEntry(
                        'reserved word "Message" used as identifier',
                        Severity.ERROR,
                        field_identifiers[i].location,
                    ),
                ],
            )

        if len(field.f_thens) == 0:
            target_id = field_identifiers[i + 1] if i + 1 < len(fields.f_fields) else None
            target_node = model.Field(target_id) if target_id else model.FINAL
            structure.append(
                model.Link(
                    source_node,
                    target_node,
                    location=node_location(field, filename, end_location=True),
                ),
            )

        for then in field.f_thens:
            if then.f_target.text.lower() != "null" and not any(
                then.f_target.text == c.f_identifier.text for c in fields.f_fields
            ):
                error.extend(
                    [
                        ErrorEntry(
                            f'undefined field "{then.f_target.text}"',
                            Severity.ERROR,
                            (
                                node_location(then.f_target, filename)
                                if then.f_target
                                else NO_LOCATION
                            ),
                        ),
                    ],
                )
                continue
            structure.append(model.Link(source_node, *extract_then(then)))

        if error.has_errors():
            continue

        merge_field_aspects(
            error,
            field_identifiers[i],
            structure,
            *extract_aspect(field.f_aspects),
        )
        if field.f_condition:
            error.extend(
                [
                    ErrorEntry(
                        "short form condition is not supported anymore",
                        Severity.ERROR,
                        node_location(field.f_condition, filename),
                    ),
                    ErrorEntry(
                        "add condition to all outgoing links of "
                        f'field "{field_identifiers[i]}" instead',
                        Severity.HELP,
                        node_location(field.f_condition, filename),
                    ),
                ],
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
                            ErrorEntry(
                                f'first aspect of field "{field_identifier}"'
                                " conflicts with previous"
                                " specification",
                                Severity.ERROR,
                                first.location,
                            ),
                            ErrorEntry(
                                "previous specification of first",
                                Severity.NOTE,
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
                            ErrorEntry(
                                f'size aspect of field "{field_identifier}" conflicts with'
                                " previous specification",
                                Severity.ERROR,
                                size.location,
                            ),
                            ErrorEntry(
                                "previous specification of size",
                                Severity.NOTE,
                                l.size.location,
                            ),
                        ],
                    )


def check_duplicate_aspect(
    error: RecordFluxError,
    name: str,
    locations: Sequence[Location],
) -> None:
    if len(locations) > 1:
        error.extend(
            [
                ErrorEntry(
                    f'duplicate aspect "{name}"',
                    Severity.ERROR,
                    locations[1],
                ),
                *[
                    ErrorEntry(
                        "previous location",
                        Severity.NOTE,
                        l,
                    )
                    for l in locations[:-1]
                ],
            ],
        )


def parse_aspects(  # noqa: PLR0912
    error: RecordFluxError,
    aspects: lang.MessageAspectList,
    filename: Path,
) -> tuple[
    Mapping[ID, Sequence[expr.Expr]],
    model.ByteOrder | dict[model.Field, model.ByteOrder],
]:
    checksum_result = {}
    byte_order_result: model.ByteOrder | dict[model.Field, model.ByteOrder] = {}

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
                            ),
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
    derivation: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedDerivedMessage | None:
    assert isinstance(derivation, lang.TypeDerivationDef)
    base_id = create_id(error, derivation.f_base, filename)
    base_name = model.internal_type_identifier(base_id, identifier.parent)
    return model.UncheckedDerivedMessage(
        identifier,
        base_name,
        location=type_location(identifier, derivation),
    )


def create_enumeration(
    error: RecordFluxError,
    identifier: ID,
    enumeration: lang.TypeDef,
    filename: Path,
    _parameters: lang.Parameters | None = None,
) -> model.UncheckedEnumeration | None:
    assert isinstance(enumeration, lang.EnumerationTypeDef)
    literals: list[tuple[ID, expr.Number]] = []

    def create_aspects(aspects: lang.AspectList) -> tuple[expr.Expr, bool] | None:
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
                    if av_expr == expr.TRUE:
                        always_valid = True
                    elif av_expr == expr.FALSE:
                        always_valid = False
                    else:
                        error.extend(
                            [
                                ErrorEntry(
                                    f"invalid Always_Valid expression: {av_expr}",
                                    Severity.ERROR,
                                    node_location(aspect.f_value, filename),
                                ),
                            ],
                        )
                else:
                    always_valid = True
        if not size:
            error.extend(
                [
                    ErrorEntry(
                        f'no size set for "{identifier}"',
                        Severity.ERROR,
                        identifier.location,
                    ),
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
            f"Enumeration kind {enumeration.f_elements.kind_name} unsupported",
        )

    aspects = create_aspects(enumeration.f_aspects)
    if aspects is None:
        return None

    size, always_valid = aspects

    return model.UncheckedEnumeration(
        identifier,
        literals,
        size,
        always_valid,
        location=type_location(identifier, enumeration),
    )


def create_refinement(
    error: RecordFluxError,
    refinement: lang.RefinementDecl,
    package: ID,
    filename: Path,
) -> model.UncheckedRefinement:
    pdu = model.internal_type_identifier(create_id(error, refinement.f_pdu, filename), package)
    sdu = model.internal_type_identifier(create_id(error, refinement.f_sdu, filename), package)

    if refinement.f_condition:
        condition = create_bool_expression(error, refinement.f_condition, filename)
    else:
        condition = expr.TRUE

    field = model.Field(create_id(error, refinement.f_field, filename))

    return model.UncheckedRefinement(
        package,
        pdu,
        field,
        sdu,
        condition,
        node_location(refinement, filename),
    )


def check_naming(error: RecordFluxError, package: lang.PackageNode, name: Path) -> None:
    identifier = package.f_identifier.text
    if identifier.startswith("RFLX"):
        error.extend(
            [
                ErrorEntry(
                    f'illegal prefix "RFLX" in package identifier "{identifier}"',
                    Severity.ERROR,
                    node_location(package.f_identifier, name),
                ),
            ],
        )
    if identifier != package.f_end_identifier.text:
        error.push(
            ErrorEntry(
                f'inconsistent package identifier "{package.f_end_identifier.text}"',
                Severity.ERROR,
                node_location(package.f_end_identifier, name),
                annotations=[
                    Annotation(
                        f'previous identifier was "{identifier}"',
                        Severity.NOTE,
                        node_location(package.f_identifier, name),
                    ),
                ],
            ),
        )
    if name != STDIN:
        expected_filename = f"{identifier.lower()}.rflx"
        expected_package_name = "_".join(
            x.capitalize() for x in Path(name.name).stem.lower().split("_")
        )
        if name.name != expected_filename:
            error.push(
                (
                    ErrorEntry(
                        f'source file name "{name.name}" must be in lower case characters only',
                        Severity.ERROR,
                        node_location(package.f_identifier, name),
                    )
                    if any(c.isupper() for c in Path(name.name).stem)
                    else ErrorEntry(
                        f'source file name does not match the package name "{identifier}"',
                        Severity.ERROR,
                        node_location(package.f_identifier, name),
                    )
                ),
            )

            error.extend(
                (
                    [
                        ErrorEntry(
                            f'rename the file to "{expected_filename}"',
                            Severity.HELP,
                            node_location(package.f_identifier, name),
                        ),
                    ]
                    if any(c.isupper() for c in Path(name.name).stem)
                    else [
                        ErrorEntry(
                            f'either rename the file to "{expected_filename}" or change '
                            f'the package name to "{expected_package_name}"',
                            Severity.HELP,
                            node_location(package.f_identifier, name),
                            annotations=[
                                Annotation(
                                    f'rename to "{expected_package_name}"',
                                    Severity.HELP,
                                    node_location(package.f_identifier, name),
                                ),
                                Annotation(
                                    f'rename to "{expected_package_name}"',
                                    Severity.HELP,
                                    node_location(package.f_end_identifier, name),
                                ),
                            ],
                            generate_default_annotation=False,
                        ),
                    ]
                ),
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
    model_style_checks: frozenset[const.StyleCheck]

    @staticmethod
    def create(
        error: RecordFluxError,
        spec: lang.Specification,
        filename: Path,
        model_style_checks: frozenset[const.StyleCheck],
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
            model_style_checks,
        )

    @property
    def package(self) -> ID:
        return ID(self.spec.f_package_declaration.f_identifier.text)


class Parser:
    def __init__(
        self,
        cache: Cache | None = None,
        workers: int = 1,
        integration_files_dir: Path | None = None,
    ) -> None:
        self._cache = AlwaysVerify() if cache is None else cache
        self._workers = workers
        self._specifications: OrderedDict[ID, SpecificationFile] = OrderedDict()
        self._integration: Integration = Integration(integration_files_dir)

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

        _check_for_duplicate_specifications(
            error,
            [*self._specifications.values(), *specifications],
        )

        self._specifications.update({s.package: s for s in specifications})

        self._parse_withed_files(
            error,
            [c for s in specifications for c in s.context_clauses],
            include_paths,
        )

        _check_for_dependency_cycles(error, specifications, self._specifications)

        self._specifications = _sort_specs_topologically(self._specifications)

        error.propagate()

    def parse_string(
        self,
        string: str,
        filename: Path = STDIN,
        rule: str = lang.GrammarRule.main_rule_rule,
    ) -> None:
        error = RecordFluxError()
        string = textwrap.dedent(string)
        source_code.register(filename, string)
        unit = lang.AnalysisContext().get_from_buffer(str(filename), string, rule=rule)

        if not diagnostics_to_error(unit.diagnostics, error, filename):
            assert isinstance(unit.root, lang.Specification)

            basic_style_checks, model_style_checks = style.determine_enabled_checks(
                error,
                string,
                filename,
            )
            spec = SpecificationFile.create(error, unit.root, filename, model_style_checks)
            _check_for_duplicate_specifications(error, [*self._specifications.values(), spec])

            self._specifications[spec.package] = spec

            _check_for_dependency_cycles(error, [spec], self._specifications)

            self._specifications = _sort_specs_topologically(self._specifications)

            style.check_string(error, string, basic_style_checks, filename)

        error.propagate()

    def create_unchecked_model(self) -> model.UncheckedModel:
        error = RecordFluxError()
        declarations: list[model.UncheckedTopLevelDeclaration] = [
            model.UNCHECKED_BOOLEAN,
            model.UNCHECKED_OPAQUE,
        ]

        style_checks: dict[Path, frozenset[const.StyleCheck]] = {}
        for spec_node in self._specifications.values():
            self._evaluate_specification(error, declarations, spec_node.spec, spec_node.filename)
            style_checks[spec_node.filename] = spec_node.model_style_checks

        return model.UncheckedModel(declarations, style_checks, error)

    def create_model(self) -> model.Model:
        unchecked_model = self.create_unchecked_model()
        error = unchecked_model.error
        checked_model = unchecked_model.checked(
            self._cache,
            self._workers,
        )
        self._integration.validate(checked_model, error)
        error.propagate()
        return checked_model

    def get_integration(self) -> Integration:
        return self._integration

    @property
    def specifications(self) -> dict[str, lang.Specification]:
        return {
            spec_node.spec.f_package_declaration.f_identifier.text: spec_node.spec
            for spec_node in self._specifications.values()
        }

    def _parse_file(self, error: RecordFluxError, filename: Path) -> SpecificationFile | None:
        logging.info("Parsing {filename}", filename=filename)

        source_code_str = filename.read_text()
        source_code.register(filename, source_code_str)
        unit = lang.AnalysisContext().get_from_buffer(str(filename), source_code_str)

        basic_style_checks, model_style_checks = style.determine_enabled_checks(
            error,
            source_code_str,
            filename,
        )
        error.extend(style.check(filename, basic_style_checks).entries)

        if diagnostics_to_error(unit.diagnostics, error, filename):
            return None

        assert isinstance(unit.root, lang.Specification)

        self._integration.load_integration_file(filename, error)

        return SpecificationFile.create(error, unit.root, filename, model_style_checks)

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
                            ],
                        )
                    break
            else:
                error.extend(
                    [
                        ErrorEntry(
                            f'cannot find specification "{context_clause.name}"',
                            Severity.ERROR,
                            context_clause.location,
                        ),
                    ],
                )

        self._parse_withed_files(error, nested_context_clauses, include_paths)

    def _evaluate_specification(
        self,
        error: RecordFluxError,
        declarations: list[model.UncheckedTopLevelDeclaration],
        spec: lang.Specification,
        filename: Path,
    ) -> None:
        handlers: Mapping[
            str,
            Callable[
                [
                    RecordFluxError,
                    ID,
                    lang.TypeDef,
                    Path,
                    lang.Parameters | None,
                ],
                model.UncheckedTypeDecl | None,
            ],
        ] = {
            "SequenceTypeDef": create_sequence,
            "ModularTypeDef": create_modular,
            "RangeTypeDef": create_range,
            "UnsignedTypeDef": create_unsigned,
            "MessageTypeDef": create_message,
            "NullMessageTypeDef": create_null_message,
            "TypeDerivationDef": create_derived_message,
            "EnumerationTypeDef": create_enumeration,
        }
        logging.info("Processing {name}", name=spec.f_package_declaration.f_identifier.text)
        package_id = create_id(error, spec.f_package_declaration.f_identifier, filename)

        for t in spec.f_package_declaration.f_declarations:
            if isinstance(t, lang.TypeDecl):
                type_id = create_id(error, t.f_identifier, filename)
                identifier = ID(package_id * type_id, location=type_id.location)
                if t.f_definition.kind_name != "MessageTypeDef" and t.f_parameters:
                    error.extend(
                        [
                            ErrorEntry(
                                "only message types can be parameterized",
                                Severity.ERROR,
                                node_location(t.f_parameters.f_parameters, filename),
                            ),
                        ],
                    )
                validate_handler(
                    error,
                    "definition",
                    t.f_definition,
                    list(handlers.keys()),
                    filename,
                )
                new_type = handlers[t.f_definition.kind_name](
                    error,
                    identifier,
                    t.f_definition,
                    filename,
                    t.f_parameters,
                )
                if new_type is not None:
                    declarations.append(new_type)
            elif isinstance(t, lang.RefinementDecl):
                declarations.append(create_refinement(error, t, package_id, filename))
            elif isinstance(t, lang.StateMachineDecl):
                declarations.append(create_state_machine(error, t, package_id, filename))
            elif isinstance(t, lang.SessionDecl):
                add_error_for_deprecated_session(error, t, filename)
            else:
                raise NotImplementedError(f"Declaration kind {t.kind_name} unsupported")


def _check_for_duplicate_specifications(
    error: RecordFluxError,
    specifications: Sequence[SpecificationFile],
) -> None:
    for i, n1 in enumerate(specifications, start=1):
        for n2 in specifications[i:]:
            if n1.package == n2.package:
                error.extend(
                    [
                        ErrorEntry(
                            "duplicate specification",
                            Severity.ERROR,
                            node_location(n2.spec.f_package_declaration.f_identifier, n2.filename),
                        ),
                        ErrorEntry(
                            "previous specification",
                            Severity.NOTE,
                            node_location(n1.spec.f_package_declaration.f_identifier, n1.filename),
                        ),
                    ],
                )


def _check_for_dependency_cycles(
    error: RecordFluxError,
    given_specs: list[SpecificationFile],
    specifications: Mapping[ID, SpecificationFile],
) -> bool:
    result = False

    for spec in given_specs:
        for p, c in [(c.name, c) for c in spec.context_clauses]:
            try:
                _check_for_dependency_cycle(p, c, [], specifications)
            except RecordFluxError as e:  # noqa: PERF203
                result = True
                error.extend(e.entries)

    return result


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
        raise RecordFluxError(
            [
                ErrorEntry(
                    f'dependency cycle when including "{cycle[0].name}"',
                    Severity.ERROR,
                    cycle[0].location,
                ),
                *[
                    ErrorEntry(
                        f'when including "{c.name}"',
                        Severity.NOTE,
                        c.location,
                    )
                    for c in cycle[1:]
                ],
            ],
        )

    for p, c in [(c.name, c) for c in specifications[package].context_clauses]:
        _check_for_dependency_cycle(p, c, [*visited, (package, context)], specifications)


def _sort_specs_topologically(
    specifications: Mapping[ID, SpecificationFile],
) -> OrderedDict[ID, SpecificationFile]:
    """
    (Reverse) Topologically sort specifications using Kahn's algorithm.

    Specifications which are part of a cycle will be removed.
    """

    result: list[ID] = []
    incoming: dict[ID, set[ID]] = {f: set() for f in specifications}
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

    return OrderedDict((f, specifications[f]) for f in result)
