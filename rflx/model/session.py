from __future__ import annotations

import itertools
from abc import abstractmethod
from collections import defaultdict
from typing import Dict, Final, Iterable, List, Mapping, Optional, Sequence

from rflx import expression as expr, typing_ as rty
from rflx.common import Base, indent, indent_next, verbose_repr
from rflx.error import Location, Severity, Subsystem
from rflx.identifier import ID, StrID

from . import declaration as decl, statement as stmt, type_ as mty
from .basic_declaration import BasicDeclaration
from .message import Message, Refinement


class Transition(Base):
    def __init__(
        self,
        target: StrID,
        condition: expr.Expr = expr.TRUE,
        description: str = None,
        location: Location = None,
    ):
        self.target = ID(target)
        self.condition = condition
        self.description = description
        self.location = location

    def __repr__(self) -> str:
        return verbose_repr(self, ["target", "condition", "description"])

    def __str__(self) -> str:
        target = self.target if self.target != FINAL_STATE.identifier else "null"
        with_aspects = f'\n   with Desc => "{self.description}"' if self.description else ""
        if_condition = (
            f"\n   if {indent_next(str(self.condition), 6)}" if self.condition != expr.TRUE else ""
        )
        return f"goto {target}{with_aspects}{if_condition}"


class State(Base):
    def __init__(
        self,
        identifier: StrID,
        transitions: Sequence[Transition] = None,
        exception_transition: Transition = None,
        actions: Sequence[stmt.Statement] = None,
        declarations: Sequence[decl.BasicDeclaration] = None,
        description: str = None,
        location: Location = None,
    ):
        # pylint: disable=too-many-arguments

        if transitions:
            assert transitions[-1].condition == expr.TRUE, "missing default transition"
        else:
            assert not actions and not declarations

        assert exception_transition.condition == expr.TRUE if exception_transition else True

        self._identifier = ID(identifier)
        self._transitions = transitions or []
        self._exception_transition = exception_transition
        self._actions = actions or []
        self.declarations = {d.identifier: d for d in declarations} if declarations else {}
        self.description = description
        self.location = location

        self._normalize()

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "transitions", "actions", "declarations"])

    def __str__(self) -> str:
        with_aspects = f'\n   with Desc => "{self.description}"\n' if self.description else " "
        if not self.declarations and not self.actions and not self.transitions:
            return f"state {self.identifier}{with_aspects}is null state"
        declarations = "".join([f"{d};\n" for d in self.declarations.values()])
        actions = "".join([f"{a};\n" for a in self.actions])
        transitions = "\n".join([f"{p}" for p in self.transitions])
        exception_transition = (
            f"\nexception\n   {self.exception_transition}" if self.exception_transition else ""
        )
        return (
            f"state {self.identifier}{with_aspects}is\n{indent(declarations, 3)}begin\n"
            f"{indent(actions, 3)}"
            f"transition\n{indent(transitions, 3)}{indent_next(exception_transition, 0)}\n"
            f"end {self.identifier}"
        )

    @property
    def identifier(self) -> ID:
        return self._identifier

    @property
    def transitions(self) -> Sequence[Transition]:
        return self._transitions or []

    @property
    def exception_transition(self) -> Optional[Transition]:
        return self._exception_transition

    @property
    def actions(self) -> Sequence[stmt.Statement]:
        return self._actions

    @property
    def has_exceptions(self) -> bool:
        return any(
            isinstance(a, (stmt.Append, stmt.Extend, stmt.MessageFieldAssignment))
            or (
                isinstance(a, stmt.VariableAssignment)
                and (
                    a.expression.findall(
                        lambda x: isinstance(
                            x,
                            (
                                expr.Selected,
                                expr.Head,
                                expr.Comprehension,
                                expr.MessageAggregate,
                                expr.DeltaMessageAggregate,
                                expr.Conversion,
                                expr.Opaque,
                            ),
                        )
                    )
                )
            )
            for a in self._actions
        )

    def _normalize(self) -> None:
        self._normalize_transitions()
        self._normalize_actions()

    def _normalize_transitions(self) -> None:
        for t in self._transitions:
            if t.target == ID("null"):
                t.target = FINAL_STATE.identifier
        if self._exception_transition and self._exception_transition.target == ID("null"):
            self._exception_transition.target = FINAL_STATE.identifier

    def _normalize_actions(self) -> None:  # pylint: disable = too-many-branches
        field_assignments: list[stmt.MessageFieldAssignment] = []
        actions: list[stmt.Statement] = []

        for a in self._actions:
            if not field_assignments:
                if isinstance(a, stmt.MessageFieldAssignment):
                    field_assignments.append(a)
                else:
                    actions.append(a)
            elif len(field_assignments) == 1:
                if isinstance(a, stmt.MessageFieldAssignment):
                    if field_assignments[0].message == a.message:
                        field_assignments.append(a)
                    else:
                        actions.extend(field_assignments)
                        field_assignments = [a]
                else:
                    actions.extend(
                        [
                            *field_assignments,
                            a,
                        ]
                    )
                    field_assignments = []
            else:
                if isinstance(a, stmt.MessageFieldAssignment):
                    if field_assignments[0].message == a.message:
                        field_assignments.append(a)
                    else:
                        actions.append(self._collect_message_field_assignments(field_assignments))
                        field_assignments = [a]
                else:
                    actions.extend(
                        [
                            self._collect_message_field_assignments(field_assignments),
                            a,
                        ]
                    )
                    field_assignments = []

        if len(field_assignments) == 1:
            actions.extend(field_assignments)
        elif len(field_assignments) > 1:
            actions.append(self._collect_message_field_assignments(field_assignments))

        self._actions = actions

    def _collect_message_field_assignments(
        self, field_assignments: list[stmt.MessageFieldAssignment]
    ) -> stmt.VariableAssignment:
        return stmt.VariableAssignment(
            field_assignments[0].identifier,
            expr.DeltaMessageAggregate(
                field_assignments[0].identifier, {a.field: a.expression for a in field_assignments}
            ),
            location=Location(
                field_assignments[0].location.start if field_assignments[0].location else (0, 0),
                field_assignments[0].location.source if field_assignments[0].location else None,
                field_assignments[-1].location.end if field_assignments[-1].location else None,
            ),
        )

    def optimize(self) -> None:
        self._optimize_structures()

    def _optimize_structures(self) -> None:
        """
        Replace local messages with structures if possible.

        If only a limited feature set of a local message object is used it can be replaced with
        a structure. This allows the generation of more size and runtime efficient code.
        """

        def find_identifier(name: ID, expression: expr.Expr) -> bool:
            return bool(
                expression.findall(lambda e: isinstance(e, expr.Variable) and e.identifier == name)
            )

        def find_attribute_prefix(name: ID, expression: expr.Expr) -> bool:
            return bool(
                expression.findall(
                    lambda e: isinstance(e, expr.Attribute) and find_identifier(name, e.prefix)
                )
            )

        def substituted(expression: expr.Expr, structure: rty.Structure) -> expr.Expr:
            def replace_expression_type(expression: expr.Expr) -> expr.Expr:
                if (
                    isinstance(expression, (expr.Variable, expr.Call))
                    and isinstance(expression.type_, rty.Message)
                    and expression.type_.identifier == structure.identifier
                ):
                    expression.type_ = structure
                return expression

            return expression.substituted(replace_expression_type)

        def contains_unsupported_feature(name: ID, action: stmt.Statement) -> bool:
            return (
                (isinstance(action, stmt.Append) and find_identifier(name, action.parameter))
                or (isinstance(action, stmt.AttributeStatement) and action.identifier == name)
                or (isinstance(action, stmt.MessageFieldAssignment) and action.message == name)
                or (
                    isinstance(action, stmt.VariableAssignment)
                    and action.identifier == name
                    and not isinstance(action.expression, expr.Call)
                )
                or (
                    isinstance(action, stmt.Assignment)
                    and find_attribute_prefix(name, action.expression)
                )
            )

        for name, declaration in self.declarations.items():
            if (
                not isinstance(declaration, decl.VariableDeclaration)
                or not isinstance(declaration.type_, rty.Message)
                or not declaration.type_.is_definite
                or (
                    isinstance(declaration, decl.VariableDeclaration)
                    and declaration.expression is not None
                )
            ):
                continue

            for action in self._actions:
                if contains_unsupported_feature(name, action):
                    break
            else:
                message_decl = decl.VariableDeclaration(
                    identifier=declaration.identifier,
                    type_identifier=declaration.type_identifier,
                    expression=declaration.expression,
                    type_=rty.Structure(
                        identifier=declaration.type_.identifier,
                        field_combinations=declaration.type_.field_combinations,
                        parameter_types=declaration.type_.parameter_types,
                        field_types=declaration.type_.field_types,
                    ),
                    location=declaration.location,
                )
                assert isinstance(message_decl.type_, rty.Structure)
                self.declarations[name] = message_decl

                for action in self._actions:
                    if isinstance(action, stmt.Assignment):
                        substituted(action.expression, message_decl.type_)


class AbstractSession(BasicDeclaration):
    # pylint: disable=too-many-arguments, too-many-instance-attributes
    @abstractmethod
    def __init__(
        self,
        identifier: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        super().__init__(identifier, location)
        self.states = [*states, FINAL_STATE] if FINAL_STATE not in states else states
        self.declarations = {d.identifier: d for d in declarations}
        self.parameters = {p.identifier: p for p in parameters}
        self.direct_dependencies = {t.identifier: t for t in types}
        self.types = self.direct_dependencies.copy()
        self.location = location

        refinements = [t for t in types if isinstance(t, Refinement)]

        for t in types:
            if isinstance(t, Message):
                t.set_refinements([r for r in refinements if r.pdu == t])

        assert all(not isinstance(d, decl.FormalDeclaration) for d in self.declarations.values())

        self._global_declarations: Mapping[ID, decl.Declaration] = {
            **self.parameters,
            **self.declarations,
        }
        self._literals = {
            **mty.qualified_type_literals(self.types.values()),
            **mty.enum_literals(self.types.values(), self.package),
        }

        self._normalize()

        self.error.propagate()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.identifier == other.identifier
                and self.states == other.states
                and self.declarations == other.declarations
                and self.parameters == other.parameters
                and self.types == other.types
            )
        return NotImplemented

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "states", "declarations", "parameters"])

    def __str__(self) -> str:
        parameters = "".join([f"{p};\n" for p in self.parameters.values()])
        declarations = "".join([f"{d};\n" for d in self.declarations.values()])
        states = "\n\n".join([f"{s};" for s in self.states if s != FINAL_STATE])
        return (
            f"generic\n{indent(parameters, 3)}session {self.identifier.name} is\n"
            f"{indent(declarations, 3)}begin\n{indent(states, 3)}\nend {self.identifier.name}"
        )

    @property
    def initial_state(self) -> State:
        return self.states[0]

    @property
    def literals(self) -> Mapping[ID, mty.Type]:
        return self._literals

    def _normalize(self) -> None:
        """
        Normalize all expressions of the session.

        Replace variables by literals or function calls where necessary. The distinction between
        variables and literals or variables and function calls without arguments is not possible in
        the parser, as both are syntactically identical.
        """
        functions = [
            p.identifier
            for p in self.parameters.values()
            if isinstance(p, decl.FunctionDeclaration)
        ]

        def substitution(expression: expr.Expr) -> expr.Expr:
            if isinstance(expression, expr.Variable):
                if expression.identifier in self.literals:
                    return expr.Literal(
                        expression.identifier,
                        expression.type_,
                        location=expression.location,
                    )
                if expression.identifier in functions:
                    return expr.Call(
                        expression.identifier,
                        [],
                        expression.negative,
                        expression.immutable,
                        expression.type_,
                        location=expression.location,
                    )
            return expression

        for d in self.declarations.values():
            if isinstance(d, decl.VariableDeclaration) and d.expression:
                d.expression = d.expression.substituted(substitution)

        for state in self.states:
            for d in state.declarations.values():
                if isinstance(d, decl.VariableDeclaration) and d.expression:
                    d.expression = d.expression.substituted(substitution)
            for a in state.actions:
                if isinstance(a, stmt.Assignment):
                    a.expression = a.expression.substituted(substitution)
                if isinstance(a, stmt.AttributeStatement):
                    a.parameters = [p.substituted(substitution) for p in a.parameters]
                if isinstance(a, stmt.Reset):
                    a.associations = {
                        i: e.substituted(substitution) for i, e in a.associations.items()
                    }
            for t in state.transitions:
                t.condition = t.condition.substituted(substitution)


class Session(AbstractSession):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        super().__init__(
            identifier,
            states,
            declarations,
            parameters,
            types,
            location,
        )
        self._validate()
        self.error.propagate()
        self._optimize()

    def _optimize(self) -> None:
        for state in self.states:
            state.optimize()

    def _validate_states(self) -> None:
        if all(s == FINAL_STATE for s in self.states):
            self.error.extend(
                [("empty states", Subsystem.MODEL, Severity.ERROR, self.location)],
            )

        self._validate_state_existence()
        self._validate_duplicate_states()
        self._validate_state_reachability()

    def _validate_state_existence(self) -> None:
        state_identifiers = {s.identifier for s in self.states}
        for s in self.states:
            for t in s.transitions:
                if t.target not in state_identifiers:
                    self.error.extend(
                        [
                            (
                                f'transition from state "{s.identifier}" to non-existent state'
                                f' "{t.target}" in "{self.identifier}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                t.target.location,
                            )
                        ],
                    )

    def _validate_duplicate_states(self) -> None:
        identifier_states = defaultdict(list)
        for s in self.states:
            identifier_states[s.identifier].append(s)

        for identifier, states in identifier_states.items():
            if len(states) >= 2:
                for s in states[1:]:
                    self.error.extend(
                        [
                            (
                                f'duplicate state "{identifier}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                s.location,
                            ),
                            (
                                f'previous definition of state "{identifier}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                states[0].location,
                            ),
                        ],
                    )

    def _validate_state_reachability(self) -> None:
        inputs: Dict[ID, List[ID]] = {}
        for s in self.states:
            for t in [
                *s.transitions,
                *([s.exception_transition] if s.exception_transition else []),
            ]:
                if t.target in inputs:
                    inputs[t.target].append(s.identifier)
                else:
                    inputs[t.target] = [s.identifier]

            if s not in [self.initial_state, FINAL_STATE] and s.identifier not in inputs:
                self.error.extend(
                    [
                        (
                            f'unreachable state "{s.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            s.location,
                        )
                    ],
                )

            if s != FINAL_STATE and not s.transitions:
                self.error.extend(
                    [
                        (
                            f'detached state "{s.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            s.location,
                        )
                    ],
                )

    def _validate_declarations(
        self,
        declarations: Mapping[ID, decl.Declaration],
        visible_declarations: Mapping[ID, decl.Declaration],
    ) -> None:
        visible_declarations = dict(visible_declarations)

        def undefined_type(type_identifier: StrID, location: Optional[Location]) -> None:
            self.error.extend(
                [
                    (
                        f'undefined type "{type_identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        location,
                    )
                ],
            )

        for k, d in declarations.items():
            if k in visible_declarations:
                self.error.extend(
                    [
                        (
                            f'local variable "{k}" shadows previous declaration',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            d.location,
                        ),
                        (
                            f'previous declaration of variable "{k}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            visible_declarations[k].location,
                        ),
                    ],
                )

            self._reference_variable_declaration(d.variables(), visible_declarations)

            if isinstance(d, decl.TypeDeclaration):
                type_identifier = mty.internal_type_identifier(k, self.package)
                if type_identifier in self.types:
                    self.error.extend(
                        [(f'type "{k}" shadows type', Subsystem.MODEL, Severity.ERROR, d.location)],
                    )
                self.types[type_identifier] = d.type_definition

            elif isinstance(d, decl.TypeCheckableDeclaration):
                type_identifier = mty.internal_type_identifier(d.type_identifier, self.package)
                if type_identifier in self.types:
                    self.error.extend(
                        d.check_type(
                            self.types[type_identifier].type_,
                            lambda x: self._typify_variable(x, visible_declarations),
                        )
                    )
                else:
                    undefined_type(d.type_identifier, d.location)
                    d.type_ = rty.Any()

                if isinstance(d, decl.FunctionDeclaration):
                    for a in d.arguments:
                        type_identifier = mty.internal_type_identifier(
                            a.type_identifier, self.package
                        )
                        if type_identifier in self.types:
                            a.type_ = self.types[type_identifier].type_
                        else:
                            a.type_ = rty.Any()
                            undefined_type(a.type_identifier, d.location)

                if d.type_identifier in self._global_declarations:
                    self._global_declarations[d.type_identifier].reference()

            visible_declarations[k] = d

    def _validate_actions(
        self,
        actions: Sequence[stmt.Statement],
        declarations: Mapping[ID, decl.Declaration],
        local_declarations: Mapping[ID, decl.Declaration],
    ) -> None:
        io_statements = [a for a in actions if isinstance(a, stmt.ChannelAttributeStatement)]

        if io_statements:
            if local_declarations:
                self.error.extend(
                    [
                        (
                            "IO state must not contain declarations",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            next(iter(local_declarations.values())).location,
                        )
                    ],
                )

            if len(io_statements) != len(actions):
                self.error.extend(
                    [
                        (
                            "channel IO must not be combined with other actions in one state",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            io_statements[0].location,
                        )
                    ],
                )

        for i, s1 in enumerate(io_statements):
            if not isinstance(s1.parameter, expr.Variable):
                self.error.extend(
                    [
                        (
                            "channel parameter must be a variable",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            s1.location,
                        )
                    ],
                )

            for j, s2 in enumerate(io_statements):
                if i >= j:
                    continue

                if s1.identifier == s2.identifier:
                    self.error.extend(
                        [
                            (
                                f'channel "{s1.identifier}" may be read or written'
                                " at most once per state",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                s1.identifier.location,
                            )
                        ],
                    )
                    self.error.extend(
                        [
                            (
                                "conflicting read/write",
                                Subsystem.MODEL,
                                Severity.INFO,
                                s2.identifier.location,
                            )
                        ],
                    )

                if (
                    isinstance(s1.parameter, expr.Variable)
                    and isinstance(s2.parameter, expr.Variable)
                    and s1.parameter.identifier == s2.parameter.identifier
                ):
                    self.error.extend(
                        [
                            (
                                f'message "{s1.parameter.identifier}" may be read or written'
                                " at most once per state",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                s1.parameter.location,
                            )
                        ],
                    )
                    self.error.extend(
                        [
                            (
                                "conflicting read/write",
                                Subsystem.MODEL,
                                Severity.INFO,
                                s2.parameter.location,
                            )
                        ],
                    )

        for a in actions:
            try:
                type_ = declarations[a.identifier].type_
            except KeyError:
                type_ = rty.Undefined()

            self.error.extend(
                a.check_type(
                    type_,
                    lambda x: self._typify_variable(x, declarations),
                )
            )

            self._reference_variable_declaration(a.variables(), declarations)

    def _validate_transitions(
        self, state: State, declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for t in state.transitions:
            t.condition = t.condition.substituted(lambda x: self._typify_variable(x, declarations))
            self.error.extend(t.condition.check_type(rty.BOOLEAN))
            self._reference_variable_declaration(t.condition.variables(), declarations)

        if not state.exception_transition and state.has_exceptions:
            self.error.extend(
                [
                    (
                        f'missing exception transition in state "{state.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        state.location,
                    )
                ],
            )

        if state.exception_transition and not state.has_exceptions:
            self.error.extend(
                [
                    (
                        f'unnecessary exception transition in state "{state.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        state.exception_transition.location,
                    )
                ],
            )

    def _validate_usage(self) -> None:
        global_declarations = self._global_declarations.items()
        local_declarations = ((k, d) for s in self.states for k, d in s.declarations.items())
        for k, d in itertools.chain(global_declarations, local_declarations):
            if not d.is_referenced:
                self.error.extend(
                    [
                        (
                            f'unused {d.DESCRIPTIVE_NAME} "{k}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            d.location,
                        )
                    ],
                )

    def _typify_variable(  # pylint: disable = too-many-branches
        self, expression: expr.Expr, declarations: Mapping[ID, decl.Declaration]
    ) -> expr.Expr:
        if isinstance(
            expression,
            (
                expr.Variable,
                expr.Literal,
                expr.Call,
                expr.Conversion,
                expr.MessageAggregate,
                expr.DeltaMessageAggregate,
            ),
        ):
            identifier = expression.identifier

            if isinstance(expression, expr.Variable):
                assert identifier not in self._literals
                if identifier in declarations:
                    expression.type_ = declarations[identifier].type_
            if isinstance(expression, expr.Literal):
                if identifier in self._literals:
                    expression.type_ = self._literals[identifier].type_
            if isinstance(expression, expr.Call):
                if identifier in declarations:
                    expression.type_ = declarations[identifier].type_
                    declaration = declarations[identifier]
                    assert isinstance(declaration, decl.FunctionDeclaration)
                    expression.argument_types = [a.type_ for a in declaration.arguments]
            if isinstance(expression, expr.Conversion):
                if identifier in self.types:
                    expression.type_ = self.types[identifier].type_
                    expression.argument_types = [
                        t.pdu.type_
                        for t in self.types.values()
                        if isinstance(t, Refinement) and t.sdu.identifier == identifier
                    ]
            if isinstance(expression, expr.MessageAggregate):
                type_identifier = mty.internal_type_identifier(identifier, self.package)
                if type_identifier in self.types:
                    expression.type_ = self.types[type_identifier].type_
            if isinstance(expression, expr.DeltaMessageAggregate):
                if identifier in declarations:
                    expression.type_ = declarations[identifier].type_

        return expression

    @staticmethod
    def _reference_variable_declaration(
        variables: Iterable[expr.Variable], declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for v in variables:
            try:
                declarations[v.identifier].reference()
            except KeyError:
                pass

    def _validate(self) -> None:
        self._validate_states()

        self._validate_declarations(self._global_declarations, {})

        for s in self.states:
            self._validate_declarations(s.declarations, self._global_declarations)

            declarations = {**self._global_declarations, **s.declarations}

            self._validate_actions(s.actions, declarations, s.declarations)
            self._validate_transitions(s, declarations)

        self._validate_usage()


class UnprovenSession(AbstractSession):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        # pylint: disable=useless-super-delegation
        super().__init__(
            identifier,
            states,
            declarations,
            parameters,
            types,
            location,
        )

    def proven(self) -> Session:
        return Session(
            self.identifier,
            self.states,
            list(self.declarations.values()),
            list(self.parameters.values()),
            list(self.types.values()),
            self.location,
        )


FINAL_STATE: Final[State] = State("Final")
