import itertools
from abc import abstractmethod
from collections import defaultdict
from typing import Dict, Iterable, List, Mapping, Optional, Sequence

from rflx import declaration as decl, expression as expr, statement as stmt, typing_ as rty
from rflx.common import Base, indent, indent_next, verbose_repr
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID
from rflx.model import Message, Refinement

from . import type_ as mty


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
        with_aspects = f'\n   with Desc => "{self.description}"' if self.description else ""
        if_condition = (
            f"\n   if {indent_next(str(self.condition), 6)}" if self.condition != expr.TRUE else ""
        )
        return f"then {self.target}{with_aspects}{if_condition}"


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

        self.__identifier = ID(identifier)
        self.__transitions = transitions or []
        self.__exception_transition = exception_transition
        self.__actions = actions or []
        self.declarations = {d.identifier: d for d in declarations} if declarations else {}
        self.description = description
        self.location = location

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "transitions", "actions", "declarations"])

    def __str__(self) -> str:
        with_aspects = f'\n   with Desc => "{self.description}"\n' if self.description else " "
        if not self.declarations and not self.actions and not self.transitions:
            return f"state {self.identifier}{with_aspects}is null state"
        declarations = "".join([f"{d};\n" for d in self.declarations.values()])
        actions = "".join([f"{a};\n" for a in self.actions])
        transitions = "\n".join([f"{p}" for p in self.transitions])
        return (
            f"state {self.identifier}{with_aspects}is\n{indent(declarations, 3)}begin\n"
            f"{indent(actions, 3)}"
            f"transition\n{indent(transitions, 3)}\nend {self.identifier}"
        )

    @property
    def identifier(self) -> ID:
        return self.__identifier

    @property
    def transitions(self) -> Sequence[Transition]:
        return self.__transitions or []

    @property
    def exception_transition(self) -> Optional[Transition]:
        return self.__exception_transition

    @property
    def actions(self) -> Sequence[stmt.Statement]:
        return self.__actions

    @property
    def is_null(self) -> bool:
        return not self.__transitions

    @property
    def has_exceptions(self) -> bool:
        return any(
            isinstance(a, (stmt.Append, stmt.Extend, stmt.Write))
            or (
                isinstance(a, stmt.Assignment)
                and (
                    a.expression.findall(
                        lambda x: isinstance(
                            x,
                            (
                                expr.Selected,
                                expr.Head,
                                expr.Comprehension,
                                expr.MessageAggregate,
                                expr.Conversion,
                                expr.Opaque,
                            ),
                        )
                    )
                )
            )
            for a in self.__actions
        )


class AbstractSession(Base):
    # pylint: disable=too-many-arguments, too-many-instance-attributes
    @abstractmethod
    def __init__(
        self,
        identifier: StrID,
        initial: StrID,
        final: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        self.identifier = ID(identifier)
        self.initial = ID(initial)
        self.final = ID(final)
        self.states = states
        self.declarations = {d.identifier: d for d in declarations}
        self.parameters = {p.identifier: p for p in parameters}
        self.types = {t.identifier: t for t in types}
        self.location = location
        self.error = RecordFluxError()

        refinements = [t for t in types if isinstance(t, Refinement)]

        for t in types:
            if isinstance(t, Message):
                t.set_refinements([r for r in refinements if r.pdu == t])

        assert all(not isinstance(d, decl.FormalDeclaration) for d in self.declarations.values())

        self._global_declarations: Mapping[ID, decl.Declaration] = {
            **self.parameters,
            **self.declarations,
        }

        if len(self.identifier.parts) != 2:
            self.error.append(
                f'invalid session name "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.identifier.location,
            )

        self._literals = {
            **mty.qualified_type_literals(self.types.values()),
            **mty.enum_literals(self.types.values(), self.package),
        }

        self.error.propagate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "initial", "states", "declarations", "parameters"])

    def __str__(self) -> str:
        parameters = "".join([f"{p};\n" for p in self.parameters.values()])
        declarations = "".join([f"{d};\n" for d in self.declarations.values()])
        states = "\n\n".join([f"{s};" for s in self.states])
        return (
            f"generic\n{indent(parameters, 3)}session {self.identifier.name} with\n"
            f"   Initial => {self.initial},\n   Final => {self.final}\n"
            f"is\n{indent(declarations, 3)}begin\n{indent(states, 3)}\nend {self.identifier.name}"
        )

    @property
    def package(self) -> ID:
        return self.identifier.parent


class Session(AbstractSession):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        initial: StrID,
        final: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        super().__init__(
            identifier, initial, final, states, declarations, parameters, types, location
        )
        self.__validate()
        self.error.propagate()

    def __validate_states(self) -> None:
        if not self.states:
            self.error.append(
                "empty states",
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.__validate_state_existence()
        self.__validate_duplicate_states()
        self.__validate_state_reachability()

    def __validate_state_existence(self) -> None:
        state_identifiers = [s.identifier for s in self.states]
        if self.initial not in state_identifiers:
            self.error.append(
                f'initial state "{self.initial}" does not exist in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.initial.location,
            )
        if self.final not in state_identifiers:
            self.error.append(
                f'final state "{self.final}" does not exist in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.final.location,
            )
        for s in self.states:
            for t in s.transitions:
                if t.target not in state_identifiers:
                    self.error.append(
                        f'transition from state "{s.identifier}" to non-existent state'
                        f' "{t.target}" in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        t.target.location,
                    )

    def __validate_duplicate_states(self) -> None:
        identifier_states = defaultdict(list)
        for s in self.states:
            identifier_states[s.identifier].append(s)

        for identifier, states in identifier_states.items():
            if len(states) >= 2:
                for s in states[1:]:
                    self.error.append(
                        f'duplicate state "{identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        s.location,
                    )
                    self.error.append(
                        f'previous definition of state "{identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        states[0].location,
                    )

    def __validate_state_reachability(self) -> None:
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

            if s.identifier != self.initial and s.identifier not in inputs:
                self.error.append(
                    f'unreachable state "{s.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    s.location,
                )

            if s.identifier != self.final and not s.transitions:
                self.error.append(
                    f'detached state "{s.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    s.location,
                )

    def __validate_declarations(
        self,
        declarations: Mapping[ID, decl.Declaration],
        visible_declarations: Mapping[ID, decl.Declaration],
    ) -> None:
        visible_declarations = dict(visible_declarations)

        def undefined_type(type_identifier: StrID, location: Optional[Location]) -> None:
            self.error.append(
                f'undefined type "{type_identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        for k, d in declarations.items():
            if k in visible_declarations:
                self.error.append(
                    f'local variable "{k}" shadows previous declaration',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    d.location,
                )
                self.error.append(
                    f'previous declaration of variable "{k}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    visible_declarations[k].location,
                )

            self.__reference_variable_declaration(d.variables(), visible_declarations)

            if isinstance(d, decl.TypeDeclaration):
                type_identifier = mty.qualified_type_identifier(k, self.package)
                if type_identifier in self.types:
                    self.error.append(
                        f'type "{k}" shadows type',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        d.location,
                    )
                self.types[type_identifier] = d.type_definition

            elif isinstance(d, decl.TypeCheckableDeclaration):
                type_identifier = mty.qualified_type_identifier(d.type_identifier, self.package)
                if type_identifier in self.types:
                    self.error.extend(
                        d.check_type(
                            self.types[type_identifier].type_,
                            lambda x: self.__typify_variable(x, visible_declarations),
                        )
                    )
                else:
                    undefined_type(d.type_identifier, d.location)
                    d.type_ = rty.Any()

                if isinstance(d, decl.FunctionDeclaration):
                    for a in d.arguments:
                        type_identifier = mty.qualified_type_identifier(
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

    def __validate_actions(
        self, actions: Sequence[stmt.Statement], declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for a in actions:
            try:
                type_ = declarations[a.identifier].type_
            except KeyError:
                type_ = rty.Undefined()

            self.error.extend(
                a.check_type(
                    type_,
                    lambda x: self.__typify_variable(x, declarations),
                )
            )

            self.__reference_variable_declaration(a.variables(), declarations)

    def __validate_transitions(
        self, state: State, declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for t in state.transitions:
            t.condition = t.condition.substituted(lambda x: self.__typify_variable(x, declarations))
            self.error.extend(t.condition.check_type(rty.BOOLEAN))
            self.__reference_variable_declaration(t.condition.variables(), declarations)

        if not state.exception_transition and state.has_exceptions:
            self.error.append(
                f'missing exception transition in state "{state.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                state.location,
            )

        if state.exception_transition and not state.has_exceptions:
            self.error.append(
                f'unnecessary exception transition in state "{state.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                state.exception_transition.location,
            )

    def __validate_usage(self) -> None:
        global_declarations = self._global_declarations.items()
        local_declarations = ((k, d) for s in self.states for k, d in s.declarations.items())
        for k, d in itertools.chain(global_declarations, local_declarations):
            if not d.is_referenced:
                self.error.append(
                    f'unused {d.DESCRIPTIVE_NAME} "{k}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    d.location,
                )

    def __typify_variable(
        self, expression: expr.Expr, declarations: Mapping[ID, decl.Declaration]
    ) -> expr.Expr:
        if isinstance(
            expression, (expr.Variable, expr.Call, expr.Conversion, expr.MessageAggregate)
        ):
            identifier = expression.identifier

            if isinstance(expression, expr.Variable):
                if identifier in declarations:
                    expression.type_ = declarations[identifier].type_
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
                type_identifier = mty.qualified_type_identifier(identifier, self.package)
                if type_identifier in self.types:
                    expression.type_ = self.types[type_identifier].type_

        return expression

    @staticmethod
    def __reference_variable_declaration(
        variables: Iterable[expr.Variable], declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for v in variables:
            try:
                declarations[v.identifier].reference()
            except KeyError:
                pass

    def __validate(self) -> None:
        self.__validate_states()

        self.__validate_declarations(self._global_declarations, {})

        for s in self.states:
            self.__validate_declarations(s.declarations, self._global_declarations)

            declarations = {**self._global_declarations, **s.declarations}

            self.__validate_actions(s.actions, declarations)
            self.__validate_transitions(s, declarations)

        self.__validate_usage()


class UnprovenSession(AbstractSession):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        initial: StrID,
        final: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.BasicDeclaration],
        parameters: Sequence[decl.FormalDeclaration],
        types: Sequence[mty.Type],
        location: Location = None,
    ):
        # pylint: disable=useless-super-delegation
        super().__init__(
            identifier, initial, final, states, declarations, parameters, types, location
        )

    def proven(self) -> Session:
        return Session(
            self.identifier,
            self.initial,
            self.final,
            self.states,
            list(self.declarations.values()),
            list(self.parameters.values()),
            list(self.types.values()),
            self.location,
        )
