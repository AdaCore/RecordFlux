import itertools
from collections import defaultdict
from typing import Dict, Iterable, List, Mapping, Optional, Sequence, cast

from rflx import declaration as decl, expression as expr, statement as stmt, typing_ as rty
from rflx.common import Base, indent, indent_next, verbose_repr
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID
from rflx.model import Message, Refinement

from . import type_ as mty

CHANNEL_FUNCTIONS = {
    ID("Data_Available"): rty.Channel(readable=True, writable=False),
}


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
        actions: Sequence[stmt.Statement] = None,
        declarations: Sequence[decl.Declaration] = None,
        description: str = None,
        location: Location = None,
    ):
        # pylint: disable=too-many-arguments
        self.__identifier = ID(identifier)
        self.__transitions = transitions or []
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
    def actions(self) -> Sequence[stmt.Statement]:
        return self.__actions


class AbstractSession(Base):
    # pylint: disable=too-many-arguments, too-many-instance-attributes
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
        # pylint: disable=unidiomatic-typecheck
        if type(self) == AbstractSession:
            raise RuntimeError("AbstractSession must not be instantiated")
        self.identifier = ID(identifier)
        self.initial = ID(initial)
        self.final = ID(final)
        self.states = states
        self.declarations = {d.identifier: d for d in declarations}
        self.parameters = {p.identifier: p for p in parameters}
        self.types = {t.identifier: t for t in types}
        self.location = location
        self.error = RecordFluxError()

        assert all(not isinstance(d, decl.FormalDeclaration) for d in self.declarations.values())

        self.__global_declarations: Mapping[ID, decl.Declaration] = {
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

        self.__literals = {
            **mty.qualified_type_literals(self.types.values()),
            **mty.qualified_enum_literals(self.types.values(), self.package),
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

    def _validate(self) -> None:
        self.__validate_states()

        self.__validate_declarations(self.__global_declarations, {})

        for s in self.states:
            self.__validate_declarations(s.declarations, self.__global_declarations)

            declarations = {**self.__global_declarations, **s.declarations}

            self.__validate_actions(s.actions, declarations)
            self.__validate_transitions(s.transitions, declarations)

        self.__validate_usage()

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
            for t in s.transitions:
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
        # pylint: disable=too-many-branches

        visible_declarations = dict(visible_declarations)

        def undefined_type(type_identifier: StrID, location: Optional[Location]) -> None:
            self.error.append(
                f'undefined type "{type_identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        for k, d in declarations.items():
            if any(str(d.identifier).upper() == str(f).upper() for f in CHANNEL_FUNCTIONS):
                self.error.append(
                    f'{d.descriptive_name} declaration shadows built-in function "{k}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    d.location,
                )

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
                    model_type = self.types[type_identifier]
                    self.error.extend(
                        d.check_type(
                            model_type.refined_type(
                                [
                                    t
                                    for t in self.types.values()
                                    if isinstance(t, Refinement)
                                    and t.pdu.identifier == d.type_identifier
                                ]
                            )
                            if isinstance(model_type, Message)
                            else model_type.type_,
                            lambda x: self.__typify_variable(x, visible_declarations),
                        )
                    )
                else:
                    undefined_type(d.type_identifier, d.location)
                    d.type_ = rty.Any()

                if isinstance(d, decl.FunctionDeclaration):
                    argument_types = []
                    for a in d.arguments:
                        type_identifier = mty.qualified_type_identifier(
                            a.type_identifier, self.package
                        )
                        if type_identifier in self.types:
                            argument_types.append(self.types[type_identifier].type_)
                        else:
                            argument_types.append(rty.Any())
                            undefined_type(a.type_identifier, d.location)
                    d.argument_types = argument_types

                if d.type_identifier in self.__global_declarations:
                    self.__global_declarations[d.type_identifier].reference()

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
        self, transitions: Sequence[Transition], declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for t in transitions:
            t.condition = t.condition.substituted(lambda x: self.__typify_variable(x, declarations))
            self.error.extend(t.condition.check_type(rty.BOOLEAN))
            self.__reference_variable_declaration(t.condition.variables(), declarations)

    def __validate_usage(self) -> None:
        global_declarations = self.__global_declarations.items()
        local_declarations = ((k, d) for s in self.states for k, d in s.declarations.items())
        for k, d in itertools.chain(global_declarations, local_declarations):
            if not d.is_referenced:
                self.error.append(
                    f'unused {d.descriptive_name} "{k}"',
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
                if identifier in self.__literals:
                    expression.type_ = self.__literals[identifier].type_
            if isinstance(expression, expr.Call):
                if identifier in declarations:
                    expression.type_ = declarations[identifier].type_
                    declaration = declarations[identifier]
                    assert isinstance(declaration, decl.FunctionDeclaration)
                    expression.argument_types = declaration.argument_types
                if identifier in CHANNEL_FUNCTIONS:
                    expression.type_ = rty.BOOLEAN
                    expression.argument_types = [CHANNEL_FUNCTIONS[identifier]]
            if isinstance(expression, expr.Conversion):
                if identifier in self.types:
                    expression.type_ = self.types[identifier].type_
                    expression.argument_types = [
                        t.pdu.type_
                        for t in self.types.values()
                        if isinstance(t, Refinement) and t.sdu.identifier == identifier
                    ]
            if isinstance(expression, expr.MessageAggregate):
                if identifier in self.types:
                    message = self.types[identifier]
                    assert isinstance(message, Message)
                    expression.type_ = message.refined_type(
                        [
                            t
                            for t in self.types.values()
                            if isinstance(t, Refinement) and t.pdu.identifier == identifier
                        ]
                    )

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


class Session(AbstractSession):
    # pylint: disable=too-many-arguments, super-init-not-called
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
        raise RuntimeError("Session must not be instantiated directly")


class UnprovenSession(AbstractSession):
    def proven(self) -> Session:
        self._validate()
        self.error.propagate()
        return cast(Session, self)
