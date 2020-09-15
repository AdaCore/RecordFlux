import itertools
from typing import Dict, Iterable, List, Mapping, Sequence

from rflx import declaration as decl, expression as expr, statement as stmt
from rflx.common import Base, indent, indent_next, verbose_repr
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID

from . import type_ as mty

BUILTIN_CHANNEL_FUNCTIONS = list(map(ID, ["Read", "Write", "Call", "Data_Available"]))
BUILTIN_LIST_FUNCTIONS = list(map(ID, ["Append", "Extend"]))
BUILTIN_FUNCTIONS = [*BUILTIN_CHANNEL_FUNCTIONS, *BUILTIN_LIST_FUNCTIONS]


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
        self.location = location
        self.description = description

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
        name: StrID,
        transitions: Sequence[Transition] = None,
        actions: Sequence[stmt.Statement] = None,
        declarations: Sequence[decl.Declaration] = None,
        location: Location = None,
    ):
        self.__name = ID(name)
        self.__transitions = transitions or []
        self.__actions = actions or []
        self.declarations = {d.identifier: d for d in declarations} if declarations else {}
        self.location = location

    def __repr__(self) -> str:
        return verbose_repr(self, ["name", "transitions", "actions", "declarations"])

    def __str__(self) -> str:
        if not self.declarations and not self.actions and not self.transitions:
            return f"state {self.name} is null state"
        declarations = "".join(f"{d};\n" for d in self.declarations.values())
        actions = "".join(f"{a};\n" for a in self.actions)
        transitions = "\n".join(f"{p}" for p in self.transitions)
        return (
            f"state {self.name} is\n{indent(declarations, 3)}begin\n{indent(actions, 3)}"
            f"transition\n{indent(transitions, 3)}\nend {self.name}"
        )

    @property
    def name(self) -> ID:
        return self.__name

    @property
    def transitions(self) -> Sequence[Transition]:
        return self.__transitions or []

    @property
    def actions(self) -> Sequence[stmt.Statement]:
        return self.__actions


class Session(Base):
    # pylint: disable=too-many-arguments, too-many-instance-attributes
    def __init__(
        self,
        identifier: StrID,
        initial: StrID,
        final: StrID,
        states: Sequence[State],
        declarations: Sequence[decl.Declaration],
        parameters: Sequence[decl.Declaration],
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

        if len(self.identifier.parts) != 2:
            self.error.append(
                f'invalid session name "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        if not states:
            self.error.append(
                "empty states",
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        self.__literals = mty.qualified_literals(self.types.values(), self.identifier.parent)

        self.__validate_state_existence()
        self.__validate_duplicate_states()
        self.__validate_state_reachability()
        self.__validate_declarations()
        self.__validate_conditions()
        self.__validate_actions()
        self.__validate_variable_usage()
        self.error.propagate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "initial", "states", "declarations", "parameters"])

    def __str__(self) -> str:
        parameters = "".join(f"{p};\n" for p in self.parameters.values())
        declarations = "".join(f"{d};\n" for d in self.declarations.values())
        states = "\n\n".join(f"{s};" for s in self.states)
        return (
            f"generic\n{indent(parameters, 3)}session {self.identifier.name} with\n"
            f"   Initial => {self.initial},\n   Final => {self.final}\n"
            f"is\n{indent(declarations, 3)}begin\n{indent(states, 3)}\nend {self.identifier.name}"
        )

    def __validate_conditions(self) -> None:
        for s in self.states:
            declarations = {
                **self.parameters,
                **self.declarations,
                **s.declarations,
            }
            for t in s.transitions:
                self.__validate_variable_declaration(t.condition.variables(), declarations)

    def __validate_actions(self) -> None:
        for s in self.states:
            declarations = {
                **self.parameters,
                **self.declarations,
                **s.declarations,
            }
            for a in s.actions:
                if isinstance(a, stmt.Assignment):
                    for c in a.expression.findall(
                        lambda x: isinstance(x, expr.Call) and x.name in BUILTIN_CHANNEL_FUNCTIONS
                    ):
                        assert isinstance(c, expr.Call)
                        self.__validate_channel(c, declarations)
                self.__validate_variable_declaration(a.variables(), declarations)

    def __validate_state_existence(self) -> None:
        state_names = [s.name for s in self.states]
        if self.initial not in state_names:
            self.error.append(
                f'initial state "{self.initial}" does not exist in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.initial.location,
            )
        if self.final not in state_names:
            self.error.append(
                f'final state "{self.final}" does not exist in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.final.location,
            )
        for s in self.states:
            for t in s.transitions:
                if t.target not in state_names:
                    self.error.append(
                        f'transition from state "{s.name}" to non-existent state'
                        f' "{t.target}" in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        t.target.location,
                    )

    def __validate_duplicate_states(self) -> None:
        state_names = [s.name for s in self.states]
        seen: Dict[ID, int] = {}
        duplicates: List[ID] = []
        for n in state_names:
            if n not in seen:
                seen[n] = 1
            else:
                duplicates.append(n.name)
                seen[n] += 1

        if duplicates:
            self.error.append(
                f'duplicate states: {", ".join(map(str, sorted(duplicates)))}',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

    def __validate_state_reachability(self) -> None:
        inputs: Dict[ID, List[ID]] = {}
        for s in self.states:
            for t in s.transitions:
                if t.target in inputs:
                    inputs[t.target].append(s.name)
                else:
                    inputs[t.target] = [s.name]
        unreachable = [
            str(s.name) for s in self.states if s.name != self.initial and s.name not in inputs
        ]
        if unreachable:
            self.error.append(
                f'unreachable states {", ".join(unreachable)}',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        detached = [str(s.name) for s in self.states if s.name != self.final and not s.transitions]
        if detached:
            self.error.append(
                f'detached states {", ".join(detached)}',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

    @staticmethod
    def __entity_name(declaration: decl.Declaration) -> str:
        if isinstance(declaration, decl.SubprogramDeclaration):
            return "subprogram"
        if isinstance(declaration, decl.VariableDeclaration):
            return "variable"
        if isinstance(declaration, decl.RenamingDeclaration):
            return "renames"
        if isinstance(declaration, decl.ChannelDeclaration):
            return "channel"
        if isinstance(declaration, decl.PrivateDeclaration):
            return "private"
        assert False, f"unsupported entity {type(decl).__name__}"

    def __validate_declarations(self) -> None:
        for s in self.states:
            for k, d in s.declarations.items():
                if k in self.declarations:
                    self.error.append(
                        f'local variable "{k}" shadows global declaration'
                        f" in state {s.name.name}",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    )

        for k, d in self.declarations.items():
            if any(str(k).upper() == str(f).upper() for f in BUILTIN_FUNCTIONS):
                self.error.append(
                    f'{self.__entity_name(d)} declaration shadows builtin subprogram "{k}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )

        for k, d in self.declarations.items():
            if isinstance(d, decl.VariableDeclaration):
                try:
                    if d.type_name in (
                        d.identifier
                        for d in self.parameters.values()
                        if isinstance(d, decl.PrivateDeclaration)
                    ):
                        continue
                    type_name = mty.qualified_type_name(d.type_name, self.identifier.parent)
                    self.types[type_name]  # pylint: disable=pointless-statement
                except KeyError:
                    self.error.append(
                        f'undefined type "{d.type_name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        d.location,
                    )

        declarations: Dict[ID, decl.Declaration] = {}
        for k, d in self.declarations.items():
            self.__validate_variable_declaration(d.variables(), declarations)
            declarations[k] = d

        for s in self.states:
            declarations = {**self.declarations}
            for k, d in s.declarations.items():
                self.__validate_variable_declaration(d.variables(), declarations)
                declarations[k] = d

    def __validate_variable_usage(self) -> None:
        global_declarations = ((k, d, None) for k, d in self.declarations.items())
        local_declarations = ((k, d, s) for s in self.states for k, d in s.declarations.items())
        for k, d, s in itertools.chain(global_declarations, local_declarations):
            if not d.is_referenced:
                state = f" in state {s.name}" if s else ""
                self.error.append(
                    f'unused {self.__entity_name(d)} "{k}"{state}',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    d.location,
                )

    def __validate_variable_declaration(
        self, variables: Iterable[expr.Variable], declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        for v in variables:
            if v.identifier in self.__literals:
                continue
            if v.identifier in BUILTIN_FUNCTIONS:
                continue
            if v.identifier not in declarations:
                self.error.append(
                    f'undeclared variable "{v.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                continue
            declarations[v.identifier].reference()

    def __validate_channel(
        self, call: expr.Call, declarations: Mapping[ID, decl.Declaration]
    ) -> None:
        if len(call.args) < 1:
            self.error.append(
                f'no channel argument in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
            return
        channel_id = call.args[0]
        if not isinstance(channel_id, expr.Variable):
            self.error.append(
                f'invalid channel ID type in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
            return
        assert isinstance(channel_id, expr.Variable)
        if channel_id.identifier not in declarations:
            self.error.append(
                f'undeclared channel "{channel_id}" in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
            return

        assert isinstance(channel_id, expr.Variable)
        channel = declarations[channel_id.identifier]
        if not isinstance(channel, decl.ChannelDeclaration):
            self.error.append(
                f'invalid channel type in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
            return

        assert isinstance(channel, decl.ChannelDeclaration)
        channel.reference()
        if call.name in map(ID, ["Write", "Call"]) and not channel.writable:
            self.error.append(
                f'channel "{channel_id}" not writable in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
        if call.name in map(ID, ["Call", "Read", "Data_Available"]) and not channel.readable:
            self.error.append(
                f'channel "{channel_id}" not readable in call to "{call.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                call.location,
            )
