from typing import Dict, List, Sequence

from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.expression import (
    TRUE,
    Channel,
    Declaration,
    Expr,
    PrivateDeclaration,
    Renames,
    Subprogram,
    VariableDeclaration,
)
from rflx.identifier import ID, StrID
from rflx.model import Base
from rflx.statement import Statement


class StateName(Base):
    def __init__(self, name: str, location: Location = None):
        self.name = name
        self.location = location

    def __hash__(self) -> int:
        return hash(self.name)

    def __lt__(self, other: object) -> bool:
        if isinstance(other, StateName):
            return self.name < other.name
        return NotImplemented


class Transition(Base):
    def __init__(self, target: StateName, condition: Expr = TRUE, location: Location = None):
        self.target = target
        self.condition = condition
        self.location = location

    def validate(self, declarations: Dict[ID, Declaration]) -> None:
        self.condition.simplified().validate(declarations)


class State(Base):
    def __init__(
        self,
        name: StateName,
        transitions: Sequence[Transition] = None,
        actions: Sequence[Statement] = None,
        declarations: Dict[StrID, Declaration] = None,
        location: Location = None,
    ):
        self.__name = name
        self.__transitions = transitions or []
        self.__actions = actions or []
        self.declarations = {ID(k): v for k, v in declarations.items()} if declarations else {}
        self.location = location

    @property
    def name(self) -> StateName:
        return self.__name

    @property
    def transitions(self) -> Sequence[Transition]:
        return self.__transitions or []

    @property
    def actions(self) -> Sequence[Statement]:
        return self.__actions


class Session(Base):
    def __init__(
        self,
        name: StrID,
        initial: StateName,
        final: StateName,
        states: Sequence[State],
        declarations: Dict[StrID, Declaration],
        location: Location = None,
    ):  # pylint: disable=too-many-arguments
        self.name = ID(name)
        self.initial = initial
        self.final = final
        self.states = states
        self.declarations = {ID(k): v for k, v in declarations.items()}
        self.location = location
        self.error = RecordFluxError()

        if not states:
            self.error.append(
                "empty states", Subsystem.SESSION, Severity.ERROR, location,
            )
        self.__validate_state_existence()
        self.__validate_duplicate_states()
        self.__validate_state_reachability()
        self.__validate_conditions()
        self.__validate_actions()
        self.__validate_declarations()
        self.error.propagate()

    def __validate_conditions(self) -> None:
        for s in self.states:
            declarations = s.declarations
            for t in s.transitions:
                try:
                    t.validate({**self.declarations, **declarations})
                except RecordFluxError as e:
                    self.error.extend(e)

    def __validate_actions(self) -> None:
        for s in self.states:
            declarations = s.declarations
            for index, a in enumerate(s.actions):
                try:
                    a.validate({**self.declarations, **declarations})
                except RecordFluxError as e:
                    self.error.append(
                        f"invalid action {index} of state {s.name.name}",
                        Subsystem.SESSION,
                        Severity.ERROR,
                        a.location,
                    )
                    self.error.extend(e)

    def __validate_state_existence(self) -> None:
        state_names = [s.name for s in self.states]
        if self.initial not in state_names:
            self.error.append(
                f'initial state "{self.initial.name}" does not exist in "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.initial.location,
            )
        if self.final not in state_names:
            self.error.append(
                f'final state "{self.final.name}" does not exist in "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.final.location,
            )
        for s in self.states:
            for t in s.transitions:
                if t.target not in state_names:
                    self.error.append(
                        f'transition from state "{s.name.name}" to non-existent state'
                        f' "{t.target.name}" in "{self.name}"',
                        Subsystem.SESSION,
                        Severity.ERROR,
                        t.target.location,
                    )

    def __validate_duplicate_states(self) -> None:
        state_names = [s.name for s in self.states]
        seen: Dict[StateName, int] = {}
        duplicates: List[str] = []
        for n in state_names:
            if n not in seen:
                seen[n] = 1
            else:
                duplicates.append(n.name)
                seen[n] += 1

        if duplicates:
            self.error.append(
                f'duplicate states: {", ".join(map(str, sorted(duplicates)))}',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

    def __validate_state_reachability(self) -> None:
        inputs: Dict[StateName, List[StateName]] = {}
        for s in self.states:
            for t in s.transitions:
                if t.target in inputs:
                    inputs[t.target].append(s.name)
                else:
                    inputs[t.target] = [s.name]
        unreachable = [
            s.name.name for s in self.states if s.name != self.initial and s.name not in inputs
        ]
        if unreachable:
            self.error.append(
                f'unreachable states {", ".join(unreachable)}',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

        detached = [s.name.name for s in self.states if s.name != self.final and not s.transitions]
        if detached:
            self.error.append(
                f'detached states {", ".join(detached)}',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

    @classmethod
    def __entity_name(cls, decl: Declaration) -> str:
        if isinstance(decl, Subprogram):
            return "subprogram"
        if isinstance(decl, VariableDeclaration):
            return "variable"
        if isinstance(decl, Renames):
            return "renames"
        if isinstance(decl, Channel):
            return "channel"
        if isinstance(decl, PrivateDeclaration):
            return "private"
        assert False, f"Unsupported entity {type(decl).__name__}"

    def __validate_declarations(self) -> None:
        for s in self.states:
            for decl in s.declarations:
                if decl in self.declarations:
                    self.error.append(
                        f'local variable "{decl}" shadows global declaration'
                        f" in state {s.name.name}",
                        Subsystem.SESSION,
                        Severity.ERROR,
                        self.location,
                    )
                if not s.declarations[decl].is_referenced:
                    self.error.append(
                        f'unused local variable "{decl}" in state {s.name.name}',
                        Subsystem.SESSION,
                        Severity.ERROR,
                        self.location,
                    )
        for k, d in self.declarations.items():
            if str(k).upper() in ["READ", "WRITE", "CALL", "DATA_AVAILABLE", "APPEND", "EXTEND"]:
                self.error.append(
                    f'{self.__entity_name(d)} declaration shadows builtin subprogram "{k}"',
                    Subsystem.SESSION,
                    Severity.ERROR,
                    self.location,
                )
            try:
                d.validate(self.declarations)
            except RecordFluxError as e:
                self.error.extend(e)
        for k, d in self.declarations.items():
            # pylint: disable=fixme
            # FIXME: We do not validate variable declarations at the moment
            if isinstance(d, PrivateDeclaration):
                continue
            if not d.is_referenced:
                self.error.append(
                    f'unused {self.__entity_name(d)} "{k}"',
                    Subsystem.SESSION,
                    Severity.ERROR,
                    self.location,
                )
