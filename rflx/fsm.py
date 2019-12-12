from typing import Dict, Iterable, Optional

import yaml

from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.model import Base


class StateName(Base):
    def __init__(self, name: str, location: Location = None):
        self.__name = name
        self.location = location

    @property
    def name(self) -> str:
        return self.__name


class Transition(Base):
    def __init__(self, target: StateName):
        self.target = target


class State(Base):
    def __init__(self, name: StateName, transitions: Optional[Iterable[Transition]] = None):
        self.__name = name
        self.__transitions = transitions or []

    @property
    def name(self) -> StateName:
        return self.__name

    @property
    def transitions(self) -> Iterable[Transition]:
        return self.__transitions or []


class StateMachine(Base):
    def __init__(
        self,
        initial: StateName,
        final: StateName,
        states: Iterable[State],
        location: Location = None,
    ):
        self.__initial = initial
        self.__final = final
        self.__states = states
        self.location = location
        self.error = RecordFluxError()

        if not states:
            self.error.append(
                "empty states", Subsystem.SESSION, Severity.ERROR, location,
            )
        self.error.propagate()

    def __validate_initial_state(self, name: str) -> None:
        states = [s.name for s in self.__states]
        if self.__initial not in states:
            self.error.append(
                f'initial state "{self.__initial.name}" does not exist in "{name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.__initial.location,
            )
        if self.__final not in states:
            self.error.append(
                f'final state "{self.__final.name}" does not exist in "{name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.__final.location,
            )
        for s in self.__states:
            for t in s.transitions:
                if t.target not in states:
                    self.error.append(
                        f'transition from state "{s.name.name}" to non-existent state'
                        f' "{t.target.name}" in "{name}"',
                        Subsystem.SESSION,
                        Severity.ERROR,
                        t.target.location,
                    )

    def validate(self, name: str) -> None:
        self.__validate_initial_state(name)


class FSM:
    def __init__(self) -> None:
        self.__fsms: Dict[str, StateMachine] = {}
        self.error = RecordFluxError()

    def __parse(self, name: str, doc: Dict) -> None:
        if "initial" not in doc:
            self.error.append(
                f'missing initial state in "{name}"', Subsystem.SESSION, Severity.ERROR, None,
            )
        if "final" not in doc:
            self.error.append(
                f'missing final state in "{name}"', Subsystem.SESSION, Severity.ERROR, None,
            )
        if "states" not in doc:
            self.error.append(
                f'missing states section in "{name}"', Subsystem.SESSION, Severity.ERROR, None,
            )
        self.error.propagate()
        self.__fsms[name] = StateMachine(
            initial=StateName(doc["initial"]),
            final=StateName(doc["final"]),
            states=[
                State(
                    StateName(s["name"]),
                    [Transition(StateName(t["target"])) for t in s["transitions"]]
                    if "transitions" in s
                    else None,
                )
                for s in doc["states"]
            ],
        )
        for f, v in self.__fsms.items():
            v.validate(f)
            self.error.extend(v.error)
        self.error.propagate()

    def parse(self, name: str, filename: str) -> None:
        with open(filename, "r") as data:
            self.__parse(name, yaml.safe_load(data))

    def parse_string(self, name: str, string: str) -> None:
        self.__parse(name, yaml.safe_load(string))

    @property
    def fsms(self) -> Dict[str, StateMachine]:
        return self.__fsms
