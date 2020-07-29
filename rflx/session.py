from typing import Any, Dict, List, Sequence

import yaml

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
from rflx.parser.session import action, declaration, expression
from rflx.statement import Statement


class Transition(Base):
    def __init__(self, target: StrID, condition: Expr = TRUE, location: Location = None):
        self.__target = ID(target)
        self.__condition = condition
        self.location = location

    @property
    def target(self) -> ID:
        return self.__target

    def validate(self, declarations: Dict[ID, Declaration]) -> None:
        self.__condition.simplified().validate(declarations)

    @property
    def condition(self) -> Expr:
        return self.__condition


class State(Base):
    def __init__(
        self,
        name: StrID,
        transitions: Sequence[Transition] = None,
        actions: Sequence[Statement] = None,
        declarations: Dict[ID, Declaration] = None,
        location: Location = None,
    ):
        self.__name = ID(name)
        self.__transitions = transitions or []
        self.__actions = actions or []
        self.__declarations = {ID(k): v for k, v in declarations.items()} if declarations else {}
        self.location = location

    @property
    def name(self) -> ID:
        return self.__name

    @property
    def transitions(self) -> Sequence[Transition]:
        return self.__transitions or []

    @property
    def declarations(self) -> Dict[ID, Declaration]:
        return self.__declarations

    @property
    def actions(self) -> Sequence[Statement]:
        return self.__actions


class Session(Base):
    def __init__(
        self,
        name: StrID,
        initial: StrID,
        final: StrID,
        states: Sequence[State],
        declarations: Dict[StrID, Declaration],
        location: Location = None,
    ):  # pylint: disable=too-many-arguments
        self.__name = ID(name)
        self.__initial = ID(initial)
        self.__final = ID(final)
        self.__states = states
        self.__declarations = {ID(k): v for k, v in declarations.items()}
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
        for s in self.__states:
            declarations = s.declarations
            for t in s.transitions:
                try:
                    t.validate({**self.__declarations, **declarations})
                except RecordFluxError as e:
                    self.error.extend(e)

    def __validate_actions(self) -> None:
        for s in self.__states:
            declarations = s.declarations
            for index, a in enumerate(s.actions):
                try:
                    a.validate({**self.__declarations, **declarations})
                except RecordFluxError as e:
                    self.error.append(
                        f"invalid action {index} of state {s.name.name}",
                        Subsystem.SESSION,
                        Severity.ERROR,
                        a.location,
                    )
                    self.error.extend(e)

    def __validate_state_existence(self) -> None:
        state_names = [s.name for s in self.__states]
        if self.__initial not in state_names:
            self.error.append(
                f'initial state "{self.__initial.name}" does not exist in "{self.__name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.__initial.location,
            )
        if self.__final not in state_names:
            self.error.append(
                f'final state "{self.__final.name}" does not exist in "{self.__name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.__final.location,
            )
        for s in self.__states:
            for t in s.transitions:
                if t.target not in state_names:
                    self.error.append(
                        f'transition from state "{s.name.name}" to non-existent state'
                        f' "{t.target.name}" in "{self.__name}"',
                        Subsystem.SESSION,
                        Severity.ERROR,
                        t.target.location,
                    )

    def __validate_duplicate_states(self) -> None:
        state_names = [s.name for s in self.__states]
        seen: Dict[ID, int] = {}
        duplicates: List[ID] = []
        for n in [x.name for x in state_names]:
            if n not in seen:
                seen[n] = 1
            else:
                if seen[n] == 1:
                    duplicates.append(n)
                seen[n] += 1

        if duplicates:
            self.error.append(
                f'duplicate states: {", ".join(map(str, sorted(duplicates)))}',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

    def __validate_state_reachability(self) -> None:
        inputs: Dict[ID, List[ID]] = {}
        for s in self.__states:
            for t in s.transitions:
                if t.target.name in inputs:
                    inputs[t.target.name].append(s.name.name)
                else:
                    inputs[t.target.name] = [s.name.name]
        unreachable = [
            str(s.name.name)
            for s in self.__states
            if s.name != self.__initial and s.name.name not in inputs
        ]
        if unreachable:
            self.error.append(
                f'unreachable states {", ".join(unreachable)}',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

        detached = [
            str(s.name.name) for s in self.__states if s.name != self.__final and not s.transitions
        ]
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
            return "private declaration"
        assert False, f"Unsupported entity {type(decl).__name__}"

    def __validate_declarations(self) -> None:
        for s in self.__states:
            for decl in s.declarations:
                if decl in self.__declarations:
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
        for k, d in self.__declarations.items():
            if str(k).upper() in ["READ", "WRITE", "CALL", "DATA_AVAILABLE", "APPEND", "EXTEND"]:
                self.error.append(
                    f'{self.__entity_name(d)} declaration shadows builtin subprogram "{k}"',
                    Subsystem.SESSION,
                    Severity.ERROR,
                    self.location,
                )
            try:
                d.validate(self.__declarations)
            except RecordFluxError as e:
                self.error.extend(e)
        for k, d in self.__declarations.items():
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

    @property
    def name(self) -> ID:
        return self.__name

    @property
    def initial(self) -> ID:
        return self.__initial

    @property
    def final(self) -> ID:
        return self.__final

    @property
    def states(self) -> Sequence[State]:
        return self.__states


class SessionFile:
    def __init__(self) -> None:
        self.__sessions: List[Session] = []
        self.error = RecordFluxError()

    def __parse_functions(self, doc: Dict[str, Any], result: Dict[ID, Declaration]) -> None:
        if "functions" not in doc:
            return
        for index, f in enumerate(doc["functions"]):
            try:
                name, decl = declaration().parseString(f)[0]
            except RecordFluxError as e:
                self.error.extend(e)
                self.error.append(
                    f"error parsing global function declaration {index}",
                    Subsystem.SESSION,
                    Severity.ERROR,
                )
                continue
            if ID(name) in result:
                self.error.append(
                    f"conflicting function {name}", Subsystem.SESSION, Severity.ERROR,
                )
            result[ID(name)] = decl
        self.error.propagate()

    def __parse_variables(self, doc: Dict[str, Any], result: Dict[ID, Declaration]) -> None:
        if "variables" not in doc:
            return
        for index, f in enumerate(doc["variables"]):
            try:
                name, decl = declaration().parseString(f)[0]
            except RecordFluxError as e:
                self.error.extend(e)
                self.error.append(
                    f"error parsing global variable declaration {index}",
                    Subsystem.SESSION,
                    Severity.ERROR,
                )
                continue
            if ID(name) in result:
                self.error.append(
                    f"conflicting variable {name}", Subsystem.SESSION, Severity.ERROR,
                )
            result[ID(name)] = decl
        self.error.propagate()

    def __parse_types(self, doc: Dict[str, Any], result: Dict[ID, Declaration]) -> None:
        if "types" not in doc:
            return
        for index, f in enumerate(doc["types"]):
            try:
                name, decl = declaration().parseString(f)[0]
            except RecordFluxError as e:
                self.error.extend(e)
                self.error.append(
                    f"error parsing private variable declaration {index}",
                    Subsystem.SESSION,
                    Severity.ERROR,
                )
                continue
            if ID(name) in result:
                self.error.append(
                    f"conflicting type {name}", Subsystem.SESSION, Severity.ERROR,
                )
            result[ID(name)] = decl
        self.error.propagate()

    def __parse_channels(self, doc: Dict[str, Any], result: Dict[ID, Declaration]) -> None:
        if "channels" not in doc:
            return
        for index, f in enumerate(doc["channels"]):
            if "name" not in f:
                self.error.append(
                    f"channel {index} has no name", Subsystem.SESSION, Severity.ERROR,
                )
                continue
            if "mode" not in f:
                self.error.append(
                    f"channel {f['name']} has no mode", Subsystem.SESSION, Severity.ERROR,
                )
                continue
            modes = {
                "Read": {"read": True, "write": False},
                "Write": {"read": False, "write": True},
                "Read_Write": {"read": True, "write": True},
            }
            if ID(f["name"]) in result:
                name = ID(f["name"])
                self.error.append(
                    f'conflicting channel "{name}"', Subsystem.SESSION, Severity.ERROR,
                )
            mode = f["mode"]
            try:
                result[ID(f["name"])] = Channel(modes[mode]["read"], modes[mode]["write"])
            except KeyError:
                self.error.append(
                    f"channel {f['name']} has invalid mode {f['mode']}",
                    Subsystem.SESSION,
                    Severity.ERROR,
                )
                continue
        self.error.propagate()

    def __parse_renames(self, doc: Dict[str, Any], result: Dict[ID, Declaration]) -> None:
        if "renames" not in doc:
            return
        for index, f in enumerate(doc["renames"]):
            try:
                name, decl = declaration().parseString(f)[0]
            except RecordFluxError as e:
                self.error.extend(e)
                self.error.append(
                    f"error parsing renames declaration {index}", Subsystem.SESSION, Severity.ERROR,
                )
                continue
            if name in result:
                self.error.append(
                    f"conflicting renames {name}", Subsystem.SESSION, Severity.ERROR,
                )
            result[name] = decl
        self.error.propagate()

    def __parse_declarations(self, doc: Dict[str, Any]) -> Dict[ID, Declaration]:
        result: Dict[ID, Declaration] = {}
        self.__parse_functions(doc, result)
        self.__parse_variables(doc, result)
        self.__parse_types(doc, result)
        self.__parse_channels(doc, result)
        self.__parse_renames(doc, result)
        return result

    def __parse_transitions(self, state: Dict) -> List[Transition]:
        transitions: List[Transition] = []
        sname = state["name"]
        if "transitions" in state:
            for index, t in enumerate(state["transitions"]):
                rest = t.keys() - ["condition", "target", "doc"]
                if rest:
                    elements = ", ".join(sorted(rest))
                    self.error.append(
                        f"unexpected elements in transition {index}"
                        f' in state "{sname}": {elements}',
                        Subsystem.SESSION,
                        Severity.ERROR,
                    )
                if "condition" in t:
                    try:
                        condition = expression().parseString(t["condition"])[0]
                    except RecordFluxError as e:
                        tname = t["target"]
                        self.error.append(
                            f'invalid condition {index} from state "{sname}" to "{tname}"',
                            Subsystem.SESSION,
                            Severity.ERROR,
                            None,
                        )
                        self.error.extend(e)
                        condition = TRUE
                else:
                    condition = TRUE
                transitions.append(Transition(target=ID(t["target"]), condition=condition))
        self.error.propagate()
        return transitions

    def __parse_states(self, doc: Dict[str, Any]) -> List[State]:
        states: List[State] = []
        for s in doc["states"]:
            rest = s.keys() - ["name", "actions", "transitions", "variables", "doc"]
            if rest:
                elements = ", ".join(sorted(rest))
                self.error.append(
                    f'unexpected elements in state "{s}": {elements}',
                    Subsystem.SESSION,
                    Severity.ERROR,
                )
                continue
            actions: List[Statement] = []
            if "actions" in s and s["actions"]:
                for a in s["actions"]:
                    try:
                        actions.append(action().parseString(a)[0])
                    except RecordFluxError as e:
                        self.error.extend(e)
                        continue
            declarations: Dict[ID, Declaration] = {}
            if "variables" in s and s["variables"]:
                for v in s["variables"]:
                    try:
                        dname, decl = declaration().parseString(v)[0]
                    except RecordFluxError as e:
                        self.error.extend(e)
                        continue
                    declarations[ID(dname)] = decl

            states.append(
                State(
                    name=ID(s["name"]),
                    transitions=self.__parse_transitions(s),
                    actions=actions,
                    declarations=declarations,
                )
            )
        return states

    def __parse(self, name: str, doc: Dict[str, Any]) -> None:
        if "initial" not in doc:
            self.error.append(
                f'missing initial state in "{name}"', Subsystem.SESSION, Severity.ERROR
            )
        if "final" not in doc:
            self.error.append(f'missing final state in "{name}"', Subsystem.SESSION, Severity.ERROR)
        if "states" not in doc:
            self.error.append(
                f'missing states section in "{name}"', Subsystem.SESSION, Severity.ERROR
            )

        self.error.propagate()

        rest = set(doc.keys()) - set(
            ["channels", "variables", "functions", "initial", "final", "states", "renames", "types"]
        )
        if rest:
            self.error.append(
                f'unexpected elements: {", ".join(sorted(rest))}', Subsystem.SESSION, Severity.ERROR
            )

        session = Session(
            name=name,
            initial=ID(doc["initial"]),
            final=ID(doc["final"]),
            states=self.__parse_states(doc),
            declarations={ID(k): v for k, v in self.__parse_declarations(doc).items()},
        )
        self.error.extend(session.error)
        self.__sessions.append(session)
        self.error.propagate()

    def parse(self, name: str, filename: str) -> None:
        with open(filename, "r") as data:
            self.__parse(name, yaml.safe_load(data))

    def parse_string(self, name: str, string: str) -> None:
        self.__parse(name, yaml.safe_load(string))

    @property
    def sessions(self) -> List[Session]:
        return self.__sessions
