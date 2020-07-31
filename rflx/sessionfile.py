from typing import Any, Dict, List

import yaml

from rflx.error import RecordFluxError, Severity, Subsystem
from rflx.expression import TRUE, Channel, Declaration
from rflx.identifier import ID, StrID
from rflx.parser.session import action, declaration, expression
from rflx.session import Session, State, StateName, Transition
from rflx.statement import Statement


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
                transitions.append(Transition(target=StateName(t["target"]), condition=condition))
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
            declarations: Dict[StrID, Declaration] = {}
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
                    name=StateName(s["name"]),
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
            initial=StateName(doc["initial"]),
            final=StateName(doc["final"]),
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
