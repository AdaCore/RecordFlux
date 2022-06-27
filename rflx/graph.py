import collections
import logging
from copy import copy
from math import sqrt
from pathlib import Path
from typing import Counter, Union

from pydotplus import Dot, Edge, Node

from rflx.expression import TRUE, UNDEFINED
from rflx.identifier import ID
from rflx.model import FINAL, INITIAL, AbstractSession, Link, Message, State, statement as stmt

log = logging.getLogger(__name__)


class Graph:
    def __init__(self, data: Union[AbstractSession, Message]) -> None:
        self._data = copy(data)
        if isinstance(self._data, AbstractSession):
            self._degree = {s.identifier.name: len(s.transitions) for s in self._data.states}
            for s in self._data.states:
                for p in self._data.states:
                    for t in p.transitions:
                        if t.target == s.identifier:
                            self._degree[s.identifier.name] += 1

    def _target_size(self, link: Link) -> str:
        assert isinstance(self._data, Message)
        return str(self._data.field_size(link.target))

    def _edge_label(self, link: Link) -> str:
        return "({cond},{sep1}{size},{sep2}{first})".format(  # pylint: disable = consider-using-f-string
            cond=str(link.condition) if link.condition != TRUE else "⊤",
            sep1=" " if link.condition == TRUE or link.size == UNDEFINED else "\n",
            size=str(link.size) if link.size != UNDEFINED else self._target_size(link),
            sep2=" " if link.first == UNDEFINED else "\n",
            first=str(link.first) if link.first != UNDEFINED else "⋆",
        )

    @property
    def get(self) -> Dot:
        if isinstance(self._data, Message):
            return self._get_message
        if isinstance(self._data, AbstractSession):
            return self._get_session
        raise NotImplementedError(f"Unsupported data format {type(self._data).__name__}")

    @classmethod
    def _graph_with_defaults(cls, name: str) -> Dot:
        """Return default pydot graph."""

        result = Dot(graph_name=f'"{name}"')
        result.set_graph_defaults(
            splines="true", ranksep="0.1 equally", pad="0.1", truecolor="true", bgcolor="#00000000"
        )
        result.set_edge_defaults(
            fontname="Fira Code", fontcolor="#6f6f6f", color="#6f6f6f", penwidth="2.5"
        )
        result.set_node_defaults(
            fontname="Arimo",
            fontcolor="#ffffff",
            color="#6f6f6f",
            fillcolor="#009641",
            width="1.5",
            style='"rounded,filled"',
            shape="box",
        )
        return result

    def _add_state(self, state: State, result: Dot, variables: Counter[ID]) -> None:

        assert isinstance(self._data, AbstractSession)

        height = sqrt(self._degree[state.identifier.name] + 1)
        width = 1.3 * sqrt(self._degree[state.identifier.name] + 1)
        variables_read: Counter[ID] = collections.Counter()
        variables_write: Counter[ID] = collections.Counter()

        if state.identifier == self._data.initial:
            result.add_node(
                Node(
                    name=str(state.identifier.name),
                    fillcolor="#ffffff",
                    fontcolor="black",
                    width=f"{width:.2f}",
                    height=f"{height:.2f}",
                )
            )
        elif state.identifier == self._data.final:
            result.add_node(
                Node(
                    name=str(state.identifier.name),
                    fillcolor="#6f6f6f",
                    width=f"{width:.2f}",
                    height=f"{height:.2f}",
                )
            )
        else:
            result.add_node(
                Node(name=str(state.identifier.name), width=f"{width:.2f}", height=f"{height:.2f}")
            )

        for index, t in enumerate(state.transitions):
            label = (
                f"{state.identifier.name} → {t.target.name}\n\n[{index}] {t.condition}"
                if t.condition != TRUE
                else ""
            )
            result.add_edge(
                Edge(src=str(state.identifier.name), dst=str(t.target.name), tooltip=label)
            )
            variables_read.update(
                [
                    v.identifier
                    for v in t.condition.variables()
                    if v.identifier not in state.declarations
                ]
            )

        for index, a in enumerate(state.actions):
            if a.identifier not in state.declarations:
                variables_write.update([a.identifier])
            if isinstance(a, stmt.Assignment):
                variables_read.update(
                    [
                        v.identifier
                        for v in a.expression.variables()
                        if v.identifier not in state.declarations
                    ]
                )

        for v in variables_read:
            result.add_edge(
                Edge(
                    src=str(v),
                    dst=str(state.identifier.name),
                    tooltip=f"{state.identifier.name}: read {v}",
                )
            )
        for v in variables_write:
            result.add_edge(
                Edge(
                    src=str(state.identifier.name),
                    dst=str(v),
                    tooltip=f"{state.identifier.name}: write {v}",
                )
            )

        variables.update(variables_read)
        variables.update(variables_write)

    @property
    def _get_session(self) -> Dot:
        """Return pydot graph representation of session."""

        assert isinstance(self._data, AbstractSession)

        variables: Counter[ID] = collections.Counter()
        result = self._graph_with_defaults("Session")
        for s in self._data.states:
            self._add_state(s, result, variables)

        for v, d in variables.items():
            height = sqrt(d + 1)
            width = 1.3 * height
            result.add_node(
                Node(name=str(v), fillcolor="#7e8ab8", width=f"{width:.2f}", height=f"{height:.2f}")
            )

        return result

    @property
    def _get_message(self) -> Dot:
        """Return pydot graph representation of message."""

        assert isinstance(self._data, Message)

        if not self._data.structure:
            # ISSUE: Componolit/RecordFlux#643
            # pylint: disable-next = protected-access
            self._data._structure = [Link(INITIAL, FINAL)]

        result = self._graph_with_defaults(self._data.full_name)
        result.add_node(
            Node(name="Initial", fillcolor="#ffffff", shape="circle", width="0.5", label="")
        )
        for f in self._data.fields:
            result.add_node(Node(name=f.name))
        for i, l in enumerate(self._data.structure):
            intermediate_node = f"intermediate_{i}"
            result.add_node(
                Node(
                    name=intermediate_node,
                    label=self._edge_label(l),
                    style="",
                    fontname="Fira Code",
                    fontcolor="#6f6f6f",
                    color="#6f6f6f",
                    penwidth="0",
                    width="0",
                    height="0",
                )
            )
            result.add_edge(Edge(src=l.source.name, dst=intermediate_node, arrowhead="none"))
            result.add_edge(Edge(src=intermediate_node, dst=l.target.name, minlen="1"))
        result.add_node(
            Node(name="Final", fillcolor="#6f6f6f", shape="circle", width="0.5", label="")
        )
        return result

    def write(self, filename: Path, fmt: str = "svg") -> None:
        log.info("Creating %s", filename)

        with open(filename, "wb") as f:
            self.get.write(f, format=fmt)
