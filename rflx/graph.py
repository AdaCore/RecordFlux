import collections
import logging
from copy import copy
from math import sqrt
from pathlib import Path
from typing import Counter, Union

from pydotplus import Dot, Edge, Node

from rflx.expression import TRUE, UNDEFINED
from rflx.identifier import ID
from rflx.model import FINAL, INITIAL, AbstractSession, Link, Message, State
from rflx.statement import Assignment

log = logging.getLogger(__name__)


class Graph:
    def __init__(self, data: Union[AbstractSession, Message]) -> None:
        self.__data = copy(data)
        if isinstance(self.__data, AbstractSession):
            self.__degree = {s.identifier.name: len(s.transitions) for s in self.__data.states}
            for s in self.__data.states:
                for p in self.__data.states:
                    for t in p.transitions:
                        if t.target == s.identifier:
                            self.__degree[s.identifier.name] += 1

    def __target_size(self, link: Link) -> str:
        assert isinstance(self.__data, Message)
        return str(self.__data.field_size(link.target))

    def __edge_label(self, link: Link) -> str:
        return "({cond},{sep1}{size},{sep2}{first})".format(
            cond=str(link.condition) if link.condition != TRUE else "⊤",
            sep1=" " if link.condition == TRUE or link.size == UNDEFINED else "\n",
            size=str(link.size) if link.size != UNDEFINED else self.__target_size(link),
            sep2=" " if link.first == UNDEFINED else "\n",
            first=str(link.first) if link.first != UNDEFINED else "⋆",
        )

    @property
    def get(self) -> Dot:
        if isinstance(self.__data, Message):
            return self.__get_message
        if isinstance(self.__data, AbstractSession):
            return self.__get_session
        raise NotImplementedError(f"Unsupported data format {type(self.__data).__name__}")

    @classmethod
    def __graph_with_defaults(cls, name: str) -> Dot:
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

    def __add_state(self, state: State, result: Dot, variables: Counter[ID]) -> None:

        assert isinstance(self.__data, AbstractSession)

        height = sqrt(self.__degree[state.identifier.name] + 1)
        width = 1.3 * sqrt(self.__degree[state.identifier.name] + 1)
        variables_read: Counter[ID] = collections.Counter()
        variables_write: Counter[ID] = collections.Counter()

        if state.identifier == self.__data.initial:
            result.add_node(
                Node(
                    name=str(state.identifier.name),
                    fillcolor="#ffffff",
                    fontcolor="black",
                    width=f"{width:.2f}",
                    height=f"{height:.2f}",
                )
            )
        elif state.identifier == self.__data.final:
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
            if isinstance(a, Assignment):
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
    def __get_session(self) -> Dot:
        """Return pydot graph representation of session."""

        assert isinstance(self.__data, AbstractSession)

        variables: Counter[ID] = collections.Counter()
        result = self.__graph_with_defaults("Session")
        for s in self.__data.states:
            self.__add_state(s, result, variables)

        for v, d in variables.items():
            height = sqrt(d + 1)
            width = 1.3 * height
            result.add_node(
                Node(name=str(v), fillcolor="#7e8ab8", width=f"{width:.2f}", height=f"{height:.2f}")
            )

        return result

    @property
    def __get_message(self) -> Dot:
        """Return pydot graph representation of message."""

        assert isinstance(self.__data, Message)

        if not self.__data.structure:
            self.__data.structure = [Link(INITIAL, FINAL)]

        result = self.__graph_with_defaults(self.__data.full_name)
        result.add_node(
            Node(name="Initial", fillcolor="#ffffff", shape="circle", width="0.5", label="")
        )
        for f in self.__data.fields:
            result.add_node(Node(name=f.name))
        for i, l in enumerate(self.__data.structure):
            intermediate_node = f"intermediate_{i}"
            result.add_node(
                Node(
                    name=intermediate_node,
                    label=self.__edge_label(l),
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
