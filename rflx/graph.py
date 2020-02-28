import logging
from copy import copy
from pathlib import Path
from typing import Union

from pydotplus import Dot, Edge, Node

from rflx.expression import TRUE, UNDEFINED
from rflx.fsm import StateMachine
from rflx.model import FINAL, INITIAL, Link, Message

log = logging.getLogger(__name__)


class Graph:
    def __init__(self, data: Union[StateMachine, Message]) -> None:
        self.__data = copy(data)

    def __target_size(self, link: Link) -> str:
        if not isinstance(self.__data, Message):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")
        return str(self.__data.field_size(link.target))

    def __edge_label(self, link: Link) -> str:
        return "({cond},{sep1}{length},{sep2}{first})".format(
            cond=str(link.condition) if link.condition != TRUE else "⊤",
            sep1=" " if link.condition == TRUE or link.length == UNDEFINED else "\n",
            length=str(link.length) if link.length != UNDEFINED else self.__target_size(link),
            sep2=" " if link.first == UNDEFINED else "\n",
            first=str(link.first) if link.first != UNDEFINED else "⋆",
        )

    @property
    def get(self) -> Dot:
        if isinstance(self.__data, Message):
            return self.__get_message
        if isinstance(self.__data, StateMachine):
            return self.__get_session
        raise NotImplementedError(f"Unsupported data format {type(self.__data).__name__}")

    @classmethod
    def __graph_with_defaults(cls, name: str) -> Dot:
        """Return default pydot graph."""

        result = Dot(graph_name=name)
        result.set_graph_defaults(
            splines="true", ranksep="0.1 equally", pad="0.1", truecolor="true", bgcolor="#00000000"
        )
        result.set_edge_defaults(fontname="Fira Code", fontcolor="#6f6f6f", color="#6f6f6f")
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

    @property
    def __get_session(self) -> Dot:
        """Return pydot graph representation of session."""

        if not isinstance(self.__data, StateMachine):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")

        result = self.__graph_with_defaults("StateMachine")
        for s in self.__data.states:
            if s.name == self.__data.initial:
                result.add_node(Node(name=s.name.name, fillcolor="#ffffff", fontcolor="black"))
            elif s.name == self.__data.final:
                result.add_node(Node(name=s.name.name, fillcolor="#6f6f6f"))
            else:
                result.add_node(Node(name=s.name.name))

            for index, t in enumerate(s.transitions):
                condition = f"[{index}] {t.condition}" if t.condition != TRUE else ""
                result.add_edge(Edge(src=s.name.name, dst=t.target.name, xlabel=condition))
                print(f"{t.condition} ==> {t.condition.variables()}")
        return result

    @property
    def __get_message(self) -> Dot:
        """Return pydot graph representation of message."""

        if not isinstance(self.__data, Message):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")

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
