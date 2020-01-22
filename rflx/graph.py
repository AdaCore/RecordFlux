from copy import copy
from typing import BinaryIO

from pydotplus import Dot, Edge, Node

from rflx.expression import TRUE, UNDEFINED
from rflx.model import FINAL, INITIAL, Link, Message


class Graph:
    def __init__(self, message: Message) -> None:
        self.__message = copy(message)
        if not self.__message.structure:
            self.__message.structure = [Link(INITIAL, FINAL)]

    def __target_size(self, link: Link) -> str:
        return str(self.__message.field_size(link.target))

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
        """Return pydot graph representation of message."""
        result = Dot(graph_name=self.__message.full_name)
        result.set_graph_defaults(splines="ortho", ranksep="0.8 equally")
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
        result.add_node(
            Node(name="Initial", fillcolor="#ffffff", shape="circle", width="0.5", label="")
        )
        for f in self.__message.fields:
            result.add_node(Node(name=f.name))
        for l in self.__message.structure:
            result.add_edge(Edge(src=l.source.name, dst=l.target.name, xlabel=self.__edge_label(l)))
        result.add_node(
            Node(name="Final", fillcolor="#6f6f6f", shape="circle", width="0.5", label="")
        )
        return result

    def write(self, handle: BinaryIO, fmt: str = "svg") -> None:
        self.get.write(handle, format=fmt)
