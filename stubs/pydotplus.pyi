#  pylint: disable=unused-argument,no-self-use
from typing import BinaryIO, Iterable, Optional

class Edge:  # noqa: E302, conflicts with isort
    def __init__(self, dst: str, src: str, **attr: Optional[str]):
        ...

    def get_source(self) -> str:
        ...

    def get_destination(self) -> str:
        ...


class Node:
    def __init__(self, name: str, **attrs: Optional[str]):
        ...

    def get_name(self) -> str:
        ...


class Dot:
    def __init__(self, graph_name: str):
        ...

    def add_node(self, node: Node) -> None:
        ...

    def add_edge(self, node: Edge) -> None:
        ...

    def write(self,
              handle: BinaryIO,
              prog: Optional[str] = None,
              format: Optional[str] = 'raw') -> None:  # pylint: disable=redefined-builtin
        ...

    def get_nodes(self) -> Iterable[Node]:
        ...

    def get_edges(self) -> Iterable[Edge]:
        ...

    def set_graph_defaults(self, **attrs: Optional[str]) -> None:
        ...

    def set_edge_defaults(self, **attrs: Optional[str]) -> None:
        ...

    def set_node_defaults(self, **attrs: Optional[str]) -> None:
        ...
