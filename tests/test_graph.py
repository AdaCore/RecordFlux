from pathlib import Path
from tempfile import TemporaryDirectory

from rflx.expression import Greater, Variable
from rflx.graph import Graph
from rflx.model import FINAL, INITIAL, Field, Less, Link, Message, ModularInteger, Number, Pow
from tests.utils import BASE_TMP_DIR


def assert_graph(graph: Graph, expected: str) -> None:
    with TemporaryDirectory(dir=BASE_TMP_DIR) as directory:
        path = Path(directory) / Path("test.dot")
        graph.write(path, fmt="raw")
        with open(path) as f:
            assert f.read().split() == expected.split()


def test_graph_object() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[Link(INITIAL, Field("X")), Link(Field("X"), FINAL)],
        types={Field("X"): f_type},
    )
    g = Graph(m).get
    assert [(e.get_source(), e.get_destination()) for e in g.get_edges()] == [
        ("Initial", "X"),
        ("X", "Final"),
    ]
    assert [n.get_name() for n in g.get_nodes()] == [
        "graph",
        "edge",
        "node",
        "Initial",
        "X",
        "Final",
    ]


def test_empty_message_graph() -> None:
    m = Message("P.M", [], {})
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            Initial -> Final [xlabel="(⊤, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[Link(INITIAL, Field("X")), Link(Field("X"), FINAL)],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            Initial -> X [xlabel="(⊤, 32, ⋆)"];
            X -> Final [xlabel="(⊤, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_condition() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100))),
        ],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            Initial -> X [xlabel="(⊤, 32, ⋆)"];
            X -> Final [xlabel="(X > 100, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_double_edge() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100))),
            Link(Field("X"), FINAL, Less(Variable("X"), Number(50))),
        ],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            Initial -> X [xlabel="(⊤, 32, ⋆)"];
            X -> Final [xlabel="(X > 100, 0, ⋆)"];
            X -> Final [xlabel="(X < 50, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)
