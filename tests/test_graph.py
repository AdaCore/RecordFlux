from pathlib import Path
from tempfile import TemporaryDirectory

from rflx.expression import Greater, Variable
from rflx.graph import Graph
from rflx.model import FINAL, INITIAL, Field, Less, Link, Message, ModularInteger, Number, Pow


def assert_graph(graph: Graph, expected: str) -> None:
    with TemporaryDirectory() as directory:
        path = Path(directory) / Path("test.dot")
        graph.write(path, fmt="raw")
        with open(path) as f:
            assert f.read().split() == expected.split()


def test_graph_object() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[Link(INITIAL, Field("F1")), Link(Field("F1"), FINAL)],
        types={Field("F1"): f_type},
    )
    g = Graph(m).get
    assert [(e.get_source(), e.get_destination()) for e in g.get_edges()] == [
        ("Initial", "F1"),
        ("F1", "Final"),
    ]
    assert [n.get_name() for n in g.get_nodes()] == [
        "graph",
        "edge",
        "node",
        "Initial",
        "F1",
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
        structure=[Link(INITIAL, Field("F1")), Link(Field("F1"), FINAL)],
        types={Field("F1"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            F1;
            Initial -> F1 [xlabel="(⊤, 32, ⋆)"];
            F1 -> Final [xlabel="(⊤, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_condition() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), FINAL, Greater(Variable("F1"), Number(100))),
        ],
        types={Field("F1"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            F1;
            Initial -> F1 [xlabel="(⊤, 32, ⋆)"];
            F1 -> Final [xlabel="(F1 > 100, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_double_edge() -> None:
    f_type = ModularInteger("P.T", Pow(Number(2), Number(32)))
    m = Message(
        "P.M",
        structure=[
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), FINAL, Greater(Variable("F1"), Number(100))),
            Link(Field("F1"), FINAL, Less(Variable("F1"), Number(50))),
        ],
        types={Field("F1"): f_type},
    )
    expected = """
        digraph "P.M" {
            graph [bgcolor="#00000000", pad="0.5", ranksep="0.8 equally", splines=ortho, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            F1;
            Initial -> F1 [xlabel="(⊤, 32, ⋆)"];
            F1 -> Final [xlabel="(F1 > 100, 0, ⋆)"];
            F1 -> Final [xlabel="(F1 < 50, 0, ⋆)"];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)
