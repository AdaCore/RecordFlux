from pathlib import Path
from tempfile import TemporaryDirectory

from rflx.declaration import VariableDeclaration
from rflx.expression import FALSE, TRUE, Equal, Greater, Less, Number, Pow, Variable
from rflx.graph import Graph
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    Field,
    Link,
    Message,
    ModularInteger,
    State,
    Transition,
    UnprovenSession,
)
from rflx.statement import Assignment, Reset


def assert_graph(graph: Graph, expected: str) -> None:
    with TemporaryDirectory() as directory:
        path = Path(directory) / Path("test.dot")
        graph.write(path, fmt="raw")
        with open(path) as f:
            assert f.read().split() == expected.split()


def test_graph_object() -> None:
    f_type = ModularInteger("P::T", Pow(Number(2), Number(32)))
    m = Message(
        "P::M",
        structure=[Link(INITIAL, Field("X")), Link(Field("X"), FINAL)],
        types={Field("X"): f_type},
    )
    g = Graph(m).get
    assert [(e.get_source(), e.get_destination()) for e in g.get_edges()] == [
        ("Initial", "intermediate_0"),
        ("intermediate_0", "X"),
        ("X", "intermediate_1"),
        ("intermediate_1", "Final"),
    ]
    assert [n.get_name() for n in g.get_nodes()] == [
        "graph",
        "edge",
        "node",
        "Initial",
        "X",
        "intermediate_0",
        "intermediate_1",
        "Final",
    ]


def test_empty_message_graph() -> None:
    m = Message("P::M", [], {})
    expected = """
        digraph "P::M" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            intermediate_0 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                     label="(⊤, 0, ⋆)", penwidth=0, style="", width=0];
            Initial -> intermediate_0 [arrowhead=none];
            intermediate_0 -> Final [minlen=1];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph() -> None:
    f_type = ModularInteger("P::T", Pow(Number(2), Number(32)))
    m = Message(
        "P::M",
        structure=[Link(INITIAL, Field("X")), Link(Field("X"), FINAL)],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P::M" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            intermediate_0 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(⊤, 32, ⋆)", penwidth=0, style="", width=0];
            Initial -> intermediate_0 [arrowhead=none];
            intermediate_0 -> X [minlen=1];
            intermediate_1 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(⊤, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_1 [arrowhead=none];
            intermediate_1 -> Final [minlen=1];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_condition() -> None:
    f_type = ModularInteger("P::T", Pow(Number(2), Number(32)))
    m = Message(
        "P::M",
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100))),
        ],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P::M" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true, truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                 shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            intermediate_0 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(⊤, 32, ⋆)", penwidth=0, style="", width=0];
            Initial -> intermediate_0 [arrowhead=none];
            intermediate_0 -> X [minlen=1];
            intermediate_1 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(X > 100, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_1 [arrowhead=none];
            intermediate_1 -> Final [minlen=1];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_dot_graph_with_double_edge() -> None:
    f_type = ModularInteger("P::T", Pow(Number(2), Number(32)))
    m = Message(
        "P::M",
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100))),
            Link(Field("X"), FINAL, Less(Variable("X"), Number(50))),
        ],
        types={Field("X"): f_type},
    )
    expected = """
        digraph "P::M" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            Initial [fillcolor="#ffffff", label="", shape=circle, width="0.5"];
            X;
            intermediate_0 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(⊤, 32, ⋆)", penwidth=0, style="", width=0];
            Initial -> intermediate_0 [arrowhead=none];
            intermediate_0 -> X [minlen=1];
            intermediate_1 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(X > 100, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_1 [arrowhead=none];
            intermediate_1 -> Final [minlen=1];
            intermediate_2 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(X < 50, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_2 [arrowhead=none];
            intermediate_2 -> Final [minlen=1];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(Graph(m), expected)


def test_session_graph() -> None:
    s = UnprovenSession(
        identifier="P::S",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                "START",
                transitions=[
                    Transition(target=ID("STATE"), condition=Equal(Variable("Global"), TRUE)),
                    Transition(target=ID("END")),
                ],
            ),
            State(
                "STATE",
                transitions=[Transition(target=ID("END"))],
                actions=[Assignment("Global", FALSE), Reset("Local")],
                declarations=[VariableDeclaration("Local", "Opaque")],
            ),
            State("END"),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN, OPAQUE],
    ).proven()
    expected = """
        digraph "Session" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            START [fillcolor="#ffffff", fontcolor=black, height="1.73", width="2.25"];
            START -> STATE [tooltip="START → STATE\\n\\n[0] Global = True"];
            START -> END [tooltip=""];
            Global -> START [tooltip="START: read Global"];
            STATE [height="1.73", width="2.25"];
            STATE -> END [tooltip=""];
            STATE -> Global [tooltip="STATE: write Global"];
            END [fillcolor="#6f6f6f", height="1.73", width="2.25"];
            Global [fillcolor="#7e8ab8", height="1.73", width="2.25"];
        }
        """

    assert_graph(Graph(s), expected)
