from pathlib import Path

import pytest
from pydotplus import Dot, InvocationException  # type: ignore[attr-defined]

from rflx.expression import FALSE, TRUE, Equal, Greater, Less, Number, Pow, Sub, Variable
from rflx.graph import create_message_graph, create_session_graph, write_graph
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    Field,
    Integer,
    Link,
    Message,
    Session,
    State,
    Transition,
    declaration as decl,
    statement as stmt,
)
from rflx.rapidflux import Location, RecordFluxError


def assert_graph(graph: Dot, expected: str, tmp_path: Path) -> None:
    path = tmp_path / Path("test.dot")
    write_graph(graph, path, fmt="raw")
    assert path.read_text().split() == expected.split()


def test_graph_object() -> None:
    f_type = Integer("P::T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    m = Message(
        ID("P::M", Location((1, 1))),
        structure=[Link(INITIAL, Field("X")), Link(Field("X"), FINAL)],
        types={Field("X"): f_type},
    )
    g = create_message_graph(m)
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


def test_empty_message_graph(tmp_path: Path) -> None:
    m = Message(ID("P::M", Location((1, 1))), [], {})
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

    assert_graph(create_message_graph(m), expected, tmp_path)


def test_dot_graph(tmp_path: Path) -> None:
    f_type = Integer("P::T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    m = Message(
        ID("P::M", Location((1, 1))),
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

    assert_graph(create_message_graph(m), expected, tmp_path)


def test_dot_graph_with_condition(tmp_path: Path) -> None:
    f_type = Integer("P::T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    m = Message(
        ID("P::M", Location((1, 1))),
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100))),
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
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(create_message_graph(m), expected, tmp_path)


def test_dot_graph_with_double_edge(tmp_path: Path) -> None:
    f_type = Integer("P::T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    m = Message(
        ID("P::M", Location((1, 1))),
        structure=[
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL, Greater(Variable("X"), Number(100), location=Location((3, 3)))),
            Link(Field("X"), FINAL, Less(Variable("X"), Number(50), location=Location((4, 4)))),
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
                            label="(X < 50, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_1 [arrowhead=none];
            intermediate_1 -> Final [minlen=1];
            intermediate_2 [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", height=0,
                            label="(X > 100, 0, ⋆)", penwidth=0, style="", width=0];
            X -> intermediate_2 [arrowhead=none];
            intermediate_2 -> Final [minlen=1];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(create_message_graph(m), expected, tmp_path)


def test_session_graph(tmp_path: Path) -> None:
    s = Session(
        identifier="P::S",
        states=[
            State(
                "START",
                transitions=[
                    Transition(target=ID("STATE"), condition=Equal(Variable("Global"), TRUE)),
                    Transition(target=ID("null")),
                ],
            ),
            State(
                "STATE",
                transitions=[Transition(target=ID("IGNORED_1")), Transition(target=ID("null"))],
                actions=[stmt.VariableAssignment("Global", FALSE), stmt.Reset("Local")],
                declarations=[decl.VariableDeclaration("Local", "Opaque")],
            ),
            State(
                "IGNORED_1",
                transitions=[Transition(target=ID("null"))],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN, OPAQUE],
    )

    expected_full = r"""
        digraph "P::S" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            START [fillcolor="#ffffff", fontcolor=black];
            START -> STATE  [minlen=3, tooltip="START → STATE\n\n[0] Global = True"];
            START -> Final  [minlen=3, tooltip=""];
            STATE;
            STATE -> IGNORED_1 [minlen=3, tooltip=""];
            STATE -> Final  [minlen=3, tooltip=""];
            IGNORED_1;
            IGNORED_1 -> Final  [minlen=3, tooltip=""];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(create_session_graph(s), expected_full, tmp_path)

    expected_filtered = r"""
        digraph "P::S" {
            graph [bgcolor="#00000000", pad="0.1", ranksep="0.1 equally", splines=true,
                   truecolor=true];
            edge [color="#6f6f6f", fontcolor="#6f6f6f", fontname="Fira Code", penwidth="2.5"];
            node [color="#6f6f6f", fillcolor="#009641", fontcolor="#ffffff", fontname=Arimo,
                  shape=box, style="rounded,filled", width="1.5"];
            START [fillcolor="#ffffff", fontcolor=black];
            START -> STATE  [minlen=3, tooltip="START → STATE\n\n[0] Global = True"];
            START -> Final  [minlen=3, tooltip=""];
            STATE;
            STATE -> Final  [minlen=3, tooltip=""];
            Final [fillcolor="#6f6f6f", label="", shape=circle, width="0.5"];
        }
        """

    assert_graph(create_session_graph(s, ignore=[r"^IGNORED_"]), expected_filtered, tmp_path)


def test_missing_graphviz(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    def write_mock(self: object, path: object, format: str = "") -> object:  # noqa: ARG001, A002
        raise InvocationException("GraphViz not found")

    monkeypatch.setattr(Dot, "write", write_mock)

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"error: GraphViz not found\n"
            r"info: GraphViz is required for creating graphs"
            r"$"
        ),
    ):
        write_graph(Dot(""), tmp_path / "graph")
