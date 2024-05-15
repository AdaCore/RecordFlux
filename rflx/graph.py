from __future__ import annotations

import logging
import re
from collections.abc import Sequence
from pathlib import Path
from typing import Optional

from pydotplus import Dot, Edge, InvocationException, Node  # type: ignore[attr-defined]

from rflx.error import RecordFluxError, Severity, Subsystem
from rflx.expression import TRUE, UNDEFINED
from rflx.identifier import ID
from rflx.model import FINAL_STATE, AbstractSession, Link, Message

log = logging.getLogger(__name__)


def _graph_with_defaults(name: str) -> Dot:
    """Return default pydot graph."""

    result = Dot(graph_name=f'"{name}"')
    result.set_graph_defaults(
        splines="true",
        ranksep="0.1 equally",
        pad="0.1",
        truecolor="true",
        bgcolor="#00000000",
    )
    result.set_edge_defaults(
        fontname="Fira Code",
        fontcolor="#6f6f6f",
        color="#6f6f6f",
        penwidth="2.5",
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


def write_graph(graph: Dot, filename: Path, fmt: str = "svg") -> None:
    log.info("Creating %s", filename)

    with filename.open("wb") as f:
        try:
            graph.write(f, format=fmt)
        except InvocationException as e:
            RecordFluxError(
                [
                    (e, Subsystem.GRAPH, Severity.ERROR, None),
                    (
                        "GraphViz is required for creating graphs",
                        Subsystem.GRAPH,
                        Severity.INFO,
                        None,
                    ),
                ],
            ).propagate()


def create_message_graph(message: Message) -> Dot:
    """Return pydot graph representation of message."""

    def _edge_label(link: Link) -> str:
        return "({cond},{sep1}{size},{sep2}{first})".format(
            cond=str(link.condition) if link.condition != TRUE else "⊤",
            sep1=" " if link.condition == TRUE or link.size == UNDEFINED else "\n",
            size=str(link.size) if link.size != UNDEFINED else message.field_size(link.target),
            sep2=" " if link.first == UNDEFINED else "\n",
            first=str(link.first) if link.first != UNDEFINED else "⋆",
        )

    result = _graph_with_defaults(message.full_name)
    result.add_node(
        Node(name="Initial", fillcolor="#ffffff", shape="circle", width="0.5", label=""),
    )
    for f in message.fields:
        result.add_node(Node(name=f.name))
    for i, l in enumerate(message.structure):
        intermediate_node = f"intermediate_{i}"
        result.add_node(
            Node(
                name=intermediate_node,
                label=_edge_label(l),
                style="",
                fontname="Fira Code",
                fontcolor="#6f6f6f",
                color="#6f6f6f",
                penwidth="0",
                width="0",
                height="0",
            ),
        )
        result.add_edge(Edge(src=l.source.name, dst=intermediate_node, arrowhead="none"))
        result.add_edge(Edge(src=intermediate_node, dst=l.target.name, minlen="1"))
    result.add_node(Node(name="Final", fillcolor="#6f6f6f", shape="circle", width="0.5", label=""))
    return result


def create_session_graph(session: AbstractSession, ignore: Optional[Sequence[str]] = None) -> Dot:
    """
    Return pydot graph representation of session.

    If present, the ignore parameter contains a list of regular expressions which are matched
    against state names and the names of edge targets in order. If a regular expression matches
    a state name or an edge target name, the respective state or edge is excluded from the result
    graph.
    """

    def _is_ignored(name: ID) -> bool:
        if not ignore:
            return False
        return any(re.search(regex, str(name), re.IGNORECASE) for regex in ignore)

    result = _graph_with_defaults(str(session.identifier))
    for state in session.states:
        if _is_ignored(state.identifier):
            continue

        if state == session.initial_state:
            result.add_node(
                Node(
                    name=str(state.identifier.name),
                    fillcolor="#ffffff",
                    fontcolor="black",
                ),
            )
        elif state == FINAL_STATE:
            result.add_node(
                Node(
                    name=str(FINAL_STATE.identifier.name),
                    fillcolor="#6f6f6f",
                    shape="circle",
                    width="0.5",
                    label="",
                ),
            )
        else:
            result.add_node(Node(name=str(state.identifier.name)))

        for index, t in enumerate(state.transitions):
            if not _is_ignored(t.target.name):
                label = (
                    f"{state.identifier.name} → {t.target.name}\n\n[{index}] {t.condition}"
                    if t.condition != TRUE
                    else ""
                )
                result.add_edge(
                    Edge(
                        src=str(state.identifier.name),
                        dst=str(t.target.name),
                        tooltip=label,
                        minlen="3",
                    ),
                )

    return result
