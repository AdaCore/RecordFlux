from __future__ import annotations

import textwrap
from typing import Callable

import pytest

from rflx import expr, typing_ as rty
from rflx.generator import common, const
from rflx.identifier import ID
from rflx.model import BUILTIN_TYPES, type_decl as mty
from rflx.model.message import FINAL, INITIAL, Field, Link, Message
from rflx.rapidflux import Location
from tests.data import models
from tests.utils import assert_equal


def test_type_translation() -> None:
    assert (common.type_to_id(rty.BASE_INTEGER)) == const.TYPES_BASE_INT
    assert (common.type_to_id(rty.NamedType("P::mytype"))) == ID("P::mytype")


@pytest.mark.parametrize("embedded", [True, False])
@pytest.mark.parametrize(
    ("left", "right"),
    [
        (expr.Variable("Value"), expr.Aggregate(expr.Number(1), expr.Number(2))),
        (expr.Aggregate(expr.Number(1), expr.Number(2)), expr.Variable("Value")),
    ],
)
@pytest.mark.parametrize("relation", [expr.Equal, expr.NotEqual])
def test_substitution_relation_aggregate(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    left: expr.Expr,
    right: expr.Expr,
    embedded: bool,
) -> None:
    aggregate = expr.Aggregate(
        expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(1)),
        expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(2)),
    )
    expected: expr.Expr
    if embedded:
        expected = relation(
            expr.Indexed(
                expr.Variable(ID("Buffer") * "all"),
                expr.ValueRange(
                    expr.Call(
                        const.TYPES_TO_INDEX,
                        rty.INDEX,
                        [
                            expr.Selected(
                                expr.Indexed(
                                    expr.Variable("Cursors"),
                                    expr.Variable("F_Value"),
                                ),
                                "First",
                            ),
                        ],
                    ),
                    expr.Call(
                        const.TYPES_TO_INDEX,
                        rty.INDEX,
                        [
                            expr.Selected(
                                expr.Indexed(
                                    expr.Variable("Cursors"),
                                    expr.Variable("F_Value"),
                                ),
                                "Last",
                            ),
                        ],
                    ),
                ),
            ),
            aggregate,
        )
    else:
        equal_call = expr.Call(
            "Equal",
            rty.BOOLEAN,
            [
                expr.Variable("Ctx"),
                expr.Variable("F_Value"),
                aggregate,
            ],
        )
        expected = equal_call if relation == expr.Equal else expr.Not(equal_call)

    assert (
        relation(left, right).substituted(common.substitution(models.tlv_message(), "", embedded))
        == expected
    )


@pytest.mark.parametrize(
    ("expressions", "expected"),
    [
        (
            (expr.Variable("Length"), expr.Number(1)),
            (expr.Call("Get_Length", rty.BASE_INTEGER, [expr.Variable("Ctx")]), expr.Number(1)),
        ),
        (
            (expr.Number(1), expr.Variable("Length")),
            (expr.Number(1), expr.Call("Get_Length", rty.BASE_INTEGER, [expr.Variable("Ctx")])),
        ),
        ((expr.Number(1), expr.Variable("Unknown")), (expr.Number(1), expr.Variable("Unknown"))),
    ],
)
@pytest.mark.parametrize(
    "relation",
    [expr.Less, expr.LessEqual, expr.Equal, expr.GreaterEqual, expr.Greater, expr.NotEqual],
)
def test_substitution_relation_scalar(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    expressions: tuple[expr.Expr, expr.Expr],
    expected: tuple[expr.Expr, expr.Expr],
) -> None:
    assert_equal(
        relation(*expressions).substituted(
            common.substitution(models.tlv_message(), "", public=True),
        ),
        relation(*expected),
    )


def test_prefixed_type_identifier() -> None:
    assert common.prefixed_type_identifier(ID("Integer"), "P") == ID("P.Integer")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_identifier(ID(t), "P") == t.name


def test_param_enumeration_condition() -> None:
    """Test proper substitution of parameter of enumeration type in link condition."""
    type_ = mty.Enumeration(
        "P::T",
        literals=[("E1", expr.Number(1)), ("E2", expr.Number(2))],
        size=expr.Number(8),
        always_valid=False,
    )
    link = Link(
        Field("A"),
        Field("B"),
        condition=expr.Equal(expr.Variable("Param", type_=type_.type_), expr.Literal("E1")),
    )

    message = Message(
        ID("P::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("A")),
            link,
            Link(
                Field("B"),
                FINAL,
            ),
        ],
        {
            Field("A"): type_,
            Field("B"): models.universal_length(),
            Field("Param"): type_,
        },
    )
    assert_equal(
        link.condition.substituted(common.substitution(message, "", embedded=True)),
        expr.Equal(
            expr.Call(
                "RFLX_Types::Base_Integer",
                rty.BASE_INTEGER,
                [expr.Call("To_Base_Integer", rty.BASE_INTEGER, [expr.Variable("Param")])],
            ),
            expr.Call(
                "RFLX_Types::Base_Integer",
                rty.BASE_INTEGER,
                [expr.Call("To_Base_Integer", rty.BASE_INTEGER, [expr.Literal("E1")])],
            ),
        ),
    )


def test_generate_string_substitution() -> None:
    subst = common.substitution(models.definite_message(), "")
    assert subst(expr.String("abc")) == expr.Aggregate(
        expr.Number(97),
        expr.Number(98),
        expr.Number(99),
    )


@pytest.mark.parametrize(
    ("lines", "width", "result"),
    [
        (
            ["TEST"],
            10,
            textwrap.dedent(
                """\
                    ----------
                    -- TEST --
                    ----------
                """,
            ),
        ),
        (
            [
                "",
                "Lorem ipsum",
                "",
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
                " incididunt ut labore et dolore magna aliqua.",
                "",
            ],
            40,
            textwrap.dedent(
                """\
                    ----------------------------------------
                    --                                    --
                    --            Lorem ipsum             --
                    --                                    --
                    -- Lorem ipsum dolor sit amet,        --
                    -- consectetur adipiscing elit, sed   --
                    -- do eiusmod tempor incididunt ut    --
                    -- labore et dolore magna aliqua.     --
                    --                                    --
                    ----------------------------------------
                """,
            ),
        ),
    ],
)
def test_comment_box(lines: list[str], width: int, result: str) -> None:
    assert common.comment_box(lines, width) == result
