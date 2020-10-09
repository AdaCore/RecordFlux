from pathlib import Path
from typing import Callable, Tuple

import pytest

import rflx.expression as expr
from rflx.ada import ID
from rflx.generator import Generator, common, const
from rflx.model import BUILTIN_TYPES, Model, Type
from tests.const import GENERATED_DIR
from tests.models import (
    ARRAYS_MODEL,
    DERIVATION_MODEL,
    ENUMERATION_MODEL,
    ETHERNET_MODEL,
    EXPRESSION_MODEL,
    MODULAR_INTEGER,
    NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
    NULL_MODEL,
    RANGE_INTEGER,
    TLV_MESSAGE,
    TLV_MODEL,
)
from tests.utils import assert_equal


def assert_specification(generator: Generator) -> None:
    for unit in generator.units.values():
        with open(f"{GENERATED_DIR}/{unit.name}.ads", "r") as f:
            assert unit.ads == f.read(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator.units.values():
        if unit.adb:
            with open(f"{GENERATED_DIR}/{unit.name}.adb", "r") as f:
                assert unit.adb == f.read(), unit.name


def generate(model: Model) -> Generator:
    generator = Generator("RFLX", reproducible=True)
    generator.generate(model)
    return generator


def test_library_files(tmp_path: Path) -> None:
    generator = Generator("RFLX", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in [f"rflx-{f}" for f in const.LIBRARY_FILES]:
        with open(tmp_path / filename) as library_file:
            with open(GENERATED_DIR / filename) as expected_file:
                assert library_file.read() == expected_file.read(), filename


def test_library_files_no_prefix(tmp_path: Path) -> None:
    generator = Generator("", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in const.LIBRARY_FILES:
        assert (tmp_path / filename).exists()


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator("RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        with open(created_file) as library_file:
            with open(GENERATED_DIR / created_file.name) as expected_file:
                assert library_file.read() == expected_file.read(), created_file.name


def test_top_level_package_no_prefix(tmp_path: Path) -> None:
    generator = Generator("", reproducible=True)
    generator.write_top_level_package(tmp_path)
    assert list(tmp_path.glob("*")) == []


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_invalid_prefix() -> None:
    with pytest.raises(AssertionError, match=r"empty part in identifier"):
        Generator("A..B")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type() -> None:
    class TestType(Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator().generate(Model([TestType("P::T")]))


def test_null_spec() -> None:
    generator = generate(NULL_MODEL)
    assert_specification(generator)


def test_null_body() -> None:
    generator = generate(NULL_MODEL)
    assert_body(generator)


def test_tlv_spec() -> None:
    generator = generate(TLV_MODEL)
    assert_specification(generator)


def test_tlv_body() -> None:
    generator = generate(TLV_MODEL)
    assert_body(generator)


def test_tlv_refinement_to_null_spec() -> None:
    generator = generate(NULL_MESSAGE_IN_TLV_MESSAGE_MODEL)
    assert_specification(generator)


def test_tlv_refinement_to_null_body() -> None:
    generator = generate(NULL_MESSAGE_IN_TLV_MESSAGE_MODEL)
    assert_body(generator)


def test_ethernet_spec() -> None:
    generator = generate(ETHERNET_MODEL)
    assert_specification(generator)


def test_ethernet_body() -> None:
    generator = generate(ETHERNET_MODEL)
    assert_body(generator)


def test_enumeration_spec() -> None:
    generator = generate(ENUMERATION_MODEL)
    assert_specification(generator)


def test_enumeration_body() -> None:
    generator = generate(ENUMERATION_MODEL)
    assert_body(generator)


def test_array_spec() -> None:
    generator = generate(ARRAYS_MODEL)
    assert_specification(generator)


def test_array_body() -> None:
    generator = generate(ARRAYS_MODEL)
    assert_body(generator)


def test_expression_spec() -> None:
    generator = generate(EXPRESSION_MODEL)
    assert_specification(generator)


def test_expression_body() -> None:
    generator = generate(EXPRESSION_MODEL)
    assert_body(generator)


def test_derivation_spec() -> None:
    generator = generate(DERIVATION_MODEL)
    assert_specification(generator)


def test_derivation_body() -> None:
    generator = generate(DERIVATION_MODEL)
    assert_body(generator)


@pytest.mark.parametrize(
    "left,right",
    [
        (expr.Variable("Value"), expr.Aggregate(expr.Number(1), expr.Number(2))),
        (expr.Aggregate(expr.Number(1), expr.Number(2)), expr.Variable("Value")),
    ],
)
@pytest.mark.parametrize("relation", [expr.Equal, expr.NotEqual])
def test_substitution_relation_aggregate(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation], left: expr.Expr, right: expr.Expr
) -> None:
    equal_call = expr.Call(
        "Equal",
        [
            expr.Variable("Ctx"),
            expr.Variable("F_Value"),
            expr.Aggregate(
                expr.Val(expr.Variable(expr.ID("Types") * "Byte"), expr.Number(1)),
                expr.Val(expr.Variable(expr.ID("Types") * "Byte"), expr.Number(2)),
            ),
        ],
    )

    assert_equal(
        relation(left, right).substituted(common.substitution(TLV_MESSAGE)),
        equal_call if relation == expr.Equal else expr.Not(equal_call),
    )


@pytest.mark.parametrize(
    "expressions,expected",
    [
        (
            (expr.Variable("Length"), expr.Number(1)),
            (expr.Call("Get_Length", [expr.Variable("Ctx")]), expr.Number(1)),
        ),
        (
            (expr.Number(1), expr.Variable("Length")),
            (expr.Number(1), expr.Call("Get_Length", [expr.Variable("Ctx")])),
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
    expressions: Tuple[expr.Expr, expr.Expr],
    expected: Tuple[expr.Expr, expr.Expr],
) -> None:
    assert_equal(
        relation(*expressions).substituted(common.substitution(TLV_MESSAGE, public=True)),
        relation(*expected),
    )


def test_prefixed_type_name() -> None:
    assert common.prefixed_type_name(ID("Modular"), "P") == ID("P.Modular")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_name(ID(t), "P") == t


def test_base_type_name() -> None:
    assert common.base_type_name(MODULAR_INTEGER) == ID("Modular")
    assert common.base_type_name(RANGE_INTEGER) == ID("Range_Base")


def test_full_base_type_name() -> None:
    assert common.full_base_type_name(MODULAR_INTEGER) == ID("P.Modular")
    assert common.full_base_type_name(RANGE_INTEGER) == ID("P.Range_Base")
