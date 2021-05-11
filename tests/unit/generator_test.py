from pathlib import Path
from typing import Any, Callable, Tuple

import pkg_resources
import pytest

import rflx.expression as expr
from rflx.ada import ID
from rflx.error import RecordFluxError
from rflx.generator import Generator, common, const
from rflx.model import BUILTIN_TYPES, Model, Type
from tests.const import GENERATED_DIR
from tests.data import models
from tests.utils import assert_equal


def assert_specification(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        with open(f"{GENERATED_DIR}/{unit.name}.ads", "r") as f:
            assert unit.ads == f.read(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.adb:
            with open(f"{GENERATED_DIR}/{unit.name}.adb", "r") as f:
                assert unit.adb == f.read(), unit.name


def generate(model: Model) -> Generator:
    generator = Generator(model, "RFLX", reproducible=True)
    return generator


def test_invalid_prefix() -> None:
    with pytest.raises(RecordFluxError, match=r"^id: error: empty part in identifier$"):
        Generator(Model(), "A..B")


def test_unsupported_checksum() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^generator: error: unsupported checksum"
            r" \(consider --ignore-unsupported-checksum option\)$"
        ),
    ):
        Generator(models.TLV_WITH_CHECKSUM_MODEL)


def test_ignore_unsupported_checksum(capsys: Any) -> None:
    Generator(models.TLV_WITH_CHECKSUM_MODEL, ignore_unsupported_checksum=True)
    captured = capsys.readouterr()
    assert "generator: warning: unsupported checksum ignored" in captured.out


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type() -> None:
    class TestType(Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator(Model([TestType("P::T")]))


def test_library_files(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in [f"rflx-{f}" for f in const.LIBRARY_FILES]:
        with open(tmp_path / filename) as library_file:
            with open(GENERATED_DIR / filename) as expected_file:
                assert library_file.read() == expected_file.read(), filename


def test_library_files_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in const.LIBRARY_FILES:
        assert (tmp_path / filename).exists()


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_directory(monkeypatch: Any, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: "non-existent directory")
    with pytest.raises(AssertionError, match="^template directory not found"):
        generator = Generator(Model())
        generator.write_library_files(tmp_path)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_files(monkeypatch: Any, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: tmp_path)
    with pytest.raises(AssertionError, match="^template file not found"):
        generator = Generator(Model())
        generator.write_library_files(tmp_path)


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        with open(created_file) as library_file:
            with open(GENERATED_DIR / created_file.name) as expected_file:
                assert library_file.read() == expected_file.read(), created_file.name


def test_top_level_package_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
    generator.write_top_level_package(tmp_path)
    assert list(tmp_path.glob("*")) == []


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
                expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(1)),
                expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(2)),
            ),
        ],
    )

    assert_equal(
        relation(left, right).substituted(common.substitution(models.TLV_MESSAGE)),
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
        relation(*expressions).substituted(common.substitution(models.TLV_MESSAGE, public=True)),
        relation(*expected),
    )


def test_prefixed_type_identifier() -> None:
    assert common.prefixed_type_identifier(ID("Modular"), "P") == ID("P.Modular")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_identifier(ID(t), "P") == t


def test_base_type_name() -> None:
    assert common.base_type_name(models.MODULAR_INTEGER) == ID("Modular")
    assert common.base_type_name(models.RANGE_INTEGER) == ID("Range_Base")


def test_full_base_type_name() -> None:
    assert common.full_base_type_name(models.MODULAR_INTEGER) == ID("P.Modular")
    assert common.full_base_type_name(models.RANGE_INTEGER) == ID("P.Range_Base")


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.SEQUENCE_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_specification(model: Model) -> None:
    generator = generate(model)
    assert_specification(generator)


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.SEQUENCE_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_body(model: Model) -> None:
    generator = generate(model)
    assert_body(generator)
