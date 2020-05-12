from pathlib import Path

import pytest

from rflx.generator import Generator, common, const
from rflx.identifier import ID
from rflx.model import Model, Type
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
    TLV_MODEL,
)

TESTDIR = Path("generated")


def assert_specification(generator: Generator) -> None:
    for unit in generator.units.values():
        with open(f"{TESTDIR}/{unit.name}.ads", "r") as f:
            assert unit.ads == f.read(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator.units.values():
        if unit.adb:
            with open(f"{TESTDIR}/{unit.name}.adb", "r") as f:
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
            with open(TESTDIR / filename) as expected_file:
                assert library_file.read() == expected_file.read(), filename


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator("RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        with open(created_file) as library_file:
            with open(TESTDIR / created_file.name) as expected_file:
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
        Generator().generate(Model([TestType("P.T")]))


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


def test_base_type_name() -> None:
    assert common.base_type_name(MODULAR_INTEGER) == ID("Modular")
    assert common.base_type_name(RANGE_INTEGER) == ID("Range_Base")


def test_full_base_type_name() -> None:
    assert common.full_base_type_name(MODULAR_INTEGER) == ID("P.Modular")
    assert common.full_base_type_name(RANGE_INTEGER) == ID("P.Range_Base")
