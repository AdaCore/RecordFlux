import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import List

from rflx.generator import Generator
from rflx.generator.common import base_type_name, full_base_type_name
from rflx.generator.core import LIBRARY_FILES
from rflx.model import Message, Refinement
from tests.models import (
    ARRAY_INNER_MESSAGE,
    ARRAY_MESSAGE,
    ARRAY_MESSAGES_MESSAGE,
    DERIVATION_MESSAGE,
    ENUMERATION_MESSAGE,
    ETHERNET_FRAME,
    MODULAR_INTEGER,
    NULL_MESSAGE,
    NULL_MESSAGE_IN_TLV_MESSAGE,
    RANGE_INTEGER,
    TLV_MESSAGE,
)


# pylint: disable=too-many-public-methods
class TestGenerator(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = Path("generated")
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_library_files(self) -> None:
        generator = Generator("RFLX", reproducible=True)
        with TemporaryDirectory() as directory:
            tmpdir = Path(directory)
            generator.write_library_files(tmpdir)
            for filename in [f"rflx-{f}" for f in LIBRARY_FILES]:
                with open(tmpdir / filename) as library_file:
                    with open(self.testdir / filename) as expected_file:
                        self.assertEqual(library_file.read(), expected_file.read(), filename)

    def test_top_level_package(self) -> None:
        generator = Generator("RFLX", reproducible=True)
        with TemporaryDirectory() as directory:
            tmpdir = Path(directory)
            generator.write_top_level_package(tmpdir)

            created_files = list(tmpdir.glob("*"))
            self.assertEqual(created_files, [tmpdir / Path("rflx.ads")])

            for created_file in created_files:
                with open(created_file) as library_file:
                    with open(self.testdir / created_file.name) as expected_file:
                        self.assertEqual(
                            library_file.read(), expected_file.read(), created_file.name
                        )

    def test_top_level_package_no_prefix(self) -> None:
        generator = Generator("", reproducible=True)
        with TemporaryDirectory() as directory:
            tmpdir = Path(directory)
            generator.write_top_level_package(tmpdir)
            self.assertEqual(list(tmpdir.glob("*")), [])

    def assert_specification(self, generator: Generator) -> None:
        for unit in generator.units.values():
            with open(f"{self.testdir}/{unit.name}.ads", "r") as f:
                self.assertEqual(unit.specification, f.read(), unit.name)

    def assert_body(self, generator: Generator) -> None:
        for unit in generator.units.values():
            if unit.body:
                with open(f"{self.testdir}/{unit.name}.adb", "r") as f:
                    self.assertEqual(unit.body, f.read(), unit.name)

    def test_null_spec(self) -> None:
        generator = generate([NULL_MESSAGE], [])
        self.assert_specification(generator)

    def test_null_body(self) -> None:
        generator = generate([NULL_MESSAGE], [])
        self.assert_body(generator)

    def test_tlv_spec(self) -> None:
        generator = generate([TLV_MESSAGE], [])
        self.assert_specification(generator)

    def test_tlv_body(self) -> None:
        generator = generate([TLV_MESSAGE], [])
        self.assert_body(generator)

    def test_tlv_refinement_to_null_spec(self) -> None:
        generator = generate([TLV_MESSAGE, NULL_MESSAGE], [NULL_MESSAGE_IN_TLV_MESSAGE])
        self.assert_specification(generator)

    def test_tlv_refinement_to_null_body(self) -> None:
        generator = generate([TLV_MESSAGE, NULL_MESSAGE], [NULL_MESSAGE_IN_TLV_MESSAGE])
        self.assert_body(generator)

    def test_ethernet_spec(self) -> None:
        generator = generate([ETHERNET_FRAME], [])
        self.assert_specification(generator)

    def test_ethernet_body(self) -> None:
        generator = generate([ETHERNET_FRAME], [])
        self.assert_body(generator)

    def test_enumeration_spec(self) -> None:
        generator = generate([ENUMERATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_enumeration_body(self) -> None:
        generator = generate([ENUMERATION_MESSAGE], [])
        self.assert_body(generator)

    def test_array_spec(self) -> None:
        generator = generate([ARRAY_MESSAGE, ARRAY_INNER_MESSAGE, ARRAY_MESSAGES_MESSAGE], [])
        self.assert_specification(generator)

    def test_array_body(self) -> None:
        generator = generate([ARRAY_MESSAGE, ARRAY_INNER_MESSAGE, ARRAY_MESSAGES_MESSAGE], [])
        self.assert_body(generator)

    # ISSUE: Componolit/RecordFlux#60

    # def test_expression_spec(self) -> None:
    #     generator = generate([EXPRESSION_MESSAGE], [])
    #     self.assert_specification(generator)

    # def test_expression_body(self) -> None:
    #     generator = generate([EXPRESSION_MESSAGE], [])
    #     self.assert_body(generator)

    def test_derivation_spec(self) -> None:
        generator = generate([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_derivation_body(self) -> None:
        generator = generate([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_body(generator)

    def test_base_type_name(self) -> None:
        self.assertEqual(base_type_name(MODULAR_INTEGER), "Modular")
        self.assertEqual(base_type_name(RANGE_INTEGER), "Range_Base")

    def test_full_base_type_name(self) -> None:
        self.assertEqual(full_base_type_name(MODULAR_INTEGER), "P.Modular")
        self.assertEqual(full_base_type_name(RANGE_INTEGER), "P.Range_Base")


def generate(pdus: List[Message], refinements: List[Refinement]) -> Generator:
    generator = Generator("RFLX", reproducible=True)
    generator.generate(pdus, refinements)
    return generator
