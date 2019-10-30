import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import List

from rflx.generator import LIBRARY_FILES, Generator
from rflx.model import Message, Refinement
from tests.models import (ARRAY_INNER_MESSAGE, ARRAY_MESSAGE, ARRAY_MESSAGES_MESSAGE,
                          DERIVATION_MESSAGE, ENUMERATION_MESSAGE, ETHERNET_FRAME, NULL_MESSAGE,
                          NULL_MESSAGE_IN_TLV_MESSAGE, TLV_MESSAGE)


class TestGenerator(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "generated"
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_library_files(self) -> None:
        generator = Generator('RFLX.')
        with TemporaryDirectory() as tmpdir:
            generator.write_library_files(Path(tmpdir))
            for filename in [f'rflx-{f}' for f in LIBRARY_FILES] + ['rflx.ads']:
                with open(tmpdir + '/' + filename) as library_file:
                    with open(self.testdir + '/' + filename) as expected_file:
                        self.assertEqual(library_file.read(), expected_file.read(), filename)

    def assert_specification(self, generator: Generator) -> None:
        for unit in generator.units.values():
            with open(f'{self.testdir}/{unit.name}.ads', 'r') as f:
                self.assertEqual(unit.specification, f.read(), unit.name)

    def assert_body(self, generator: Generator) -> None:
        for unit in generator.units.values():
            if unit.body:
                with open(f'{self.testdir}/{unit.name}.adb', 'r') as f:
                    self.assertEqual(unit.body, f.read(), unit.name)

    def test_null_dissector_spec(self) -> None:
        generator = generate_dissector([NULL_MESSAGE], [])
        self.assert_specification(generator)

    def test_null_dissector_body(self) -> None:
        generator = generate_dissector([NULL_MESSAGE], [])
        self.assert_body(generator)

    def test_tlv_dissector_spec(self) -> None:
        generator = generate_dissector([TLV_MESSAGE], [])
        self.assert_specification(generator)

    def test_tlv_dissector_body(self) -> None:
        generator = generate_dissector([TLV_MESSAGE], [])
        self.assert_body(generator)

    def test_tlv_refinement_to_null_spec(self) -> None:
        generator = generate_dissector([TLV_MESSAGE, NULL_MESSAGE], [NULL_MESSAGE_IN_TLV_MESSAGE])
        self.assert_specification(generator)

    def test_tlv_refinement_to_null_body(self) -> None:
        generator = generate_dissector([TLV_MESSAGE, NULL_MESSAGE], [NULL_MESSAGE_IN_TLV_MESSAGE])
        self.assert_body(generator)

    def test_ethernet_dissector_spec(self) -> None:
        generator = generate_dissector([ETHERNET_FRAME], [])
        self.assert_specification(generator)

    def test_ethernet_dissector_body(self) -> None:
        generator = generate_dissector([ETHERNET_FRAME], [])
        self.assert_body(generator)

    def test_enumeration_dissector_spec(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_enumeration_dissector_body(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE], [])
        self.assert_body(generator)

    def test_array_dissector_spec(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, ARRAY_INNER_MESSAGE, ARRAY_MESSAGES_MESSAGE],
                                       [])
        self.assert_specification(generator)

    def test_array_dissector_body(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, ARRAY_INNER_MESSAGE, ARRAY_MESSAGES_MESSAGE],
                                       [])
        self.assert_body(generator)

    # ISSUE: Componolit/RecordFlux#60

    # def test_expression_dissector_spec(self) -> None:
    #     generator = generate_dissector([EXPRESSION_MESSAGE], [])
    #     self.assert_specification(generator)

    # def test_expression_dissector_body(self) -> None:
    #     generator = generate_dissector([EXPRESSION_MESSAGE], [])
    #     self.assert_body(generator)

    def test_derivation_dissector_spec(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_derivation_dissector_body(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_body(generator)


def generate_dissector(pdus: List[Message], refinements: List[Refinement]) -> Generator:
    generator = Generator('RFLX.')
    generator.generate_dissector(pdus, refinements)
    return generator
