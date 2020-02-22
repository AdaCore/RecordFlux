import unittest

from pyparsing import ParseException

from rflx.fsm_declaration import Argument, Subprogram
from rflx.fsm_parser import FSMParser
from rflx.identifier import ID


class TestFSM(unittest.TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_simple_function_declaration(self) -> None:
        result = FSMParser.declaration().parseString(
            "Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type"
        )[0]
        expected = (
            ID("Foo"),
            Subprogram([Argument("Arg1", "Arg1_Type"), Argument("Arg2", "Arg2_Type")], "Foo_Type",),
        )
        self.assertEqual(result, expected)

    def test_invalid_function_name(self) -> None:
        with self.assertRaises(ParseException):
            # pylint: disable=expression-not-assigned
            FSMParser.declaration().parseString(
                "Foo.Bar (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type"
            )[0]

    def test_invalid_parameter_name(self) -> None:
        with self.assertRaises(ParseException):
            # pylint: disable=expression-not-assigned
            FSMParser.declaration().parseString(
                "Foo (Arg1 : Arg1_Type; Arg2.Invalid : Arg2_Type) return Foo_Type"
            )[0]

    def test_parameterless_function_declaration(self) -> None:
        result = FSMParser.declaration().parseString("Foo return Foo_Type")[0]
        expected = (ID("Foo"), Subprogram([], "Foo_Type"))
        self.assertEqual(result, expected)
