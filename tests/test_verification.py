import unittest

from rflx.model import ModelError
from rflx.parser import Parser


class TestVerification(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ModelError, regex):
            Parser().parse_string(string)

    def test_exclusive_valid(self) -> None:  # pylint: disable=no-self-use
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                message
                    F1 : Element
                        then null
                            if F1 > 100,
                        then F2
                            if F1 <= 100;
                    F2 : Element;
                end message;
            end Foo;
            """)

    def test_exclusive_enum_valid(self) -> None:  # pylint: disable=no-self-use
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Kind is (Val1 => 1, Val2 => 2) with Size => 8;
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                message
                    F1 : Kind
                        then null
                            if F1 = Val1,
                        then F2
                            if F1 = Val2;
                    F2 : Element;
                end message;
            end Foo;
            """)

    def test_exclusive_conflict(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                message
                    F1 : Element
                        then null
                            if F1 > 50,
                        then F2
                            if F1 < 100;
                    F2 : Element;
                end message;
            end Foo;
            """,
            r'^F1: Conflicting conditions')

    def test_exclusive_with_length_valid(self) -> None:  # pylint: disable=no-self-use
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                message
                    F1 : Element
                        then null
                            if F1'Length = 32 and F1 < 50,
                        then F2
                            if F1'Length = 32 and F1 > 100;
                    F2 : Element;
                end message;
            end Foo;
            """)

    def test_exclusive_with_length_invalid(self) -> None:  # pylint: disable=no-self-use
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                message
                    F1 : Element
                        then null
                            if F1'Length = 32,
                        then F2
                            if F1'Length = 32;
                    F2 : Element;
                end message;
            end Foo;
            """,
            r'^F1: Conflicting conditions')
