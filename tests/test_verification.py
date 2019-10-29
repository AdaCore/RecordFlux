import unittest

from rflx.model import ModelError
from rflx.parser import Parser


class TestVerification(unittest.TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ModelError, regex):
            Parser().parse_string(string)

    @staticmethod
    def test_exclusive_valid() -> None:
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

    @staticmethod
    def test_exclusive_enum_valid() -> None:
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
            r'^conflicting conditions for field "F1"')

    @staticmethod
    def test_exclusive_with_length_valid() -> None:
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

    def test_exclusive_with_length_invalid(self) -> None:
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
            r'^conflicting conditions for field "F1"')

    def test_no_valid_path(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then F2
                                if F1 <= 100,
                            then F3
                                if F1 > 100;
                        F2 : Element
                            then F3
                                if F1 > 100;
                        F3 : Element
                            then null
                                if F1 <= 100;
                    end message;
            end Foo;
            """,
            r'^unreachable field "Final"')

    def test_invalid_path_1(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element then null if 1 = 2;
                    end message;
            end Foo;
            """,
            r'^unreachable field "Final"')

    def test_invalid_path_2(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if 1 = 2;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    def test_contradiction(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then F2 if 1 = 2,
                            then F2 if F1 < 50,
                            then null if F1 > 100;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^contradicting condition 0 from field "F1" to "F2"')
