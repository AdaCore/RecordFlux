from unittest import TestCase, mock

from rflx.expression import Add, First, Greater, Last, LessEqual, Number, Pow, Sub, Variable
from rflx.model import FINAL, INITIAL, Field, Link, Message, ModelError, ModularInteger
from rflx.parser import Parser


# pylint: disable=too-many-public-methods
class TestVerification(TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_model_error(self, string: str, regex: str) -> None:
        parser = Parser()
        with self.assertRaisesRegex(ModelError, regex):
            parser.parse_string(string)
            parser.create_model()

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
            """
        )

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
            """
        )

    def test_exclusive_conflict(self) -> None:
        self.assert_model_error(
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
            r'^conflicting conditions for field "F1" in "Foo.Bar"',
        )

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
            """
        )

    def test_exclusive_with_length_invalid(self) -> None:
        self.assert_model_error(
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
            r'^conflicting conditions for field "F1" in "Foo.Bar"',
        )

    def test_no_valid_path(self) -> None:
        self.assert_model_error(
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
            r'^unreachable field "Final" in "Foo.Bar"',
        )

    def test_invalid_path_1(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element then null if 1 = 2;
                    end message;
            end Foo;
            """,
            r'^unreachable field "Final" in "Foo.Bar"',
        )

    def test_invalid_path_2(self) -> None:
        self.assert_model_error(
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
            r'^unreachable field "F2" in "Foo.Bar"',
        )

    def test_contradiction(self) -> None:
        self.assert_model_error(
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
            r'^contradicting condition 0 from field "F1" to "F2" in "Foo.Bar"',
        )

    def test_invalid_type_condition_range_low(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is range 100 .. 1000 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if F1 < 100;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2" in "Foo.Bar"',
        )

    def test_invalid_type_condition_range_high(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is range 5 .. 50000 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if F1 > 60000;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2" in "Foo.Bar"',
        )

    def test_invalid_type_condition_modular_upper(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 if F1 > 1000;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2" in "Foo.Bar"',
        )

    def test_invalid_type_condition_modular_lower(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 if F1 < 0;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2" in "Foo.Bar"',
        )

    # ISSUE: Componolit/RecordFlux#87
    def disabled_test_invalid_type_condition_enum(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element1 is (E1, E2, E3) with Size => 8;
                type Element2 is (E4, E5, E6) with Size => 8;
                type Bar is
                    message
                        F1 : Element1 then F2 if F1 = E4;
                        F2 : Element2;
                    end message;
            end Foo;
            """,
            r'^invalid type of "E4" in condition 0 from field "F1" to "F2"',
        )

    @staticmethod
    def test_tlv_valid_enum() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Tag is (T1, T2, T3) with Size => 8;
                type Length is range 0 .. 2**14 with Size => 16;
                type Bar is
                    message
                        L : Length;
                        T : Tag
                            then V
                                with Length => L
                                if T /= T2 and L <= 2**13;
                        V : Opaque;
                    end message;
            end Foo;
            """
        )

    def test_invalid_fixed_size_field_with_length(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => 300;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^fixed size field "F2" with length expression in "Foo.Bar"',
        )

    @staticmethod
    def test_valid_first() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'First;
                        F2 : Element;
                    end message;
            end Foo;
            """
        )

    def test_invalid_first(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'First + 8;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^invalid First for field "F2" in First expression 0 from field "F1" to "F2"'
            r' in "Foo.Bar"',
        )

    def test_invalid_first_is_last(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'Last;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^invalid First for field "F2" in First expression 0 from field "F1" to "F2"'
            r' in "Foo.Bar"',
        )

    def test_invalid_first_forward_reference(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F3'First;
                        F2 : Element;
                        F3 : Element;
                    end message;
            end Foo;
            """,
            r'^subsequent field "F3'
            "'"
            'First" referenced in First expression 0 from field "F1"'
            r' to "F2" in "Foo.Bar"',
        )

    @staticmethod
    def test_valid_length_reference() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => F1;
                        F2 : Opaque;
                    end message;
            end Foo;
            """
        )

    def test_invalid_length_forward_reference(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        null then F1 with Length => F2;
                        F1 : Opaque;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^subsequent field "F2" referenced in Length expression 0 from field "Initial"'
            r' to "F1" in "Foo.Bar"',
        )

    def test_invalid_negative_field_length(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => F1 - 300;
                        F2 : Opaque;
                    end message;
            end Foo;
            """,
            r'^negative length for field "F2" on path F1 -> F2 in "Foo.Bar"',
        )

    @staticmethod
    def test_valid_length_from_message_last() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Element
                            then null
                                with Length => Message'Last - F1'Last;
                    end message;
            end Foo;
            """
        )

    def test_payload_no_length(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Opaque;
                    end message;
            end Foo;
            """,
            r'^unconstrained field "F2" without length expression in "Foo.Bar"',
        )

    def test_array_no_length(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type Element is mod 2**8;
                type Element_List is array of Element;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Element_List;
                    end message;
            end Foo;
            """,
            r'^unconstrained field "F2" without length expression in "Foo.Bar"',
        )

    def test_incongruent_overlay(self) -> None:
        self.assert_model_error(
            """
            package Foo is
                type U8 is mod 2**8;
                type U16 is mod 2**16;
                type Bar is
                    message
                        F1 : U8;
                        F2 : U8
                            then F3
                            with First => F1'First;
                        F3 : U16;
                        F4 : U16;
                    end message;
            end Foo;
            """,
            r'^field "F3" not congruent with overlaid field "F1" in "Foo.Bar"',
        )

    def test_field_coverage_1(self) -> None:
        foo_type = ModularInteger("P.Foo", Pow(Number(2), Number(32)))
        structure = [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), first=Add(First(Variable("Message")), Number(64))),
            Link(Field("F2"), FINAL),
        ]

        types = {Field("F1"): foo_type, Field("F2"): foo_type}
        with mock.patch("rflx.model.Message._AbstractMessage__verify_conditions", lambda x: None):
            with self.assertRaisesRegex(ModelError, "^path F1 -> F2 does not cover whole message"):
                Message("P.M", structure, types)

    def test_field_coverage_2(self) -> None:
        foo_type = ModularInteger("P.Foo", Pow(Number(2), Number(32)))
        structure = [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2")),
            Link(Field("F2"), Field("F4"), Greater(Variable("F1"), Number(100))),
            Link(
                Field("F2"),
                Field("F3"),
                LessEqual(Variable("F1"), Number(100)),
                first=Add(Last(Variable("F2")), Number(64)),
            ),
            Link(Field("F3"), Field("F4")),
            Link(Field("F4"), FINAL),
        ]

        types = {
            Field("F1"): foo_type,
            Field("F2"): foo_type,
            Field("F3"): foo_type,
            Field("F4"): foo_type,
        }
        with mock.patch("rflx.model.Message._AbstractMessage__verify_conditions", lambda x: None):
            with self.assertRaisesRegex(
                ModelError, "^path F1 -> F2 -> F3 -> F4 does not cover whole message"
            ):
                Message("P.M", structure, types)

    def test_field_after_message_start(self) -> None:
        foo_type = ModularInteger("P.Foo", Pow(Number(2), Number(32)))
        structure = [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), first=Sub(First(Variable("Message")), Number(1000))),
            Link(Field("F2"), FINAL),
        ]

        types = {Field("F1"): foo_type, Field("F2"): foo_type}
        with mock.patch("rflx.model.Message._AbstractMessage__verify_conditions", lambda x: None):
            with self.assertRaisesRegex(
                ModelError, '^start of field "F2" on path F1 -> F2 before' " message start"
            ):
                Message("P.M", structure, types)

    @staticmethod
    def test_valid_use_message_length() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
               type Finished is
                  message
                     null
                        then Verify_Data
                           with Length => Message'Length;
                     Verify_Data : Opaque;
                  end message;
            end Foo;
            """
        )

    @staticmethod
    def test_valid_use_message_first_last() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
               type Finished is
                  message
                     null
                        then Verify_Data
                           with Length => Message'Last - Message'First + 1;
                     Verify_Data : Opaque;
                  end message;
            end Foo;
            """
        )
