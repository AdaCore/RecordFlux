from pathlib import Path
from typing import Sequence

import pytest

from rflx.error import RecordFluxError
from rflx.specification import parser


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    assert " model: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        for filename in filenames:
            p.parse(Path(filename))
        p.create_model()


def assert_error_string(string: str, regex: str) -> None:
    assert " model: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse_string(string)
        p.create_model()


def test_message_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:5:22: model: error: missing type for field "Foo" in "Test::PDU"$',
    )


def test_message_field_first_conflict() -> None:
    assert_error_string(
        """
            package Test is

               type T is mod 256;

               type M is
                  message
                     A : T
                        then B
                           with First => A'First;
                     B : T
                        with First => A'First;
                  end message;

            end Test;
        """,
        r"^"
        r'<stdin>:12:39: model: error: first aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:10:42: model: info: previous specification of first"
        r"$",
    )


def test_message_field_size_conflict() -> None:
    assert_error_string(
        """
            package Test is

               type T is mod 256;

               type M is
                  message
                     A : T
                        then B
                           with Size => 8;
                     B : T
                        with Size => 8;
                  end message;

            end Test;
        """,
        r"^"
        r'<stdin>:12:38: model: error: size aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:10:41: model: info: previous specification of size"
        r"$",
    )


def test_message_derivation_of_derived_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is null message;
               type Bar is new Foo;
               type Baz is new Bar;
            end Test;
        """,
        r'^<stdin>:5:16: model: error: illegal derivation "Test::Baz"\n'
        r'<stdin>:4:16: model: info: illegal base message type "Test::Bar"$',
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """
            package Test is
               type Boolean is mod 2;
            end Test;
        """,
        r'^<stdin>:3:16: model: error: illegal redefinition of built-in type "Boolean"',
    )


def test_invalid_modular_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 2**128;
            end Test;
        """,
        r'^<stdin>:3:30: model: error: modulus of "T" exceeds limit \(2\*\*64\)',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """
            package Test is
               type T is (FOO, BAR, BAZ) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:16: model: error: size of "T" too small',
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (FOO => 0, BAR => 0) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:44: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (FOO => 0, FOO_1 => 1, BAR => 0, BAR_1 => 1) with Size => 8;
            end Test;
        """,
        r'<stdin>:3:56: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence\n"
        r'<stdin>:3:68: model: error: duplicate enumeration value "1" in "T"\n'
        r"<stdin>:3:46: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_identical_literals() -> None:
    assert_error_string(
        """
            package Test is
               type T1 is (FOO, BAR) with Size => 1;
               type T2 is (BAR, BAZ) with Size => 1;
            end Test;
        """,
        r"<stdin>:4:16: model: error: conflicting literals: BAR\n"
        r'<stdin>:3:33: model: info: previous occurrence of "BAR"',
    )


def test_refinement_invalid_field() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Bar => PDU);
            end Test;
        """,
        r'^<stdin>:8:29: model: error: invalid field "Bar" in refinement',
    )


def test_refinement_invalid_condition() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     null
                        then Foo
                           with Size => 8;
                     Foo : Opaque;
                  end message;
               for PDU use (Foo => PDU)
                  if X < Y + 1;
            end Test;
        """,
        r"^"
        r'<stdin>:11:22: model: error: unknown field or literal "X"'
        r' in refinement condition of "Test::PDU"\n'
        r'<stdin>:11:26: model: error: unknown field or literal "Y"'
        r' in refinement condition of "Test::PDU"'
        r"$",
    )


def test_model_name_conflict_messages() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:8:21: model: error: name conflict for type "Test::PDU"\n'
        r'<stdin>:4:21: model: info: previous occurrence of "Test::PDU"$',
    )


def test_model_conflicting_refinements() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     null
                        then Foo
                           with Size => 8;
                     Foo : Opaque;
                  end message;
               for Test::PDU use (Foo => Test::PDU);
               for PDU use (Foo => PDU);
            end Test;
        """,
        r'^<stdin>:11:16: model: error: conflicting refinement of "Test::PDU" with "Test::PDU"\n'
        r"<stdin>:10:16: model: info: previous occurrence of refinement",
    )


def test_model_name_conflict_derivations() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     Foo : T;
                  end message;
               type Bar is new Test::Foo;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:9:21: model: error: name conflict for type "Test::Bar"\n'
        r'<stdin>:8:21: model: info: previous occurrence of "Test::Bar"',
    )


def test_model_name_conflict_sessions() -> None:
    assert_error_string(
        """
            package Test is
               type X is mod 2**8;

               generic
               session X with
                  Initial => A,
                  Final => A
               is
               begin
                  state A is null state;
               end X;
            end Test;
        """,
        r'^<stdin>:5:16: model: error: name conflict for session "Test::X"\n'
        r'<stdin>:3:21: model: info: previous occurrence of "Test::X"$',
    )


def test_model_illegal_first_aspect_at_initial_link() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     null
                        then Foo
                           with First => 0;
                     Foo : T;
                  end message;
            end Test;
        """,
        r"^<stdin>:8:42: model: error: illegal first aspect at initial link$",
    )


def test_message_with_two_size_fields() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is
              type Length is mod 2**8;
              type Packet is
                 message
                    Length_1 : Length;
                    Length_2 : Length
                       then Payload
                          with Size => 8 * (Length_1 + Length_2);
                    Payload : Opaque;
                 end message;
           end Test;
        """
    )
    p.create_model()


def test_message_same_field_and_type_name_with_different_size() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is

              type T is mod 2**8;

              type M is
                 message
                    A : T;
                    T : Opaque
                       with Size => 16;
                 end message;

           end Test;
        """
    )
    p.create_model()
