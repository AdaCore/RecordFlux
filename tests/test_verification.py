# pylint: disable=too-many-lines
from typing import Any, Mapping, Sequence

import pytest

from rflx.error import Location, RecordFluxError
from rflx.expression import (
    Add,
    Aggregate,
    And,
    Equal,
    First,
    Greater,
    Last,
    Length,
    Less,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Sub,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    Opaque,
    RangeInteger,
    Type,
)
from rflx.parser import Parser
from tests.models import ARRAYS_MODULAR_VECTOR, ENUMERATION, MODULAR_INTEGER, RANGE_INTEGER


def assert_model_error(string: str, regex: str) -> None:
    parser = Parser()
    with pytest.raises(RecordFluxError, match=regex):
        parser.parse_string(string)
        parser.create_model()


def assert_message_model_error(
    structure: Sequence[Link], types: Mapping[Field, Type], regex: str
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Message("P.M", structure, types)


def test_exclusive_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Greater(Variable("F1"), Number(80))),
        Link(Field("F1"), Field("F2"), condition=LessEqual(Variable("F1"), Number(80))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    Message("P.M", structure, types)


def test_exclusive_enum_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Equal(Variable("F1"), Variable("ONE"))),
        Link(Field("F1"), Field("F2"), condition=Equal(Variable("F1"), Variable("TWO"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): MODULAR_INTEGER,
    }
    Message("P.M", structure, types)


def test_exclusive_conflict() -> None:
    f1 = Field(ID("F1", Location((8, 4))))
    structure = [
        Link(INITIAL, f1),
        Link(f1, FINAL, condition=Greater(Variable("F1"), Number(50), Location((10, 5)))),
        Link(f1, Field("F2"), condition=Less(Variable("F1"), Number(80), Location((11, 7)))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:8:4: model: error: conflicting conditions for field "F1"\n'
        r"<stdin>:10:5: model: info: condition 0 [(]F1 -> Final[)]: F1 > 50\n"
        r"<stdin>:11:7: model: info: condition 1 [(]F1 -> F2[)]: F1 < 80"
        r"$",
    )


def test_exclusive_with_length_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1"), length=Number(32)),
        Link(
            Field("F1"),
            FINAL,
            condition=And(Equal(Length("F1"), Number(32)), Less(Variable("F1"), Number(50))),
        ),
        Link(
            Field("F1"),
            Field("F2"),
            condition=And(Equal(Length("F1"), Number(32)), Greater(Variable("F1"), Number(80))),
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): Opaque(),
        Field("F2"): MODULAR_INTEGER,
    }
    Message("P.M", structure, types)


def test_exclusive_with_length_invalid() -> None:
    f1 = Field(ID("F1", Location((98, 10))))
    structure = [
        Link(INITIAL, f1, length=Number(32)),
        Link(f1, FINAL, condition=Equal(Length("F1"), Number(32), Location((10, 2)))),
        Link(f1, Field("F2"), condition=Equal(Length("F1"), Number(32), Location((12, 4)))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): Opaque(),
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:98:10: model: error: conflicting conditions for field "F1"\n'
        r"<stdin>:10:2: model: info: condition 0 [(]F1 -> Final[)]: F1\'Length = 32\n"
        r"<stdin>:12:4: model: info: condition 1 [(]F1 -> F2[)]: F1\'Length = 32"
        r"$",
    )


def test_no_valid_path() -> None:
    f1 = Field(ID("F1", Location((10, 5))))
    f2 = Field(ID("F2", Location((11, 6))))
    f3 = Field(ID("F3", Location((12, 7))))
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2, condition=LessEqual(Variable("F1"), Number(80), Location((20, 2)))),
        Link(f1, f3, condition=Greater(Variable("F1"), Number(80), Location((21, 3)))),
        Link(f2, f3, condition=Greater(Variable("F1"), Number(80), Location((22, 4)))),
        Link(f3, FINAL, condition=LessEqual(Variable("F1"), Number(80), Location((23, 5)))),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
        Field("F3"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:11:6: model: error: unreachable field "F2" in "P.M"\n'
        r"<stdin>:11:6: model: info: path 0 [(]F1 -> F2[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:11:6: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:7: model: error: unreachable field "F3" in "P.M"\n'
        r"<stdin>:12:7: model: info: path 0 [(]F1 -> F2 -> F3[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r"<stdin>:12:7: model: info: path 1 [(]F1 -> F3[)]:\n"
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:7: model: info: unsatisfied "F1 <= 80"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> F3 -> Final[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r"model: info: path 1 [(]F1 -> F3 -> Final[)]:\n"
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: info: unsatisfied "F1 <= 80"'
        r"$",
    )


def test_invalid_path_1(monkeypatch: Any) -> None:
    f1 = Field(ID("F1", Location((20, 10))))
    structure = [
        Link(INITIAL, f1),
        Link(f1, FINAL, condition=Equal(Number(1), Number(2), Location((5, 10)))),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
    }
    monkeypatch.setattr(Message, "_AbstractMessage__prove_reachability", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:5:10: model: error: contradicting condition in "P.M"\n'
        r'<stdin>:20:10: model: info: on path "F1"\n'
        r'<stdin>:5:10: model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_invalid_path_2(monkeypatch: Any) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Number(1), Number(2))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    monkeypatch.setattr(Message, "_AbstractMessage__prove_reachability", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_contradiction() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Number(1), Number(2))),
        Link(Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(50))),
        Link(Field("F1"), FINAL, condition=Greater(Variable("F1"), Number(60))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_invalid_type_condition_range_low() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(1))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "F1 >= 1"\n'
        r'model: info: unsatisfied "F1 < 1"'
        r"$",
    )


def test_invalid_type_condition_range_high() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Greater(Variable("F1"), Number(200))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "F1 <= 100"\n'
        r'model: info: unsatisfied "F1 > 200"'
        r"$",
    )


def test_invalid_type_condition_modular_upper() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Greater(Variable("F1"), Number(2 ** 16 + 1))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "F1 < 256"\n'
        r'model: info: unsatisfied "F1 > 65537"'
        r"$",
    )


def test_invalid_type_condition_modular_lower() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(0))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "F1"\n'
        r'model: info: unsatisfied "F1 >= 0"\n'
        r'model: info: unsatisfied "F1 < 0"'
        r"$",
    )


@pytest.mark.skip(reason="ISSUE: Componolit/RecordFlux#87")
def test_invalid_type_condition_enum() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Variable("F1"), Variable("E4"))),
        Link(Field("F2"), FINAL),
    ]
    e1 = Enumeration("P.E1", {"E1": Number(1), "E2": Number(2), "E3": Number(3)}, Number(8), False)
    e2 = Enumeration("P.E2", {"E4": Number(1), "E5": Number(2), "E6": Number(3)}, Number(8), False)
    types = {
        Field("F1"): e1,
        Field("F2"): e2,
    }
    assert_message_model_error(
        structure, types, r'^invalid type of "E4" in condition 0 from field "F1" to "F2" in "P.M"',
    )


def test_tlv_valid_enum() -> None:
    structure = [
        Link(INITIAL, Field("L")),
        Link(Field("L"), Field("T")),
        Link(
            Field("T"),
            Field("V"),
            length=Variable("L"),
            condition=And(
                NotEqual(Variable("T"), Variable("TWO")), LessEqual(Variable("L"), Number(8192))
            ),
        ),
        Link(Field("V"), FINAL),
    ]
    types = {
        Field("L"): RANGE_INTEGER,
        Field("T"): ENUMERATION,
        Field("V"): Opaque(),
    }
    Message("P.M", structure, types)


def test_invalid_fixed_size_field_with_length() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), length=Number(300)),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure, types, r'^model: error: fixed size field "F2" with length expression$',
    )


def test_valid_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=First("F1")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    Message("P.M", structure, types)


def test_invalid_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Add(First("F1"), Number(8), Location((5, 14)))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure, types, r'^<stdin>:5:14: model: error: invalid First for field "F2"$',
    )


def test_invalid_first_is_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Last(ID("F1", Location((11, 20))))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure, types, r'^<stdin>:11:20: model: error: invalid First for field "F2"$',
    )


def test_invalid_first_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=First("F3")),
        Link(Field("F2"), Field("F3")),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): MODULAR_INTEGER,
    }
    assert_message_model_error(structure, types, '^model: error: subsequent field "F3" referenced$')


def test_valid_length_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), length=Variable("F1")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): Opaque(),
    }
    Message("P.M", structure, types)


def test_invalid_length_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), length=Variable("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): Opaque(),
    }
    assert_message_model_error(structure, types, '^model: error: subsequent field "F2" referenced$')


def test_invalid_negative_field_length() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), length=Sub(Variable("F1"), Number(300))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): Opaque(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: negative length for field "F2" [(]F1 -> F2[)]\n'
        r'model: info: unsatisfied "F1 < 256"\n'
        r'model: info: unsatisfied "F1 - 300 >= 0"'
        r"$",
    )


def test_payload_no_length() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): Opaque(),
    }
    assert_message_model_error(
        structure, types, r'^model: error: unconstrained field "F2" without length expression$'
    )


def test_array_no_length() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): ARRAYS_MODULAR_VECTOR,
    }
    assert_message_model_error(
        structure, types, '^model: error: unconstrained field "F2" without length expression$'
    )


def test_incongruent_overlay() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), first=First("F1")),
        Link(Field("F3"), Field("F4")),
        Link(Field("F4"), FINAL),
    ]
    u8 = ModularInteger("P.U8", Number(256))
    u16 = ModularInteger("P.U16", Number(65536))
    types = {
        Field("F1"): u8,
        Field("F2"): u8,
        Field("F3"): u16,
        Field("F4"): u16,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: field "F3" not congruent with overlaid field "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'Last = [(][(]Message\'First [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "[(][(]F1\'First [+] 16[)][)] - 1 = F1\'Last"'
        r"$",
    )


def test_field_coverage_1(monkeypatch: Any) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Add(First("Message"), Number(64))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): MODULAR_INTEGER, Field("F2"): MODULAR_INTEGER}
    monkeypatch.setattr(Message, "_AbstractMessage__verify_conditions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r"model: error: path does not cover whole message\n"
        r'model: info: on path "F1"\n'
        r'model: info: on path "F2"'
        r"$",
    )


def test_field_coverage_2(monkeypatch: Any) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F4"), Greater(Variable("F1"), Number(100))),
        Link(
            Field("F2"),
            Field("F3"),
            LessEqual(Variable("F1"), Number(100)),
            first=Add(Last("F2"), Number(64)),
        ),
        Link(Field("F3"), Field("F4")),
        Link(Field("F4"), FINAL),
    ]

    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): MODULAR_INTEGER,
        Field("F4"): MODULAR_INTEGER,
    }
    monkeypatch.setattr(Message, "_AbstractMessage__verify_conditions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r"model: error: path does not cover whole message\n"
        r'model: info: on path "F1"\n'
        r'model: info: on path "F2"\n'
        r'model: info: on path "F3"\n'
        r'model: info: on path "F4"'
        r"$",
    )


def test_field_after_message_start(monkeypatch: Any) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Sub(First("Message"), Number(1000))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): MODULAR_INTEGER, Field("F2"): MODULAR_INTEGER}
    monkeypatch.setattr(Message, "_AbstractMessage__verify_conditions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: negative length for field "F2" [(]F1 -> F2[)]\n'
        r'model: info: unsatisfied "Message\'First - 1000 >= Message\'First"'
        r"$",
    )


def test_valid_use_message_length() -> None:
    structure = [
        Link(INITIAL, Field("Verify_Data"), length=Length("Message")),
        Link(Field("Verify_Data"), FINAL),
    ]
    types = {Field("Verify_Data"): Opaque()}
    Message("P.M", structure, types)


def test_valid_use_message_first_last() -> None:
    structure = [
        Link(
            INITIAL,
            Field("Verify_Data"),
            length=Add(Sub(Last("Message"), First("Message")), Number(1)),
        ),
        Link(Field("Verify_Data"), FINAL),
    ]
    types = {Field("Verify_Data"): Opaque()}
    Message("P.M", structure, types)


def test_no_path_to_final() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), Greater(Variable("F1"), Number(100))),
        Link(Field("F2"), Field("F4"), LessEqual(Variable("F1"), Number(100))),
        Link(Field("F3"), FINAL),
    ]

    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): MODULAR_INTEGER,
        Field("F4"): MODULAR_INTEGER,
    }
    assert_message_model_error(structure, types, '^model: error: no path to FINAL for field "F4"$')


def test_no_path_to_final_transitive() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), Greater(Variable("F1"), Number(100))),
        Link(Field("F3"), FINAL),
        Link(Field("F2"), Field("F4"), LessEqual(Variable("F1"), Number(100))),
        Link(Field("F4"), Field("F5")),
        Link(Field("F5"), Field("F6")),
    ]

    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): MODULAR_INTEGER,
        Field("F4"): MODULAR_INTEGER,
        Field("F5"): MODULAR_INTEGER,
        Field("F6"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: no path to FINAL for field "F4"\n'
        r'model: error: no path to FINAL for field "F5"\n'
        r'model: error: no path to FINAL for field "F6"'
        r"$",
    )


def test_conditionally_unreachable_field_mod_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P.M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_mod_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Last("F1"), Last("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_range_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P.M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_range_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Last("F1"), Last("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_enum_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): ENUMERATION,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P.M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_enum_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Last("F1"), Last("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): ENUMERATION,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(][(][(]F1\'Last [+] 1[)] [+] 8[)][)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_outgoing() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(32))),
        Link(Field("F1"), FINAL, Greater(Variable("F1"), Number(32))),
        Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(32))),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P.M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1 <= 32"\n'
        r'model: info: unsatisfied "F1 > 32"'
        r"$",
    )


def test_conditionally_unreachable_field_outgoing_multi() -> None:
    f2 = Field(ID("F2", Location((90, 12))))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), f2, LessEqual(Variable("F1"), Number(32), Location((66, 3)))),
        Link(Field("F1"), Field("F3"), Greater(Variable("F1"), Number(32))),
        Link(
            f2,
            Field("F3"),
            And(
                Greater(Variable("F1"), Number(32)),
                LessEqual(Variable("F1"), Number(48)),
                Location((22, 34)),
            ),
        ),
        Link(f2, FINAL, Greater(Variable("F1"), Number(48))),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): ModularInteger("P.Modular", Number(256)),
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:90:12: model: error: unreachable field "F2" in "P.M"\n'
        r"<stdin>:90:12: model: info: path 0 [(]F1 -> F2[)]:\n"
        r'<stdin>:66:3: model: info: unsatisfied "F1 <= 32"\n'
        r'<stdin>:90:12: model: info: unsatisfied "[(]F1 > 32 and F1 <= 48[)] or F1 > 48"'
        r"$",
    )


def test_length_attribute_final() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, length=Number(100, location=Location((4, 12)))),
    ]
    types = {
        Field("F1"): MODULAR_INTEGER,
        Field("F2"): MODULAR_INTEGER,
    }
    assert_message_model_error(
        structure, types, '^<stdin>:4:12: model: error: length attribute for final field in "P.M"$'
    )


def test_aggregate_equal_valid_length() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(40)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=Equal(
                Variable("Magic"), Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
            ),
        ),
    ]
    types = {
        Field("Magic"): Opaque(),
    }
    Message("P.M", structure, types)


def test_aggregate_equal_invalid_length1() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(40)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=Equal(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): Opaque(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Length"\n'
        r'model: info: unsatisfied "Magic\'Length = 40"'
        r"$",
    )


def test_aggregate_equal_invalid_length2() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(40)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=Equal(Aggregate(Number(1), Number(2)), Variable("Magic")),
        ),
    ]
    types = {
        Field("Magic"): Opaque(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Length"\n'
        r'model: info: unsatisfied "Magic\'Length = 40"'
        r"$",
    )


def test_aggregate_inequal_valid_length() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(40)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=NotEqual(
                Variable("Magic"), Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
            ),
        ),
    ]
    types = {
        Field("Magic"): Opaque(),
    }
    Message("P.M", structure, types)


def test_aggregate_inequal_invalid_length() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(40)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=NotEqual(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): Opaque(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P.M"\n'
        r'model: info: on path "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Length"\n'
        r'model: info: unsatisfied "Magic\'Length = 40"'
        r"$",
    )


def test_aggregate_equal_array_valid_length() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), length=Number(14)),
        Link(
            Field("Magic"),
            Field("Final"),
            condition=NotEqual(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): Array("P.Arr", ModularInteger("P.Modular", Number(128))),
    }
    Message("P.M", structure, types)


def test_aggregate_equal_array_invalid_length() -> None:
    magic = Field(ID("Magic", Location((3, 5))))
    final = Field(ID("Final", Location((10, 7))))
    structure = [
        Link(INITIAL, magic, length=Number(40, location=Location((19, 17)))),
        Link(
            magic,
            final,
            condition=NotEqual(
                Variable("Magic"), Aggregate(Number(1), Number(2)), Location((17, 3))
            ),
        ),
    ]
    types = {
        Field("Magic"): Array(
            "P.Arr", ModularInteger("P.Modular", Number(128), location=Location((66, 3)))
        ),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:17:3: model: error: contradicting condition in "P.M"\n'
        r'<stdin>:3:5: model: info: on path "Magic"\n'
        r'<stdin>:17:3: model: info: unsatisfied "2 [*] Modular\'Length = Magic\'Length"\n'
        r'<stdin>:66:3: model: info: unsatisfied "Modular\'Length = 7"\n'
        r'<stdin>:19:17: model: info: unsatisfied "Magic\'Length = 40"'
        r"$",
    )


def test_aggregate_equal_invalid_length_field() -> None:

    length = Field(ID("Length", Location((2, 5))))
    magic = Field(ID("Magic", Location((3, 5))))

    structure = [
        Link(INITIAL, length),
        Link(length, magic, length=Mul(Number(8), Variable("Length"), Location((6, 5)))),
        Link(
            magic,
            FINAL,
            condition=Equal(
                Variable("Magic"), Aggregate(Number(1), Number(2)), location=Location((10, 5))
            ),
        ),
    ]
    types = {
        Field("Length"): RangeInteger(
            "P.Length_Type", Number(10), Number(100), Number(8), Location((5, 10))
        ),
        Field(ID("Magic", Location((17, 3)))): Opaque(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:5: model: error: contradicting condition in "P.M"\n'
        r'<stdin>:2:5: model: info: on path "Length"\n'
        r'<stdin>:3:5: model: info: on path "Magic"\n'
        r'<stdin>:10:5: model: info: unsatisfied "2 [*] 8 = Magic\'Length"\n'
        r'<stdin>:5:10: model: info: unsatisfied "Length >= 10"\n'
        r'<stdin>:6:5: model: info: unsatisfied "Magic\'Length = 8 [*] Length"'
        r"$",
    )


def test_no_contradiction_multi() -> None:
    structure = [
        Link(INITIAL, Field("F0")),
        Link(Field("F0"), Field("F1"), condition=Equal(Variable("F0"), Number(1))),
        Link(Field("F0"), Field("F2"), condition=Equal(Variable("F0"), Number(2))),
        Link(Field("F1"), Field("F3")),
        Link(Field("F2"), Field("F3")),
        Link(Field("F3"), Field("F4"), condition=Equal(Variable("F0"), Number(1))),
        Link(Field("F3"), Field("F5"), condition=Equal(Variable("F0"), Number(2))),
        Link(Field("F4"), FINAL),
        Link(Field("F5"), FINAL),
    ]
    types = {
        Field("F0"): RANGE_INTEGER,
        Field("F1"): RANGE_INTEGER,
        Field("F2"): RANGE_INTEGER,
        Field("F3"): RANGE_INTEGER,
        Field("F4"): RANGE_INTEGER,
        Field("F5"): RANGE_INTEGER,
    }
    Message("P.M", structure, types)
