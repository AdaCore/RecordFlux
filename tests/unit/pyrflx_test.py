# ruff: noqa: SLF001

from collections import abc
from pathlib import Path

import pytest

from rflx import expression as expr
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    Enumeration,
    Integer,
    Link,
    Message,
    Opaque,
    Sequence,
    Type,
)
from rflx.pyrflx import (
    Bitstring,
    EnumValue,
    IntegerValue,
    MessageValue,
    OpaqueValue,
    Package,
    PyRFLX,
    SequenceValue,
    TypeValue,
    utils,
)
from rflx.pyrflx.error import PyRFLXError
from tests.const import SPEC_DIR


def assert_bytestring_error(msg: MessageValue, msg_name: ID) -> None:
    with pytest.raises(
        PyRFLXError,
        match=f"^pyrflx: error: cannot create bytestring of invalid message: {msg_name}$",
    ):
        msg.bytestring  # noqa: B018


def test_file_not_found(tmp_path: Path) -> None:
    with pytest.raises(FileNotFoundError):
        PyRFLX.from_specs([f"{tmp_path}/test.rflx"])


def test_package_name() -> None:
    p = Package("Test")
    assert p.name == "Test"


def test_package_iterator(tlv_package: Package) -> None:
    assert [m.name for m in tlv_package] == ["Message"]


def test_pyrflx_iterator(pyrflx_: PyRFLX) -> None:
    assert {p.name for p in pyrflx_} == {
        "Endianness",
        "Ethernet",
        "ICMP",
        "IPv4",
        "Message_Size",
        "TLS_Alert",
        "TLS_Record",
        "TLV",
        "UDP",
        "Sequence_Message",
        "Sequence_Type",
        "Null_Message",
        "Parameterized",
        "TLV_With_Checksum",
        "No_Conditionals",
        "Message_Type_Size_Condition",
        "Always_Valid_Aspect",
        "Low_Order",
        "Aggregate_In_Relation",
    }


def test_attributes(pyrflx_: PyRFLX) -> None:
    pyrflx_ = PyRFLX.from_specs([SPEC_DIR / "tlv.rflx"])
    assert isinstance(pyrflx_.package("TLV"), Package)
    tlv_package = pyrflx_.package("TLV")
    assert isinstance(tlv_package.new_message("Message"), MessageValue)


def test_no_verification(icmp_message_value: MessageValue) -> None:
    pyrflx_ = PyRFLX.from_specs(
        [SPEC_DIR / "icmp.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    icmp_message_value_unv = pyrflx_.package("ICMP").new_message("Message")
    icmp_message_value.set("Tag", "Echo_Request")
    icmp_message_value.set("Code_Zero", 0)
    icmp_message_value.set("Checksum", 1234)
    icmp_message_value.set("Identifier", 5)
    icmp_message_value.set("Sequence_Number", 1)
    icmp_message_value.set("Data", b"\x00")
    icmp_message_value_unv.set("Tag", "Echo_Request")
    icmp_message_value_unv.set("Code_Zero", 0)
    icmp_message_value_unv.set("Checksum", 1234)
    icmp_message_value_unv.set("Identifier", 5)
    icmp_message_value_unv.set("Sequence_Number", 1)
    icmp_message_value_unv.set("Data", b"\x00")
    assert icmp_message_value_unv.bytestring == icmp_message_value.bytestring
    assert icmp_message_value_unv.valid_message


def test_message_value_identifier(ethernet_frame_value: MessageValue) -> None:
    assert ethernet_frame_value.identifier == ID("Ethernet") * "Frame"
    assert ethernet_frame_value.package == ID("Ethernet")
    assert ethernet_frame_value.name == "Frame"


def test_message_value_eq(tlv_package: Package) -> None:
    m1 = tlv_package.new_message("Message")
    assert m1 == tlv_package.new_message("Message")
    assert m1 is not tlv_package.new_message("Message")
    assert tlv_package.new_message("Message") == tlv_package.new_message("Message")
    assert tlv_package.new_message("Message") is not tlv_package.new_message("Message")
    assert m1 is not None


def test_message_value_bitstring(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.bitstring == Bitstring("")
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.bitstring == Bitstring("00000001")
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.bitstring == Bitstring("000000010000000000000001")
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.bitstring == Bitstring("00000001000000000000000100000001")


def test_message_value_all_fields(
    tlv_message_value: MessageValue,
    ethernet_frame_value: MessageValue,
) -> None:
    assert tlv_message_value.fields == ["Tag", "Length", "Value"]
    assert ethernet_frame_value.fields == [
        "Destination",
        "Source",
        "Type_Length_TPID",
        "TPID",
        "TCI",
        "Type_Length",
        "Payload",
    ]


def test_message_value_accessible_fields_initial_fields(
    tlv_message_value: MessageValue,
    ethernet_frame_value: MessageValue,
) -> None:
    assert tlv_message_value.accessible_fields == ["Tag"]
    assert ethernet_frame_value.accessible_fields == ["Destination", "Source", "Type_Length_TPID"]


def test_message_value_accessible_fields_tag_fields(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.accessible_fields == ["Tag", "Length"]


def test_message_value_accessible_fields_length_fields(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.accessible_fields == ["Tag", "Length", "Value"]
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.accessible_fields == ["Tag", "Length", "Value"]


def test_message_value_accessible_fields_error_fields(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.accessible_fields == ["Tag"]


def test_message_value_accessible_fields_error_reset_fields(
    tlv_message_value: MessageValue,
) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.accessible_fields == ["Tag", "Length", "Value"]
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.accessible_fields == ["Tag"]


def test_message_value_accessible_fields_fields_complex(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.accessible_fields == ["Tag"]
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.accessible_fields == ["Tag"]
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.accessible_fields == ["Tag", "Length"]
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.accessible_fields == ["Tag", "Length", "Value"]
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.accessible_fields == ["Tag", "Length", "Value"]
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.accessible_fields == ["Tag"]


def test_message_value_valid_message(tlv_message_value: MessageValue) -> None:
    assert not tlv_message_value.valid_message
    assert_bytestring_error(tlv_message_value, tlv_message_value.identifier)
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\x03"  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598
    tlv_message_value.set("Tag", "Msg_Data")
    assert not tlv_message_value.valid_message
    assert_bytestring_error(tlv_message_value, tlv_message_value.identifier)
    tlv_message_value.set("Length", 1)
    assert not tlv_message_value.valid_message
    assert_bytestring_error(tlv_message_value, tlv_message_value.identifier)
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\x01\x00\x01\x01"


def test_message_value_valid_fields(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.valid_fields == []
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.valid_fields == ["Tag"]
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.valid_fields == ["Tag", "Length"]
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.valid_fields == ["Tag", "Length", "Value"]


def test_message_value_set_value(tlv_message_value: MessageValue) -> None:
    v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
    v2 = b"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10"
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 8)
    tlv_message_value.set("Value", v1)
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set value for field Value\n"
            "pyrflx: error: invalid data size: input size is 80 while expected input size is 64"
            "$"
        ),
    ):
        tlv_message_value.set("Value", v2)


def test_message_value_generate(tlv_message_value: MessageValue) -> None:
    test_message_value_payload = b"\x01\x02\x03\x04\x05\x06\x07\x08"
    test_message_value_data = b"\x01\x00\x08" + test_message_value_payload
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 8)
    tlv_message_value.set("Value", test_message_value_payload)
    assert tlv_message_value.bytestring == test_message_value_data


def test_message_value_change_field(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 1)
    tlv_message_value.set("Tag", "Msg_Data")
    assert "Length" in tlv_message_value.valid_fields
    tlv_message_value.set("Value", b"a")
    tlv_message_value.set("Length", 2)
    assert "Value" not in tlv_message_value.valid_fields
    tlv_message_value.set("Value", b"ab")
    assert "Value" in tlv_message_value.valid_fields


def test_message_value_binary_length(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 8)
    tlv_message_value.set("Value", bytes(8))
    assert tlv_message_value.bytestring == b"\x01\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00"


def test_message_value_set_get_value(tlv_message_value: MessageValue) -> None:
    v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 8)
    tlv_message_value.set("Value", v1)
    assert tlv_message_value.get("Tag") == "TLV::Msg_Data"
    assert tlv_message_value.get("Length") == 8
    assert tlv_message_value.get("Value") == v1


def test_message_value_get_invalid_field(tlv_message_value: MessageValue) -> None:
    with pytest.raises(
        PyRFLXError,
        match=r'^pyrflx: error: "nofield" is not a field of this message$',
    ):
        tlv_message_value.get("nofield")

    with pytest.raises(PyRFLXError, match=r'^pyrflx: error: "Length" is not set$'):
        tlv_message_value.get("Length")


def test_message_value_set_invalid_field(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: cannot access field Value$"):
        tlv_message_value.set("Value", b"")
    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: cannot access field Checksum$"):
        tlv_message_value.set("Checksum", 8)
    tlv_message_value.set("Tag", "Msg_Error")
    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: cannot access field Length$"):
        tlv_message_value.set("Length", 8)


def test_message_value_invalid_value(tlv_message_value: MessageValue) -> None:
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set value for field Tag\n"
            "pyrflx: error: cannot assign different types: str != int"
            "$"
        ),
    ):
        tlv_message_value.set("Tag", 1)
    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set value for field Length\n"
            "pyrflx: error: cannot assign different types: int != str"
            "$"
        ),
    ):
        tlv_message_value.set("Length", "blubb")


def test_message_value_next(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value._next_field(INITIAL.name) == "Tag"
    assert tlv_message_value._next_field("Tag") == "Length"
    assert tlv_message_value._next_field(FINAL.name) == ""


def test_message_value_prev(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value._prev_field("Tag") == INITIAL.name
    assert tlv_message_value._prev_field(INITIAL.name) == ""
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value._prev_field("Length") == ""


def test_message_value_required_fields(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.required_fields == ["Tag"]
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.required_fields == ["Length"]
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.required_fields == ["Value"]
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.required_fields == []


def test_message_value_empty_opaque_field(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 0)
    tlv_message_value.set("Value", b"")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\x01\x00\x00"


def test_message_value_field_eq() -> None:
    f1 = MessageValue.Field(OpaqueValue(Opaque()), "f1")
    assert f1 == MessageValue.Field(OpaqueValue(Opaque()), "f1")
    f1.typeval.parse(b"")
    assert f1 != MessageValue.Field(OpaqueValue(Opaque()), "f2")
    assert f1 is not None


def test_message_value_field_set() -> None:
    f = MessageValue.Field(OpaqueValue(Opaque()), "f")
    assert not f.set
    f.typeval.parse(b"\x01")
    assert not f.set
    f.first = expr.Number(1)
    assert f.set


def test_message_value_is_valid_opaque_field(
    tlv_message_value: MessageValue,
    ethernet_frame_value: MessageValue,
) -> None:
    assert not tlv_message_value._is_valid_composite_field("Value")
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 1000)
    assert tlv_message_value._is_valid_composite_field("Value")
    ethernet_frame_value.set("Destination", 2**48 - 1)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 1501)
    assert not ethernet_frame_value._is_valid_composite_field("Payload")
    ethernet_frame_value.set("Type_Length_TPID", 1500)
    ethernet_frame_value.set("Type_Length", 1500)
    assert ethernet_frame_value._is_valid_composite_field("Payload")


def test_message_value_parse(ethernet_frame_value: MessageValue) -> None:
    test_bytes = (
        b"\xe0\x28\x6d\x39\x80\x1e\x1c\x1b\x0d\xe0\xd8\xa8\x08\x00\x45\x00"
        b"\x00\x4c\x1f\x04\x40\x00\x40\x01\xe1\x6a\xc0\xa8\xbc\x3d\xac\xd9"
        b"\x10\x83\x08\x00\xe1\x26\x00\x09\x00\x01\x4a\xfc\x0d\x00\x00\x00"
        b"\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d"
        b"\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d"
        b"\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    ethernet_frame_value.parse(test_bytes)
    assert ethernet_frame_value.valid_message
    assert ethernet_frame_value.bytestring == test_bytes


def test_message_value_parse_incorrect_nested_message(ethernet_frame_value: MessageValue) -> None:
    incorrect_message = (
        b"\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00"
        b"\x81\x00\x00\x01\x08\x00\x45\x00\x00\xe2\x00\x01"
        b"\x00\x00\x40\x00\x7c\xe7\x7f\x00\x00\x01\x7f\x00"
        b"\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x0a"
    )
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set value for field Payload\n"
            "pyrflx: error: Error while parsing nested message IPv4::Packet\n"
            "pyrflx: error: Bitstring representing the message is too short - "
            "stopped while parsing field: Payload"
            "$"
        ),
    ):
        ethernet_frame_value.parse(incorrect_message)


def test_message_value_parse_from_bitstring(
    tlv_message_value: MessageValue,
    enum_value: EnumValue,
) -> None:
    intval = IntegerValue(Integer("Test::Int", expr.Number(0), expr.Number(255), expr.Number(8)))
    intval.parse(b"\x02")
    assert intval.value == 2
    enum_value.parse(b"\x01")
    assert enum_value.value == "Test::One"
    assert enum_value.numeric_value == expr.Number(1)
    msg_sequence = SequenceValue(Sequence("Test::MsgSequence", tlv_message_value._type))
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 4)
    tlv_message_value.set("Value", b"\x00\x00\x00\x00")
    msg_sequence.parse(tlv_message_value.bytestring)


def test_message_value_parse_from_bitstring_invalid(tlv_message_value: MessageValue) -> None:
    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: Bitstring does not consist of only 0 and 1$",
    ):
        Bitstring("123")
    assert Bitstring("01") + Bitstring("00") == Bitstring("0100")

    test_bytes = b"\x01"
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: Bitstring representing the message is too short"
            " - stopped while parsing field: Length"
            "$"
        ),
    ):
        tlv_message_value.parse(test_bytes)
    assert not tlv_message_value.valid_message


def test_message_value_set_invalid(ethernet_frame_value: MessageValue) -> None:
    ethernet_frame_value.set("Destination", 2**48 - 1)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 1501)
    with pytest.raises(
        PyRFLXError,
        match=(
            r"^"
            "pyrflx: error: none of the field conditions .* for field Type_Length"
            " have been met by the assigned value: 1501"
            "$"
        ),
    ):
        ethernet_frame_value.set("Type_Length", 1501)


def test_integer_value() -> None:
    rangetype = Integer("Test::Int", expr.Number(8), expr.Number(16), expr.Number(8))
    rangevalue = IntegerValue(rangetype)
    assert not rangevalue.initialized
    with pytest.raises(PyRFLXError, match="^pyrflx: error: value Test::Int not initialized$"):
        rangevalue.value  # noqa: B018
    with pytest.raises(PyRFLXError, match="^pyrflx: error: value Test::Int not initialized$"):
        rangevalue.expr  # noqa: B018
    rangevalue.assign(10)
    assert rangevalue.initialized
    assert rangevalue.value == 10  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598
    assert str(rangevalue.bitstring) == "00001010"
    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: value 17 not in type range 8 .. 16$"):
        rangevalue.assign(17)
    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: value 7 not in type range 8 .. 16$"):
        rangevalue.assign(7)


@pytest.fixture(name="enum_value")
def fixture_enum_value() -> EnumValue:
    return EnumValue(
        Enumeration(
            "Test::Enum",
            [("One", expr.Number(1)), ("Two", expr.Number(2))],
            expr.Number(8),
            always_valid=False,
        ),
    )


def test_enum_value_literals(enum_value: EnumValue) -> None:
    assert enum_value.literals == {
        expr.Literal("One"): expr.Number(1),
        expr.Literal("Test::One"): expr.Number(1),
        expr.Literal("Two"): expr.Number(2),
        expr.Literal("Test::Two"): expr.Number(2),
    }
    assert not enum_value.initialized


def test_enum_value_assign(enum_value: EnumValue) -> None:
    with pytest.raises(PyRFLXError, match="^pyrflx: error: value Test::Enum not initialized$"):
        enum_value.value  # noqa: B018
    with pytest.raises(PyRFLXError, match="^pyrflx: error: value Test::Enum not initialized$"):
        enum_value.expr  # noqa: B018

    enum_value.assign("One")
    assert enum_value.initialized
    assert enum_value.value == "Test::One"
    assert str(enum_value.bitstring) == "00000001"

    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: Three is not a valid enum value$"):
        enum_value.assign("Three")


def test_enum_value_parse(enum_value: EnumValue) -> None:
    enum_value.parse(b"\x01")
    assert enum_value.value == "Test::One"

    with pytest.raises(PyRFLXError, match=r"^pyrflx: error: Number 15 is not a valid enum value$"):
        enum_value.parse(Bitstring("1111"))


@pytest.fixture(name="enum_value_imported")
def fixture_enum_value_imported() -> EnumValue:
    return EnumValue(
        Enumeration(
            "Test::Enum",
            [("One", expr.Number(1)), ("Two", expr.Number(2))],
            expr.Number(8),
            always_valid=False,
        ),
        imported=True,
    )


def test_enum_value_imported(enum_value_imported: EnumValue) -> None:
    assert enum_value_imported.literals == {
        expr.Literal("Test::One"): expr.Number(1),
        expr.Literal("Test::Two"): expr.Number(2),
    }
    assert not enum_value_imported.initialized


def test_enum_value_imported_assign(enum_value_imported: EnumValue) -> None:
    enum_value_imported.assign("One")
    assert enum_value_imported.value == "Test::One"
    enum_value_imported.assign("Test::Two")
    assert enum_value_imported.value == "Test::Two"


def test_enum_value_imported_parse(enum_value_imported: EnumValue) -> None:
    enum_value_imported.parse(b"\x01")
    assert enum_value_imported.value == "Test::One"


@pytest.fixture(name="enum_value_builtin")
def fixture_enum_value_builtin() -> EnumValue:
    return EnumValue(BOOLEAN, imported=True)


def test_enum_value_builtin(enum_value_builtin: EnumValue) -> None:
    assert enum_value_builtin.literals == {
        expr.Literal("False"): expr.Number(0),
        expr.Literal("True"): expr.Number(1),
    }
    assert not enum_value_builtin.initialized


def test_enum_value_builtin_assign(enum_value_builtin: EnumValue) -> None:
    enum_value_builtin.assign("True")
    assert enum_value_builtin.value == "True"
    enum_value_builtin.assign("False")
    assert enum_value_builtin.value == "False"


def test_enum_value_builtin_parse(enum_value_builtin: EnumValue) -> None:
    enum_value_builtin.parse(b"\x00")
    assert enum_value_builtin.value == "False"
    enum_value_builtin.parse(b"\x01")
    assert enum_value_builtin.value == "True"


def test_opaque_value() -> None:
    opaquevalue = OpaqueValue(Opaque())
    assert not opaquevalue.initialized
    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: value __INTERNAL__::Opaque not initialized$",
    ):
        opaquevalue.value  # noqa: B018
    opaquevalue.assign(b"\x01\x02")
    assert opaquevalue.initialized
    assert opaquevalue.value == b"\x01\x02"  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598
    k = opaquevalue.size
    assert isinstance(k, expr.Number)
    assert k.value == 16
    assert str(opaquevalue.bitstring) == "0000000100000010"
    opaquevalue.parse(Bitstring("1111"))
    assert opaquevalue._value == b"\x0f"


def test_opaque_value_eq(enum_value: EnumValue) -> None:
    ov = OpaqueValue(Opaque())
    ev = enum_value
    rangetype = Integer("Test::Int", expr.Number(8), expr.Number(16), expr.Number(8))
    rv = IntegerValue(rangetype)
    rangetype2 = Integer("Test::Int", expr.Number(0), expr.Number(100), expr.Number(16))
    rv2 = IntegerValue(rangetype2)
    rv3 = IntegerValue(rangetype2)
    assert ov == ov  # noqa: PLR0124
    assert ev == ev  # noqa: PLR0124
    assert rv == rv  # noqa: PLR0124
    assert rv2 == rv2  # noqa: PLR0124
    assert ev != rv
    assert rv2 == rv3
    rv2.assign(2)
    assert rv2 != rv3
    rv3.assign(10)
    assert rv2 != rv3
    rv2.assign(10)
    assert rv2 == rv3
    rv.assign(10)
    assert rv2 != rv


def test_opaque_value_clear() -> None:
    ov = OpaqueValue(Opaque())
    assert not ov.initialized
    ov.assign(b"")
    assert ov.initialized
    ov.clear()  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598
    assert not ov.initialized


def test_invalid_value() -> None:
    class TestType(Type):
        pass

    t = TestType("Test::Type")
    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: cannot construct unknown type: TestType$",
    ):
        TypeValue.construct(t)


def test_sequence_messages(
    message_sequence_value: MessageValue,
    sequence_message_package: Package,
) -> None:
    sequence_element_one = sequence_message_package.new_message("Sequence_Element")
    sequence_element_one.set("Byte", 5)
    sequence_element_two = sequence_message_package.new_message("Sequence_Element")
    sequence_element_two.set("Byte", 6)
    sequence = [sequence_element_one, sequence_element_two]
    message_sequence_value.set("Length", 2)
    message_sequence_value.set("Sequence_Field", sequence)
    assert message_sequence_value.valid_message
    assert message_sequence_value.bytestring == b"\x02\x05\x06"


@pytest.fixture(name="sequence_type_package", scope="session")
def fixture_sequence_type_package(pyrflx_: PyRFLX) -> Package:
    return pyrflx_.package("Sequence_Type")


@pytest.fixture(name="sequence_type_foo_value")
def fixture_sequence_type_foo_value(sequence_type_package: Package) -> MessageValue:
    return sequence_type_package.new_message("Foo")


def test_sequence_scalars(sequence_type_foo_value: MessageValue) -> None:
    a = IntegerValue(
        Integer("Sequence_Type::Byte_One", expr.Number(0), expr.Number(255), expr.Number(8)),
    )
    b = IntegerValue(
        Integer("Sequence_Type::Byte_Two", expr.Number(0), expr.Number(255), expr.Number(8)),
    )
    c = IntegerValue(
        Integer("Sequence_Type::Byte_Three", expr.Number(0), expr.Number(255), expr.Number(8)),
    )
    a.assign(5)
    b.assign(6)
    c.assign(7)
    byte_sequence = [a, b, c]
    sequence_type_foo_value.set("Length", 3)
    sequence_type_foo_value.set("Bytes", byte_sequence)
    assert sequence_type_foo_value.valid_message
    assert sequence_type_foo_value.bytestring == b"\x03\x05\x06\x07"


def test_sequence_preserve_value(enum_value: EnumValue) -> None:
    intval = IntegerValue(Integer("Test::Int", expr.Number(0), expr.Number(255), expr.Number(8)))
    intval.assign(1)
    enum_value.assign("One")
    type_sequence = SequenceValue(
        Sequence(
            "Test::Sequence",
            Integer("Test::Mod_Int", expr.Number(0), expr.Number(255), expr.Number(8)),
        ),
    )
    type_sequence.assign([intval])
    assert type_sequence.value == [intval]
    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: cannot assign EnumValue to an sequence of Integer$",
    ):
        type_sequence.assign([enum_value])
    assert type_sequence.value == [intval]


def test_sequence_parse_from_bytes(
    message_sequence_value: MessageValue,
    sequence_type_foo_value: MessageValue,
) -> None:
    message_sequence_value.parse(b"\x02\x05\x06")
    assert message_sequence_value.bytestring == b"\x02\x05\x06"
    sequence_type_foo_value.parse(b"\x03\x05\x06\x07")
    assert sequence_type_foo_value.bytestring == b"\x03\x05\x06\x07"
    sequence_message_value = message_sequence_value.clone()
    sequence_message_value.parse(b"\x02\x05\x06")
    assert sequence_message_value.bytestring == b"\x02\x05\x06"


def test_sequence_assign_invalid(
    tlv_message_value: MessageValue,
    ethernet_frame_value: MessageValue,
    sequence_type_foo_value: MessageValue,
    enum_value: EnumValue,
) -> None:
    type_sequence = SequenceValue(
        Sequence(
            "Test::Sequence",
            Integer("Test::Mod_Int", expr.Number(0), expr.Number(255), expr.Number(8)),
        ),
    )
    msg_sequence = SequenceValue(Sequence("Test::MsgSequence", tlv_message_value._type))

    intval = IntegerValue(Integer("Test::Int", expr.Number(0), expr.Number(255), expr.Number(8)))
    enum_value.assign("One")
    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: cannot assign EnumValue to an sequence of Integer$",
    ):
        type_sequence.assign([enum_value])

    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            'pyrflx: error: cannot assign message "Message" to sequence of messages: '
            "all messages must be valid"
            "$"
        ),
    ):
        msg_sequence.assign([tlv_message_value])

    with pytest.raises(
        PyRFLXError,
        match="^pyrflx: error: cannot assign EnumValue to an sequence of Message$",
    ):
        msg_sequence.assign([enum_value])

    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 4)
    tlv_message_value.set("Value", b"\x00\x00\x00\x00")

    ethernet_frame_value.set("Destination", 0)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 47)
    ethernet_frame_value.set("Type_Length", 1537)
    ethernet_frame_value.set("Payload", bytes(46))

    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: cannot assign "Frame" to an sequence of "Message"$',
    ):
        msg_sequence.assign([tlv_message_value, ethernet_frame_value])

    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot parse nested messages in sequence of type TLV::Message\n"
            "pyrflx: error: cannot set value for field Tag\n"
            "pyrflx: error: Number 0 is not a valid enum value"
            "$"
        ),
    ):
        msg_sequence.parse(Bitstring("00000000000000"))

    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value._fields["Length"].typeval.assign(111111111111111, check=False)
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            'pyrflx: error: cannot assign message "Message" to sequence of messages:'
            " all messages must be valid"
            "$"
        ),
    ):
        msg_sequence.assign([tlv_message_value])
    assert msg_sequence.value == []

    intval.assign(5)
    sequence_type_foo_value.set("Length", 42)
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set value for field Bytes\n"
            "pyrflx: error: invalid data size: input size is 8 while expected input size is 336"
            "$"
        ),
    ):
        sequence_type_foo_value.set("Bytes", [intval])


def icmp_checksum_function(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
    if first_arg is None:
        first_arg = kwargs.get("Message'First .. Checksum'First - 1")
    assert isinstance(first_arg, tuple)
    tag_first, checksum_first_minus_one = first_arg
    assert tag_first == 0
    assert checksum_first_minus_one == 15
    second_arg = kwargs.get("Checksum'Last + 1 .. Message'Last")
    assert isinstance(second_arg, tuple)
    checksum_last_plus_one, data_last = second_arg
    assert checksum_last_plus_one == 32
    assert data_last == 511
    checksum_size = kwargs.get("Checksum'Size")
    assert isinstance(checksum_size, int)
    assert checksum_size == 16

    checksum_bytes = message[tag_first : (checksum_first_minus_one + 1) // 8]
    checksum_bytes += b"\x00" * (checksum_size // 8)
    checksum_bytes += message[(checksum_last_plus_one // 8) : (data_last + 1) // 8]
    return utils.internet_checksum(checksum_bytes)


def test_checksum_field_not_defined(icmp_checksum_message_value: MessageValue) -> None:
    with pytest.raises(
        PyRFLXError,
        match=(
            "^pyrflx: error: cannot set checksum function: field NonExistingField is not defined$"
        ),
    ):
        icmp_checksum_message_value.set_checksum_function(
            {"NonExistingField": icmp_checksum_function},
        )

    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot set checksum function: field Tag has not been defined as "
            "a checksum field"
            "$"
        ),
    ):
        icmp_checksum_message_value.set_checksum_function({"Tag": icmp_checksum_function})


def test_checksum_function_not_set(icmp_checksum_message_value: MessageValue) -> None:
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "pyrflx: error: cannot calculate checksum for Checksum: "
            "no callable checksum function provided"
            "$"
        ),
    ):
        icmp_checksum_message_value.set("Data", b"\x00")


def test_checksum_manual(icmp_checksum_message_value: MessageValue) -> None:
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Checksum", 1234)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    assert not icmp_checksum_message_value.valid_message
    assert icmp_checksum_message_value.get("Checksum") == 1234
    assert_bytestring_error(icmp_checksum_message_value, icmp_checksum_message_value.identifier)
    icmp_checksum_message_value.set("Checksum", 12824)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    assert icmp_checksum_message_value.valid_message
    assert icmp_checksum_message_value.get("Checksum") == 12824  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598
    assert icmp_checksum_message_value.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data


def test_checksum_auto(icmp_checksum_message_value: MessageValue) -> None:
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    assert icmp_checksum_message_value.get("Checksum") == 12824
    assert icmp_checksum_message_value.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data
    assert icmp_checksum_message_value.valid_message


def test_checksum_auto_change_field(icmp_checksum_message_value: MessageValue) -> None:
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    assert icmp_checksum_message_value.get("Checksum") == 12824
    assert icmp_checksum_message_value.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data
    assert icmp_checksum_message_value.valid_message
    icmp_checksum_message_value.set("Sequence_Number", 2)
    icmp_checksum_message_value.set("Data", test_data)
    assert icmp_checksum_message_value.get("Checksum") == 12823
    assert icmp_checksum_message_value.bytestring == b"\x08\x00\x32\x17\x00\x05\x00\x02" + test_data
    assert icmp_checksum_message_value.valid_message


def test_checksum_parse(icmp_checksum_message_value: MessageValue) -> None:
    test_data = (
        b"\x08\x00\x32\x18\x00\x05\x00\x01\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.parse(test_data)
    assert icmp_checksum_message_value.valid_message


def test_checksum_parse_invalid(icmp_checksum_message_value: MessageValue) -> None:
    test_data = (
        b"\x08\x00\x35\x19\x00\x05\x00\x01\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.parse(test_data)
    assert not icmp_checksum_message_value.valid_message


def test_checksum_parse_invalid_tlv(tlv_checksum_package: Package) -> None:
    tlv_checksum_message = tlv_checksum_package.new_message("Message")
    tlv_checksum_message.set_checksum_function({"Checksum": checksum_function_255})
    tlv_checksum_message.parse(b"\x01\x00\x02\xAB\xCD\x00\x00\x00\xF1")
    assert not tlv_checksum_message.valid_message


def test_checksum_message_first(icmp_checksum_message_first: MessageValue) -> None:
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_message_first.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_first.set("Tag", "Echo_Request")
    icmp_checksum_message_first.set("Code_Zero", 0)
    icmp_checksum_message_first.set("Identifier", 5)
    icmp_checksum_message_first.set("Sequence_Number", 1)
    icmp_checksum_message_first.set("Data", test_data)
    assert icmp_checksum_message_first.get("Checksum") == 12824
    assert icmp_checksum_message_first.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data
    assert icmp_checksum_message_first.valid_message


def test_checksum_no_verification() -> None:
    pyrflx_ = PyRFLX.from_specs(
        [SPEC_DIR / "icmp.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    icmp_message = pyrflx_.package("ICMP").new_message("Message")._type
    icmp_msg = MessageValue(
        icmp_message.copy(
            structure=[
                Link(
                    l.source,
                    l.target,
                    condition=expr.And(l.condition, expr.ValidChecksum("Checksum")),
                )
                if l.target == FINAL
                else l
                for l in icmp_message.structure
            ],
            checksums={
                ID("Checksum"): [
                    expr.ValueRange(
                        expr.First("Message"),
                        expr.Sub(expr.First("Checksum"), expr.Number(1)),
                    ),
                    expr.Size("Checksum"),
                    expr.ValueRange(
                        expr.Add(expr.Last("Checksum"), expr.Number(1)),
                        expr.Last("Message"),
                    ),
                ],
            },
        ),
    )
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_msg.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_msg.set("Tag", "Echo_Request")
    icmp_msg.set("Code_Zero", 0)
    icmp_msg.set("Identifier", 5)
    icmp_msg.set("Sequence_Number", 1)
    icmp_msg.set("Data", test_data)
    icmp_msg.update_checksums()
    assert icmp_msg.get("Checksum") == 12824
    assert icmp_msg.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data
    assert icmp_msg.valid_message


@pytest.fixture(name="tlv_checksum_package", scope="session")
def fixture_tlv_checksum_package(pyrflx_: PyRFLX) -> Package:
    return pyrflx_.package("TLV_With_Checksum")


@pytest.fixture(name="tlv_checksum_message")
def fixture_tlv_checksum_message(tlv_checksum_package: Package) -> Message:
    return tlv_checksum_package.new_message("Message")._type


def test_checksum_is_checksum_settable(tlv_checksum_message: Message) -> None:
    tlv_msg = MessageValue(tlv_checksum_message)
    tlv_msg.set_checksum_function({"Checksum": lambda message, **kwargs: 0})  # noqa: ARG005
    assert not tlv_msg._is_checksum_settable(tlv_msg._checksums["Checksum"])
    tlv_msg.set("Tag", "Msg_Data")
    assert not tlv_msg._is_checksum_settable(tlv_msg._checksums["Checksum"])
    tlv_msg.set("Length", 5)
    assert not tlv_msg._is_checksum_settable(tlv_msg._checksums["Checksum"])
    tlv_msg.set("Value", bytes(5))
    assert tlv_msg._is_checksum_settable(tlv_msg._checksums["Checksum"])
    tlv_msg.set("Checksum", 0)
    assert tlv_msg._is_checksum_settable(tlv_msg._checksums["Checksum"])


@pytest.fixture(name="no_conditionals_package", scope="session")
def fixture_no_conditionals_package(pyrflx_: PyRFLX) -> Package:
    return pyrflx_.package("No_Conditionals")


@pytest.fixture(name="no_conditionals_message")
def fixture_no_conditionals_message(no_conditionals_package: Package) -> Message:
    return no_conditionals_package.new_message("Message")._type


def test_checksum_value_range(no_conditionals_message: Message) -> None:
    message = no_conditionals_message.copy(
        structure=[
            Link(l.source, l.target, condition=expr.ValidChecksum("Checksum"))
            if l.target == FINAL
            else l
            for l in no_conditionals_message.structure
        ],
        checksums={ID("Checksum"): [expr.ValueRange(expr.First("Tag"), expr.Last("Data"))]},
    )
    msg = MessageValue(message)
    msg.set("Tag", 0)
    msg.set("Data", 0)
    assert not msg._is_checksum_settable(msg._checksums["Checksum"])


def checksum_function_zero(message: bytes, **_kwargs: object) -> int:  # noqa: ARG001
    return 0


def checksum_function_255(message: bytes, **_kwargs: object) -> int:  # noqa: ARG001
    return 0xFF


@pytest.fixture(name="pyrflx_checksum")
def fixture_prflx_checksum() -> PyRFLX:
    return PyRFLX.from_specs(
        [SPEC_DIR / "refinement_with_checksum.rflx", SPEC_DIR / "tlv_with_checksum.rflx"],
    )


def test_refinement_with_checksum(
    pyrflx_checksum: PyRFLX,
) -> None:
    refinement_package = pyrflx_checksum.package("Refinement_With_Checksum")
    tlv_package = pyrflx_checksum.package("TLV_With_Checksum")
    refinement_package.set_checksum_functions({"Message": {"Checksum": checksum_function_255}})
    tlv_package.set_checksum_functions({"Message": {"Checksum": checksum_function_zero}})
    data = b"\x09\xff\x01\x00\x02\x01\x02\x00\x00\x00\x00"
    message = refinement_package.new_message("Message")
    message.set_checksum_function({"Checksum": checksum_function_255})
    message.parse(data)
    assert message.valid_message
    tlv_message = message.get("Payload")
    assert isinstance(tlv_message, MessageValue)
    assert tlv_message.valid_message


def test_set_checksum_to_pyrflx(
    pyrflx_checksum: PyRFLX,
) -> None:
    pyrflx_checksum.set_checksum_functions(
        {
            "Refinement_With_Checksum::Message": {"Checksum": checksum_function_255},
            "TLV_With_Checksum::Message": {"Checksum": checksum_function_zero},
        },
    )
    refinement_with_checksum_msg = pyrflx_checksum.package("Refinement_With_Checksum").new_message(
        "Message",
    )
    tlv_with_checksum_msg = pyrflx_checksum.package("TLV_With_Checksum").new_message("Message")
    refinement_checksum_function = refinement_with_checksum_msg._checksums["Checksum"].function
    tlv_checksum_function = tlv_with_checksum_msg._checksums["Checksum"].function
    assert callable(refinement_checksum_function)
    assert callable(tlv_checksum_function)
    # https://github.com/python/mypy/issues/10976
    assert (
        refinement_checksum_function.__name__  # type: ignore[attr-defined]
        == checksum_function_255.__name__
    )
    assert (
        tlv_checksum_function.__name__  # type: ignore[attr-defined]
        == checksum_function_zero.__name__
    )


def test_set_checksum_to_pyrflx_invalid_id(
    pyrflx_checksum: PyRFLX,
) -> None:
    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: "Refinement_With_Checksum:Message" is not a valid identifier:'
        ' id: error: ":" in identifier parts of "Refinement_With_Checksum:Message"$',
    ):
        pyrflx_checksum.set_checksum_functions(
            {
                "Refinement_With_Checksum:Message": {"Checksum": checksum_function_255},
                "TLV_With_Checksum::Message": {"Checksum": checksum_function_zero},
            },
        )

    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: "Not_A_Package" is not a valid identifier$',
    ):
        pyrflx_checksum.set_checksum_functions(
            {
                "Not_A_Package": {"Checksum": checksum_function_255},
                "TLV_With_Checksum::Message": {"Checksum": checksum_function_zero},
            },
        )


def test_set_checksum_to_pyrflx_package_not_found(
    pyrflx_checksum: PyRFLX,
) -> None:
    pyrflx_checksum.set_checksum_functions(
        {
            "Not_A_Package::Not_A_Message": {"Checksum": checksum_function_255},
        },
    )


def test_set_checksum_to_pyrflx_message_not_found(
    pyrflx_checksum: PyRFLX,
) -> None:
    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: "Not_A_Message" is not a message in TLV_With_Checksum$',
    ):
        pyrflx_checksum.set_checksum_functions(
            {
                "TLV_With_Checksum::Not_A_Message": {"Checksum": checksum_function_255},
            },
        )


@pytest.mark.parametrize(
    ("data", "f1", "f2", "f3"),
    [
        (b"\x00\x00\x00\x00", 0, 0, 0),
        (b"\x11\x11\x11\x11", 0x8, 0x44, 0x11111),
        (b"\x11\x22\x33\x44", 0x8, 0x48, 0x23344),
        (b"\xff\xff\xff\xff", 0x7F, 0x7F, 0x3FFFF),
    ],
)
def test_unaligned_field_serialization(data: bytes, f1: int, f2: int, f3: int) -> None:
    msg = (
        PyRFLX.from_specs([SPEC_DIR / "unaligned_field.rflx"])
        .package("Unaligned_Field")
        .new_message("M1")
    )
    msg.parse(data)
    assert msg.get("F1") == f1
    assert msg.get("F2") == f2
    assert msg.get("F3") == f3
    assert data == msg.bytestring


def test_message_size(message_size_value: MessageValue) -> None:
    message_size_value.parse(b"\x02\x01\x02")

    assert message_size_value.valid_message
    assert message_size_value.get("A") == 2
    assert message_size_value.get("B") == b"\x01\x02"

    message_size_value.set("A", 2)
    message_size_value.set("B", b"\x01\x02")
    # Eng/RecordFlux/RecordFlux#422
    # Serialization of optional fields dependent of message size not supported.
    # message_size_value.set("C", 3)

    assert message_size_value.valid_message

    message_size_value.parse(b"\x02\x01\x02\x03")

    assert message_size_value.valid_message
    assert message_size_value.get("A") == 2
    assert message_size_value.get("B") == b"\x01\x02"
    assert message_size_value.get("C") == 3


def test_message_size_unverified() -> None:
    pyrflx_ = PyRFLX.from_specs(
        [SPEC_DIR / "message_size.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    message = pyrflx_.package("Message_Size").new_message("M")
    message.set("A", 2)
    message.set("B", b"\x01\x02")
    assert message.valid_message


def test_message_type_size_condition(message_type_size_value: MessageValue) -> None:
    message_type_size_value.parse(b"\x08\x02")
    assert message_type_size_value.valid_message
    assert message_type_size_value.get("F1") == 8
    assert message_type_size_value.get("F2") == 2


def test_always_valid_aspect(
    message_always_valid_aspect_value: MessageValue,
) -> None:
    message_always_valid_aspect_value.parse(b"\x04\x25\xAB")
    assert message_always_valid_aspect_value.valid_message
    assert message_always_valid_aspect_value.get("F1") == "RFLX_UNKNOWN_ENUM_1"
    assert message_always_valid_aspect_value.get("F2") == "RFLX_UNKNOWN_ENUM_2"
    assert message_always_valid_aspect_value.get("F3") == 171


def test_get_inner_messages(
    sequence_message_package: Package,
    message_sequence_refinement_value: MessageValue,
) -> None:
    sequence_element_one = sequence_message_package.new_message("Sequence_Element")
    sequence_element_one.set("Byte", 5)
    sequence_element_two = sequence_message_package.new_message("Sequence_Element")
    sequence_element_two.set("Byte", 6)
    sequence_element_three = sequence_message_package.new_message("Sequence_Element")
    sequence_element_three.set("Byte", 7)

    sequence_contents: abc.Sequence[TypeValue] = [sequence_element_one, sequence_element_two]
    message_sequence_refinement_value.set("Length", 2)
    message_sequence_refinement_value.set("Sequence_Field", sequence_contents)
    message_sequence_refinement_value.set("Payload", sequence_element_three.bytestring)
    assert message_sequence_refinement_value.valid_message

    inner_messages = message_sequence_refinement_value.inner_messages()
    assert len(inner_messages) == 3
    assert all(isinstance(m, MessageValue) for m in inner_messages)
    assert {m.get("Byte") for m in inner_messages} == {5, 6, 7}


def test_get_path(icmp_message_value: MessageValue) -> None:
    test_bytes = (
        b"\x08\x00\xe1\x1e\x00\x11\x00\x01\x4a\xfc\x0d\x00\x00\x00\x00\x00"
        b"\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
        b"\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f"
        b"\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_message_value.parse(test_bytes)
    path = icmp_message_value.path
    field_pairs = [
        ("Initial", "Tag"),
        ("Tag", "Code_Zero"),
        ("Code_Zero", "Checksum"),
        ("Checksum", "Identifier"),
        ("Identifier", "Sequence_Number"),
        ("Sequence_Number", "Data"),
        ("Data", "Final"),
    ]
    assert [(l.source.name, l.target.name) for l in path] == field_pairs


def test_get_model(icmp_message_value: MessageValue) -> None:
    assert isinstance(icmp_message_value.model, Message)


def test_parameterized_message(parameterized_package: Package) -> None:
    message = parameterized_package.new_message(
        "Message",
        {
            "Length": 8,
            "Tag_Mode": "Without_Tag",
            "Tag_Value": "Parameterized::Tag_A",
            "Use_Tag": True,
        },
    )
    assert message.fields == ["Payload", "Tag"]
    assert message.required_fields == ["Payload"]
    message.set("Payload", bytes(8))
    assert message.required_fields == []
    assert message.valid_message
    assert message.bytestring == bytes(8)


def test_parameterized_message_no_verification() -> None:
    pyrflx_ = PyRFLX.from_specs(
        [SPEC_DIR / "parameterized.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    message_unv = pyrflx_.package("Parameterized").new_message(
        "Message",
        {
            "Length": 8,
            "Tag_Mode": "Parameterized::Without_Tag",
            "Tag_Value": "Tag_A",
            "Use_Tag": True,
        },
    )
    assert message_unv.fields == ["Payload", "Tag"]
    message_unv.set("Payload", bytes(8))
    assert message_unv.valid_message
    assert message_unv.bytestring == bytes(8)


def test_parameterized_message_unsupported_type(parameterized_package: Package) -> None:
    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: message argument for "Length" has unsupported type "bytes"$',
    ):
        parameterized_package.new_message(
            "Message",
            {
                "Length": bytes(8),  # type: ignore[dict-item]
                "Tag_Mode": "Without_Tag",
                "Tag_Value": "Parameterized::Tag_A",
                "Use_Tag": True,
            },
        )


def test_parameterized_message_invalid_type(parameterized_package: Package) -> None:
    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: message argument for "Tag_Mode" has invalid type "int"$',
    ):
        parameterized_package.new_message(
            "Message",
            {
                "Length": 8,
                "Tag_Mode": 2,
                "Tag_Value": "Parameterized::Tag_A",
                "Use_Tag": True,
            },
        )
    with pytest.raises(
        PyRFLXError,
        match='^pyrflx: error: message argument for "Use_Tag" has invalid type "str"$',
    ):
        parameterized_package.new_message(
            "Message",
            {
                "Length": 8,
                "Tag_Mode": "Without_Tag",
                "Tag_Value": "Parameterized::Tag_A",
                "Use_Tag": "True",
            },
        )


def test_json_serialization() -> None:
    integer_value = IntegerValue(
        Integer("Test::Int", expr.Number(0), expr.Number(255), expr.Number(8)),
    )
    integer_value.assign(128)
    assert integer_value.as_json() == 128

    enum_value = EnumValue(
        Enumeration(
            "Test::Enum",
            [("One", expr.Number(1)), ("Two", expr.Number(2))],
            expr.Number(8),
            always_valid=False,
        ),
    )
    enum_value.assign("Two")
    assert enum_value.as_json() == ("Test::Two", 2)

    sequence_value = SequenceValue(
        Sequence(
            "Test::Sequence",
            Integer("Test::Int", expr.Number(0), expr.Number(255), expr.Number(8)),
        ),
    )
    sequence_value.assign([integer_value, integer_value, integer_value])
    assert sequence_value.as_json() == [128, 128, 128]

    opaque_value = OpaqueValue(Opaque())
    opaque_value.assign(b"RecordFlux")
    assert opaque_value.as_json() == b"RecordFlux"


def test_message_endianness_parse_be(endianness_package: Package) -> None:
    message = endianness_package.new_message("Message")

    message.parse(b"\x00\x01")

    assert message.valid_message
    assert message.get("Tag") == "Endianness::None"

    message.parse(b"\x00\x02\x00\x04\x01\x02\x03\x04")

    assert message.valid_message
    assert message.get("Tag") == "Endianness::Data"
    assert message.get("Length") == 4
    assert message.get("Payload") == b"\x01\x02\x03\x04"


def test_message_endianness_parse_le(endianness_package: Package) -> None:
    message_le = endianness_package.new_message("Message_LE")

    message_le.parse(b"\x01\x00")

    assert message_le.valid_message
    assert message_le.get("Tag") == "Endianness::None"

    message_le.parse(b"\x02\x00\x04\x00\x01\x02\x03\x04")

    assert message_le.valid_message
    assert message_le.get("Tag") == "Endianness::Data"
    assert message_le.get("Length") == 4
    assert message_le.get("Payload") == b"\x01\x02\x03\x04"


def test_message_endianness_set_be(endianness_package: Package) -> None:
    message = endianness_package.new_message("Message")

    message.set("Tag", "None")

    assert message.valid_message
    assert message.bytestring == b"\x00\x01"

    message.set("Tag", "Data")
    message.set("Length", 4)
    message.set("Payload", b"\x01\x02\x03\x04")

    assert message.valid_message
    assert message.bytestring == b"\x00\x02\x00\x04\x01\x02\x03\x04"


def test_message_endianness_set_le(endianness_package: Package) -> None:
    message_le = endianness_package.new_message("Message_LE")

    message_le.set("Tag", "None")

    assert message_le.valid_message
    assert message_le.bytestring == b"\x01\x00"

    message_le.set("Tag", "Data")
    message_le.set("Length", 4)
    message_le.set("Payload", b"\x01\x02\x03\x04")

    assert message_le.valid_message
    assert message_le.bytestring == b"\x02\x00\x04\x00\x01\x02\x03\x04"


def test_low_order(low_order_package: Package) -> None:
    m1 = low_order_package.new_message("M1")
    m1.set("R1", 0)
    m1.set("F1", "True")
    m1.set("F2", b"\x00")

    assert m1.valid_message
    assert m1.bytestring == b"\x01\x00"


def test_aggregate_in_relation_valid(aggregate_in_relation_package: Package) -> None:
    msg = aggregate_in_relation_package.new_message("Aggregate_In_Relation_Msg")
    msg.parse(b"\xAA\xAA\xBB\xCC\xCC\xAA\xAA")
    assert msg.bytestring == b"\xAA\xAA\xBB\xCC\xCC\xAA\xAA"
    assert msg.valid_message
