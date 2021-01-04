# pylint: disable=too-many-lines
from pathlib import Path

import pytest

from rflx import expression as expr
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    Array,
    Enumeration,
    Link,
    Message,
    ModularInteger,
    Opaque,
    RangeInteger,
    Type,
)
from rflx.pyrflx import (
    ArrayValue,
    Bitstring,
    EnumValue,
    IntegerValue,
    MessageValue,
    NotInitializedError,
    OpaqueValue,
    Package,
    PyRFLX,
    TypeValue,
    utils,
)
from tests.const import EX_SPEC_DIR, SPEC_DIR


def assert_bytestring_error(msg: MessageValue) -> None:
    with pytest.raises(RuntimeError, match="cannot create bytestring of invalid message"):
        # pylint: disable=pointless-statement
        msg.bytestring


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
        "Ethernet",
        "ICMP",
        "IPv4",
        "TLS_Alert",
        "TLS_Record",
        "TLV",
        "UDP",
        "Array_Message",
        "Array_Type",
        "Null_Message",
        "TLV_With_Checksum",
        "No_Conditionals",
    }


def test_attributes(pyrflx_: PyRFLX) -> None:
    pyrflx_ = PyRFLX.from_specs([f"{SPEC_DIR}/tlv.rflx"])
    assert isinstance(pyrflx_["TLV"], Package)
    tlv_package = pyrflx_["TLV"]
    assert isinstance(tlv_package["Message"], MessageValue)


def test_no_verification(icmp_message_value: MessageValue) -> None:
    pyrflx_ = PyRFLX.from_specs(
        [f"{EX_SPEC_DIR}/icmp.rflx"], skip_model_verification=True, skip_message_verification=True
    )
    icmp_message_value_unv = pyrflx_["ICMP"]["Message"]
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
    m1 = tlv_package["Message"]
    assert m1 == tlv_package["Message"]
    assert m1 is not tlv_package["Message"]
    assert tlv_package["Message"] == tlv_package["Message"]
    assert tlv_package["Message"] is not tlv_package["Message"]
    assert m1 is not None


def test_message_value_bitstring(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.bitstring == Bitstring("")
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.bitstring == Bitstring("01")
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.bitstring == Bitstring("0100000000000001")
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.bitstring == Bitstring("010000000000000100000001")


def test_message_value_all_fields(
    tlv_message_value: MessageValue, ethernet_frame_value: MessageValue
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
    tlv_message_value: MessageValue, ethernet_frame_value: MessageValue
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
    assert_bytestring_error(tlv_message_value)
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\xc0"
    tlv_message_value.set("Tag", "Msg_Data")
    assert not tlv_message_value.valid_message
    assert_bytestring_error(tlv_message_value)
    tlv_message_value.set("Length", 1)
    assert not tlv_message_value.valid_message
    assert_bytestring_error(tlv_message_value)
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\x40\x01\x01"


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
        ValueError,
        match=(
            "^"
            "Error while setting value for field Value:"
            " invalid data size: input size is 80 while expected input size is 64"
            "$"
        ),
    ):
        tlv_message_value.set("Value", v2)


def test_message_value_generate(tlv_message_value: MessageValue) -> None:
    test_message_value_payload = b"\x01\x02\x03\x04\x05\x06\x07\x08"
    test_message_value_data = b"\x40\x08" + test_message_value_payload
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
    assert tlv_message_value.bytestring == b"\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00"


def test_message_value_set_get_value(tlv_message_value: MessageValue) -> None:
    v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 8)
    tlv_message_value.set("Value", v1)
    assert tlv_message_value.get("Tag") == "Msg_Data"
    assert tlv_message_value.get("Length") == 8
    assert tlv_message_value.get("Value") == v1


def test_message_value_get_invalid_field(tlv_message_value: MessageValue) -> None:
    with pytest.raises(ValueError, match=r"field nofield not valid"):
        tlv_message_value.get("nofield")


def test_message_value_set_invalid_field(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(KeyError, match=r"cannot access field Value"):
        tlv_message_value.set("Value", b"")
    with pytest.raises(KeyError, match=r"cannot access field Checksum"):
        tlv_message_value.set("Checksum", 8)
    tlv_message_value.set("Tag", "Msg_Error")
    with pytest.raises(KeyError, match=r"cannot access field Length"):
        tlv_message_value.set("Length", 8)


def test_message_value_invalid_value(tlv_message_value: MessageValue) -> None:
    with pytest.raises(
        ValueError,
        match="Error while setting value for field Tag: "
        "cannot assign different types: str != int",
    ):
        tlv_message_value.set("Tag", 1)
    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(
        ValueError,
        match="Error while setting value for field Length: "
        "cannot assign different types: int != str",
    ):
        tlv_message_value.set("Length", "blubb")


def test_message_value_next(tlv_message_value: MessageValue) -> None:
    # pylint: disable=protected-access
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value._next_field(INITIAL.name) == "Tag"
    assert tlv_message_value._next_field("Tag") == "Length"
    assert tlv_message_value._next_field(FINAL.name) == ""


def test_message_value_prev(tlv_message_value: MessageValue) -> None:
    # pylint: disable=protected-access
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
    assert tlv_message_value.bytestring == b"\x40\x00"


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
    tlv_message_value: MessageValue, ethernet_frame_value: MessageValue
) -> None:
    # pylint: disable=protected-access
    assert not tlv_message_value._is_valid_opaque_field("Value")
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 1000)
    assert tlv_message_value._is_valid_opaque_field("Value")
    ethernet_frame_value.set("Destination", 2 ** 48 - 1)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 1501)
    assert not ethernet_frame_value._is_valid_opaque_field("Payload")
    ethernet_frame_value.set("Type_Length_TPID", 1500)
    ethernet_frame_value.set("Type_Length", 1500)
    assert ethernet_frame_value._is_valid_opaque_field("Payload")


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
        ValueError,
        match=(
            "Error while setting value for field Payload: "
            "Error while parsing nested message IPv4::Packet: "
            "Bitstring representing the message is too short - "
            "stopped while parsing field: Payload"
        ),
    ):
        ethernet_frame_value.parse(incorrect_message)


def test_message_value_parse_from_bitstring(
    tlv_message_value: MessageValue, enum_value: EnumValue
) -> None:
    # pylint: disable=protected-access
    intval = IntegerValue(ModularInteger("Test::Int", expr.Number(256)))
    intval.parse(b"\x02")
    assert intval.value == 2
    enum_value.parse(b"\x01")
    assert enum_value.value == "One"
    msg_array = ArrayValue(Array("Test::MsgArray", tlv_message_value._type))
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 4)
    tlv_message_value.set("Value", b"\x00\x00\x00\x00")
    msg_array.parse(tlv_message_value.bytestring)


def test_message_value_parse_from_bitstring_invalid(tlv_message_value: MessageValue) -> None:
    with pytest.raises(ValueError, match="Bitstring does not consist of only 0 and 1"):
        Bitstring("123")
    assert Bitstring("01") + Bitstring("00") == Bitstring("0100")

    test_bytes = b"\x40"
    with pytest.raises(
        IndexError,
        match=(
            "Bitstring representing the message is too short"
            " - stopped while parsing field: Length"
        ),
    ):
        tlv_message_value.parse(test_bytes)
    assert not tlv_message_value.valid_message


def test_message_value_set_invalid(ethernet_frame_value: MessageValue) -> None:
    ethernet_frame_value.set("Destination", 2 ** 48 - 1)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 1501)
    with pytest.raises(
        ValueError,
        match=r"none of the field conditions .* for field Type_Length"
        " have been met by the assigned value: 1501",
    ):
        ethernet_frame_value.set("Type_Length", 1501)


def test_integer_value_mod() -> None:
    # pylint: disable=pointless-statement
    modtype = ModularInteger("Test::Int", expr.Number(2 ** 16))
    modvalue = IntegerValue(modtype)
    assert not modvalue.initialized
    with pytest.raises(NotInitializedError, match="value not initialized"):
        modvalue.value
    with pytest.raises(NotInitializedError, match="value not initialized"):
        modvalue.expr
    modvalue.assign(128)
    assert modvalue.initialized
    assert modvalue.value == 128
    assert str(modvalue.bitstring) == "0000000010000000"
    with pytest.raises(ValueError, match=r"value 65536 not in type range 0 .. 65535"):
        modvalue.assign(2 ** 16)
    with pytest.raises(ValueError, match=r"value -1 not in type range 0 .. 65535"):
        modvalue.assign(-1)


def test_integer_value_range() -> None:
    # pylint: disable=pointless-statement
    rangetype = RangeInteger("Test::Int", expr.Number(8), expr.Number(16), expr.Number(8))
    rangevalue = IntegerValue(rangetype)
    assert not rangevalue.initialized
    with pytest.raises(NotInitializedError, match="value not initialized"):
        rangevalue.value
    with pytest.raises(NotInitializedError, match="value not initialized"):
        rangevalue.expr
    rangevalue.assign(10)
    assert rangevalue.initialized
    assert rangevalue.value == 10
    assert str(rangevalue.bitstring) == "00001010"
    with pytest.raises(ValueError, match=r"value 17 not in type range 8 .. 16"):
        rangevalue.assign(17)
    with pytest.raises(ValueError, match=r"value 7 not in type range 8 .. 16"):
        rangevalue.assign(7)


@pytest.fixture(name="enum_value")
def fixture_enum_value() -> EnumValue:
    return EnumValue(
        Enumeration(
            "Test::Enum", [("One", expr.Number(1)), ("Two", expr.Number(2))], expr.Number(8), False
        )
    )


def test_enum_value_literals(enum_value: EnumValue) -> None:
    assert enum_value.literals == {
        expr.Variable("One"): expr.Number(1),
        expr.Variable("Test::One"): expr.Number(1),
        expr.Variable("Two"): expr.Number(2),
        expr.Variable("Test::Two"): expr.Number(2),
    }
    assert not enum_value.initialized


def test_enum_value_assign(enum_value: EnumValue) -> None:
    with pytest.raises(NotInitializedError, match="value not initialized"):
        enum_value.value  # pylint: disable=pointless-statement
    with pytest.raises(NotInitializedError, match="value not initialized"):
        enum_value.expr  # pylint: disable=pointless-statement

    enum_value.assign("One")
    assert enum_value.initialized
    assert enum_value.value == "One"
    assert str(enum_value.bitstring) == "00000001"

    with pytest.raises(KeyError, match=r"Three is not a valid enum value"):
        enum_value.assign("Three")


def test_enum_value_parse(enum_value: EnumValue) -> None:
    enum_value.parse(b"\x01")
    assert enum_value.value == "One"

    with pytest.raises(KeyError, match=r"Number 15 is not a valid enum value"):
        enum_value.parse(Bitstring("1111"))


@pytest.fixture(name="enum_value_imported")
def fixture_enum_value_imported() -> EnumValue:
    return EnumValue(
        Enumeration(
            "Test::Enum", [("One", expr.Number(1)), ("Two", expr.Number(2))], expr.Number(8), False
        ),
        imported=True,
    )


def test_enum_value_imported(enum_value_imported: EnumValue) -> None:
    assert enum_value_imported.literals == {
        expr.Variable("Test::One"): expr.Number(1),
        expr.Variable("Test::Two"): expr.Number(2),
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
        expr.Variable("False"): expr.Number(0),
        expr.Variable("True"): expr.Number(1),
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
    # pylint: disable=pointless-statement
    # pylint: disable=protected-access
    opaquevalue = OpaqueValue(Opaque())
    assert not opaquevalue.initialized
    with pytest.raises(NotInitializedError, match="value not initialized"):
        opaquevalue.value
    opaquevalue.assign(b"\x01\x02")
    assert opaquevalue.initialized
    assert opaquevalue.value == b"\x01\x02"
    k = opaquevalue.size
    assert isinstance(k, expr.Number)
    assert k.value == 16
    assert str(opaquevalue.bitstring) == "0000000100000010"
    opaquevalue.parse(Bitstring("1111"))
    assert opaquevalue._value == b"\x0f"


def test_opaque_value_eq(enum_value: EnumValue) -> None:
    # pylint: disable=comparison-with-itself
    ov = OpaqueValue(Opaque())
    ev = enum_value
    rangetype = RangeInteger("Test::Int", expr.Number(8), expr.Number(16), expr.Number(8))
    rv = IntegerValue(rangetype)
    modtype = ModularInteger("Test::Int", expr.Number(2 ** 16))
    mv = IntegerValue(modtype)
    mv2 = IntegerValue(modtype)
    assert ov == ov
    assert ev == ev
    assert rv == rv
    assert mv == mv
    assert ev != rv
    assert mv == mv2
    mv.assign(2)
    assert mv != mv2
    mv2.assign(10)
    assert mv != mv2
    mv.assign(10)
    assert mv == mv2
    rv.assign(10)
    assert mv != rv


def test_opaque_value_clear() -> None:
    ov = OpaqueValue(Opaque())
    assert not ov.initialized
    ov.assign(b"")
    assert ov.initialized
    ov.clear()
    assert not ov.initialized


def test_invalid_value() -> None:
    class TestType(Type):
        pass

    t = TestType("Test::Type")
    with pytest.raises(ValueError, match="cannot construct unknown type: TestType"):
        TypeValue.construct(t)


def test_array_messages(array_message_package: Package, array_message_value: MessageValue) -> None:
    array_message_one = array_message_package["Foo"]
    array_message_one.set("Byte", 5)
    array_message_two = array_message_package["Foo"]
    array_message_two.set("Byte", 6)
    foos = [array_message_one, array_message_two]
    array_message_value.set("Length", 2)
    array_message_value.set("Bar", foos)
    assert array_message_value.valid_message
    assert array_message_value.bytestring == b"\x02\x05\x06"


@pytest.fixture(name="array_type_package", scope="session")
def fixture_array_type_package(pyrflx_: PyRFLX) -> Package:
    return pyrflx_["Array_Type"]


@pytest.fixture(name="array_type_foo_value")
def fixture_array_type_foo_value(array_type_package: Package) -> MessageValue:
    return array_type_package["Foo"]


def test_array_scalars(array_type_foo_value: MessageValue) -> None:
    a = IntegerValue(ModularInteger("Array_Type::Byte_One", expr.Number(256)))
    b = IntegerValue(ModularInteger("Array_Type::Byte_Two", expr.Number(256)))
    c = IntegerValue(ModularInteger("Array_Type::Byte_Three", expr.Number(256)))
    a.assign(5)
    b.assign(6)
    c.assign(7)
    byte_array = [a, b, c]
    array_type_foo_value.set("Length", 3)
    array_type_foo_value.set("Bytes", byte_array)
    assert array_type_foo_value.valid_message
    assert array_type_foo_value.bytestring == b"\x03\x05\x06\x07"


def test_array_preserve_value(enum_value: EnumValue) -> None:
    intval = IntegerValue(ModularInteger("Test::Int", expr.Number(256)))
    intval.assign(1)
    enum_value.assign("One")
    type_array = ArrayValue(Array("Test::Array", ModularInteger("Test::Mod_Int", expr.Number(256))))
    type_array.assign([intval])
    assert type_array.value == [intval]
    with pytest.raises(ValueError, match="cannot assign EnumValue to an array of ModularInteger"):
        type_array.assign([enum_value])
    assert type_array.value == [intval]


def test_array_parse_from_bytes(
    array_message_value: MessageValue, array_type_foo_value: MessageValue
) -> None:
    array_message_value.parse(b"\x02\x05\x06")
    assert array_message_value.bytestring == b"\x02\x05\x06"
    array_type_foo_value.parse(b"\x03\x05\x06\x07")
    assert array_type_foo_value.bytestring == b"\x03\x05\x06\x07"
    array_message_value = array_message_value.clone()
    array_message_value.parse(b"\x02\x05\x06", False)
    assert array_message_value.bytestring == b"\x02\x05\x06"


def test_array_assign_invalid(
    tlv_message_value: MessageValue,
    ethernet_frame_value: MessageValue,
    array_type_foo_value: MessageValue,
    enum_value: EnumValue,
) -> None:
    # pylint: disable=protected-access
    type_array = ArrayValue(Array("Test::Array", ModularInteger("Test::Mod_Int", expr.Number(256))))
    msg_array = ArrayValue(Array("Test::MsgArray", tlv_message_value._type))

    intval = IntegerValue(ModularInteger("Test::Int", expr.Number(256)))
    enum_value.assign("One")
    with pytest.raises(ValueError, match="cannot assign EnumValue to an array of ModularInteger"):
        type_array.assign([enum_value])

    tlv_message_value.set("Tag", "Msg_Data")
    with pytest.raises(
        ValueError,
        match='cannot assign message "Message" to array of messages: all messages must be valid',
    ):
        msg_array.assign([tlv_message_value])

    with pytest.raises(ValueError, match="cannot assign EnumValue to an array of Message"):
        msg_array.assign([enum_value])

    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 4)
    tlv_message_value.set("Value", b"\x00\x00\x00\x00")

    ethernet_frame_value.set("Destination", 0)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 47)
    ethernet_frame_value.set("Type_Length", 1537)
    ethernet_frame_value.set("Payload", bytes(46))

    with pytest.raises(ValueError, match='cannot assign "Frame" to an array of "Message"'):
        msg_array.assign([tlv_message_value, ethernet_frame_value])

    with pytest.raises(
        ValueError,
        match="cannot parse nested messages in array of type TLV::Message: Error while setting "
        "value for field Tag: 'Number 0 is not a valid enum value'",
    ):
        msg_array.parse(Bitstring("0001111"))

    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value._fields["Length"].typeval.assign(111111111111111, False)
    with pytest.raises(
        ValueError,
        match='cannot assign message "Message" to array of messages: all messages must be valid',
    ):
        msg_array.assign([tlv_message_value])
    assert msg_array.value == []

    intval.assign(5)
    array_type_foo_value.set("Length", 42)
    with pytest.raises(
        ValueError,
        match=(
            "^"
            "Error while setting value for field Bytes:"
            " invalid data size: input size is 8 while expected input size is 336"
            "$"
        ),
    ):
        array_type_foo_value.set("Bytes", [intval])


def icmp_checksum_function(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
    if first_arg is None:
        first_arg = kwargs.get("Message'First .. Checksum'First - 1")
    assert isinstance(first_arg, tuple)
    tag_first, checksum_first_minus_one = first_arg
    assert tag_first == 0 and checksum_first_minus_one == 15
    second_arg = kwargs.get("Checksum'Last + 1 .. Message'Last")
    assert isinstance(second_arg, tuple)
    checksum_last_plus_one, data_last = second_arg
    assert checksum_last_plus_one == 32 and data_last == 511
    checksum_size = kwargs.get("Checksum'Size")
    assert isinstance(checksum_size, int)
    assert checksum_size == 16

    checksum_bytes = message[tag_first : (checksum_first_minus_one + 1) // 8]
    checksum_bytes += b"\x00" * (checksum_size // 8)
    checksum_bytes += message[(checksum_last_plus_one // 8) : (data_last + 1) // 8]
    return utils.internet_checksum(checksum_bytes)


def test_checksum_field_not_defined(icmp_checksum_message_value: MessageValue) -> None:
    with pytest.raises(
        KeyError,
        match="cannot set checksum function: field NonExistingField is not defined",
    ):
        icmp_checksum_message_value.set_checksum_function(
            {"NonExistingField": icmp_checksum_function}
        )

    with pytest.raises(
        KeyError,
        match="cannot set checksum function: field Tag has not been defined as " "a checksum field",
    ):
        icmp_checksum_message_value.set_checksum_function({"Tag": icmp_checksum_function})


def test_checksum_function_not_set(icmp_checksum_message_value: MessageValue) -> None:
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    with pytest.raises(
        AttributeError,
        match="cannot calculate checksum for Checksum: no callable checksum function provided",
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
    assert_bytestring_error(icmp_checksum_message_value)
    icmp_checksum_message_value.set("Checksum", 12824)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    assert icmp_checksum_message_value.valid_message
    assert icmp_checksum_message_value.get("Checksum") == 12824
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
    # pylint: disable = protected-access
    pyrflx_ = PyRFLX.from_specs(
        [f"{EX_SPEC_DIR}/icmp.rflx"], skip_model_verification=True, skip_message_verification=True
    )
    icmp_message = pyrflx_["ICMP"]["Message"]._type
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
            aspects={
                ID("Checksum"): {
                    ID("Checksum"): [
                        expr.ValueRange(
                            expr.First("Message"), expr.Sub(expr.First("Checksum"), expr.Number(1))
                        ),
                        expr.Size("Checksum"),
                        expr.ValueRange(
                            expr.Add(expr.Last("Checksum"), expr.Number(1)), expr.Last("Message")
                        ),
                    ]
                }
            },
        )
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
    return pyrflx_["TLV_With_Checksum"]


@pytest.fixture(name="tlv_checksum_message")
def fixture_tlv_checksum_message(tlv_checksum_package: Package) -> Message:
    return tlv_checksum_package["Message"]._type  # pylint: disable=protected-access


def test_checksum_is_checksum_settable(tlv_checksum_message: Message) -> None:
    # pylint: disable=protected-access
    tlv_msg = MessageValue(tlv_checksum_message)
    tlv_msg.set_checksum_function({"Checksum": lambda x, **y: 0})
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
    return pyrflx_["No_Conditionals"]


@pytest.fixture(name="no_conditionals_message")
def fixture_no_conditionals_message(no_conditionals_package: Package) -> Message:
    # pylint: disable = protected-access
    return no_conditionals_package["Message"]._type


def test_checksum_value_range(no_conditionals_message: Message) -> None:
    # pylint: disable=protected-access
    message = no_conditionals_message.copy(
        structure=[
            Link(l.source, l.target, condition=expr.ValidChecksum("Checksum"))
            if l.target == FINAL
            else l
            for l in no_conditionals_message.structure
        ],
        aspects={
            ID("Checksum"): {
                ID("Checksum"): [expr.ValueRange(expr.First("Tag"), expr.Last("Data"))]
            }
        },
    )
    msg = MessageValue(message)
    msg.set("Tag", 0)
    msg.set("Data", 0)
    assert not msg._is_checksum_settable(msg._checksums["Checksum"])


def test_refinement_with_checksum() -> None:
    def tlv_checksum(_: bytes, **__: object) -> int:
        return 0

    def msg_checksum(_: bytes, **__: object) -> int:
        return 0xFF

    pyrflx_ = PyRFLX.from_specs(
        [f"{SPEC_DIR}/refinement_with_checksum.rflx", f"{SPEC_DIR}/tlv_with_checksum.rflx"]
    )
    refinement_package = pyrflx_["Refinement_With_Checksum"]
    tlv_package = pyrflx_["TLV_With_Checksum"]
    refinement_package.set_checksum_functions({"Message": {"Checksum": msg_checksum}})
    tlv_package.set_checksum_functions({"Message": {"Checksum": tlv_checksum}})
    data = b"\x08\xff\x40\x02\x01\x02\x00\x00\x00\x00"
    message = refinement_package["Message"]
    message.set_checksum_function({"Checksum": msg_checksum})
    message.parse(data)
    assert message.valid_message
    tlv_message = message.get("Payload")
    assert isinstance(tlv_message, MessageValue)
    assert tlv_message.valid_message


@pytest.mark.parametrize(
    "data, f1, f2, f3",
    [
        (b"\x00\x00\x00\x00", 0, 0, 0),
        (b"\x11\x11\x11\x11", 0x8, 0x44, 0x11111),
        (b"\x11\x22\x33\x44", 0x8, 0x48, 0x23344),
        (b"\xff\xff\xff\xff", 0x7F, 0x7F, 0x3FFFF),
    ],
)
def test_unaligned_field_serialization(data: bytes, f1: int, f2: int, f3: int) -> None:
    msg = PyRFLX.from_specs([f"{SPEC_DIR}/unaligned_field.rflx"])["Unaligned_Field"]["M1"]
    msg.parse(data)
    assert msg.get("F1") == f1
    assert msg.get("F2") == f2
    assert msg.get("F3") == f3
    assert data == msg.bytestring
