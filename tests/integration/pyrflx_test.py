from pathlib import Path
from typing import List

import pytest

from rflx.expression import Add, And, First, Last, Number, Size, Sub, ValidChecksum, ValueRange
from rflx.identifier import ID
from rflx.model import FINAL, Link, Message
from rflx.pyrflx import MessageValue, Package, PyRFLX, PyRFLXError, TypeValue, utils
from tests.const import CAPTURED_DIR, EX_SPEC_DIR, SPEC_DIR


def test_ethernet_set_tltpid(ethernet_frame_value: MessageValue) -> None:
    ethernet_frame_value.set("Destination", 0)
    ethernet_frame_value.set("Source", 1)
    ethernet_frame_value.set("Type_Length_TPID", 0x8100)
    assert ethernet_frame_value.valid_fields == ["Destination", "Source", "Type_Length_TPID"]
    assert ethernet_frame_value.accessible_fields == [
        "Destination",
        "Source",
        "Type_Length_TPID",
        "TPID",
        "TCI",
        "Type_Length",
    ]
    ethernet_frame_value.set("Type_Length_TPID", 64)
    assert ethernet_frame_value.valid_fields == ["Destination", "Source", "Type_Length_TPID"]
    assert ethernet_frame_value.accessible_fields == [
        "Destination",
        "Source",
        "Type_Length_TPID",
        "Type_Length",
    ]


def test_ethernet_set_nonlinear(ethernet_frame_value: MessageValue) -> None:
    assert ethernet_frame_value.accessible_fields == ["Destination", "Source", "Type_Length_TPID"]
    ethernet_frame_value.set("Type_Length_TPID", 0x8100)
    ethernet_frame_value.set("TCI", 100)
    assert ethernet_frame_value.valid_fields == ["Type_Length_TPID", "TCI"]


def test_ethernet_final(ethernet_frame_value: MessageValue) -> None:
    assert not ethernet_frame_value.valid_message
    ethernet_frame_value.set("Destination", 0)
    assert not ethernet_frame_value.valid_message
    ethernet_frame_value.set("Source", 1)
    assert not ethernet_frame_value.valid_message
    ethernet_frame_value.set("Type_Length_TPID", 46)
    assert not ethernet_frame_value.valid_message
    ethernet_frame_value.set("Type_Length", 46)
    assert not ethernet_frame_value.valid_message
    ethernet_frame_value.set("Payload", bytes(46))
    assert ethernet_frame_value.valid_message


def test_ethernet_802_3(ethernet_frame_value: MessageValue) -> None:
    ethernet_frame_value.set("Destination", 2 ** 48 - 1)
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Type_Length_TPID", 46)
    ethernet_frame_value.set("Type_Length", 46)
    ethernet_frame_value.set(
        "Payload",
        (
            b"\x45\x00\x00\x14"
            b"\x00\x01\x00\x00"
            b"\x40\x00\x7c\xe7"
            b"\x7f\x00\x00\x01"
            b"\x7f\x00\x00\x01"
            b"\x00\x00\x00\x00"
            b"\x00\x00\x00\x00"
            b"\x00\x00\x00\x00"
            b"\x00\x00\x00\x00"
            b"\x00\x00\x00\x00"
            b"\x00\x00\x00\x00"
            b"\x00\x00"
        ),
    )
    assert ethernet_frame_value.valid_message
    with open(CAPTURED_DIR / "ethernet_802.3.raw", "rb") as raw:
        assert ethernet_frame_value.bytestring == raw.read()


def test_ethernet_payload(ethernet_frame_value: MessageValue) -> None:
    ethernet_frame_value.set("Source", 0)
    ethernet_frame_value.set("Destination", 0)
    ethernet_frame_value.set("Type_Length_TPID", 47)
    ethernet_frame_value.set("Type_Length", 1537)
    assert ethernet_frame_value.accessible_fields == [
        "Destination",
        "Source",
        "Type_Length_TPID",
        "Type_Length",
        "Payload",
    ]
    ethernet_frame_value.set("Payload", bytes(46))
    assert ethernet_frame_value.valid_message


def test_tls_fields(tls_record_value: MessageValue) -> None:
    assert tls_record_value.accessible_fields == ["Tag", "Legacy_Record_Version", "Length"]
    tls_record_value.set("Tag", "INVALID")
    tls_record_value.set("Length", 3)
    assert tls_record_value.accessible_fields == [
        "Tag",
        "Legacy_Record_Version",
        "Length",
        "Fragment",
    ]


def test_tls_invalid_outgoing(tls_record_value: MessageValue) -> None:
    tls_record_value.set("Tag", "INVALID")
    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions .* for field Length"
        " have been met by the assigned value: 16385$",
    ):
        tls_record_value.set("Length", 2 ** 14 + 1)


def test_tls_invalid_path(tls_alert_value: MessageValue) -> None:
    tls_alert_value.set("Level", "WARNING")
    tls_alert_value.set("Description", "CLOSE_NOTIFY")
    assert tls_alert_value.valid_message
    assert tls_alert_value.valid_fields == ["Level", "Description"]
    tls_alert_value.set("Level", "FATAL")
    assert not tls_alert_value.valid_message
    assert tls_alert_value.valid_fields == ["Level"]


def test_icmp_echo_request(icmp_message_value: MessageValue) -> None:
    test_data = (
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )

    icmp_message_value.set("Tag", "Echo_Request")
    icmp_message_value.set("Code_Zero", 0)
    icmp_message_value.set("Checksum", 12824)
    icmp_message_value.set("Identifier", 5)
    icmp_message_value.set("Sequence_Number", 1)
    icmp_message_value.set(
        "Data",
        test_data,
    )
    assert icmp_message_value.bytestring == b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data
    assert icmp_message_value.valid_message


def test_icmp_parse_binary(icmp_message_value: MessageValue) -> None:
    test_bytes = (
        b"\x08\x00\xe1\x1e\x00\x11\x00\x01\x4a\xfc\x0d\x00\x00\x00\x00\x00"
        b"\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
        b"\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f"
        b"\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_message_value.parse(test_bytes)
    assert icmp_message_value.valid_message
    assert icmp_message_value.bytestring == test_bytes
    assert icmp_message_value.accepted_type == bytes
    assert icmp_message_value.size == Number(448)


def test_imported_literals(tmp_path: Path) -> None:
    with open(tmp_path / "test.rflx", "x") as f:
        f.write(
            """
            with Foo;

            package Test is

               type T is (E1 => 1, E2 => 2) with Size => 8;

               type Message is
                  message
                     A : Foo::T
                        then null
                           if A = Foo::E1;
                  end message;

            end Test;
            """
        )

    with open(tmp_path / "foo.rflx", "x") as f:
        f.write(
            """
            package Foo is

               type T is (E1 => 11, E2 => 12) with Size => 8;

            end Foo;
            """
        )

    pyrflx_ = PyRFLX.from_specs([str(tmp_path / "test.rflx")])
    m = pyrflx_["Test"]["Message"]

    m.set("A", "E1")
    assert m.valid_message

    m.set("A", "Foo::E1")
    assert m.valid_message

    m.parse(b"\x0B")
    assert m.valid_message
    assert m.get("A") == "Foo::E1"

    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions \['A = Foo::E1'\] for field A have "
        r"been met by the assigned value: 00001100$",
    ):
        m.parse(b"\x0C")
        assert not m.valid_message

    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions \['A = Foo::E1'\] for field A have "
        r"been met by the assigned value: E2$",
    ):
        m.set("A", "E2")
        assert not m.valid_message

    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions \['A = Foo::E1'\] for field A have "
        r"been met by the assigned value: Foo::E2$",
    ):
        m.set("A", "Foo::E2")
        assert not m.valid_message


def test_no_verification_ethernet(ethernet_frame_value: MessageValue) -> None:
    payload = (
        b"\x45\x00\x00\x2e\x00\x01\x00\x00\x40\x11\x7c\xbc"
        b"\x7f\x00\x00\x01\x7f\x00\x00\x01\x00\x35\x00\x35"
        b"\x00\x1a\x01\x4e\x00\x00\x00\x00\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    )
    ethernet_frame_value.set("Destination", int("FFFFFFFFFFFF", 16))
    ethernet_frame_value.set("Source", int("0", 16))
    ethernet_frame_value.set("Type_Length_TPID", int("0800", 16))
    ethernet_frame_value.set("Type_Length", int("0800", 16))
    ethernet_frame_value.set("Payload", payload)
    assert ethernet_frame_value.valid_message
    pyrflx_ = PyRFLX.from_specs(
        [f"{EX_SPEC_DIR}/ethernet.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    frame_unv = pyrflx_["Ethernet"]["Frame"]
    frame_unv.set("Destination", int("FFFFFFFFFFFF", 16))
    frame_unv.set("Source", int("0", 16))
    frame_unv.set("Type_Length_TPID", int("0800", 16))
    frame_unv.set("Type_Length", int("0800", 16))
    frame_unv.set("Payload", payload)
    assert frame_unv.valid_message
    assert frame_unv.bytestring == ethernet_frame_value.bytestring


def test_no_verification_array_nested_messages(
    array_message_package: Package, message_array_value: MessageValue
) -> None:
    array_message_one = array_message_package["Array_Element"]
    array_message_one.set("Byte", 5)
    array_message_two = array_message_package["Array_Element"]
    array_message_two.set("Byte", 6)
    array: List[TypeValue] = [array_message_one, array_message_two]
    message_array_value.set("Length", 2)
    message_array_value.set("Array_Field", array)
    assert message_array_value.valid_message

    pyrflx_ = PyRFLX.from_specs(
        [f"{SPEC_DIR}/array_message.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    array_message_package_unv = pyrflx_["Array_Message"]
    array_message_unv = array_message_package_unv["Message_Array"]
    array_element_one_unv = array_message_package_unv["Array_Element"]
    array_element_one_unv.set("Byte", 5)
    array_element_two_unv = array_message_package_unv["Array_Element"]
    array_element_two_unv.set("Byte", 6)
    array_unv: List[TypeValue] = [array_element_one_unv, array_element_two_unv]
    array_message_unv.set("Length", 2)
    array_message_unv.set("Array_Field", array_unv)
    assert array_message_unv.valid_message
    assert array_message_unv.bytestring == message_array_value.bytestring


def icmp_checksum_function(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
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


def test_no_verification_icmp_checksum(
    icmp_checksum_message_value: MessageValue, icmp_message: Message
) -> None:
    test_data = (
        b"\x47\xb4\x67\x5e\x00\x00\x00\x00"
        b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
        b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
        b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
    )
    icmp_checksum_unv = MessageValue(
        icmp_message.copy(
            structure=[
                Link(l.source, l.target, condition=And(l.condition, ValidChecksum("Checksum")))
                if l.target == FINAL
                else l
                for l in icmp_message.structure
            ],
            aspects={
                ID("Checksum"): {
                    ID("Checksum"): [
                        ValueRange(First("Tag"), Sub(First("Checksum"), Number(1))),
                        Size("Checksum"),
                        ValueRange(Add(Last("Checksum"), Number(1)), Last("Message")),
                    ]
                }
            },
        ),
        skip_verification=True,
    )
    icmp_checksum_message_value.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_message_value.set("Tag", "Echo_Request")
    icmp_checksum_message_value.set("Code_Zero", 0)
    icmp_checksum_message_value.set("Identifier", 5)
    icmp_checksum_message_value.set("Sequence_Number", 1)
    icmp_checksum_message_value.set("Data", test_data)
    icmp_checksum_unv.set_checksum_function({"Checksum": icmp_checksum_function})
    icmp_checksum_unv.set("Tag", "Echo_Request")
    icmp_checksum_unv.set("Code_Zero", 0)
    icmp_checksum_unv.set("Checksum", 0)
    icmp_checksum_unv.set("Identifier", 5)
    icmp_checksum_unv.set("Sequence_Number", 1)
    icmp_checksum_unv.set("Data", test_data)
    icmp_checksum_unv.update_checksums()
    assert icmp_checksum_unv.valid_message
    assert icmp_checksum_unv.get("Checksum") == icmp_checksum_message_value.get("Checksum")
    assert icmp_checksum_unv.bytestring == icmp_checksum_message_value.bytestring
