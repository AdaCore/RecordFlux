from __future__ import annotations

import re
from functools import reduce
from pathlib import Path

import pytest

from rflx.error import FatalError
from rflx.expression import (
    Add,
    And,
    Equal,
    First,
    Last,
    Mul,
    Not,
    NotEqual,
    Number,
    Or,
    Size,
    Sub,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import FINAL, Link, Message
from rflx.model.message import INITIAL, Field
from rflx.model.model import Model
from rflx.model.type_ import OPAQUE
from rflx.pyrflx import MessageValue, Package, PyRFLX, PyRFLXError, TypeValue, utils
from tests.const import CAPTURED_DIR, SPEC_DIR
from tests.data import models


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
    ethernet_frame_value.set("Destination", 2**48 - 1)
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
    assert ethernet_frame_value.bytestring == (CAPTURED_DIR / "ethernet_802.3.raw").read_bytes()


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
        tls_record_value.set("Length", 2**14 + 1)


def test_tls_invalid_path(tls_alert_value: MessageValue) -> None:
    tls_alert_value.set("Level", "WARNING")
    tls_alert_value.set("Description", "CLOSE_NOTIFY")
    assert tls_alert_value.valid_message
    assert tls_alert_value.valid_fields == ["Level", "Description"]
    tls_alert_value.set("Level", "FATAL")
    assert not tls_alert_value.valid_message
    assert tls_alert_value.valid_fields == ["Level"]  # type: ignore[unreachable]
    # https://github.com/python/mypy/issues/12598


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
    (tmp_path / "test.rflx").write_text(
        """--
with Foo;

package Test is

   type T is (E1 => 1, E2 => 2) with Size => 8;

   type Message is
      message
         A : Foo::T
            then null
               if A = Foo::E1;
      end message;

end Test;""",
    )

    (tmp_path / "foo.rflx").write_text(
        """--
package Foo is

   type T is (E1 => 11, E2 => 12) with Size => 8;

end Foo;""",
    )

    pyrflx_ = PyRFLX.from_specs([str(tmp_path / "test.rflx")])
    m = pyrflx_.package("Test").new_message("Message")

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

    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions \['A = Foo::E1'\] for field A have "
        r"been met by the assigned value: E2$",
    ):
        m.set("A", "E2")

    with pytest.raises(
        PyRFLXError,
        match=r"^pyrflx: error: none of the field conditions \['A = Foo::E1'\] for field A have "
        r"been met by the assigned value: Foo::E2$",
    ):
        m.set("A", "Foo::E2")


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
        [SPEC_DIR / "ethernet.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    frame_unv = pyrflx_.package("Ethernet").new_message("Frame")
    frame_unv.set("Destination", int("FFFFFFFFFFFF", 16))
    frame_unv.set("Source", int("0", 16))
    frame_unv.set("Type_Length_TPID", int("0800", 16))
    frame_unv.set("Type_Length", int("0800", 16))
    frame_unv.set("Payload", payload)
    assert frame_unv.valid_message
    assert frame_unv.bytestring == ethernet_frame_value.bytestring


def test_no_verification_sequence_nested_messages(
    sequence_message_package: Package,
    message_sequence_value: MessageValue,
) -> None:
    sequence_message_one = sequence_message_package.new_message("Sequence_Element")
    sequence_message_one.set("Byte", 5)
    sequence_message_two = sequence_message_package.new_message("Sequence_Element")
    sequence_message_two.set("Byte", 6)
    sequence: list[TypeValue] = [sequence_message_one, sequence_message_two]
    message_sequence_value.set("Length", 2)
    message_sequence_value.set("Sequence_Field", sequence)
    assert message_sequence_value.valid_message

    pyrflx_ = PyRFLX.from_specs(
        [SPEC_DIR / "sequence_message.rflx"],
        skip_model_verification=True,
        skip_message_verification=True,
    )
    sequence_message_package_unv = pyrflx_.package("Sequence_Message")
    sequence_message_unv = sequence_message_package_unv.new_message("Message_Sequence")
    sequence_element_one_unv = sequence_message_package_unv.new_message("Sequence_Element")
    sequence_element_one_unv.set("Byte", 5)
    sequence_element_two_unv = sequence_message_package_unv.new_message("Sequence_Element")
    sequence_element_two_unv.set("Byte", 6)
    sequence_unv: list[TypeValue] = [sequence_element_one_unv, sequence_element_two_unv]
    sequence_message_unv.set("Length", 2)
    sequence_message_unv.set("Sequence_Field", sequence_unv)
    assert sequence_message_unv.valid_message
    assert sequence_message_unv.bytestring == message_sequence_value.bytestring


def icmp_checksum_function(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
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


def test_no_verification_icmp_checksum(
    icmp_checksum_message_value: MessageValue,
    icmp_message: Message,
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
            checksums={
                ID("Checksum"): [
                    ValueRange(First("Tag"), Sub(First("Checksum"), Number(1))),
                    Size("Checksum"),
                    ValueRange(Add(Last("Checksum"), Number(1)), Last("Message")),
                ],
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


def test_sequence_message_serialization(
    sequence_message_package: Package,
    message_sequence_value: MessageValue,
) -> None:
    sequence_message_one = sequence_message_package.new_message("Sequence_Element")
    sequence_message_one.set("Byte", 5)
    sequence_message_two = sequence_message_package.new_message("Sequence_Element")
    sequence_message_two.set("Byte", 6)
    sequence: list[TypeValue] = [sequence_message_one, sequence_message_two]
    message_sequence_value.set("Length", 2)
    message_sequence_value.set("Sequence_Field", sequence)
    assert message_sequence_value.valid_message
    assert message_sequence_value.as_json() == {
        "Length": {"first": 0, "last": 7, "value": 2},
        "Sequence_Field": {
            "first": 8,
            "last": 23,
            "value": [
                {"Byte": {"first": 0, "last": 7, "value": 5}},
                {"Byte": {"first": 0, "last": 7, "value": 6}},
            ],
        },
    }


def test_tlv_message_serialization(tlv_message_value: MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 3)
    tlv_message_value.set("Value", b"abc")
    assert tlv_message_value.valid_message
    assert tlv_message_value.as_json() == {
        "Length": {"first": 8, "last": 23, "value": 3},
        "Tag": {"first": 0, "last": 7, "value": "TLV::Msg_Data"},
        "Value": {"first": 24, "last": 47, "value": "616263"},
    }


def test_tlv_message_with_not_operator() -> None:
    message = Message(
        "TLV::Message_With_Not_Operator",
        [
            Link(INITIAL, Field("Tag")),
            Link(
                Field("Tag"),
                Field("Length"),
                Not(Not(Not(NotEqual(Variable("Tag"), Variable("Msg_Data"))))),
            ),
            Link(
                Field("Tag"),
                FINAL,
                Not(
                    Not(
                        Not(
                            Or(
                                Not(Not(Equal(Variable("Tag"), Variable("Msg_Data")))),
                                Not(Equal(Variable("Tag"), Variable("Msg_Error"))),
                            ),
                        ),
                    ),
                ),
            ),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), FINAL),
        ],
        {
            Field("Tag"): models.tlv_tag(),
            Field("Length"): models.tlv_length(),
            Field("Value"): OPAQUE,
        },
    )

    model = PyRFLX(model=Model([models.tlv_tag(), models.tlv_length(), message]))
    pkg = model.package("TLV")
    msg = pkg.new_message("Message_With_Not_Operator")
    test_bytes = b"\x01\x00\x04\x00\x00\x00\x00"
    msg.parse(test_bytes)
    assert msg.valid_message
    assert msg.bytestring == test_bytes


def test_tlv_message_with_not_operator_exhausting() -> None:
    message = Message(
        "TLV::Message_With_Not_Operator_Exhausting",
        [
            Link(INITIAL, Field("Tag")),
            Link(
                Field("Tag"),
                Field("Length"),
                Not(Not(Not(NotEqual(Variable("Tag"), Variable("Msg_Data"))))),
            ),
            Link(
                Field("Tag"),
                FINAL,
                reduce(
                    lambda acc, f: f(acc),
                    [Not, Not] * 16,
                    Not(
                        Or(
                            Not(Not(Equal(Variable("Tag"), Variable("Msg_Data")))),
                            Not(Equal(Variable("Tag"), Variable("Msg_Error"))),
                        ),
                    ),
                ),
            ),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), FINAL),
        ],
        {
            Field("Tag"): models.tlv_tag(),
            Field("Length"): models.tlv_length(),
            Field("Value"): OPAQUE,
        },
    )

    with pytest.raises(
        FatalError,
        match=(
            "^"
            + re.escape(
                "pyrflx: error: "
                "failed to simplify complex expression `not (not (not (not "
                "(not (not (not (not (not (not (not (not (not (not (not (not "
                "(not (not (not (not (not (not (not (not (not (not (not (not "
                "(not (not (not (not (not (not (not (Tag = TLV::Msg_Data))\n"
                "                                 "
                "or not (Tag = TLV::Msg_Error))))))))))))))))))))))))))))))))))` "
                "after `16` iterations, best effort: "
                "`not (not (not (not (not (not (not (not (not (not (not (not (not "
                "(not (not (not (not (Tag = TLV::Msg_Data\n"
                "                 or Tag /= TLV::Msg_Error)))))))))))))))))`",
            )
            + "$"
        ),
    ):
        PyRFLX(model=Model([models.tlv_tag(), models.tlv_length(), message]))
