import itertools
from pathlib import Path

import pytest

from rflx import expr, pyrflx
from rflx.identifier import ID
from rflx.model.cache import AlwaysVerify
from rflx.pyrflx import PyRFLXError
from rflx.specification.parser import Parser
from tests.const import CAPTURED_DIR, GENERATED_DIR, SPEC_DIR
from tests.utils import assert_equal_code_specs


def test_ethernet(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [SPEC_DIR / "ethernet.rflx"],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


# rflx-ethernet-tests.adb


def test_ethernet_parsing_ethernet_2(ethernet_frame_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_ipv4_udp.raw").read_bytes()
    ethernet_frame_value.parse(msg_as_bytes)
    assert ethernet_frame_value.get("Destination") == int("ffffffffffff", 16)
    assert ethernet_frame_value.get("Source") == int("0", 16)
    assert ethernet_frame_value.get("Type_Length_TPID") == int("0800", 16)
    k = ethernet_frame_value._fields["Payload"].typeval.size
    assert isinstance(k, expr.Number)
    assert k.value // 8 == 46
    assert ethernet_frame_value.valid_message
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_parsing_ieee_802_3(ethernet_frame_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_802.3.raw").read_bytes()
    ethernet_frame_value.parse(msg_as_bytes)
    assert ethernet_frame_value.valid_message
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_parsing_ethernet_2_vlan(ethernet_frame_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_vlan_tag.raw").read_bytes()
    ethernet_frame_value.parse(msg_as_bytes)
    assert ethernet_frame_value.get("Destination") == int("ffffffffffff", 16)
    assert ethernet_frame_value.get("Source") == int("0", 16)
    assert ethernet_frame_value.get("Type_Length_TPID") == int("8100", 16)
    assert ethernet_frame_value.get("TPID") == int("8100", 16)
    assert ethernet_frame_value.get("TCI") == int("1", 16)
    assert ethernet_frame_value.get("Type_Length") == int("0800", 16)
    k = ethernet_frame_value._fields["Payload"].typeval.size
    assert isinstance(k, expr.Number)
    assert k.value // 8 == 47
    assert ethernet_frame_value.valid_message
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_parsing_invalid_ethernet_2_too_short(
    ethernet_frame_value: pyrflx.MessageValue,
) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_invalid_too_short.raw").read_bytes()
    with pytest.raises(
        PyRFLXError,
        match=r"^error: none of the field conditions .* for field Payload"
        " have been met by the assigned value: [01]*$",
    ):
        ethernet_frame_value.parse(msg_as_bytes)
    assert not ethernet_frame_value.valid_message


def test_ethernet_parsing_invalid_ethernet_2_too_long(
    ethernet_frame_value: pyrflx.MessageValue,
) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_invalid_too_long.raw").read_bytes()
    with pytest.raises(
        PyRFLXError,
        match=r"^error: none of the field conditions .* for field Payload"
        " have been met by the assigned value: [01]*$",
    ):
        ethernet_frame_value.parse(msg_as_bytes)
    assert not ethernet_frame_value.valid_message


def test_ethernet_parsing_invalid_ethernet_2_undefined_type(
    ethernet_frame_value: pyrflx.MessageValue,
) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_undefined.raw").read_bytes()
    with pytest.raises(
        PyRFLXError,
        match=r"^error: none of the field conditions .* for field Type_Length"
        " have been met by the assigned value: [01]*$",
    ):
        ethernet_frame_value.parse(msg_as_bytes)

    assert not ethernet_frame_value.valid_message


def test_ethernet_parsing_ieee_802_3_invalid_length(
    ethernet_frame_value: pyrflx.MessageValue,
) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_802.3_invalid_length.raw").read_bytes()
    with pytest.raises(
        PyRFLXError,
        match="^error: Bitstring representing the message is too short"
        " - stopped while parsing field: Payload$",
    ):
        ethernet_frame_value.parse(msg_as_bytes)

    assert not ethernet_frame_value.valid_message


def test_ethernet_parsing_incomplete(ethernet_frame_value: pyrflx.MessageValue) -> None:
    test_bytes = b"\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x02"
    with pytest.raises(
        PyRFLXError,
        match="^error: Bitstring representing the message is too short"
        " - stopped while parsing field: Type_Length_TPID$",
    ):
        ethernet_frame_value.parse(test_bytes)

    assert ethernet_frame_value.get("Destination") == int("000000000001", 16)
    assert ethernet_frame_value.get("Source") == int("000000000002", 16)
    assert len(ethernet_frame_value.valid_fields) == 2
    assert not ethernet_frame_value.valid_message


def test_ethernet_generating_ethernet_2(ethernet_frame_value: pyrflx.MessageValue) -> None:
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
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_ipv4_udp.raw").read_bytes()
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_generating_ieee_802_3(ethernet_frame_value: pyrflx.MessageValue) -> None:
    payload = (
        b"\x45\x00\x00\x14\x00\x01\x00\x00\x40\x00\x7c\xe7"
        b"\x7f\x00\x00\x01\x7f\x00\x00\x01\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    )
    ethernet_frame_value.set("Destination", int("FFFFFFFFFFFF", 16))
    ethernet_frame_value.set("Source", int("0", 16))
    ethernet_frame_value.set("Type_Length_TPID", 46)
    ethernet_frame_value.set("Type_Length", 46)
    ethernet_frame_value.set("Payload", payload)
    assert ethernet_frame_value.valid_message
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_802.3.raw").read_bytes()
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_generating_ethernet_2_vlan(ethernet_frame_value: pyrflx.MessageValue) -> None:
    payload = (
        b"\x45\x00\x00\x2f\x00\x01\x00\x00\x40\x00\x7c\xe7"
        b"\x7f\x00\x00\x01\x7f\x00\x00\x01\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a"
    )
    ethernet_frame_value.set("Destination", int("FFFFFFFFFFFF", 16))
    ethernet_frame_value.set("Source", int("0", 16))
    ethernet_frame_value.set("Type_Length_TPID", int("8100", 16))
    ethernet_frame_value.set("TPID", int("8100", 16))
    ethernet_frame_value.set("TCI", 1)
    ethernet_frame_value.set("Type_Length", int("0800", 16))
    ethernet_frame_value.set("Payload", payload)
    assert ethernet_frame_value.valid_message
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_vlan_tag.raw").read_bytes()
    assert ethernet_frame_value.bytestring == msg_as_bytes


def test_ethernet_generating_ethernet_2_vlan_dynamic() -> None:
    pass  # not relevant for Python implementation, as it tests correct verification in SPARK


def test_ipv4(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [SPEC_DIR / "ipv4.rflx"],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


# rflx-ipv4-tests.adb


def test_ipv4_parsing_ipv4(ipv4_packet_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ipv4_udp.raw").read_bytes()
    ipv4_packet_value.parse(msg_as_bytes)
    assert ipv4_packet_value.get("Version") == 4
    assert ipv4_packet_value.get("IHL") == 5
    assert ipv4_packet_value.get("DSCP") == 0
    assert ipv4_packet_value.get("ECN") == 0
    assert ipv4_packet_value.get("Total_Length") == 44
    assert ipv4_packet_value.get("Identification") == 1
    assert ipv4_packet_value.get("Flag_R") == "False"
    assert ipv4_packet_value.get("Flag_DF") == "False"
    assert ipv4_packet_value.get("Flag_MF") == "False"
    assert ipv4_packet_value.get("Fragment_Offset") == 0
    assert ipv4_packet_value.get("TTL") == 64
    assert ipv4_packet_value.get("Protocol") == "IPv4::P_UDP"
    assert ipv4_packet_value.get("Header_Checksum") == int("7CBE", 16)
    assert ipv4_packet_value.get("Source") == int("7f000001", 16)
    assert ipv4_packet_value.get("Destination") == int("7f000001", 16)
    assert ipv4_packet_value._fields["Payload"].typeval.size == expr.Number(192)


def test_ipv4_parsing_ipv4_option_value(ipv4_option_value: pyrflx.MessageValue) -> None:
    expected = b"\x44\x03\x2a"
    ipv4_option_value.parse(expected)
    assert ipv4_option_value.get("Copied") == "False"
    assert ipv4_option_value.get("Option_Class") == "IPv4::Debugging_And_Measurement"
    assert ipv4_option_value.get("Option_Number") == 4
    assert ipv4_option_value.get("Option_Length") == 3
    ip_option = ipv4_option_value.get("Option_Data")
    assert isinstance(ip_option, bytes)
    assert len(ip_option) == 1


@pytest.mark.skip(reason="Eng/RecordFlux/RecordFlux#61")
def test_ipv4_parsing_ipv4_with_options(
    ipv4_packet_value: pyrflx.MessageValue,
) -> None:  # pragma: no cover
    msg_as_bytes: bytes = (CAPTURED_DIR / "ipv4-options_udp.raw").read_bytes()
    ipv4_packet_value.parse(msg_as_bytes)
    assert ipv4_packet_value.valid_message


def test_ipv4_generating_ipv4(ipv4_packet_value: pyrflx.MessageValue) -> None:
    data = (
        b"\x00\x35\x00\x35\x00\x18\x01\x52\x00\x00\x00\x00"
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    )
    ipv4_packet_value.set("Version", 4)
    ipv4_packet_value.set("IHL", 5)
    ipv4_packet_value.set("DSCP", 0)
    ipv4_packet_value.set("ECN", 0)
    ipv4_packet_value.set("Total_Length", 44)
    ipv4_packet_value.set("Identification", 1)
    ipv4_packet_value.set("Flag_R", "False")
    ipv4_packet_value.set("Flag_DF", "False")
    ipv4_packet_value.set("Flag_MF", "False")
    ipv4_packet_value.set("Fragment_Offset", 0)
    ipv4_packet_value.set("TTL", 64)
    ipv4_packet_value.set("Protocol", "P_UDP")
    ipv4_packet_value.set("Header_Checksum", int("7CBE", 16))
    ipv4_packet_value.set("Source", int("7f000001", 16))
    ipv4_packet_value.set("Destination", int("7f000001", 16))
    ipv4_packet_value.set("Options", [])
    ipv4_packet_value.set("Payload", data)
    assert ipv4_packet_value.valid_message


def test_ipv4_generating_ipv4_option_value(ipv4_option_value: pyrflx.MessageValue) -> None:
    ipv4_option_value.set("Copied", "False")
    ipv4_option_value.set("Option_Class", "Debugging_And_Measurement")
    ipv4_option_value.set("Option_Number", 4)
    ipv4_option_value.set("Option_Length", 3)
    ipv4_option_value.set("Option_Data", b"\x2a")
    assert ipv4_option_value.valid_message


def test_in_ethernet(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [
            SPEC_DIR / "ethernet.rflx",
            SPEC_DIR / "ipv4.rflx",
            SPEC_DIR / "in_ethernet.rflx",
        ],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


def test_udp(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [SPEC_DIR / "udp.rflx"],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


def test_in_ipv4(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [
            SPEC_DIR / "ipv4.rflx",
            SPEC_DIR / "udp.rflx",
            SPEC_DIR / "in_ipv4.rflx",
        ],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


# rflx in_ipv4-tests.adb


def test_in_ipv4_parsing_udp_in_ipv4(ipv4_packet_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ipv4_udp.raw").read_bytes()
    ipv4_packet_value.parse(msg_as_bytes)
    nested_udp = ipv4_packet_value.get("Payload")
    assert isinstance(nested_udp, pyrflx.MessageValue)
    assert nested_udp.valid_message


def test_in_ipv4_parsing_udp_in_ipv4_in_ethernet(ethernet_frame_value: pyrflx.MessageValue) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_ipv4_udp.raw").read_bytes()
    ethernet_frame_value.parse(msg_as_bytes)
    nested_ipv4 = ethernet_frame_value.get("Payload")
    assert isinstance(nested_ipv4, pyrflx.MessageValue)
    assert nested_ipv4.valid_message
    assert nested_ipv4.identifier == ID("IPv4") * "Packet"
    nested_udp = nested_ipv4.get("Payload")
    assert isinstance(nested_udp, pyrflx.MessageValue)
    assert nested_udp.valid_message
    assert nested_udp.identifier == ID("UDP") * "Datagram"


def test_in_ipv4_generating_udp_in_ipv4_in_ethernet(
    ethernet_frame_value: pyrflx.MessageValue,
    ipv4_packet_value: pyrflx.MessageValue,
    udp_datagram_value: pyrflx.MessageValue,
) -> None:
    msg_as_bytes: bytes = (CAPTURED_DIR / "ethernet_ipv4_udp.raw").read_bytes()
    ethernet_frame_value.parse(msg_as_bytes)
    parsed_frame = ethernet_frame_value.bytestring

    b = b""
    for _ in itertools.repeat(None, 18):
        b += b"\x00"

    udp_datagram_value.set("Source_Port", 53)
    udp_datagram_value.set("Destination_Port", 53)
    udp_datagram_value.set("Length", 26)
    udp_datagram_value.set("Checksum", int("014E", 16))
    udp_datagram_value.set("Payload", b)
    udp_binary = udp_datagram_value.bytestring

    ipv4_packet_value.set("Version", 4)
    ipv4_packet_value.set("IHL", 5)
    ipv4_packet_value.set("DSCP", 0)
    ipv4_packet_value.set("ECN", 0)
    ipv4_packet_value.set("Total_Length", 46)
    ipv4_packet_value.set("Identification", 1)
    ipv4_packet_value.set("Flag_R", "False")
    ipv4_packet_value.set("Flag_DF", "False")
    ipv4_packet_value.set("Flag_MF", "False")
    ipv4_packet_value.set("Fragment_Offset", 0)
    ipv4_packet_value.set("TTL", 64)
    ipv4_packet_value.set("Protocol", "P_UDP")
    ipv4_packet_value.set("Header_Checksum", int("7CBC", 16))
    ipv4_packet_value.set("Source", int("7f000001", 16))
    ipv4_packet_value.set("Destination", int("7f000001", 16))
    ipv4_packet_value.set("Options", [])
    ipv4_packet_value.set("Payload", udp_binary)
    ip_binary = ipv4_packet_value.bytestring

    ethernet_frame_value.set("Destination", int("FFFFFFFFFFFF", 16))
    ethernet_frame_value.set("Source", int("0", 16))
    ethernet_frame_value.set("Type_Length_TPID", int("0800", 16))
    ethernet_frame_value.set("Type_Length", int("0800", 16))
    ethernet_frame_value.set("Payload", ip_binary)

    assert ethernet_frame_value.valid_message
    assert ethernet_frame_value.bytestring == parsed_frame


def test_tlv(tmp_path: Path) -> None:
    assert_equal_code_specs(
        [SPEC_DIR / "tlv.rflx"],
        GENERATED_DIR,
        tmp_path,
        accept_extra_files=True,
    )


# rflx-tlv-tests.adb


def test_tlv_parsing_tlv_data(tlv_message_value: pyrflx.MessageValue) -> None:
    test_bytes = b"\x01\x00\x04\x00\x00\x00\x00"
    tlv_message_value.parse(test_bytes)
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == test_bytes


def test_tlv_parsing_tlv_data_zero(tlv_message_value: pyrflx.MessageValue) -> None:
    test_bytes = b"\x01\x00\x00"
    tlv_message_value.parse(test_bytes)
    assert tlv_message_value.get("Tag") == "TLV::Msg_Data"
    assert tlv_message_value.get("Length") == 0
    assert tlv_message_value.valid_message


def test_tlv_parsing_tlv_error(tlv_message_value: pyrflx.MessageValue) -> None:
    test_bytes = b"\x03"
    tlv_message_value.parse(test_bytes)
    assert tlv_message_value.get("Tag") == "TLV::Msg_Error"
    assert tlv_message_value.valid_message


def test_tlv_parsing_invalid_tlv_invalid_tag(tlv_message_value: pyrflx.MessageValue) -> None:
    test_bytes = b"\x00\x00"
    with pytest.raises(
        PyRFLXError,
        match=(
            "^"
            "error: cannot set value for field Tag\n"
            "error: Number 0 is not a valid enum value"
            "$"
        ),
    ):
        tlv_message_value.parse(test_bytes)
    assert not tlv_message_value.valid_message


def test_tlv_generating_tlv_data(tlv_message_value: pyrflx.MessageValue) -> None:
    expected = b"\x01\x00\x04\x00\x00\x00\x00"
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 4)
    tlv_message_value.set("Value", b"\x00\x00\x00\x00")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == expected


def test_tlv_generating_tlv_data_zero(tlv_message_value: pyrflx.MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Data")
    tlv_message_value.set("Length", 0)
    assert not tlv_message_value.valid_message


def test_tlv_generating_tlv_error(tlv_message_value: pyrflx.MessageValue) -> None:
    tlv_message_value.set("Tag", "Msg_Error")
    assert tlv_message_value.valid_message
    assert tlv_message_value.bytestring == b"\x03"


@pytest.mark.parametrize("file", SPEC_DIR.glob("*.rflx"))
def test_specs(file: Path) -> None:
    parser = Parser(AlwaysVerify())
    parser.parse(file)
    parser.create_model()
