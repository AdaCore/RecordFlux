import pytest

from rflx import expression as expr, model, pyrflx
from rflx.identifier import ID
from tests.const import EX_SPEC_DIR, SPEC_DIR


@pytest.fixture(name="pyrflx_", scope="session")
def fixture_pyrflx() -> pyrflx.PyRFLX:
    return pyrflx.PyRFLX.from_specs(
        [
            f"{EX_SPEC_DIR}/ethernet.rflx",
            f"{EX_SPEC_DIR}/icmp.rflx",
            f"{EX_SPEC_DIR}/in_ethernet.rflx",
            f"{EX_SPEC_DIR}/in_ipv4.rflx",
            f"{EX_SPEC_DIR}/ipv4.rflx",
            f"{EX_SPEC_DIR}/tls_alert.rflx",
            f"{EX_SPEC_DIR}/tls_record.rflx",
            f"{EX_SPEC_DIR}/udp.rflx",
            f"{SPEC_DIR}/array_message.rflx",
            f"{SPEC_DIR}/array_type.rflx",
            f"{SPEC_DIR}/no_conditionals.rflx",
            f"{SPEC_DIR}/null_message.rflx",
            f"{SPEC_DIR}/tlv.rflx",
            f"{SPEC_DIR}/tlv_with_checksum.rflx",
            f"{SPEC_DIR}/message_size.rflx",
        ],
        skip_model_verification=True,
    )


@pytest.fixture(name="ethernet_package", scope="session")
def fixture_ethernet_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["Ethernet"]


@pytest.fixture(name="ethernet_frame_value")
def fixture_ethernet_frame_value(ethernet_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ethernet_package["Frame"]


@pytest.fixture(name="icmp_package", scope="session")
def fixture_icmp_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["ICMP"]


@pytest.fixture(name="icmp_message_value")
def fixture_icmp_message_value(icmp_package: pyrflx.Package) -> pyrflx.MessageValue:
    return icmp_package["Message"]


@pytest.fixture(name="icmp_message")
def fixture_icmp_message(icmp_package: pyrflx.Package) -> model.Message:
    # pylint: disable = protected-access
    return icmp_package["Message"]._type


@pytest.fixture(name="icmp_checksum_message_value")
def fixture_icmp_checksum_message_value(icmp_message: model.Message) -> pyrflx.MessageValue:
    return pyrflx.MessageValue(
        icmp_message.copy(
            structure=[
                model.Link(
                    l.source,
                    l.target,
                    condition=expr.And(l.condition, expr.ValidChecksum("Checksum")),
                )
                if l.target == model.FINAL
                else l
                for l in icmp_message.structure
            ],
            aspects={
                ID("Checksum"): {
                    ID("Checksum"): [
                        expr.ValueRange(
                            expr.First("Tag"), expr.Sub(expr.First("Checksum"), expr.Number(1))
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


@pytest.fixture(name="icmp_checksum_message_first")
def fixture_icmp_checksum_message_first(icmp_message: model.Message) -> pyrflx.MessageValue:
    return pyrflx.MessageValue(
        icmp_message.copy(
            structure=[
                model.Link(
                    l.source,
                    l.target,
                    condition=expr.And(l.condition, expr.ValidChecksum("Checksum")),
                )
                if l.target == model.FINAL
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


@pytest.fixture(name="ipv4_package", scope="session")
def fixture_ipv4_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["IPv4"]


@pytest.fixture(name="ipv4_packet_value")
def fixture_ipv4_packet_value(ipv4_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ipv4_package["Packet"]


@pytest.fixture(name="ipv4_option_value")
def fixture_ipv4_option_value(ipv4_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ipv4_package["Option"]


@pytest.fixture(name="tls_record_package", scope="session")
def fixture_tls_record_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["TLS_Record"]


@pytest.fixture(name="tls_record_value")
def fixture_tls_record_value(tls_record_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tls_record_package["TLS_Record"]


@pytest.fixture(name="tls_alert_package", scope="session")
def fixture_tls_alert_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["TLS_Alert"]


@pytest.fixture(name="tls_alert_value")
def fixture_tls_alert_value(tls_alert_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tls_alert_package["Alert"]


@pytest.fixture(name="udp_package", scope="session")
def fixture_udp_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["UDP"]


@pytest.fixture(name="udp_datagram_value")
def fixture_udp_datagram_value(udp_package: pyrflx.Package) -> pyrflx.MessageValue:
    return udp_package["Datagram"]


@pytest.fixture(name="tlv_package", scope="session")
def fixture_tlv_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["TLV"]


@pytest.fixture(name="tlv_message_value")
def fixture_tlv_message_value(tlv_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tlv_package["Message"]


@pytest.fixture(name="array_message_package", scope="session")
def fixture_array_message_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["Array_Message"]


@pytest.fixture(name="array_message_value")
def fixture_array_message_value(array_message_package: pyrflx.Package) -> pyrflx.MessageValue:
    return array_message_package["Message"]


@pytest.fixture(name="message_size_package", scope="session")
def fixture_message_size_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_["Message_Size"]


@pytest.fixture(name="message_size")
def fixture_message_size(message_size_package: pyrflx.Package) -> pyrflx.MessageValue:
    return message_size_package["Msg"]
