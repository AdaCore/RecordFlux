import pytest

from rflx import expression as expr, model, pyrflx
from rflx.identifier import ID
from tests.const import SPEC_DIR


@pytest.fixture(name="pyrflx_", scope="session")
def fixture_pyrflx() -> pyrflx.PyRFLX:
    return pyrflx.PyRFLX.from_specs(
        [
            f"{SPEC_DIR}/ethernet.rflx",
            f"{SPEC_DIR}/icmp.rflx",
            f"{SPEC_DIR}/in_ethernet.rflx",
            f"{SPEC_DIR}/in_ipv4.rflx",
            f"{SPEC_DIR}/ipv4.rflx",
            f"{SPEC_DIR}/tls_alert.rflx",
            f"{SPEC_DIR}/tls_record.rflx",
            f"{SPEC_DIR}/udp.rflx",
            f"{SPEC_DIR}/sequence_message.rflx",
            f"{SPEC_DIR}/sequence_type.rflx",
            f"{SPEC_DIR}/no_conditionals.rflx",
            f"{SPEC_DIR}/null_message.rflx",
            f"{SPEC_DIR}/tlv.rflx",
            f"{SPEC_DIR}/tlv_with_checksum.rflx",
            f"{SPEC_DIR}/message_size.rflx",
            f"{SPEC_DIR}/message_type_size_condition.rflx",
            f"{SPEC_DIR}/always_valid_aspect.rflx",
            f"{SPEC_DIR}/parameterized.rflx",
            f"{SPEC_DIR}/endianness.rflx",
            f"{SPEC_DIR}/low_order.rflx",
            f"{SPEC_DIR}/aggregate_in_relation.rflx",
        ],
        skip_model_verification=True,
    )


@pytest.fixture(name="ethernet_package", scope="session")
def fixture_ethernet_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Ethernet")


@pytest.fixture(name="ethernet_frame_value")
def fixture_ethernet_frame_value(ethernet_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ethernet_package.new_message("Frame")


@pytest.fixture(name="icmp_package", scope="session")
def fixture_icmp_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("ICMP")


@pytest.fixture(name="icmp_message_value")
def fixture_icmp_message_value(icmp_package: pyrflx.Package) -> pyrflx.MessageValue:
    return icmp_package.new_message("Message")


@pytest.fixture(name="icmp_message")
def fixture_icmp_message(icmp_package: pyrflx.Package) -> model.Message:
    return icmp_package.new_message("Message")._type  # noqa: SLF001


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
            checksums={
                ID("Checksum"): [
                    expr.ValueRange(
                        expr.First("Tag"),
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


@pytest.fixture(name="ipv4_package", scope="session")
def fixture_ipv4_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("IPv4")


@pytest.fixture(name="ipv4_packet_value")
def fixture_ipv4_packet_value(ipv4_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ipv4_package.new_message("Packet")


@pytest.fixture(name="ipv4_option_value")
def fixture_ipv4_option_value(ipv4_package: pyrflx.Package) -> pyrflx.MessageValue:
    return ipv4_package.new_message("Option")


@pytest.fixture(name="tls_record_package", scope="session")
def fixture_tls_record_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("TLS_Record")


@pytest.fixture(name="tls_record_value")
def fixture_tls_record_value(tls_record_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tls_record_package.new_message("TLS_Record")


@pytest.fixture(name="tls_alert_package", scope="session")
def fixture_tls_alert_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("TLS_Alert")


@pytest.fixture(name="tls_alert_value")
def fixture_tls_alert_value(tls_alert_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tls_alert_package.new_message("Alert")


@pytest.fixture(name="udp_package", scope="session")
def fixture_udp_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("UDP")


@pytest.fixture(name="udp_datagram_value")
def fixture_udp_datagram_value(udp_package: pyrflx.Package) -> pyrflx.MessageValue:
    return udp_package.new_message("Datagram")


@pytest.fixture(name="tlv_package", scope="session")
def fixture_tlv_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("TLV")


@pytest.fixture(name="tlv_message_value")
def fixture_tlv_message_value(tlv_package: pyrflx.Package) -> pyrflx.MessageValue:
    return tlv_package.new_message("Message")


@pytest.fixture(name="sequence_message_package", scope="session")
def fixture_sequence_message_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Sequence_Message")


@pytest.fixture(name="message_sequence_value")
def fixture_sequence_message_value(sequence_message_package: pyrflx.Package) -> pyrflx.MessageValue:
    return sequence_message_package.new_message("Message_Sequence")


@pytest.fixture(name="message_sequence_refinement_value")
def fixture_sequence_message_refinement_value(
    sequence_message_package: pyrflx.Package,
) -> pyrflx.MessageValue:
    return sequence_message_package.new_message("Message_Sequence_And_Refinement")


@pytest.fixture(name="message_size_package", scope="session")
def fixture_message_size_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Message_Size")


@pytest.fixture(name="message_size_value")
def fixture_message_size_value(message_size_package: pyrflx.Package) -> pyrflx.MessageValue:
    return message_size_package.new_message("M")


@pytest.fixture(name="message_type_size_package", scope="session")
def fixture_message_type_size(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Message_Type_Size_Condition")


@pytest.fixture(name="message_type_size_value")
def fixture_message_type_size_value(
    message_type_size_package: pyrflx.Package,
) -> pyrflx.MessageValue:
    return message_type_size_package.new_message("Message")


@pytest.fixture(name="always_valid_aspect_package", scope="session")
def fixture_always_valid_aspect_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Always_Valid_Aspect")


@pytest.fixture(name="message_always_valid_aspect_value")
def fixture_always_valid_aspect_value(
    always_valid_aspect_package: pyrflx.Package,
) -> pyrflx.MessageValue:
    return always_valid_aspect_package.new_message("Message")


@pytest.fixture(name="parameterized_package", scope="session")
def fixture_parameterized_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Parameterized")


@pytest.fixture(name="endianness_package", scope="session")
def fixture_endianness_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Endianness")


@pytest.fixture(name="low_order_package", scope="session")
def fixture_low_order_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Low_Order")


@pytest.fixture(name="aggregate_in_relation_package", scope="session")
def fixture_aggregate_in_relation_package(pyrflx_: pyrflx.PyRFLX) -> pyrflx.Package:
    return pyrflx_.package("Aggregate_In_Relation")
