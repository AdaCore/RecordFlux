from rflx.pyrflx import MessageValue
from rflx.pyrflx.bitstring import Bitstring


def test_message_value_bitstring(tlv_message_value: MessageValue) -> None:
    assert tlv_message_value.bitstring == Bitstring("")
    tlv_message_value.set("Tag", "Msg_Data")
    assert tlv_message_value.bitstring == Bitstring("00000001")
    tlv_message_value.set("Length", 1)
    assert tlv_message_value.bitstring == Bitstring("000000010000000000000001")
    tlv_message_value.set("Value", b"\x01")
    assert tlv_message_value.bitstring == Bitstring("00000001000000000000000100000001")
