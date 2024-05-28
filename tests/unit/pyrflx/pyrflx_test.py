from pathlib import Path

import pytest

from rflx.pyrflx import MessageValue, Package, PyRFLX
from tests.const import SPEC_DIR


def test_file_not_found(tmp_path: Path) -> None:
    with pytest.raises(FileNotFoundError):
        PyRFLX.from_specs([f"{tmp_path}/test.rflx"])


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
