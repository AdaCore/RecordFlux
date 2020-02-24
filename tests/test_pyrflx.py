import unittest

from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger
from rflx.pyrflx.message import Field, Message
from rflx.pyrflx.package import Package
from rflx.pyrflx.pyrflx import PyRFLX
from rflx.pyrflx.typevalue import EnumValue, IntegerValue, NotInitializedError, OpaqueValue


class TestPyRFLX(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "specs"

    def test_attributes(self) -> None:
        pyrflx = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])
        self.assertIsInstance(pyrflx["TLV"], Package)
        package_tlv = pyrflx["TLV"]
        self.assertIsInstance(package_tlv["Message"], Message)

    def test_fields(self) -> None:
        tlv: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])["TLV"]["Message"].new()
        self.assertEqual(tlv.fields, ["Tag", "Length", "Value", "Checksum"])
        self.assertEqual(tlv.accessible_fields, ["Tag"])
        tlv.set("Tag", "Msg_Data")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length"])
        tlv.set("Length", 1)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Value", b"\x01")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Tag", "Msg_Error")
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_set_value(self) -> None:
        tlv: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])["TLV"]["Message"].new()
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        v2 = b"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10"
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 8)
        tlv.set("Value", v1)
        with self.assertRaises(ValueError):
            tlv.set("Value", v2)

    def test_tlv_message(self) -> None:
        tlv: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])["TLV"]["Message"].new()
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 8)
        tlv.set("Value", v1)
        tlv.set("Checksum", 2 ** 32 - 1)

    def test_tlv_generate(self) -> None:
        test_payload = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        test_data = b"\x40\x08" + test_payload + b"\xff\xff\xff\xff"
        msg: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])["TLV"]["Message"].new()
        msg.set("Tag", "Msg_Data")
        msg.set("Length", 8)
        msg.set("Value", test_payload)
        msg.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(msg.binary, test_data)

    def test_value_mod(self) -> None:
        # pylint: disable=pointless-statement
        modtype = ModularInteger("Test.Int", Number(2 ** 16))
        modvalue = IntegerValue(modtype)
        self.assertFalse(modvalue.initialized)
        with self.assertRaises(NotInitializedError):
            modvalue.value
        with self.assertRaises(NotInitializedError):
            modvalue.expr
        modvalue.assign(128)
        self.assertTrue(modvalue.initialized)
        self.assertEqual(modvalue.binary, "0000000010000000")
        with self.assertRaises(ValueError):
            modvalue.assign(2 ** 16)
        with self.assertRaises(ValueError):
            modvalue.assign(-1)

    def test_value_range(self) -> None:
        # pylint: disable=pointless-statement
        rangetype = RangeInteger("Test.Int", Number(8), Number(16), Number(8))
        rangevalue = IntegerValue(rangetype)
        self.assertFalse(rangevalue.initialized)
        with self.assertRaises(NotInitializedError):
            rangevalue.value
        with self.assertRaises(NotInitializedError):
            rangevalue.expr
        rangevalue.assign(10)
        self.assertTrue(rangevalue.initialized)
        self.assertEqual(rangevalue.binary, "00001010")
        with self.assertRaises(ValueError):
            rangevalue.assign(17)
        with self.assertRaises(ValueError):
            rangevalue.assign(7)

    def test_value_enum(self) -> None:
        # pylint: disable=pointless-statement
        enumtype = Enumeration("Test.Enum", {"One": Number(1), "Two": Number(2)}, Number(8), False)
        enumvalue = EnumValue(enumtype)
        self.assertFalse(enumvalue.initialized)
        with self.assertRaises(NotInitializedError):
            enumvalue.value
        with self.assertRaises(NotInitializedError):
            enumvalue.expr
        enumvalue.assign("One")
        self.assertTrue(enumvalue.initialized)
        self.assertEqual(enumvalue.binary, "00000001")
        with self.assertRaises(KeyError):
            enumvalue.assign("Three")

    def test_value_opaque(self) -> None:
        # pylint: disable=pointless-statement
        opaquevalue = OpaqueValue(Opaque())
        self.assertFalse(opaquevalue.initialized)
        with self.assertRaises(NotInitializedError):
            opaquevalue.value
        opaquevalue.assign(b"\x01\x02")
        self.assertTrue(opaquevalue.initialized)
        self.assertEqual(opaquevalue.length, 16)
        self.assertEqual(opaquevalue.binary, "0000000100000010")

    def test_field_append(self) -> None:
        f = Field("f")
        f1 = f
        f2 = Field("f2")
        f1.successor = f2
        f.append(f2)
        self.assertEqual(f, f1)
        f3 = Field("f2")
        f2.successor = f3
        f.append(f3)
        self.assertEqual(f, f1)
        f2.successor = None
        f.append(f2)
        self.assertEqual(f, f1)

    def test_field_keys(self) -> None:
        f = Field("f1")
        f.append(Field("f2"))
        f.append(Field("f3"))
        self.assertEqual(f.keys(), ["f1", "f2", "f3"])

    def test_field_items(self) -> None:
        f1 = Field("f1")
        f2 = Field("f2")
        f3 = Field("f3")
        f1.append(f2)
        f1.append(f3)
        self.assertEqual(f1.items(), [f1, f2, f3])

    def test_field_getitem(self) -> None:
        f1 = Field("f1")
        f2 = Field("f2")
        f3 = Field("f3")
        f1.append(f2)
        f1.append(f3)
        self.assertEqual(f1["f1"], f1)
        self.assertEqual(f1["f2"], f2)
        self.assertEqual(f1["f3"], f3)
        self.assertEqual(f1["f2"], f2["f2"])
        self.assertEqual(f1["f3"], f2["f3"])

    def test_field_contains(self) -> None:
        f1 = Field("f1")
        f2 = Field("f2")
        f3 = Field("f3")
        f1.append(f2)
        f1.append(f3)
        self.assertTrue("f1" in f1)
        self.assertTrue("f2" in f1)
        self.assertTrue("f3" in f1)
        self.assertTrue("f2" in f2)
