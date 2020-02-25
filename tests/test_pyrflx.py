import unittest

from rflx.expression import UNDEFINED, Expr
from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger, Type
from rflx.pyrflx.message import Field, Message
from rflx.pyrflx.package import Package
from rflx.pyrflx.pyrflx import PyRFLX
from rflx.pyrflx.typevalue import (
    EnumValue,
    IntegerValue,
    NotInitializedError,
    OpaqueValue,
    TypeValue,
)


# pylint: disable=too-many-public-methods
class TestPyRFLX(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "specs"
        self.tlv: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])["TLV"][
            "Message"
        ].new()

    def test_new_message(self) -> None:
        new_tlv = self.tlv.new()
        self.assertEqual(new_tlv, self.tlv)
        self.assertIsNot(new_tlv, self.tlv)

    def test_attributes(self) -> None:
        pyrflx = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])
        self.assertIsInstance(pyrflx["TLV"], Package)
        package_tlv = pyrflx["TLV"]
        self.assertIsInstance(package_tlv["Message"], Message)

    def test_all_fields(self) -> None:
        tlv = self.tlv.new()
        self.assertEqual(tlv.fields, ["Tag", "Length", "Value", "Checksum"])

    def test_initial_fields(self) -> None:
        tlv = self.tlv.new()
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_tag_fields(self) -> None:
        tlv = self.tlv.new()
        tlv.set("Tag", "Msg_Data")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length"])

    def test_length_fields(self) -> None:
        tlv = self.tlv.new()
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 1)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Value", b"\x01")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])

    def test_error_fields(self) -> None:
        tlv = self.tlv.new()
        tlv.set("Tag", "Msg_Error")
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_error_reset_fields(self) -> None:
        tlv = self.tlv.new()
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 1)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Tag", "Msg_Error")
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_fields_complex(self) -> None:
        tlv = self.tlv.new()
        self.assertEqual(tlv.accessible_fields, ["Tag"])
        tlv.set("Tag", "Msg_Error")
        self.assertEqual(tlv.accessible_fields, ["Tag"])
        tlv.set("Tag", "Msg_Data")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length"])
        tlv.set("Length", 1)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Value", b"\x01")
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tlv.set("Tag", "Msg_Error")
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_final(self) -> None:
        tlv = self.tlv.new()
        self.assertFalse(tlv.final)
        tlv.set("Tag", "Msg_Error")
        self.assertTrue(tlv.final)
        tlv.set("Tag", "Msg_Data")
        self.assertFalse(tlv.final)
        tlv.set("Length", 1)
        self.assertFalse(tlv.final)
        tlv.set("Value", b"\x01")
        self.assertFalse(tlv.final)
        tlv.set("Checksum", 0xFFFFFFFF)
        self.assertTrue(tlv.final)

    def test_set_fields(self) -> None:
        tlv = self.tlv.new()
        self.assertEqual(tlv.set_fields, [])
        tlv.set("Tag", "Msg_Data")
        self.assertEqual(tlv.set_fields, ["Tag"])
        tlv.set("Length", 1)
        self.assertEqual(tlv.set_fields, ["Tag", "Length"])
        tlv.set("Value", b"\x01")
        self.assertEqual(tlv.set_fields, ["Tag", "Length", "Value"])
        tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(tlv.set_fields, ["Tag", "Length", "Value", "Checksum"])

    def test_set_value(self) -> None:
        tlv = self.tlv.new()
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        v2 = b"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10"
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 8)
        tlv.set("Value", v1)
        with self.assertRaises(ValueError):
            tlv.set("Value", v2)

    def test_tlv_message(self) -> None:
        tlv = self.tlv.new()
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 8)
        tlv.set("Value", v1)
        tlv.set("Checksum", 2 ** 32 - 1)

    def test_tlv_generate(self) -> None:
        msg = self.tlv.new()
        test_payload = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        test_data = b"\x40\x08" + test_payload + b"\xff\xff\xff\xff"
        msg.set("Tag", "Msg_Data")
        msg.set("Length", 8)
        msg.set("Value", test_payload)
        msg.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(msg.binary, test_data)

    def test_tlv_binary_length(self) -> None:
        # pylint: disable=pointless-statement
        msg = self.tlv.new()
        msg.set("Tag", "Msg_Data")
        with self.assertRaises(ValueError):
            msg.binary
        msg.set("Length", 8)
        self.assertEqual(msg.binary, b"\x40\x08")

    def test_tlv_value(self) -> None:
        tlv = self.tlv.new()
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        tlv.set("Tag", "Msg_Data")
        tlv.set("Length", 8)
        tlv.set("Value", v1)
        tlv.set("Checksum", 2 ** 32 - 1)
        self.assertEqual(tlv.get("Tag").value, "Msg_Data")
        self.assertEqual(tlv.get("Length").value, 8)
        self.assertEqual(tlv.get("Value").value, v1)
        self.assertEqual(tlv.get("Checksum").value, 0xFFFFFFFF)

    def test_tlv_get_invalid_field(self) -> None:
        tlv = self.tlv.new()
        with self.assertRaises(IndexError):
            tlv.get("nofield")

    def test_tlv_set_invalid_field(self) -> None:
        tlv = self.tlv.new()
        with self.assertRaises(RuntimeError):
            tlv.set("Checksum", 8)

    def test_tlv_invalid_value(self) -> None:
        tlv = self.tlv.new()
        with self.assertRaises(TypeError):
            tlv.set("Tag", 1)
        tlv.set("Tag", "Msg_Data")
        with self.assertRaises(TypeError):
            tlv.set("Length", "blubb")

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
        self.assertEqual(modvalue.value, 128)
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
        self.assertEqual(rangevalue.value, 10)
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
        self.assertEqual(enumvalue.value, "One")
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
        self.assertEqual(opaquevalue.value, b"\x01\x02")
        self.assertEqual(opaquevalue.length, 16)
        self.assertEqual(opaquevalue.binary, "0000000100000010")

    def test_value_equal(self) -> None:
        ov = OpaqueValue(Opaque())
        enumtype = Enumeration("Test.Enum", {"One": Number(1), "Two": Number(2)}, Number(8), False)
        ev = EnumValue(enumtype)
        rangetype = RangeInteger("Test.Int", Number(8), Number(16), Number(8))
        rv = IntegerValue(rangetype)
        modtype = ModularInteger("Test.Int", Number(2 ** 16))
        mv = IntegerValue(modtype)
        mv2 = IntegerValue(modtype)
        self.assertEqual(ov, ov)
        self.assertEqual(ev, ev)
        self.assertEqual(rv, rv)
        self.assertEqual(mv, mv)
        self.assertNotEqual(ev, rv)
        self.assertEqual(mv, mv2)
        mv.assign(2)
        self.assertNotEqual(mv, mv2)
        mv2.assign(10)
        self.assertNotEqual(mv, mv2)
        mv.assign(10)
        self.assertEqual(mv, mv2)
        rv.assign(10)
        self.assertNotEqual(mv, rv)

    def test_value_invalid(self) -> None:
        class TestType(Type):
            @property
            def size(self) -> Expr:
                return UNDEFINED

            def constraints(self, name: str, proof: bool = False) -> Expr:
                return UNDEFINED

        t = TestType("Test.Type")
        with self.assertRaises(ValueError):
            TypeValue.construct(t)

    def test_field_equal(self) -> None:
        f1 = Field("f")
        f2 = Field("f")
        self.assertEqual(f1, f2)
        self.assertEqual(f1, f1)
        f2.typeval.assign(b"", True)
        self.assertNotEqual(f1, f2)
        self.assertNotEqual(f1, None)

    def test_package_name(self) -> None:
        p = Package("Test")
        self.assertEqual(p.name, "Test")
