import unittest

from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger
from rflx.pyrflx import (
    EnumValue,
    Field,
    Message,
    ModularValue,
    NotInitializedError,
    OpaqueValue,
    PyRFLX,
    RangeValue,
)


class TestPyRFLX(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "specs"

    def test_attributes(self) -> None:
        # pylint: disable=no-member
        pyrflx = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])
        self.assertTrue(hasattr(pyrflx, "TLV"))
        package_tlv = pyrflx.TLV
        self.assertTrue(hasattr(package_tlv, "Message"))

    def test_fields(self) -> None:
        # pylint: disable=no-member
        tlv: Message = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"]).TLV.Message
        self.assertEqual(tlv.fields, ["Tag", "Length", "Value", "Checksum"])
        self.assertEqual(tlv.accessible_fields, ["Tag"])
        tag_value = tlv.field_type("Tag")
        assert isinstance(tag_value, EnumValue)
        tag_value.assign("Msg_Data")
        tlv.set("Tag", tag_value)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length"])
        length_value = tlv.field_type("Length")
        assert isinstance(length_value, ModularValue)
        length_value.assign(1)
        tlv.set("Length", length_value)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        value = tlv.field_type("Value")
        assert isinstance(value, OpaqueValue)
        value.assign(b"\x01")
        tlv.set("Value", value)
        self.assertEqual(tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        tag_value.assign("Msg_Error")
        tlv.set("Tag", tag_value)
        self.assertEqual(tlv.accessible_fields, ["Tag"])

    def test_value_mod(self) -> None:
        # pylint: disable=pointless-statement
        modtype = ModularInteger("Test.Int", Number(2 ** 16))
        modvalue = ModularValue(modtype)
        self.assertFalse(modvalue.initialized)
        with self.assertRaises(NotInitializedError):
            modvalue.value
        with self.assertRaises(NotInitializedError):
            modvalue.expr
        modvalue.assign(128)
        self.assertTrue(modvalue.initialized)
        with self.assertRaises(ValueError):
            modvalue.assign(2 ** 16)
        with self.assertRaises(ValueError):
            modvalue.assign(-1)

    def test_value_range(self) -> None:
        # pylint: disable=pointless-statement
        rangetype = RangeInteger("Test.Int", Number(8), Number(16), Number(8))
        rangevalue = RangeValue(rangetype)
        self.assertFalse(rangevalue.initialized)
        with self.assertRaises(NotInitializedError):
            rangevalue.value
        with self.assertRaises(NotInitializedError):
            rangevalue.expr
        rangevalue.assign(10)
        self.assertTrue(rangevalue.initialized)
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
