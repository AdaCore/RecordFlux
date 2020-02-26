import unittest
from tempfile import TemporaryDirectory

from rflx.expression import UNDEFINED, Expr
from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger, Type
from rflx.pyrflx import (
    EnumValue,
    Field,
    IntegerValue,
    Message,
    NotInitializedError,
    OpaqueValue,
    Package,
    PyRFLX,
    TypeValue,
)


# pylint: disable=too-many-public-methods
class TestPyRFLX(unittest.TestCase):

    testdir: str
    package: Package

    @classmethod
    def setUpClass(cls) -> None:
        cls.testdir = "tests"
        cls.package = PyRFLX([f"{cls.testdir}/tlv_with_checksum.rflx"])["TLV"]

    def setUp(self) -> None:
        self.tlv = self.package["Message"]

    def test_file_not_found(self) -> None:
        with self.assertRaises(FileNotFoundError):
            with TemporaryDirectory() as tmpdir:
                PyRFLX([f"{tmpdir}/test.rflx"])

    def test_message_eq(self) -> None:
        m1 = self.package["Message"]
        self.assertEqual(m1, self.package["Message"])
        self.assertIsNot(m1, self.package["Message"])
        self.assertEqual(self.package["Message"], self.package["Message"])
        self.assertIsNot(self.package["Message"], self.package["Message"])
        self.assertNotEqual(m1, None)

    def test_attributes(self) -> None:
        pyrflx = PyRFLX([f"{self.testdir}/tlv_with_checksum.rflx"])
        self.assertIsInstance(pyrflx["TLV"], Package)
        package_tlv = pyrflx["TLV"]
        self.assertIsInstance(package_tlv["Message"], Message)

    def test_all_fields(self) -> None:
        self.assertEqual(self.tlv.fields, ["Tag", "Length", "Value", "Checksum"])

    def test_initial_fields(self) -> None:
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])

    def test_tag_fields(self) -> None:
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length"])

    def test_length_fields(self) -> None:
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 1)
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        self.tlv.set("Value", b"\x01")
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])

    def test_error_fields(self) -> None:
        self.tlv.set("Tag", "Msg_Error")
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])

    def test_error_reset_fields(self) -> None:
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 1)
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        self.tlv.set("Tag", "Msg_Error")
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])

    def test_fields_complex(self) -> None:
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])
        self.tlv.set("Tag", "Msg_Error")
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length"])
        self.tlv.set("Length", 1)
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        self.tlv.set("Value", b"\x01")
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        self.tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(self.tlv.accessible_fields, ["Tag", "Length", "Value", "Checksum"])
        self.tlv.set("Tag", "Msg_Error")
        self.assertEqual(self.tlv.accessible_fields, ["Tag"])

    def test_valid_message(self) -> None:
        self.assertFalse(self.tlv.valid_message)
        self.tlv.set("Tag", "Msg_Error")
        self.assertTrue(self.tlv.valid_message)
        self.tlv.set("Tag", "Msg_Data")
        self.assertFalse(self.tlv.valid_message)
        self.tlv.set("Length", 1)
        self.assertFalse(self.tlv.valid_message)
        self.tlv.set("Value", b"\x01")
        self.assertFalse(self.tlv.valid_message)
        self.tlv.set("Checksum", 0xFFFFFFFF)
        self.assertTrue(self.tlv.valid_message)

    def test_valid_fields(self) -> None:
        self.assertEqual(self.tlv.valid_fields, [])
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv.valid_fields, ["Tag"])
        self.tlv.set("Length", 1)
        self.assertEqual(self.tlv.valid_fields, ["Tag", "Length"])
        self.tlv.set("Value", b"\x01")
        self.assertEqual(self.tlv.valid_fields, ["Tag", "Length", "Value"])
        self.tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(self.tlv.valid_fields, ["Tag", "Length", "Value", "Checksum"])

    def test_set_value(self) -> None:
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        v2 = b"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10"
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 8)
        self.tlv.set("Value", v1)
        with self.assertRaisesRegex(ValueError, "invalid data length: 64 != 80"):
            self.tlv.set("Value", v2)

    def test_tlv_message(self) -> None:
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 8)
        self.tlv.set("Value", v1)
        self.tlv.set("Checksum", 2 ** 32 - 1)

    def test_tlv_generate(self) -> None:
        test_payload = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        test_data = b"\x40\x08" + test_payload + b"\xff\xff\xff\xff"
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 8)
        self.tlv.set("Value", test_payload)
        self.tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(self.tlv.binary, test_data)

    def test_tlv_binary_length(self) -> None:
        # pylint: disable=pointless-statement
        self.tlv.set("Tag", "Msg_Data")
        with self.assertRaisesRegex(ValueError, r"message length must be dividable by 8 \(2\)"):
            self.tlv.binary
        self.tlv.set("Length", 8)
        self.assertEqual(self.tlv.binary, b"\x40\x08")

    def test_tlv_value(self) -> None:
        v1 = b"\x01\x02\x03\x04\x05\x06\x07\x08"
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 8)
        self.tlv.set("Value", v1)
        self.tlv.set("Checksum", 2 ** 32 - 1)
        self.assertEqual(self.tlv.get("Tag"), "Msg_Data")
        self.assertEqual(self.tlv.get("Length"), 8)
        self.assertEqual(self.tlv.get("Value"), v1)
        self.assertEqual(self.tlv.get("Checksum"), 0xFFFFFFFF)

    def test_tlv_get_invalid_field(self) -> None:
        with self.assertRaisesRegex(IndexError, r"field nofield not found"):
            self.tlv.get("nofield")

    def test_tlv_set_invalid_field(self) -> None:
        with self.assertRaisesRegex(RuntimeError, r"failed to add field Checksum"):
            self.tlv.set("Checksum", 8)

    def test_tlv_invalid_value(self) -> None:
        with self.assertRaisesRegex(TypeError, r"cannot assign different types: str != int"):
            self.tlv.set("Tag", 1)
        self.tlv.set("Tag", "Msg_Data")
        with self.assertRaisesRegex(TypeError, r"cannot assign different types: int != str"):
            self.tlv.set("Length", "blubb")

    def test_value_mod(self) -> None:
        # pylint: disable=pointless-statement
        modtype = ModularInteger("Test.Int", Number(2 ** 16))
        modvalue = IntegerValue(modtype)
        self.assertFalse(modvalue.initialized)
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            modvalue.value
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            modvalue.expr
        modvalue.assign(128)
        self.assertTrue(modvalue.initialized)
        self.assertEqual(modvalue.value, 128)
        self.assertEqual(modvalue.binary, "0000000010000000")
        with self.assertRaisesRegex(ValueError, r"value 65536 not in type range 0 .. 65535"):
            modvalue.assign(2 ** 16)
        with self.assertRaisesRegex(ValueError, r"value -1 not in type range 0 .. 65535"):
            modvalue.assign(-1)

    def test_value_range(self) -> None:
        # pylint: disable=pointless-statement
        rangetype = RangeInteger("Test.Int", Number(8), Number(16), Number(8))
        rangevalue = IntegerValue(rangetype)
        self.assertFalse(rangevalue.initialized)
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            rangevalue.value
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            rangevalue.expr
        rangevalue.assign(10)
        self.assertTrue(rangevalue.initialized)
        self.assertEqual(rangevalue.value, 10)
        self.assertEqual(rangevalue.binary, "00001010")
        with self.assertRaisesRegex(ValueError, r"value 17 not in type range 8 .. 16"):
            rangevalue.assign(17)
        with self.assertRaisesRegex(ValueError, r"value 7 not in type range 8 .. 16"):
            rangevalue.assign(7)

    def test_value_enum(self) -> None:
        # pylint: disable=pointless-statement
        enumtype = Enumeration("Test.Enum", {"One": Number(1), "Two": Number(2)}, Number(8), False)
        enumvalue = EnumValue(enumtype)
        self.assertFalse(enumvalue.initialized)
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            enumvalue.value
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            enumvalue.expr
        enumvalue.assign("One")
        self.assertTrue(enumvalue.initialized)
        self.assertEqual(enumvalue.value, "One")
        self.assertEqual(enumvalue.binary, "00000001")
        with self.assertRaisesRegex(KeyError, r"Three is not a valid enum value"):
            enumvalue.assign("Three")

    def test_value_opaque(self) -> None:
        # pylint: disable=pointless-statement
        opaquevalue = OpaqueValue(Opaque())
        self.assertFalse(opaquevalue.initialized)
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
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
        with self.assertRaisesRegex(ValueError, "cannot construct unknown type: TestType"):
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
