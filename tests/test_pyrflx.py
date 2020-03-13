import unittest
from tempfile import TemporaryDirectory

from rflx.expression import UNDEFINED, Expr
from rflx.model import (
    FINAL,
    INITIAL,
    Enumeration,
    ModularInteger,
    Number,
    Opaque,
    RangeInteger,
    Type,
)
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
    specdir: str
    package_tlv: Package
    package_ethernet: Package
    package_tls_record: Package
    package_tls_alert: Package
    package_icmp: Package
    package_test_odd_length: Package

    @classmethod
    def setUpClass(cls) -> None:
        cls.testdir = "tests"
        cls.specdir = "specs"
        pyrflx = PyRFLX(
            [
                f"{cls.testdir}/tlv_with_checksum.rflx",
                f"{cls.specdir}/ethernet.rflx",
                f"{cls.specdir}/tls_record.rflx",
                f"{cls.specdir}/tls_alert.rflx",
                f"{cls.specdir}/icmp.rflx",
                f"{cls.testdir}/test_odd_length.rflx",
            ]
        )
        cls.package_tlv = pyrflx["TLV"]
        cls.package_ethernet = pyrflx["Ethernet"]
        cls.package_tls_record = pyrflx["TLS_Record"]
        cls.package_tls_alert = pyrflx["TLS_Alert"]
        cls.package_icmp = pyrflx["ICMP"]
        cls.package_test_odd_length = pyrflx["TEST"]

    def setUp(self) -> None:
        self.tlv = self.package_tlv["Message"]
        self.frame = self.package_ethernet["Frame"]
        self.record = self.package_tls_record["TLS_Record"]
        self.alert = self.package_tls_alert["Alert"]
        self.icmp = self.package_icmp["Echo_Message"]
        self.odd_length = self.package_test_odd_length["Test"]

    def test_partially_supported_packages(self) -> None:
        p = PyRFLX([f"{self.testdir}/array_message.rflx"])["Test"]
        self.assertEqual([m.name for m in p], ["Foo"])

    def test_file_not_found(self) -> None:
        with self.assertRaises(FileNotFoundError):
            with TemporaryDirectory() as tmpdir:
                PyRFLX([f"{tmpdir}/test.rflx"])

    def test_message_eq(self) -> None:
        m1 = self.package_tlv["Message"]
        self.assertEqual(m1, self.package_tlv["Message"])
        self.assertIsNot(m1, self.package_tlv["Message"])
        self.assertEqual(self.package_tlv["Message"], self.package_tlv["Message"])
        self.assertIsNot(self.package_tlv["Message"], self.package_tlv["Message"])
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

    def test_tlv_change_field(self) -> None:
        self.tlv.set("Tag", "Msg_Data")
        self.tlv.set("Length", 1)
        self.tlv.set("Tag", "Msg_Data")
        self.assertIn("Length", self.tlv.valid_fields)
        self.tlv.set("Value", b"a")
        self.tlv.set("Checksum", 0)
        self.tlv.set("Length", 2)
        self.assertNotIn("Value", self.tlv.valid_fields)
        self.assertNotIn("Checksum", self.tlv.valid_fields)
        self.tlv.set("Value", b"ab")
        self.assertIn("Checksum", self.tlv.valid_fields)

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
        with self.assertRaisesRegex(ValueError, r"field nofield not valid"):
            self.tlv.get("nofield")

    def test_tlv_set_invalid_field(self) -> None:
        self.tlv.set("Tag", "Msg_Data")
        with self.assertRaisesRegex(KeyError, r"cannot access field Value"):
            self.tlv.set("Value", b"")
        with self.assertRaisesRegex(KeyError, r"cannot access field Checksum"):
            self.tlv.set("Checksum", 8)
        self.tlv.set("Tag", "Msg_Error")
        with self.assertRaisesRegex(KeyError, r"cannot access field Length"):
            self.tlv.set("Length", 8)

    def test_tlv_invalid_value(self) -> None:
        with self.assertRaisesRegex(TypeError, r"cannot assign different types: str != int"):
            self.tlv.set("Tag", 1)
        self.tlv.set("Tag", "Msg_Data")
        with self.assertRaisesRegex(TypeError, r"cannot assign different types: int != str"):
            self.tlv.set("Length", "blubb")

    def test_tlv_next(self) -> None:
        # pylint: disable=protected-access
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv._next_field(INITIAL.name), "Tag")
        self.assertEqual(self.tlv._next_field("Tag"), "Length")
        self.assertEqual(self.tlv._next_field(FINAL.name), "")

    def test_tlv_prev(self) -> None:
        # pylint: disable=protected-access
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv._prev_field("Tag"), INITIAL.name)
        self.assertEqual(self.tlv._prev_field(INITIAL.name), "")
        self.tlv.set("Tag", "Msg_Error")
        self.assertEqual(self.tlv._prev_field("Length"), "")

    def test_tlv_required_fields(self) -> None:
        self.assertEqual(self.tlv.required_fields, ["Tag"])
        self.tlv.set("Tag", "Msg_Data")
        self.assertEqual(self.tlv.required_fields, ["Length"])
        self.tlv.set("Length", 1)
        self.assertEqual(self.tlv.required_fields, ["Value", "Checksum"])
        self.tlv.set("Value", b"\x01")
        self.assertEqual(self.tlv.required_fields, ["Checksum"])
        self.tlv.set("Checksum", 0xFFFFFFFF)
        self.assertEqual(self.tlv.required_fields, [])

    def test_tlv_length_unchecked(self) -> None:
        # pylint: disable=protected-access
        self.tlv.set("Tag", "Msg_Error")
        self.assertNotIsInstance(self.tlv._get_length_unchecked("Value"), Number)
        self.tlv.set("Tag", "Msg_Data")
        self.assertNotIsInstance(self.tlv._get_length_unchecked("Value"), Number)
        self.tlv.set("Length", 1)
        self.assertIsInstance(self.tlv._get_length_unchecked("Value"), Number)

    def test_tlv_first_unchecked(self) -> None:
        # pylint: disable=protected-access
        self.tlv.set("Tag", "Msg_Error")
        self.assertNotIsInstance(self.tlv._get_first_unchecked("Checksum"), Number)
        self.tlv.set("Tag", "Msg_Data")
        self.assertNotIsInstance(self.tlv._get_first_unchecked("Checksum"), Number)
        self.tlv.set("Length", 1)
        self.assertIsInstance(self.tlv._get_first_unchecked("Checksum"), Number)

    def test_ethernet_all_fields(self) -> None:
        self.assertEqual(
            self.frame.fields,
            ["Destination", "Source", "Type_Length_TPID", "TPID", "TCI", "Type_Length", "Payload"],
        )

    def test_ethernet_initial(self) -> None:
        self.assertEqual(
            self.frame.accessible_fields, ["Destination", "Source", "Type_Length_TPID"]
        )

    def test_ethernet_set_tltpid(self) -> None:
        self.frame.set("Destination", 0)
        self.frame.set("Source", 1)
        self.frame.set("Type_Length_TPID", 0x8100)
        self.assertEqual(self.frame.valid_fields, ["Destination", "Source", "Type_Length_TPID"])
        self.assertEqual(
            self.frame.accessible_fields,
            ["Destination", "Source", "Type_Length_TPID", "TPID", "TCI", "Type_Length"],
        )
        self.frame.set("Type_Length_TPID", 64)
        self.assertEqual(self.frame.valid_fields, ["Destination", "Source", "Type_Length_TPID"])
        self.assertEqual(
            self.frame.accessible_fields,
            ["Destination", "Source", "Type_Length_TPID", "Type_Length"],
        )

    def test_ethernet_set_nonlinear(self) -> None:
        self.assertEqual(
            self.frame.accessible_fields, ["Destination", "Source", "Type_Length_TPID"]
        )
        self.frame.set("Type_Length_TPID", 0x8100)
        self.frame.set("TCI", 100)
        self.assertEqual(self.frame.valid_fields, ["Type_Length_TPID", "TCI"])

    def test_ethernet_final(self) -> None:
        self.assertFalse(self.frame.valid_message)
        self.frame.set("Destination", 0)
        self.assertFalse(self.frame.valid_message)
        self.frame.set("Source", 1)
        self.assertFalse(self.frame.valid_message)
        self.frame.set("Type_Length_TPID", 46)
        self.assertFalse(self.frame.valid_message)
        self.frame.set("Type_Length", 46)
        self.assertFalse(self.frame.valid_message)
        self.frame.set("Payload", bytes(46))
        self.assertTrue(self.frame.valid_message)

    def test_ethernet_802_3(self) -> None:
        self.frame.set("Destination", 2 ** 48 - 1)
        self.frame.set("Source", 0)
        self.frame.set("Type_Length_TPID", 46)
        self.frame.set("Type_Length", 46)
        self.frame.set(
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
        self.assertTrue(self.frame.valid_message)
        with open(f"{self.testdir}/ethernet_802.3.raw", "rb") as raw:
            self.assertEqual(self.frame.binary, raw.read())

    def test_ethernet_payload(self) -> None:
        self.frame.set("Source", 0)
        self.frame.set("Destination", 0)
        self.frame.set("Type_Length_TPID", 47)
        self.frame.set("Type_Length", 1537)
        self.assertEqual(
            self.frame.accessible_fields,
            ["Destination", "Source", "Type_Length_TPID", "Type_Length", "Payload"],
        )
        self.frame.set("Payload", bytes(46))
        self.assertTrue(self.frame.valid_message)

    def test_ethernet_invalid(self) -> None:
        self.frame.set("Destination", 2 ** 48 - 1)
        self.frame.set("Source", 0)
        self.frame.set("Type_Length_TPID", 1501)
        with self.assertRaisesRegex(ValueError, "value does not fulfill field condition"):
            self.frame.set("Type_Length", 1501)

    def test_tls_fields(self) -> None:
        self.assertEqual(self.record.accessible_fields, ["Tag", "Legacy_Record_Version", "Length"])
        self.record.set("Tag", "INVALID")
        self.record.set("Length", 3)
        self.assertEqual(
            self.record.accessible_fields, ["Tag", "Legacy_Record_Version", "Length", "Fragment"]
        )

    def test_tls_invalid_outgoing(self) -> None:
        self.record.set("Tag", "INVALID")
        with self.assertRaisesRegex(ValueError, "value does not fulfill field condition"):
            self.record.set("Length", 2 ** 14 + 1)

    def test_tls_invalid_path(self) -> None:
        self.alert.set("Level", "WARNING")
        self.alert.set("Description", "CLOSE_NOTIFY")
        self.assertTrue(self.alert.valid_message)
        self.assertEqual(self.alert.valid_fields, ["Level", "Description"])
        self.alert.set("Level", "FATAL")
        self.assertFalse(self.alert.valid_message)
        self.assertEqual(self.alert.valid_fields, ["Level"])

    def test_tls_length_unchecked(self) -> None:
        # pylint: disable=protected-access
        self.record.set("Tag", "APPLICATION_DATA")
        self.record.set("Legacy_Record_Version", "TLS_1_2")
        self.assertNotIsInstance(self.record._get_length_unchecked("Fragment"), Number)

    def test_icmp_echo_request(self) -> None:
        test_data = (
            b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
            b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
            b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
        )

        self.icmp.set("Tag", "Echo_Request")
        self.icmp.set("Code", 0)
        self.icmp.set("Checksum", 12824)
        self.icmp.set("Identifier", 5)
        self.icmp.set("Sequence_Number", 1)
        self.icmp.set(
            "Data", test_data,
        )
        self.assertEqual(self.icmp.binary, b"\x08\x00\x32\x18\x00\x05\x00\x01" + test_data)
        self.assertTrue(self.icmp.valid_message)

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
        with self.assertRaisesRegex(KeyError, r"Number 15 is not a valid enum value"):
            enumvalue.assign_bitvalue("1111", True)

    def test_value_opaque(self) -> None:
        # pylint: disable=pointless-statement
        # pylint: disable=protected-access
        opaquevalue = OpaqueValue(Opaque())
        self.assertFalse(opaquevalue.initialized)
        with self.assertRaisesRegex(NotInitializedError, "value not initialized"):
            opaquevalue.value
        opaquevalue.assign(b"\x01\x02")
        self.assertTrue(opaquevalue.initialized)
        self.assertEqual(opaquevalue.value, b"\x01\x02")
        self.assertEqual(opaquevalue.length, 16)
        self.assertEqual(opaquevalue.binary, "0000000100000010")
        opaquevalue.assign_bitvalue("1111", True)
        self.assertEqual(opaquevalue._value, b"\x0f")

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

    def test_value_clear(self) -> None:
        ov = OpaqueValue(Opaque())
        self.assertFalse(ov.initialized)
        ov.assign(b"", True)
        self.assertTrue(ov.initialized)
        ov.clear()
        self.assertFalse(ov.initialized)

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

    def test_field_eq(self) -> None:
        f1 = Field(OpaqueValue(Opaque()))
        self.assertEqual(f1, Field(OpaqueValue(Opaque())))
        f1.typeval.assign(b"", True)
        self.assertNotEqual(f1, Field(OpaqueValue(Opaque())))
        self.assertNotEqual(f1, None)

    def test_field_set(self) -> None:
        f = Field(OpaqueValue(Opaque()))
        self.assertFalse(f.set)
        f.typeval.assign(b"", True)
        self.assertFalse(f.set)
        f.first = Number(1)
        self.assertFalse(f.set)
        f.length = Number(2)
        self.assertTrue(f.set)

    def test_package_name(self) -> None:
        p = Package("Test")
        self.assertEqual(p.name, "Test")

    def test_package_iterator(self) -> None:
        self.assertEqual([m.name for m in self.package_tlv], ["Message"])

    def test_tlv_get_first_unchecked_undefined(self) -> None:
        # pylint: disable=protected-access
        self.assertEqual(self.tlv._get_first_unchecked("Length"), UNDEFINED)

    def test_check_nodes_opaque(self) -> None:
        # pylint: disable=protected-access
        self.assertFalse(self.tlv._check_nodes_opaque("Length"))
        self.assertTrue(self.tlv._check_nodes_opaque("Value"))

        self.frame.set("Destination", 2 ** 48 - 1)
        self.frame.set("Source", 0)
        self.frame.set("Type_Length_TPID", 1501)
        self.frame._fields["Type_Length"].typeval.assign(1501, True)

        self.assertTrue(self.frame._check_nodes_opaque("Payload"))

    def test_icmp_parse_binary(self) -> None:
        test_bytes = (
            b"\x08\x00\xe1\x1e\x00\x11\x00\x01\x4a\xfc\x0d\x00\x00\x00\x00\x00"
            b"\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
            b"\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f"
            b"\x30\x31\x32\x33\x34\x35\x36\x37"
        )

        self.icmp.parse_from_bytes(test_bytes)

        self.assertTrue(self.icmp.valid_message)
        self.assertEqual(self.icmp.binary, test_bytes)

    def test_ethernet_parse_binary(self) -> None:
        test_bytes = (
            b"\xe0\x28\x6d\x39\x80\x1e\x1c\x1b\x0d\xe0\xd8\xa8\x08\x00\x45\x00"
            b"\x00\x4c\x1f\x04\x40\x00\x40\x01\xe1\x6a\xc0\xa8\xbc\x3d\xac\xd9"
            b"\x10\x83\x08\x00\xe1\x26\x00\x09\x00\x01\x4a\xfc\x0d\x00\x00\x00"
            b"\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d"
            b"\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d"
            b"\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
        )

        self.frame.parse_from_bytes(test_bytes)
        print(self.frame.binary.hex())
        print(test_bytes.hex())

        self.assertTrue(self.frame.valid_message)

        self.assertEqual(self.frame.binary, test_bytes)

    def test_tlv_checksum_binary(self) -> None:
        test_bytes = b"\x01"
        self.tlv.parse_from_bytes(test_bytes)
        self.assertFalse(self.tlv.valid_message)

    def test_odd_length_binary(self) -> None:
        test_bytes = b"\x01\x02\x01\xff\xb8"
        self.odd_length.parse_from_bytes(test_bytes)
        self.assertTrue(self.odd_length.valid_message)
