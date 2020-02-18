import unittest

from rflx.pyrflx import PyRFLX


class TestPyRFLX(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"

    def test_attributes(self) -> None:
        pyrflx = PyRFLX([f"{self.testdir}/message_type.rflx"])
        self.assertTrue(hasattr(pyrflx, "Test"))
        package_test = pyrflx.Test
        self.assertTrue(hasattr(package_test, "PDU"))
        self.assertTrue(hasattr(package_test, "Simple_PDU"))
        self.assertTrue(hasattr(package_test, "Empty_PDU"))

    def test_fields(self) -> None:
        simple_pdu = PyRFLX([f"{self.testdir}/message_type.rflx"]).Test.Simple_PDU.new()
        self.assertEqual(simple_pdu.fields(), ["Bar", "Baz"])
        pdu = PyRFLX([f"{self.testdir}/message_type.rflx"]).Test.PDU.new()
        self.assertEqual(pdu.fields(), ["Foo"])
