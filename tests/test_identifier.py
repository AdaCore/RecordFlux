from unittest import TestCase

from rflx.identifier import ID


class TestIdentifier(TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_id_constructor(self) -> None:
        self.assertEqual(ID(["A"]), ID("A"))
        self.assertEqual(ID(["A", "B"]), ID("A.B"))
        self.assertEqual(ID(["A", "B", "C"]), ID("A.B.C"))
        self.assertEqual(ID(ID("A")), ID("A"))
        self.assertEqual(ID(ID("A.B")), ID("A.B"))
        self.assertEqual(ID(ID("A.B.C")), ID("A.B.C"))

    def test_id_invalid_type(self) -> None:
        with self.assertRaisesRegex(AssertionError, r'^unexpected identifier type "int"$'):
            ID(0)  # type: ignore

    def test_id_invalid_empty(self) -> None:
        with self.assertRaisesRegex(AssertionError, r"^empty identifier$"):
            ID("")

    def test_id_invalid_empty_part(self) -> None:
        with self.assertRaisesRegex(AssertionError, r'^empty part in identifier "A..B"$'):
            ID("A..B")

    def test_id_invalid_empty_first_part(self) -> None:
        with self.assertRaisesRegex(AssertionError, r'^empty part in identifier ".A.B"$'):
            ID(".A.B")

    def test_id_invalid_empty_last_part(self) -> None:
        with self.assertRaisesRegex(AssertionError, r'^empty part in identifier "A.B."$'):
            ID("A.B.")

    def test_id_invalid_whitespace(self) -> None:
        with self.assertRaisesRegex(AssertionError, r'^whitespace in identifier "A.B C.D"$'):
            ID("A.B C.D")

    def test_id_str(self) -> None:
        self.assertEqual(str(ID("A.B.C")), "A.B.C")

    def test_id_add(self) -> None:
        self.assertEqual(ID("A") + ID("B.C"), ID("AB.C"))
        self.assertEqual(ID("B.C") + ID("D"), ID("B.CD"))

    def test_id_add_str(self) -> None:
        self.assertEqual("A" + ID("B.C"), ID("AB.C"))
        self.assertEqual(ID("B.C") + "D", ID("B.CD"))
        self.assertEqual(ID("B.C") + "", ID("B.C"))
        self.assertEqual("" + ID("B.C"), ID("B.C"))

    def test_id_mul_id(self) -> None:
        self.assertEqual(ID("A") * ID("B.C"), ID("A.B.C"))
        self.assertEqual(ID("B.C") * ID("D"), ID("B.C.D"))

    def test_id_mul_str(self) -> None:
        self.assertEqual("A" * ID("B.C"), ID("A.B.C"))
        self.assertEqual(ID("B.C") * "D", ID("B.C.D"))
        self.assertEqual("" * ID("B.C"), ID("B.C"))
        self.assertEqual(ID("B.C") * "", ID("B.C"))

    def test_id_name(self) -> None:
        self.assertEqual(ID("A.B.C").name, ID("C"))

    def test_id_parent(self) -> None:
        self.assertEqual(ID("A.B.C").parent, ID("A.B"))
