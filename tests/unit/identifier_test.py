import pytest

from rflx.error import FatalError
from rflx.identifier import ID


def test_id_constructor() -> None:
    assert ID(["A"]) == ID("A")
    assert ID(["A", "B"]) == ID("A::B")
    assert ID(["A", "B", "C"]) == ID("A::B::C")
    assert ID(ID("A")) == ID("A")
    assert ID(ID("A::B")) == ID("A::B")
    assert ID(ID("A::B::C")) == ID("A::B::C")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_type() -> None:
    with pytest.raises(AssertionError, match=r'^unexpected identifier type "int"$'):
        ID(0)  # type: ignore


def test_id_invalid_empty() -> None:
    with pytest.raises(FatalError, match=r"^id: error: empty identifier$"):
        ID([])


def test_id_invalid_empty_string() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier ""$'):
        ID("")


def test_id_invalid_empty_part() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier "A::::B"$'):
        ID("A::::B")


def test_id_invalid_empty_first_part() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier "::A::B"$'):
        ID("::A::B")


def test_id_invalid_empty_last_part() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier "A::B::"$'):
        ID("A::B::")


def test_id_invalid_whitespace() -> None:
    with pytest.raises(FatalError, match=r'^id: error: " " in identifier parts of "A::B C::D"$'):
        ID("A::B C::D")


def test_id_invalid_colon() -> None:
    with pytest.raises(FatalError, match=r'^id: error: ":" in identifier parts of "A::B:C::D"$'):
        ID("A::B:C::D")


def test_id_eq() -> None:
    assert ID("A") == ID("a")


def test_id_hash() -> None:
    assert hash(ID("A")) == hash(ID("a"))


def test_id_str() -> None:
    assert str(ID("A::B::C")) == "A::B::C"


def test_id_add() -> None:
    assert ID("A") + ID("B::C") == ID("AB::C")
    assert ID("B::C") + ID("D") == ID("B::CD")


def test_id_add_str() -> None:
    assert "A" + ID("B::C") == ID("AB::C")
    assert ID("B::C") + "D" == ID("B::CD")
    assert ID("B::C") + "" == ID("B::C")
    assert "" + ID("B::C") == ID("B::C")
    assert "A.B" + ID("C") == ID("A::BC")
    assert ID("A") + "B.C" == ID("AB::C")


def test_id_mul_id() -> None:
    assert ID("A") * ID("B::C") == ID("A::B::C")
    assert ID("B::C") * ID("D") == ID("B::C::D")


def test_id_mul_str() -> None:
    assert "A" * ID("B::C") == ID("A::B::C")
    assert ID("B::C") * "D" == ID("B::C::D")
    assert "" * ID("B::C") == ID("B::C")
    assert ID("B::C") * "" == ID("B::C")
    assert "A.B" * ID("C") == ID("A::B::C")
    assert ID("A") * "B.C" == ID("A::B::C")


def test_id_name() -> None:
    assert ID("A::B::C").name == ID("C")


def test_id_parent() -> None:
    assert ID("A::B::C").parent == ID("A::B")


def test_id_parent_error() -> None:
    with pytest.raises(FatalError, match=r"^id: error: empty identifier$"):
        ID("A").parent  # noqa: B018


def test_id_flat() -> None:
    assert ID("A::B::C").flat == "A_B_C"


def test_id_sorted() -> None:
    assert sorted([ID("B"), ID("A")]) == [ID("A"), ID("B")]
