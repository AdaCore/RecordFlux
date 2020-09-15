import pytest

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


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_empty() -> None:
    with pytest.raises(AssertionError, match=r"^empty identifier$"):
        ID([])


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_empty_string() -> None:
    with pytest.raises(AssertionError, match=r"^empty part in identifier$"):
        ID("")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_empty_part() -> None:
    with pytest.raises(AssertionError, match=r"^empty part in identifier$"):
        ID("A::::B")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_empty_first_part() -> None:
    with pytest.raises(AssertionError, match=r"^empty part in identifier$"):
        ID("::A::B")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_empty_last_part() -> None:
    with pytest.raises(AssertionError, match=r"^empty part in identifier$"):
        ID("A::B::")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_whitespace() -> None:
    with pytest.raises(AssertionError, match=r'^" " in identifier parts$'):
        ID("A::B C::D")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_colon() -> None:
    with pytest.raises(AssertionError, match=r'^":" in identifier parts$'):
        ID("A::B:C::D")


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


def test_id_mul_id() -> None:
    assert ID("A") * ID("B::C") == ID("A::B::C")
    assert ID("B::C") * ID("D") == ID("B::C::D")


def test_id_mul_str() -> None:
    assert "A" * ID("B::C") == ID("A::B::C")
    assert ID("B::C") * "D" == ID("B::C::D")
    assert "" * ID("B::C") == ID("B::C")
    assert ID("B::C") * "" == ID("B::C")


def test_id_name() -> None:
    assert ID("A::B::C").name == ID("C")


def test_id_parent() -> None:
    assert ID("A::B::C").parent == ID("A::B")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_parent_error() -> None:
    with pytest.raises(AssertionError, match=r"^empty identifier$"):
        ID("A").parent  # pylint: disable=expression-not-assigned


def test_id_sorted() -> None:
    assert sorted([ID("B"), ID("A")]) == [ID("A"), ID("B")]
