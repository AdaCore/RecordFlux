import pytest

from rflx.common import Base


class C1(Base):
    def __init__(self, data: int) -> None:
        self.data = data


class C2(C1):
    def __init__(self, data1: int, data2: object) -> None:
        super().__init__(data1)
        self._data2 = data2


@pytest.mark.parametrize("left, right", [(C1(1), C1(1)), (C2(2, 2), C2(2, 2))])
def test_base_compare_equal(left: Base, right: Base) -> None:
    assert left == right


@pytest.mark.parametrize("left, right", [(C1(1), C1(2)), (C2(1, 2), C2(2, 1)), (C1(1), C2(1, 1))])
def test_base_compare_inequal(left: Base, right: Base) -> None:
    assert left != right


def test_base_repr(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("RFLX_TESTING", "1")

    assert repr(C2(1, {2: 3, 4: 5})) == "C2(data=1, _data2={2: 3, 4: 5})\n"

    monkeypatch.setenv("RFLX_TESTING", "")

    assert repr(C2(1, {2: 3, 4: 5})) == "\n    C2(\n        data=1,\n        _data2={2: 3, 4: 5})"
