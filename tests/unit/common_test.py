import pytest

from rflx.common import Base


class C1(Base):
    def __init__(self, data: int) -> None:
        self.__data = data


class C2(C1):
    def __init__(self, data1: int, data2: int) -> None:
        super().__init__(data1)
        self.__data2 = data2


@pytest.mark.parametrize("left, right", [(C1(1), C1(1)), (C2(2, 2), C2(2, 2))])
def test_base_compare_equal(left: Base, right: Base) -> None:
    assert left == right


@pytest.mark.parametrize("left, right", [(C1(1), C1(2)), (C2(1, 2), C2(2, 1)), (C1(1), C2(1, 1))])
def test_base_compare_inequal(left: Base, right: Base) -> None:
    assert left != right
