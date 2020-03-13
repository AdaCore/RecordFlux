from abc import ABC, abstractmethod, abstractproperty
from typing import Any, Mapping

from rflx.common import generic_repr
from rflx.expression import TRUE, Expr, Name, Variable
from rflx.model import Enumeration, Integer, Number, Opaque, Scalar, Type


class NotInitializedError(Exception):
    pass


class TypeValue(ABC):

    _value: Any = None

    def __init__(self, vtype: Type) -> None:
        self._type = vtype

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self._value == other._value and self._type == other._type
        return NotImplemented

    @property
    def initialized(self) -> bool:
        return self._value is not None

    def _raise_initialized(self) -> None:
        if not self.initialized:
            raise NotInitializedError("value not initialized")

    def clear(self) -> None:
        self._value = None

    @abstractmethod
    def assign(self, value: Any, check: bool) -> None:
        raise NotImplementedError

    @abstractmethod
    def assign_bitvalue(self, value: Any, check: bool) -> None:
        raise NotImplementedError

    @abstractproperty
    def value(self) -> Any:
        raise NotImplementedError

    @abstractproperty
    def binary(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def accepted_type(self) -> type:
        raise NotImplementedError

    @abstractproperty
    def literals(self) -> Mapping[Name, Expr]:
        raise NotImplementedError

    @classmethod
    def construct(cls, vtype: Type) -> "TypeValue":
        if isinstance(vtype, Integer):
            return IntegerValue(vtype)
        if isinstance(vtype, Enumeration):
            return EnumValue(vtype)
        if isinstance(vtype, Opaque):
            return OpaqueValue(vtype)
        raise ValueError("cannot construct unknown type: " + type(vtype).__name__)

    @staticmethod
    def convert_bytes_to_bitstring(msg: bytes) -> str:

        return format(int.from_bytes(msg, "big"), f"0{len(msg) * 8}b")

    @staticmethod
    def convert_bits_to_integer(bitstring: str) -> int:

        return int(bitstring, 2)


class ScalarValue(TypeValue):
    def __init__(self, vtype: Scalar) -> None:
        super().__init__(vtype)

    @abstractproperty
    def expr(self) -> Expr:
        return NotImplemented

    @abstractproperty
    def literals(self) -> Mapping[Name, Expr]:
        raise NotImplementedError

    @property
    def size(self) -> int:
        assert isinstance(self._type, Scalar)
        size_expr = self._type.size.simplified()
        assert isinstance(size_expr, Number)
        return size_expr.value


class IntegerValue(ScalarValue):

    _value: int

    def __init__(self, vtype: Integer) -> None:
        super().__init__(vtype)

    @property
    def _first(self) -> int:
        assert isinstance(self._type, Integer)
        first = self._type.first.simplified()
        assert isinstance(first, Number)
        return first.value

    @property
    def _last(self) -> int:
        assert isinstance(self._type, Integer)
        last = self._type.last.simplified()
        assert isinstance(last, Number)
        return last.value

    def assign(self, value: int, check: bool = True) -> None:
        if (
            self._type.constraints("__VALUE__", check).simplified(
                {Variable("__VALUE__"): Number(value)}
            )
            != TRUE
        ):
            raise ValueError(f"value {value} not in type range {self._first} .. {self._last}")
        self._value = value

    def assign_bitvalue(self, value: str, check: bool = True) -> None:

        self.assign(self.convert_bits_to_integer(value))

    @property
    def expr(self) -> Number:
        self._raise_initialized()
        return Number(self._value)

    @property
    def value(self) -> int:
        self._raise_initialized()
        return self._value

    @property
    def binary(self) -> str:
        self._raise_initialized()
        return format(self._value, f"0{self.size}b")

    @property
    def accepted_type(self) -> type:
        return int

    @property
    def literals(self) -> Mapping[Name, Expr]:
        return {}


class EnumValue(ScalarValue):

    _value: str

    def __init__(self, vtype: Enumeration) -> None:
        super().__init__(vtype)

    def assign(self, value: str, check: bool = True) -> None:
        assert isinstance(self._type, Enumeration)
        if value not in self._type.literals:
            raise KeyError(f"{value} is not a valid enum value")
        assert (
            isinstance(self._type, Enumeration)
            and self._type.constraints("__VALUE__", check).simplified(
                {
                    **{Variable(k): v for k, v in self._type.literals.items()},
                    **{Variable("__VALUE__"): self._type.literals[value]},
                }
            )
            == TRUE
        )
        self._value = value

    def assign_bitvalue(self, value: str, check: bool = True) -> None:

        value_as_int: int = self.convert_bits_to_integer(value)
        if not Number(value_as_int) in self.literals.values():
            raise KeyError(f"Number {value_as_int} is not a valid enum value")

        for k, v in self.literals.items():
            if v == Number(value_as_int):
                self._value = str(k.name)

    @property
    def value(self) -> str:
        self._raise_initialized()
        return self._value

    @property
    def expr(self) -> Variable:
        self._raise_initialized()
        return Variable(self._value)

    @property
    def binary(self) -> str:
        assert isinstance(self._type, Enumeration)
        self._raise_initialized()
        return format(self._type.literals[self._value].value, f"0{self.size}b")

    @property
    def accepted_type(self) -> type:
        return str

    @property
    def literals(self) -> Mapping[Name, Expr]:
        assert isinstance(self._type, Enumeration)
        return {Variable(k): v for k, v in self._type.literals.items()}


class OpaqueValue(TypeValue):

    _value: bytes

    def __init__(self, vtype: Opaque) -> None:
        super().__init__(vtype)

    def assign(self, value: bytes, check: bool = True) -> None:
        self._value = value

    def assign_bitvalue(self, value: str, check: bool = True) -> None:

        while len(value) % 8 != 0:
            value = "0" + value

        bytestring = b"".join(
            [int(value[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(value), 8)]
        )

        self._value = bytestring

    @property
    def length(self) -> int:
        self._raise_initialized()
        return len(self._value) * 8

    @property
    def value(self) -> bytes:
        self._raise_initialized()
        return self._value

    @property
    def binary(self) -> str:
        self._raise_initialized()
        return format(int.from_bytes(self._value, "big"), f"0{self.length}b")

    @property
    def accepted_type(self) -> type:
        return bytes

    @property
    def literals(self) -> Mapping[Name, Expr]:
        return {}
