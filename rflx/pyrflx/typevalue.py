from abc import abstractmethod, abstractproperty
from typing import Any, Union

from rflx.expression import TRUE, Expr, Variable
from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger, Scalar, Type


class NotInitializedError(Exception):
    pass


class TypeValue:

    _value: Any

    def __init__(self, vtype: Type) -> None:
        self._type = vtype
        self._initialized = False

    def __repr__(self) -> str:
        args = ", ".join([f"{k}={v}" for k, v in self.__dict__.items()])
        return f"{self.__class__.__name__}({args})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self._initialized == other._initialized
                and (
                    self._value == other._value
                    if (self._initialized and other._initialized)
                    else True
                )
                and self._type == other._type
            )
        return False

    @property
    def initialized(self) -> bool:
        return self._initialized

    def _raise_initialized(self) -> None:
        if not self._initialized:
            raise NotInitializedError("value not initialized")

    @abstractmethod
    def assign(self, value: Any, check: bool) -> None:
        raise NotImplementedError

    @abstractproperty
    def value(self) -> Any:
        return NotImplemented

    @abstractproperty
    def binary(self) -> str:
        return NotImplemented

    @abstractproperty
    def accepted_type(self) -> type:
        return NotImplemented

    @classmethod
    def construct(cls, vtype: Type) -> "TypeValue":
        if isinstance(vtype, (ModularInteger, RangeInteger)):
            return IntegerValue(vtype)
        if isinstance(vtype, Enumeration):
            return EnumValue(vtype)
        if isinstance(vtype, Opaque):
            return OpaqueValue(vtype)
        raise ValueError("cannot construct unknown type: " + repr(vtype))


class ScalarValue(TypeValue):
    def __init__(self, vtype: Scalar) -> None:
        super(ScalarValue, self).__init__(vtype)

    @abstractproperty
    def expr(self) -> Expr:
        return NotImplemented

    @property
    def size(self) -> int:
        assert isinstance(self._type, Scalar)
        size_expr = self._type.size.simplified()
        if isinstance(size_expr, Number):
            return size_expr.value
        raise TypeError("could not resolve size_expr: " + repr(size_expr))


class IntegerValue(ScalarValue):

    _value: int

    def __init__(self, vtype: Union[RangeInteger, ModularInteger]) -> None:
        super(IntegerValue, self).__init__(vtype)

    def assign(self, value: int, check: bool = True) -> None:
        if (
            self._type.constraints("value", check).simplified({Variable("value"): Number(value)})
            != TRUE
        ):
            raise ValueError("value not in type range: " + repr(value))
        self._value = value
        self._initialized = True

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


class EnumValue(ScalarValue):

    _value: str

    def __init__(self, vtype: Enumeration) -> None:
        super(EnumValue, self).__init__(vtype)

    def assign(self, value: str, check: bool = True) -> None:
        assert isinstance(self._type, Enumeration)
        if (
            self._type.constraints("value", check).simplified(
                {
                    **{Variable(k): v for k, v in self._type.literals.items()},
                    **{Variable("value"): self._type.literals[value]},
                }
            )
            != TRUE
        ):
            raise ValueError("value not in type range: " + repr(value))
        self._value = value
        self._initialized = True

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


class OpaqueValue(TypeValue):

    _value: bytes

    def __init__(self, vtype: Opaque) -> None:
        super(OpaqueValue, self).__init__(vtype)

    def assign(self, value: bytes, check: bool = True) -> None:
        self._value = value
        self._initialized = True

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
