from abc import abstractmethod, abstractproperty
from typing import Any

from rflx.expression import TRUE, Expr, Variable
from rflx.model import Enumeration, ModularInteger, Number, Opaque, RangeInteger, Scalar, Type


class NotInitializedError(Exception):
    pass


class TypeValue:
    def __init__(self, vtype: Type) -> None:
        self.__type = vtype
        self._initialized = False

    def __repr__(self) -> str:
        args = ", ".join([f"{k}={v}" for k, v in self.__dict__.items()])
        return f"{self.__class__.__name__}({args})"

    @property
    def type(self) -> Type:
        return self.__type

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

    @abstractmethod
    def copy(self) -> "TypeValue":
        return NotImplemented

    @classmethod
    def construct(cls, vtype: Type) -> "TypeValue":
        if isinstance(vtype, ModularInteger):
            return ModularValue(vtype)
        if isinstance(vtype, RangeInteger):
            return RangeValue(vtype)
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
        assert isinstance(self.type, Scalar)
        size_expr = self.type.size.simplified()
        if isinstance(size_expr, Number):
            return size_expr.value
        raise TypeError("could not resolve size_expr: " + repr(size_expr))


class ModularValue(ScalarValue):

    __value: int

    def __init__(self, vtype: ModularInteger) -> None:
        super(ModularValue, self).__init__(vtype)

    def assign(self, value: int, check: bool = True) -> None:
        if (
            self.type.constraints("value", check).simplified({Variable("value"): Number(value)})
            != TRUE
        ):
            raise ValueError("value not in type range: " + repr(value))
        self.__value = value
        self._initialized = True

    @property
    def value(self) -> int:
        self._raise_initialized()
        return self.__value

    @property
    def expr(self) -> Number:
        self._raise_initialized()
        return Number(self.__value)

    def copy(self) -> "ModularValue":
        assert isinstance(self.type, ModularInteger)
        t = ModularValue(self.type)
        if self._initialized:
            t.assign(self.__value)
        return t


class RangeValue(ScalarValue):

    __value: int

    def __init__(self, vtype: RangeInteger) -> None:
        super(RangeValue, self).__init__(vtype)

    def assign(self, value: int, check: bool = True) -> None:
        if (
            self.type.constraints("value", check).simplified({Variable("value"): Number(value)})
            != TRUE
        ):
            raise ValueError("value not in type range: " + repr(value))
        self.__value = value
        self._initialized = True

    @property
    def value(self) -> int:
        self._raise_initialized()
        return self.__value

    @property
    def expr(self) -> Number:
        self._raise_initialized()
        return Number(self.__value)

    def copy(self) -> "RangeValue":
        assert isinstance(self.type, RangeInteger)
        t = RangeValue(self.type)
        if self._initialized:
            t.assign(self.__value)
        return t


class EnumValue(ScalarValue):

    __value: str

    def __init__(self, vtype: Enumeration) -> None:
        super(EnumValue, self).__init__(vtype)

    def assign(self, value: str, check: bool = True) -> None:
        assert isinstance(self.type, Enumeration)
        if (
            self.type.constraints("value", check).simplified(
                {
                    **{Variable(k): v for k, v in self.type.literals.items()},
                    **{Variable("value"): self.type.literals[value]},
                }
            )
            != TRUE
        ):
            raise ValueError("value not in type range: " + repr(value))
        self.__value = value
        self._initialized = True

    @property
    def value(self) -> str:
        self._raise_initialized()
        return self.__value

    @property
    def expr(self) -> Variable:
        self._raise_initialized()
        return Variable(self.__value)

    def copy(self) -> "EnumValue":
        assert isinstance(self.type, Enumeration)
        t = EnumValue(self.type)
        if self._initialized:
            t.assign(self.__value)
        return t


class OpaqueValue(TypeValue):

    __value: bytes

    def __init__(self, vtype: Opaque) -> None:
        super(OpaqueValue, self).__init__(vtype)

    def assign(self, value: bytes, check: bool = True) -> None:
        self.__value = value
        self._initialized = True

    @property
    def length(self) -> int:
        self._raise_initialized()
        return len(self.__value) * 8

    @property
    def value(self) -> bytes:
        self._raise_initialized()
        return self.__value

    def copy(self) -> "OpaqueValue":
        assert isinstance(self.type, Opaque)
        t = OpaqueValue(self.type)
        if self._initialized:
            t.assign(self.__value)
        return t
