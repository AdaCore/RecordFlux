
from typing import Any
from abc import abstractproperty, abstractmethod
from rflx.expression import TRUE, Variable, Expr
from rflx.model import Type, ModularInteger, Number, RangeInteger, Enumeration, Opaque, Scalar


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
            raise NotInitializedError

    @abstractmethod
    def assign(self, value: Any, check: bool) -> None:
        raise NotImplementedError

    @abstractproperty
    def value(self) -> Any:
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
        raise ValueError


class ScalarValue(TypeValue):

    def __init__(self, vtype: Scalar) -> None:
        super(ScalarValue, self).__init__(vtype)

    @abstractproperty
    def expr(self) -> Expr:
        return NotImplemented


class ModularValue(ScalarValue):

    __value: int

    def __init__(self, vtype: ModularInteger) -> None:
        super(ModularValue, self).__init__(vtype)

    def assign(self, value: int, check: bool = True) -> None:
        if self.type.constraints("value", check).simplified(
                {Variable("value"): Number(value)}) != TRUE:
            raise ValueError
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


class RangeValue(ScalarValue):

    __value: int

    def __init__(self, vtype: RangeInteger) -> None:
        super(RangeValue, self).__init__(vtype)

    def assign(self, value: int, check: bool = True) -> None:
        if self.type.constraints("value", check).simplified(
                {Variable("value"): Number(value)}) != TRUE:
            raise ValueError
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


class EnumValue(ScalarValue):

    __value: str

    def __init__(self, vtype: Enumeration) -> None:
        super(EnumValue, self).__init__(vtype)

    def assign(self, value: str, check: bool = True) -> None:
        assert isinstance(self.type, Enumeration)
        if self.type.constraints("value", check).simplified(
                {**{Variable(k): v for k, v in self.type.literals.items()},
                 **{Variable("value"): self.type.literals[value]}}) != TRUE:
            raise ValueError
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


class OpaqueValue(TypeValue):

    __value: bytes

    def __init__(self, vtype: Opaque) -> None:
        super(OpaqueValue, self).__init__(vtype)

    def assign(self, value: bytes, check: bool = True) -> None:
        self.__value = value
        self._initialized = True

    @property
    def value(self) -> bytes:
        self._raise_initialized()
        return self.__value
