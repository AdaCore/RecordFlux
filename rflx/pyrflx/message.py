from typing import List, Mapping, Union

import rflx.model as model
from rflx.common import unique
from rflx.expression import TRUE, UNDEFINED, Add, Expr, First, Length, Name, Number, Variable

from .typevalue import OpaqueValue, ScalarValue, TypeValue


class Field:
    name: str
    typeval: TypeValue
    first: Expr
    length: Expr
    successor: Union["Field", None]

    def __init__(self, name: str) -> None:
        self.name = name
        self.typeval = TypeValue.construct(model.Opaque())
        self.first = UNDEFINED
        self.length = UNDEFINED
        self.successor = None

    def __repr__(self) -> str:
        return (
            f"Field(name={self.name}, "
            f"typeval={repr(self.typeval)}, "
            f"first={repr(self.first)}, "
            f"length={repr(self.length)})"
            f" -> {repr(self.successor)}"
        )

    def append(self, succ: "Field") -> None:
        if self.name == succ.name:
            self.typeval = succ.typeval
            self.first = succ.first
            self.length = succ.length
            self.successor = succ.successor
        if self.successor:
            self.successor.append(succ)
        else:
            self.successor = succ

    def keys(self) -> List[str]:
        return [self.name] + (self.successor.keys() if self.successor else [])

    def items(self) -> List["Field"]:
        return [self] + (self.successor.items() if self.successor else [])

    def __getitem__(self, key: str) -> "Field":
        if key == self.name:
            return self
        if self.successor:
            return self.successor[key]
        raise KeyError

    def __contains__(self, key: str) -> bool:
        if key == self.name:
            return True
        if self.successor:
            return key in self.successor
        return False


class Message:
    def __init__(self, message_model: model.Message) -> None:
        self.__model = message_model
        self.__fields = Field(model.INITIAL.name)
        self.__fields.typeval = TypeValue.construct(model.Opaque())
        self.__fields.first = Number(0)
        self.__fields.length = Number(0)
        self.__fields.successor = None

    def new(self) -> "Message":
        m = Message(self.__model)
        return m

    def field_type(self, fld: str) -> TypeValue:
        return TypeValue.construct(self.__model.types[model.Field(fld)])

    def set(self, fld: str, typedvalue: TypeValue) -> None:
        if typedvalue.type != self.field_type(fld).type:
            raise TypeError(
                f"cannot assign different types: {repr(typedvalue.type)}"
                f"!= {repr(self.__fields[fld].typeval.type)}"
            )
        if not typedvalue.initialized:
            raise ValueError("cannot assign uninitialized value")
        incoming = self.__model.incoming(model.Field(fld))
        for i in incoming:
            if i.source.name in self.__fields:
                source = self.__fields[i.source.name]
                self.__add_field(
                    i.target.name, typedvalue, Add(source.first, source.length), i.length
                )
                return
        raise RuntimeError(f"failed to add field {fld}")

    @property
    def fields(self) -> List[str]:
        return [f.name for f in self.__model.all_fields[1:-1]]

    @property
    def accessible_fields(self) -> List[str]:
        fields = self.__fields.keys()
        new_fields = fields
        while new_fields:
            current_fields = new_fields
            new_fields = []
            for f in current_fields:
                new_fields.extend(self.__accessible_fields(f))
            fields += new_fields
        return list(unique(fields))[1:]

    @property
    def __field_values(self) -> Mapping[Name, Expr]:
        return {
            **{
                Variable(v.name): v.typeval.expr
                for v in self.__fields.items()
                if isinstance(v.typeval, ScalarValue) and v.typeval.initialized
            },
            **{Length(v.name): v.length for v in self.__fields.items()},
            **{First(v.name): v.first for v in self.__fields.items()},
        }

    def __add_field(
        self, name: str, typeval: TypeValue, first: Expr, length: Expr = UNDEFINED
    ) -> None:
        fld = Field(name)
        fld.typeval = typeval
        fld.first = first
        if isinstance(fld.typeval, ScalarValue):
            fld.length = Number(fld.typeval.size)
        else:
            fld.length = length.simplified(self.__field_values)
            if not isinstance(fld.length, Number):
                raise RuntimeError(f"unable to resolve length: {repr(fld.length)}")
            if isinstance(fld.typeval, OpaqueValue) and fld.length.value != fld.typeval.length:
                raise ValueError(f"invalid data length: {fld.length.value} != {fld.typeval.length}")
        self.__fields.append(fld)

    def __accessible_fields(self, fld: str) -> List[str]:
        outgoing = self.__model.outgoing(model.Field(fld))
        flds: List[str] = []
        for l in outgoing:
            if (
                l.condition.simplified(self.__field_values) == TRUE
                and (
                    l.length == UNDEFINED
                    or isinstance(l.length.simplified(self.__field_values), Number)
                )
                and l.target != model.FINAL
            ):
                flds.append(l.target.name)
        return flds
