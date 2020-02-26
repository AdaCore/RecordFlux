from typing import Any, List, Mapping

import rflx.model as model
from rflx.expression import TRUE, UNDEFINED, Add, Expr, First, Length, Name, Number, Variable

from .typevalue import OpaqueValue, ScalarValue, TypeValue


class Field:
    def __init__(self, name: str) -> None:
        self.name = name
        self.typeval = TypeValue.construct(model.Opaque())
        self.first: Expr = UNDEFINED
        self.length: Expr = UNDEFINED

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.name == other.name
                and self.first == other.first
                and self.length == other.length
                and self.typeval == other.typeval
            )
        return NotImplemented

    def __repr__(self) -> str:
        return (
            f"Field(name={self.name}, "
            f"typeval={repr(self.typeval)}, "
            f"first={repr(self.first)}, "
            f"length={repr(self.length)})"
        )


class Message:
    def __init__(self, message_model: model.Message) -> None:
        self._model = message_model
        initial = Field(model.INITIAL.name)
        initial.first = Number(0)
        initial.length = Number(0)
        self._fields: List[Field] = [initial]

    def __copy__(self) -> "Message":
        new = Message(self._model)
        return new

    def __repr__(self) -> str:
        args = ", ".join([f"{k}={v}" for k, v in self.__dict__.items()])
        return f"{self.__class__.__name__}({args})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self._fields == other._fields and self._model == other._model
        return NotImplemented

    def __get_field(self, fld: str) -> Field:
        for f in self._fields:
            if f.name == fld:
                return f
        raise IndexError(f"field {fld} not found")

    def __field_type(self, fld: str) -> TypeValue:
        return TypeValue.construct(self._model.types[model.Field(fld)])

    def set(self, fld: str, value: Any) -> None:
        typedvalue = self.__field_type(fld)
        if not isinstance(value, typedvalue.accepted_type):
            raise TypeError(
                f"cannot assign different types: {typedvalue.accepted_type.__name__}"
                f" != {type(value).__name__}"
            )
        typedvalue.assign(value, True)
        incoming = self._model.incoming(model.Field(fld))
        for i in incoming:
            if i.source.name in [f.name for f in self._fields]:
                source = self.__get_field(i.source.name)
                self.__add_field(
                    i.target.name, typedvalue, Add(source.first, source.length), i.length
                )
                return
        raise RuntimeError(f"failed to add field {fld}")

    def get(self, fld: str) -> Any:
        return self.__get_field(fld).typeval.value

    @property
    def binary(self) -> bytes:
        bits = "".join([f.typeval.binary for f in self._fields[1:]])
        if len(bits) % 8:
            raise ValueError(f"message length must be dividable by 8 ({len(bits)})")
        return b"".join(
            [int(bits[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(bits), 8)]
        )

    @property
    def fields(self) -> List[str]:
        return [f.name for f in self._model.all_fields[1:-1]]

    @property
    def accessible_fields(self) -> List[str]:
        fields = [f.name for f in self._fields]
        added = True
        while added:
            added = False
            for l in self._model.outgoing(model.Field(fields[-1])):
                if (
                    l.target.name not in fields
                    and l.target != model.FINAL
                    and l.condition.simplified(self.__field_values) == TRUE
                    and (
                        l.length == UNDEFINED
                        or isinstance(l.length.simplified(self.__field_values), Number)
                    )
                ):
                    fields.append(l.target.name)
                    added = True
                    break
        return fields[1:]

    @property
    def valid_fields(self) -> List[str]:
        return [f.name for f in self._fields[1:]]

    @property
    def valid_message(self) -> bool:
        for o in self._model.outgoing(model.Field(self._fields[-1].name)):
            if o.target == model.FINAL and o.condition.simplified(self.__field_values) == TRUE:
                return True
        return False

    @property
    def __field_values(self) -> Mapping[Name, Expr]:
        return {
            **{
                Variable(v.name): v.typeval.expr
                for v in self._fields
                if isinstance(v.typeval, ScalarValue) and v.typeval.initialized
            },
            **{Length(v.name): v.length for v in self._fields},
            **{First(v.name): v.first for v in self._fields},
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
            assert isinstance(fld.length, Number)
            if isinstance(fld.typeval, OpaqueValue) and fld.length.value != fld.typeval.length:
                raise ValueError(f"invalid data length: {fld.length.value} != {fld.typeval.length}")
        for f in self._fields:
            if fld.name == f.name:
                self._fields = self._fields[: self._fields.index(f)]
                break
        self._fields.append(fld)
