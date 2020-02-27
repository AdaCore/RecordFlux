from typing import Any, Dict, List, Mapping

import rflx.model as model
from rflx.expression import FALSE, TRUE, UNDEFINED, Add, Expr, First, Length, Name, Number, Variable

from .typevalue import OpaqueValue, ScalarValue, TypeValue


class Field:
    def __init__(self, t: TypeValue):
        self.typeval = t
        self.length: Expr = UNDEFINED
        self.first: Expr = UNDEFINED

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Field):
            return (
                self.length == other.length
                and self.first == other.first
                and self.typeval == other.typeval
            )
        return NotImplemented

    def __repr__(self) -> str:
        return f"Field(typeval={self.typeval}, first={self.first}, length={self.length})"

    @property
    def set(self) -> bool:
        return (
            self.typeval.initialized
            and isinstance(self.length, Number)
            and isinstance(self.first, Number)
        )


class Message:
    def __init__(self, message_model: model.Message) -> None:
        self._model = message_model
        self._fields: Dict[str, Field] = {
            f.name: Field(TypeValue.construct(self._model.types[f])) for f in self._model.fields
        }
        self.__type_literals: Mapping[Name, Expr] = {}
        for t in [f.typeval.literals for f in self._fields.values()]:
            self.__type_literals = {**self.__type_literals, **t}
        initial = Field(OpaqueValue(model.Opaque()))
        initial.first = Number(0)
        initial.length = Number(0)
        self._fields[model.INITIAL.name] = initial
        self._preset_fields(model.INITIAL.name)

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

    def _next_field(self, fld: str) -> str:
        if fld == model.FINAL.name:
            return ""
        for l in self._model.outgoing(model.Field(fld)):
            if self.__simplified(l.condition) == TRUE:
                return l.target.name
        return ""

    def _prev_field(self, fld: str) -> str:
        if fld == model.INITIAL.name:
            return ""
        for l in self._model.incoming(model.Field(fld)):
            if self.__simplified(l.condition) == TRUE:
                return l.source.name
        return ""

    def _get_length(self, fld: str) -> Number:
        prv = self._prev_field(fld)
        for l in self._model.outgoing(model.Field(prv)):
            length = self.__simplified(l.length)
            if isinstance(length, Number):
                return length
        typeval = self._fields[fld].typeval
        if isinstance(typeval, ScalarValue):
            return Number(typeval.size)
        raise RuntimeError(f"cannot determine length of {fld}")

    def _has_length(self, fld: str) -> bool:
        prv = self._prev_field(fld)
        for l in self._model.outgoing(model.Field(prv)):
            if isinstance(self.__simplified(l.length), Number):
                return True
        return isinstance(self._fields[fld].typeval, ScalarValue)

    def _get_first(self, fld: str) -> Number:
        for l in self._model.incoming(model.Field(fld)):
            if self.__simplified(l.condition) == TRUE and l.first != UNDEFINED:
                first = self.__simplified(l.first)
                break
        else:
            prv = self._prev_field(fld)
            first = self.__simplified(Add(self._fields[prv].first, self._fields[prv].length))
        if isinstance(first, Number):
            return first
        raise RuntimeError(f"cannot determine first of {fld}")

    def _has_first(self, fld: str) -> bool:
        prv = self._prev_field(fld)
        first = self.__simplified(Add(self._fields[prv].first, self._fields[prv].length))
        return isinstance(first, Number)

    def set(self, fld: str, value: Any) -> None:
        if not isinstance(value, self._fields[fld].typeval.accepted_type):
            raise TypeError(
                f"cannot assign different types: {self._fields[fld].typeval.accepted_type.__name__}"
                f" != {type(value).__name__}"
            )
        try:
            field = self._fields[fld]
            field.first = self._get_first(fld)
            field.length = self._get_length(fld)
            self._fields[fld].typeval.assign(value, True)
            if all(
                [
                    self.__simplified(o.condition) == FALSE
                    for o in self._model.outgoing(model.Field(fld))
                ]
            ):
                self._fields[fld].typeval.clear()
                raise ValueError("value does not allow path to next field")
        except KeyError:
            raise KeyError(f"cannot access field {fld}")
        if isinstance(field.typeval, OpaqueValue) and field.typeval.length != field.length.value:
            flength = field.typeval.length
            field.typeval.clear()
            raise ValueError(f"invalid data length: {field.length.value} != {flength}")
        self._preset_fields(fld)

    def _preset_fields(self, fld: str) -> None:
        nxt = self._next_field(fld)
        while nxt and nxt != model.FINAL.name:
            field = self._fields[nxt]
            try:
                field.first = self._get_first(nxt)
                field.length = self._get_length(nxt)
                if (
                    field.set
                    and isinstance(field.typeval, OpaqueValue)
                    and field.typeval.length != field.length.value
                ):
                    raise RuntimeError
            except RuntimeError:
                field.first = UNDEFINED
                field.length = UNDEFINED
                field.typeval.clear()
                break
            nxt = self._next_field(nxt)

    def get(self, fld: str) -> Any:
        if fld not in self.valid_fields:
            raise ValueError(f"field {fld} not valid")
        return self._fields[fld].typeval.value

    @property
    def binary(self) -> bytes:
        bits = ""
        field = self._next_field(model.INITIAL.name)
        while True:
            if not field or field == model.FINAL.name:
                break
            field_val = self._fields[field]
            if (
                not field_val.set
                or not isinstance(field_val.first, Number)
                or not field_val.first.value <= len(bits)
            ):
                break
            bits = bits[: field_val.first.value] + self._fields[field].typeval.binary
            field = self._next_field(field)
        if len(bits) % 8:
            raise ValueError(f"message length must be dividable by 8 ({len(bits)})")
        return b"".join(
            [int(bits[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(bits), 8)]
        )

    @property
    def fields(self) -> List[str]:
        return [f.name for f in self._model.fields]

    @property
    def accessible_fields(self) -> List[str]:
        nxt = self._next_field(model.INITIAL.name)
        fields: List[str] = []
        while nxt and nxt != model.FINAL.name:
            if (
                self.__simplified(self._model.field_condition(model.Field(nxt))) != TRUE
                or not self._has_first(nxt)
                or not self._has_length(nxt)
            ):
                break
            fields.append(nxt)
            nxt = self._next_field(nxt)
        return fields

    @property
    def valid_fields(self) -> List[str]:
        return [f for f in self.accessible_fields if self._fields[f].set]

    @property
    def required_fields(self) -> List[str]:
        accessible = self.accessible_fields
        valid = self.valid_fields
        return [f for f in accessible if f not in valid]

    @property
    def valid_message(self) -> bool:
        return (
            bool(self.valid_fields) and self._next_field(self.valid_fields[-1]) == model.FINAL.name
        )

    def __simplified(self, expr: Expr) -> Expr:
        field_values: Mapping[Name, Expr] = {
            **{
                Variable(k): v.typeval.expr
                for k, v in self._fields.items()
                if isinstance(v.typeval, ScalarValue) and v.set
            },
            **{Length(k): v.length for k, v in self._fields.items() if v.set},
            **{First(k): v.first for k, v in self._fields.items() if v.set},
        }
        return expr.simplified(field_values).simplified(self.__type_literals)
