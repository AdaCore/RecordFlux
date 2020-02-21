from typing import Dict, List, Mapping

import rflx.model as model
from rflx.expression import TRUE, Attribute, Expr, Number, Variable, Name
from rflx.common import unique

from .typevalue import TypeValue, ScalarValue, RangeValue, ModularValue, EnumValue, OpaqueValue


class Message:
    def __init__(self, message_model: model.Message) -> None:
        self.__model = message_model
        self.__fields: Dict[str, TypeValue] = {
            f.name: TypeValue.construct(self.__model.types[f]) for
            f in self.__model.all_fields[1:-1]}

    def new(self) -> "Message":
        m = Message(self.__model)
        return m

    def field_type(self, fld: str) -> TypeValue:
        return TypeValue.construct(self.__model.types[model.Field(fld)])

    def set(self, fld: str, typedvalue: TypeValue, check: bool = True) -> None:
        if typedvalue.type != self.__fields[fld].type:
            raise TypeError(f"cannot assign different types: {repr(typedvalue.type)}"
                            f"!= {repr(self.__fields[fld].type)}")
        if not typedvalue.initialized:
            raise ValueError("cannot assign uninitialized value")
        if check:
            if isinstance(typedvalue, OpaqueValue):
                for l in self.__model.incoming(model.Field(fld)):
                    if self.__accessible_field(l.source.name):
                        length = l.length.simplified(self.__field_values)
                        assert isinstance(length, Number)
                        if typedvalue.length == length.value:
                            self.__fields[fld] = typedvalue
                            break
                        raise ValueError(f"different length: "
                                         f"{typedvalue.length} != {length.value}")
                else:
                    raise RuntimeError(f"cannot assign inaccessible field: {fld}")
        self.__fields[fld] = typedvalue

    @property
    def fields(self) -> List[str]:
        return list(self.__fields.keys())

    @property
    def accessible_fields(self) -> List[str]:
        return [f for f in self.fields if self.__accessible_field(f)]

    @property
    def __field_values(self) -> Mapping[Name, Expr]:
        return {Variable(k) : v.expr for k, v in self.__fields.items()
                if isinstance(v, ScalarValue) and v.initialized}

    def __accessible_field(self, fld: str) -> bool:
        if model.Field(fld) == model.INITIAL:
            return True
        if fld not in self.__fields:
            return False
        condition = self.__model.field_condition(model.Field(fld))
        incoming = self.__model.incoming(model.Field(fld))
        for l in incoming:
            if l.source == model.INITIAL:
                return True
        initialized_req: List[str] = [v.representation for v in condition.variables()]
        return (condition.simplified(self.__field_values) == TRUE
                and all(self.__fields[ir].initialized for ir in initialized_req
                        if ir in self.__fields))
