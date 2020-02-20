from typing import Dict, List, Mapping

import rflx.model as model
from rflx.expression import TRUE, Attribute, Expr, Number, Variable, Name

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

    def field(self, fld: str) -> TypeValue:
        return self.__fields[fld]

    def __valid_field(self, fld: str) -> bool:
        if fld not in self.__fields:
            return False
        field_values: Mapping[Name, Expr] = {
            Variable(k) : v.expr for k, v in self.__fields.items()
            if isinstance(v, ScalarValue) and v.initialized}
        condition = self.__model.field_condition(model.Field(fld))
        return condition.simplified(field_values) == TRUE

    def fields(self) -> List[str]:
        return [f for f in self.__fields if self.__valid_field(f)]
