from typing import Dict, List

import rflx.model as model
from rflx.expression import TRUE, Attribute, Expr, Number


class Message:
    def __init__(self, message_model: model.Message) -> None:
        self.__model = message_model
        self.__fields = {f.name: None for f in self.__model.all_fields[1:-1]}

    def new(self) -> "Message":
        m = Message(self.__model)
        return m

    def __predef_variables(self, field: str) -> Dict[Attribute, Expr]:
        length: Dict[Attribute, Expr] = {}
        for pred in self.__model.definite_predecessors(model.Field(field)):
            for link in self.__model.incoming(pred):
                if link.length == model.UNDEFINED:
                    length[model.Length(pred.name)] = self.__model.types[pred].size
                else:
                    raise NotImplementedError
        return length

    def __valid(self, field: str) -> bool:
        try:
            expr = self.__model.field_condition(model.Field(field))
            return (
                expr.simplified(
                    {
                        **{
                            v: Number(self.__fields[v.name])
                            for v in expr.variables()
                            if isinstance(self.__fields[v.name], int)
                        },
                        **self.__predef_variables(field),
                    }
                )
                == TRUE
            )
        except TypeError:
            return False

    def set(self, field: str, value: int) -> None:
        if self.__valid(field):
            self.__fields[field] = value

    def __valid_fields(self, field: model.Field) -> List[model.Field]:
        fields = [
            f.target
            for f in self.__model.outgoing(field)
            if self.__valid(f.target.name) and f.target != model.FINAL
        ]
        source_fields = []
        for f in fields:
            source_fields.extend(self.__valid_fields(f))
        fields.extend(source_fields)
        return fields

    def fields(self) -> List[str]:
        return list({f.name for f in self.__valid_fields(model.INITIAL)})
