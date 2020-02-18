from typing import Dict, List

import z3

from rflx.error import Location
from rflx.expression import Attribute, Expr, Not, Precedence, Relation
from rflx.identifier import ID, StrID


class Valid(Attribute):
    pass


class Present(Attribute):
    pass


class Head(Attribute):
    pass


class Quantifier(Expr):
    def __init__(
        self, quantifier: StrID, iteratable: Expr, predicate: Expr, location: Location = None
    ) -> None:
        super().__init__(location)
        self.__quantifier = ID(quantifier)
        self.__iterable = iteratable
        self.__predicate = predicate
        self.symbol: str = ""

    def __str__(self) -> str:
        return f"for {self.symbol} {self.__quantifier} in {self.__iterable} => {self.__predicate}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def simplified(self) -> Expr:
        raise NotImplementedError

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class ForSome(Quantifier):
    symbol: str = "some"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def simplified(self) -> Expr:
        raise NotImplementedError

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class ForAll(Quantifier):
    symbol: str = "all"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def simplified(self) -> Expr:
        raise NotImplementedError

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Contains(Relation):
    @property
    def symbol(self) -> str:
        return " in "

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class NotContains(Relation):
    @property
    def symbol(self) -> str:
        return " not in "

    def __neg__(self) -> Expr:
        return Not(Contains(self.left, self.right))

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class SubprogramCall(Expr):
    def __init__(self, name: StrID, arguments: List[Expr], location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)
        self.__arguments = arguments

    def __str__(self) -> str:
        arguments = ", ".join([f"{a}" for a in self.__arguments])
        return f"{self.__name} ({arguments})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Field(Expr):
    def __init__(self, expression: Expr, field: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.__expression = expression
        self.__field = ID(field)

    def __str__(self) -> str:
        return f"{self.__expression}.{self.__field}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Comprehension(Expr):
    def __init__(
        self,
        iterator: StrID,
        array: Expr,
        selector: Expr,
        condition: Expr,
        location: Location = None,
    ) -> None:
        super().__init__(location)
        self.__iterator = ID(iterator)
        self.__array = array
        self.__selector = selector
        self.__condition = condition

    def __str__(self) -> str:
        return (
            f"[for {self.__iterator} in {self.__array} => "
            f"{self.__selector} when {self.__condition}]"
        )

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class MessageAggregate(Expr):
    def __init__(self, name: StrID, data: Dict[StrID, Expr], location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)
        self.__data = {ID(k): v for k, v in data.items()}

    def __str__(self) -> str:
        data = ", ".join([f"{k} => {self.__data[k]}" for k in self.__data])
        return f"{self.__name}'({data})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Binding(Expr):
    def __init__(self, expr: Expr, data: Dict[StrID, Expr], location: Location = None) -> None:
        super().__init__(location)
        self.__expr = expr
        self.__data = {ID(k): v for k, v in data.items()}

    def __str__(self) -> str:
        data = ", ".join(["{k} = {v}".format(k=k, v=self.__data[k]) for k in self.__data])
        return f"{self.__expr} where {data}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError
