from typing import Callable, Dict, List, Mapping

import z3

from rflx.contract import require
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import (
    Attribute,
    Channel,
    Declaration,
    Expr,
    Name,
    Not,
    Precedence,
    Relation,
    Variable,
    VariableDeclaration,
    substitution,
)
from rflx.identifier import ID, StrID


class FSMAttribute(Attribute):
    def variables(self) -> List["Variable"]:
        return self.prefix.variables()


class Valid(FSMAttribute):
    pass


class Present(FSMAttribute):
    pass


class Head(FSMAttribute):
    pass


class Opaque(Attribute):
    pass


class Quantifier(Expr):
    def __init__(
        self, quantifier: StrID, iteratable: Expr, predicate: Expr, location: Location = None
    ) -> None:
        super().__init__(location)
        self.quantifier = ID(quantifier)
        self.iterable = iteratable
        self.predicate = predicate
        self.symbol: str = ""

    def __str__(self) -> str:
        return f"for {self.symbol} {self.quantifier} in {self.iterable} => {self.predicate}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def simplified(self) -> Expr:
        return Quantifier(
            self.quantifier, self.iterable.simplified(), self.predicate.simplified(), self.location,
        )

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Quantifier):
            return expr.__class__(
                expr.quantifier,
                expr.iterable.substituted(func),
                expr.predicate.substituted(func),
                expr.location,
            )
        return expr

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        quantifier: Mapping[ID, Declaration] = {self.quantifier: VariableDeclaration()}
        self.iterable.validate({**declarations, **quantifier})
        self.predicate.validate({**declarations, **quantifier})

    def variables(self) -> List["Variable"]:
        return [
            v
            for v in self.predicate.variables() + self.iterable.variables()
            if v.identifier != self.quantifier
        ]


class ForSome(Quantifier):
    symbol: str = "some"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class ForAll(Quantifier):
    symbol: str = "all"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

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
        self.name = ID(name)
        self.arguments = arguments
        self.error = RecordFluxError()

    def __str__(self) -> str:
        arguments = ", ".join([f"{a}" for a in self.arguments])
        return f"{self.name} ({arguments})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        first = [a if isinstance(a, ID) else a.simplified() for a in self.arguments[0:1]]
        arguments = [
            *first,
            *[a.simplified() for a in self.arguments[1:]],
        ]
        return SubprogramCall(self.name, arguments, self.location)

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, SubprogramCall):
            return expr.__class__(
                expr.name, [a.substituted(func) for a in expr.arguments], expr.location
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def __validate_channel(self, declarations: Mapping[ID, Declaration]) -> None:
        channel_id = self.arguments[0]
        if not isinstance(channel_id, Variable):
            fail(
                f'invalid channel ID type in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )
        assert isinstance(channel_id, Variable)
        if channel_id.identifier not in declarations:
            fail(
                f'undeclared channel "{channel_id}" in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

        assert isinstance(channel_id, Variable)
        channel = declarations[channel_id.identifier]
        if not isinstance(channel, Channel):
            fail(
                f'invalid channel type in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )

        assert isinstance(channel, Channel)
        channel.reference()
        if self.name.name in map(ID, ["Write", "Call"]) and not channel.writable:
            self.error.append(
                f'channel "{channel_id}" not writable in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )
        if self.name.name in map(ID, ["Call", "Read", "Data_Available"]) and not channel.readable:
            self.error.append(
                f'channel "{channel_id}" not readable in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )
        for a in self.arguments[1:]:
            a.validate(declarations)

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if len(self.arguments) < 1:
            fail(
                f'no channel argument in call to "{self.name}"',
                Subsystem.SESSION,
                Severity.ERROR,
                self.location,
            )
        if self.name.name in map(ID, ["Read", "Write", "Call", "Data_Available"]):
            self.__validate_channel(declarations)
        else:
            if self.name not in map(ID, ["Append", "Extend"]):
                if self.name not in declarations:
                    fail(
                        f'undeclared subprogram "{self.name}" called',
                        Subsystem.SESSION,
                        Severity.ERROR,
                        self.location,
                    )
                declarations[self.name].reference()
            for a in self.arguments:
                try:
                    a.validate(declarations)
                except RecordFluxError as e:
                    self.error.extend(e)
        self.error.propagate()

    def variables(self) -> List["Variable"]:
        result = []
        for t in self.arguments:
            result.extend(t.variables())
        return result


class Conversion(Expr):
    def __init__(self, name: StrID, argument: Expr, location: Location = None) -> None:
        super().__init__(location)
        self.name = ID(name)
        self.argument = argument

    def __str__(self) -> str:
        return f"{self.name} ({self.argument})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Conversion):
            return expr.__class__(self.name, self.argument.substituted(func))
        return expr

    def simplified(self) -> Expr:
        return Conversion(self.name, self.argument.simplified(), self.location)

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        self.argument.validate(declarations)

    def variables(self) -> List["Variable"]:
        return self.argument.variables()


class Field(Expr):
    def __init__(self, expression: Expr, field: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.expression = expression
        self.field = ID(field)

    def __str__(self) -> str:
        return f"{self.expression}.{self.field}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return Field(self.expression.simplified(), self.field, self.location)

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Field):
            return expr.__class__(expr.expression.substituted(func), expr.field, expr.location,)
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        self.expression.validate(declarations)

    def variables(self) -> List["Variable"]:
        return self.expression.variables()


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
        self.iterator = ID(iterator)
        self.array = array
        self.selector = selector
        self.condition = condition

    def __str__(self) -> str:
        return f"[for {self.iterator} in {self.array} => {self.selector} when {self.condition}]"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return Comprehension(
            self.iterator,
            self.array.simplified(),
            self.selector.simplified(),
            self.condition.simplified(),
            self.location,
        )

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Comprehension):
            return expr.__class__(
                expr.iterator,
                expr.array.substituted(func),
                expr.selector.substituted(func),
                expr.condition.substituted(func),
                expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        decls: Mapping[ID, Declaration] = {
            **declarations,
            self.iterator: VariableDeclaration(),
        }
        self.array.validate(decls)
        self.selector.validate(decls)
        self.condition.validate(decls)

    def variables(self) -> List["Variable"]:
        return [
            v
            for v in self.array.variables() + self.selector.variables() + self.condition.variables()
            if v.identifier != self.iterator
        ]


class MessageAggregate(Expr):
    def __init__(self, name: StrID, data: Dict[StrID, Expr], location: Location = None) -> None:
        super().__init__(location)
        self.name = ID(name)
        self.data = {ID(k): v for k, v in data.items()}

    def __str__(self) -> str:
        data = ", ".join([f"{k} => {self.data[k]}" for k in self.data])
        return f"{self.name}'({data})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return MessageAggregate(
            self.name, {k: self.data[k].simplified() for k in self.data}, self.location
        )

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, MessageAggregate):
            return expr.__class__(
                expr.name, {k: expr.data[k].substituted(func) for k in expr.data}, expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        for k in self.data:
            self.data[k].validate(declarations)

    def variables(self) -> List["Variable"]:
        result = []
        for v in self.data.values():
            result.extend(v.variables())
        return result


class Binding(Expr):
    def __init__(self, expr: Expr, data: Dict[StrID, Expr], location: Location = None) -> None:
        super().__init__(location)
        self.expr = expr
        self.data = {ID(k): v for k, v in data.items()}

    def __str__(self) -> str:
        data = ", ".join(["{k} = {v}".format(k=k, v=self.data[k]) for k in self.data])
        return f"{self.expr} where {data}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        facts: Mapping[Name, Expr] = {Variable(k): self.data[k].simplified() for k in self.data}
        return self.expr.substituted(mapping=facts).simplified()

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Binding):
            return expr.__class__(
                expr.expr, {k: self.data[k].substituted(func) for k in expr.data}, expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return self.simplified().variables()


class String(Expr):
    def __init__(self, data: str, location: Location = None) -> None:
        super().__init__(location)
        self.data = data

    def __str__(self) -> str:
        return f'"{self.data}"'

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return self

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        return self

    @property
    def precedence(self) -> Precedence:
        return Precedence.undefined

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        pass
