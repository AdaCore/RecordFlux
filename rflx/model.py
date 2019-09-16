import itertools
from abc import ABC, abstractproperty
from math import log
from typing import Dict, Mapping, NamedTuple, Sequence, Set, Tuple

from rflx.expression import (TRUE, UNDEFINED, And, Expr, GreaterEqual, LessEqual, Number, Or, Pow,
                             Sub, Variable)


class Element(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')


class Type(Element):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractproperty
    def size(self) -> Expr:
        raise NotImplementedError

    @property
    def constraints(self) -> Expr:
        return TRUE

    @property
    def base_name(self) -> str:
        return f'{self.name}_Base'


class Scalar(Type):
    @property
    def size(self) -> Expr:
        raise NotImplementedError


class ModularInteger(Scalar):
    def __init__(self, name: str, modulus: Expr) -> None:
        modulus_num = modulus.simplified()
        if not isinstance(modulus_num, Number):
            raise ModelError(f'modulus of "{name}" contains variable')
        modulus_int = int(modulus_num)
        if modulus_int > 2**64:
            raise ModelError(f'modulus of "{name}" exceeds limit (2**64)')
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            raise ModelError(f'modulus of "{name}" not power of two')
        super().__init__(name)
        self.__modulus = modulus
        self.__size = Number(int(log(modulus_int) / log(2)))

    @property
    def modulus(self) -> Expr:
        return self.__modulus

    @property
    def size(self) -> Expr:
        return self.__size


class RangeInteger(Scalar):
    def __init__(self, name: str, first: Expr, last: Expr, size: Expr) -> None:
        first_num = first.simplified()
        if not isinstance(first_num, Number):
            raise ModelError(f'first of "{name}" contains variable')
        last_num = last.simplified()
        if not isinstance(last_num, Number):
            raise ModelError(f'last of "{name}" contains variable')
        if first_num < Number(0):
            raise ModelError(f'first of "{name}" negative')
        if first_num > last_num:
            raise ModelError(f'range of "{name}" negative')
        size_num = size.simplified()
        if not isinstance(size_num, Number):
            raise ModelError(f'size of "{name}" contains variable')
        if log(int(last_num) + 1) / log(2) > int(size_num):
            raise ModelError(f'size for "{name}" too small')
        super().__init__(name)
        self.__first = first
        self.__last = last
        self.__size = size

        constraints: Expr = TRUE
        if self.first.simplified() != self.base_first.simplified():
            constraints = GreaterEqual(Variable('Value'), self.first)
        if self.last.simplified() != self.base_last.simplified():
            constraints = And(constraints, LessEqual(Variable('Value'), self.last))
        self.__constraints = constraints.simplified()

    @property
    def first(self) -> Expr:
        return self.__first

    @property
    def last(self) -> Expr:
        return self.__last

    @property
    def size(self) -> Expr:
        return self.__size

    @property
    def constraints(self) -> Expr:
        return self.__constraints

    @property
    def base_first(self) -> Expr:
        return Number(0)

    @property
    def base_last(self) -> Expr:
        return Sub(Pow(Number(2), self.size), Number(1))


class Enumeration(Scalar):
    def __init__(self, name: str, literals: Dict[str, Number], size: Number,
                 always_valid: bool) -> None:
        if log(max(map(int, literals.values())) + 1) / log(2) > int(size):
            raise ModelError(f'size for "{name}" too small')
        if len(set(literals.values())) < len(literals.values()):
            raise ModelError(f'"{name}" contains elements with same value')
        super().__init__(name)
        self.literals = literals
        self.__size = size
        self.always_valid = always_valid

    @property
    def size(self) -> Number:
        return self.__size

    @property
    def enum_name(self) -> str:
        return f'{self.name}_Enum'


class Composite(Type):
    @property
    def size(self) -> Expr:
        raise NotImplementedError


class Array(Composite):
    def __init__(self, name: str, element_type: Type) -> None:
        super().__init__(name)
        self.element_type = element_type

    @property
    def size(self) -> Expr:
        raise ModelError(f'size of "{self.name}" undefined')


class Payload(Composite):
    def __init__(self) -> None:
        super().__init__('Payload')

    @property
    def size(self) -> Expr:
        raise NotImplementedError


class Reference(Type):
    @property
    def size(self) -> Expr:
        raise NotImplementedError


class Field(NamedTuple):
    name: str

    @property
    def affixed_name(self) -> str:
        return f'F_{self.name}'


INITIAL = Field('Initial')
FINAL = Field('Final')


class Link(NamedTuple):
    source: Field
    target: Field
    condition: Expr = TRUE
    length: Expr = UNDEFINED
    first: Expr = UNDEFINED


class Message(Element):
    def __init__(self, full_name: str, structure: Sequence[Link],
                 types: Mapping[Field, Type]) -> None:
        self.full_name = full_name
        self.structure = structure
        self.__types = types

        if structure or types:
            self.__verify()

            self.__fields = self.__compute_topological_sorting()
            self.__types = {f: self.__types[f]
                            for f in self.__fields}
            self.__paths = {f: self.__compute_paths(f)
                            for f in self.all_fields}
            self.__definite_predecessors = {f: self.__compute_definite_predecessors(f)
                                            for f in self.all_fields}
            self.__field_condition = {f: self.__compute_field_condition(f).simplified()
                                      for f in self.all_fields}
        else:
            self.__fields = ()
            self.__paths = {}
            self.__definite_predecessors = {}
            self.__field_condition = {}

    @property
    def generic_name(self) -> str:
        package, name = self.full_name.rsplit('.', 1)
        return f'{package}.Generic_{name}'

    @property
    def package(self) -> str:
        return self.full_name.rsplit('.', 1)[0]

    @property
    def fields(self) -> Tuple[Field, ...]:
        """Return fields topologically sorted."""
        return self.__fields

    @property
    def all_fields(self) -> Tuple[Field, ...]:
        return (INITIAL, *self.__fields, FINAL)

    @property
    def definite_fields(self) -> Tuple[Field, ...]:
        """Return all fields which are part of all possible paths."""
        return self.__definite_predecessors[FINAL]

    @property
    def types(self) -> Mapping[Field, Type]:
        """Return fields and corresponding types topologically sorted."""
        return self.__types

    def incoming(self, field: Field) -> Sequence[Link]:
        return [l for l in self.structure if l.target == field]

    def outgoing(self, field: Field) -> Sequence[Link]:
        return [l for l in self.structure if l.source == field]

    def predecessors(self, field: Field) -> Tuple[Field, ...]:
        if field == INITIAL:
            return ()
        if field == FINAL:
            return self.fields
        return self.fields[:self.fields.index(field)]

    def successors(self, field: Field) -> Tuple[Field, ...]:
        if field == INITIAL:
            return self.fields
        if field == FINAL:
            return ()
        return self.fields[self.fields.index(field) + 1:]

    def direct_predecessors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.source for l in self.incoming(field)]))

    def direct_successors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.target for l in self.outgoing(field)]))

    def definite_predecessors(self, field: Field) -> Tuple[Field, ...]:
        """Return preceding fields which are part of all possible paths."""
        return self.__definite_predecessors[field]

    def field_condition(self, field: Field) -> Expr:
        return self.__field_condition[field]

    def __verify(self) -> None:
        type_fields = self.__types.keys() | {INITIAL, FINAL}
        structure_fields = {l.source for l in self.structure} | {l.target for l in self.structure}
        for f in structure_fields - type_fields:
            raise ModelError(f'missing type for field "{f.name}" of "{self.full_name}"')
        for f in type_fields - structure_fields:
            raise ModelError(f'superfluous field "{f.name}" in field types of "{self.full_name}"')
        if len(self.outgoing(INITIAL)) != 1:
            raise ModelError(f'ambiguous first field in "{self.full_name}"')

    def __compute_topological_sorting(self) -> Tuple[Field, ...]:
        """Return fields topologically sorted (Kahn's algorithm)."""
        result: Tuple[Field, ...] = ()
        fields = [INITIAL]
        visited = set()
        while fields:
            n = fields.pop(0)
            result += (n,)
            for e in self.outgoing(n):
                visited.add(e)
                if set(self.incoming(e.target)) <= visited:
                    fields.append(e.target)
        if set(self.structure) - visited:
            raise ModelError(f'structure of "{self.full_name}" contains cycle')
        return result[1:-1]

    def __compute_paths(self, final: Field) -> Set[Tuple[Link, ...]]:
        if final == INITIAL:
            return {()}
        return set(itertools.chain.from_iterable(
            ((p + (l,) for p in self.__compute_paths(l.source)) for l in self.incoming(final))))

    def __compute_definite_predecessors(self, final: Field) -> Tuple[Field, ...]:
        return tuple(f for f in self.__fields
                     if all(any(f == pf.source for pf in p)
                            for p in self.__paths[final]))

    def __compute_field_condition(self, final: Field) -> Expr:
        if final == INITIAL:
            return TRUE
        return Or(*[And(self.__compute_field_condition(l.source), l.condition)
                    for l in self.incoming(final)])


class DerivedMessage(Message):
    def __init__(self, full_name: str, base_name: str, structure: Sequence[Link],
                 types: Mapping[Field, Type]) -> None:
        super().__init__(full_name, structure, types)
        self.base_name = base_name

    @property
    def generic_base_name(self) -> str:
        package, name = self.base_name.rsplit('.', 1)
        return f'{package}.Generic_{name}'

    @property
    def base_package(self) -> str:
        return self.base_name.rsplit('.', 1)[0]


class Refinement(Type):
    # pylint: disable=too-many-arguments
    def __init__(self, package: str, pdu: str, field: Field, sdu: str,
                 condition: Expr = TRUE) -> None:
        super().__init__('')
        self.package = package
        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (self.package == other.package
                    and self.pdu == other.pdu
                    and self.field == other.field
                    and self.sdu == other.sdu)
        return NotImplemented

    @property
    def size(self) -> Number:
        raise NotImplementedError


class ModelError(Exception):
    pass
