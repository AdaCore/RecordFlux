# pylint: disable=too-many-lines
import itertools
from abc import ABC, abstractmethod
from collections import defaultdict
from copy import copy
from pathlib import Path
from typing import Dict, List, Mapping, NamedTuple, Optional, Sequence, Set, Tuple, Union

from rflx.common import flat_name, generic_repr
from rflx.contract import ensure, invariant
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.expression import (
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    Attribute,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    Mul,
    Not,
    NotEqual,
    Number,
    Or,
    Pow,
    ProofResult,
    Relation,
    Sub,
    Variable,
)
from rflx.identifier import ID, StrID

BUILTINS_PACKAGE = ID("__BUILTINS__")
INTERNAL_PACKAGE = ID("__INTERNAL__")


class Base(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)


class Type(Base):
    def __init__(
        self, identifier: StrID, location: Location = None, error: RecordFluxError = None
    ) -> None:
        identifier = ID(identifier)
        self.error = error or RecordFluxError()

        if len(identifier.parts) != 2:
            self.error.append(
                f'unexpected format of type name "{identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        self.identifier = identifier
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return {k: v for k, v in self.__dict__.items() if k != "location"} == {
                k: v for k, v in other.__dict__.items() if k != "location"
            }
        return NotImplemented

    @property
    def full_name(self) -> str:
        return str(self.identifier)

    @property
    def name(self) -> str:
        return str(self.identifier.name)

    @property
    def package(self) -> ID:
        return self.identifier.parent


class Scalar(Type):
    def __init__(self, identifier: StrID, size: Expr, location: Location = None) -> None:
        super().__init__(identifier, location)
        self._size = size

    @property
    def size(self) -> Expr:
        return self._size

    @abstractmethod
    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> Sequence[Expr]:
        raise NotImplementedError


class Integer(Scalar):
    @property
    @abstractmethod
    def first(self) -> Expr:
        raise NotImplementedError

    @property
    @abstractmethod
    def last(self) -> Expr:
        raise NotImplementedError


class ModularInteger(Integer):
    def __init__(self, identifier: StrID, modulus: Expr, location: Location = None) -> None:
        super().__init__(identifier, UNDEFINED, location)

        modulus_num = modulus.simplified()

        if not isinstance(modulus_num, Number):
            self.error.append(
                f'modulus of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return

        modulus_int = int(modulus_num)

        if modulus_int > 2 ** 64:
            self.error.append(
                f'modulus of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                modulus.location,
            )
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            self.error.append(
                f'modulus of "{self.name}" not power of two',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.__modulus = modulus
        self._size = Number((modulus_int - 1).bit_length())

    @property
    def modulus(self) -> Expr:
        return self.__modulus

    @property
    def first(self) -> Expr:
        return Number(0)

    @property
    def last(self) -> Expr:
        return Sub(self.modulus, Number(1))

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> Sequence[Expr]:
        if proof:
            return [
                Less(Variable(name), self.__modulus, location=self.location),
                GreaterEqual(Variable(name), Number(0), location=self.location),
                Equal(Length(name), self.size, location=self.location),
            ]
        return [TRUE]


class RangeInteger(Integer):
    def __init__(
        self, identifier: StrID, first: Expr, last: Expr, size: Expr, location: Location = None
    ) -> None:
        super().__init__(identifier, size, location)

        first_num = first.simplified()
        last_num = last.simplified()
        size_num = size.simplified()

        if not isinstance(first_num, Number):
            self.error.append(
                f'first of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if not isinstance(last_num, Number):
            self.error.append(
                f'last of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return
        if int(last_num) >= 2 ** 63:
            self.error.append(
                f'last of "{self.name}" exceeds limit (2**63 - 1)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if not isinstance(size_num, Number):
            self.error.append(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if self.error.check():
            return

        assert isinstance(last_num, Number)
        assert isinstance(size_num, Number)

        if first_num < Number(0):
            self.error.append(
                f'first of "{self.name}" negative', Subsystem.MODEL, Severity.ERROR, self.location,
            )
        if first_num > last_num:
            self.error.append(
                f'range of "{self.name}" negative', Subsystem.MODEL, Severity.ERROR, self.location,
            )

        if int(last_num).bit_length() > int(size_num):
            self.error.append(
                f'size of "{self.name}" too small', Subsystem.MODEL, Severity.ERROR, self.location,
            )
        if int(size_num) > 64:
            self.error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.__first = first
        self.__last = last

    @property
    def first(self) -> Expr:
        return self.__first

    @property
    def last(self) -> Expr:
        return self.__last

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> Sequence[Expr]:
        if proof:
            return [
                GreaterEqual(Variable(name), self.first, location=self.location),
                LessEqual(Variable(name), self.last, location=self.location),
                Equal(Length(name), self.size, location=self.location),
            ]

        if self.first.simplified() == self.last.simplified():
            return [Equal(Variable(name), self.first)]

        c: Expr = TRUE
        if self.first.simplified() != self.base_first.simplified():
            c = GreaterEqual(Variable(name), self.first)
        if self.last.simplified() != self.base_last.simplified():
            c = And(c, LessEqual(Variable(name), self.last))
        return [c.simplified()]

    @property
    def base_first(self) -> Expr:
        return Number(0)

    @property
    def base_last(self) -> Expr:
        return Sub(Pow(Number(2), self.size), Number(1))


class Enumeration(Scalar):
    def __init__(
        self,
        identifier: StrID,
        literals: Sequence[Tuple[StrID, Number]],
        size: Expr,
        always_valid: bool,
        location: Location = None,
    ) -> None:
        # pylint: disable=too-many-branches, too-many-locals
        super().__init__(identifier, size, location)

        for i1, e1 in enumerate(literals):
            for i2, e2 in enumerate(literals):
                if i2 < i1 and e1[0] == e2[0]:
                    self.error.append(
                        f'duplicate literal "{e1[0]}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        e1[0].location if isinstance(e1[0], ID) else self.location,
                    )
                    self.error.append(
                        "previous occurrence",
                        Subsystem.MODEL,
                        Severity.INFO,
                        e2[0].location if isinstance(e2[0], ID) else self.location,
                    )

        self.literals = {}
        for k, v in literals:
            if " " in str(k) or "." in str(k):
                self.error.append(
                    f'invalid literal name "{k}" in "{self.name}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                continue
            self.literals[ID(k)] = v

        size_num = size.simplified()

        if not isinstance(size_num, Number):
            self.error.append(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return

        if self.literals.values():
            min_literal_value = min(map(int, self.literals.values()))
            max_literal_value = max(map(int, self.literals.values()))
            if min_literal_value < 0 or max_literal_value > 2 ** 63 - 1:
                self.error.append(
                    f'enumeration value of "{self.name}"'
                    " outside of permitted range (0 .. 2**63 - 1)",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
            if max_literal_value.bit_length() > int(size_num):
                self.error.append(
                    f'size of "{self.name}" too small',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
        if int(size_num) > 64:
            self.error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        for i1, v1 in enumerate(self.literals.values()):
            for i2, v2 in enumerate(self.literals.values()):
                if i1 < i2 and v1 == v2:
                    self.error.append(
                        f'duplicate enumeration value "{v1}" in "{self.name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        v2.location,
                    )
                    self.error.append(
                        "previous occurrence", Subsystem.MODEL, Severity.INFO, v1.location
                    )

        if always_valid and len(self.literals) == 2 ** int(size_num):
            self.error.append(
                f'unnecessary always-valid aspect on "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.always_valid = always_valid

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> Sequence[Expr]:
        if proof:
            prefixed_literals = {self.package * l: v for l, v in self.literals.items()}
            if self.package == BUILTINS_PACKAGE:
                literals = self.literals
            elif same_package:
                literals = {**self.literals, **prefixed_literals}
            else:
                literals = prefixed_literals
            result: List[Expr] = [
                Or(
                    *[Equal(Variable(name), Variable(l), self.location) for l in literals],
                    location=self.location,
                )
            ]
            result.extend([Equal(Variable(l), v, self.location) for l, v in literals.items()])
            result.append(Equal(Length(name), self.size, self.location))
            return result
        return [TRUE]


class Composite(Type):
    @property
    @abstractmethod
    def element_size(self) -> Expr:
        raise NotImplementedError


class Array(Composite):
    def __init__(self, identifier: StrID, element_type: Type, location: Location = None) -> None:
        super().__init__(identifier, location)
        self.element_type = element_type

        if not isinstance(element_type, Scalar) and not (
            isinstance(element_type, Message) and element_type.structure
        ):
            self.error.append(
                f'invalid element type of array "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )
            self.error.append(
                f'type "{element_type.name}" must be scalar or non-null message',
                Subsystem.MODEL,
                Severity.INFO,
                element_type.location,
            )

        if isinstance(element_type, Scalar):
            element_type_size = element_type.size.simplified()
            if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
                self.error.append(
                    f'unsupported element type size of array "{self.name}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )
                self.error.append(
                    f'type "{element_type.name}" has size {element_type_size},'
                    r" must be multiple of 8",
                    Subsystem.MODEL,
                    Severity.INFO,
                    element_type.location,
                )

    @property
    def element_size(self) -> Expr:
        return Length(self.element_type.name)


class Opaque(Composite):
    def __init__(self, location: Location = None) -> None:
        super().__init__(INTERNAL_PACKAGE * "Opaque", location)

    @property
    def element_size(self) -> Expr:
        return Number(8)


class Field(Base):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __lt__(self, other: "Field") -> int:
        return self.identifier < other.identifier

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def affixed_name(self) -> str:
        return f"F_{self.name}"


INITIAL = Field("Initial")
FINAL = Field("Final")


class Link(NamedTuple):
    source: Field
    target: Field
    condition: Expr = TRUE
    length: Expr = UNDEFINED
    first: Expr = UNDEFINED
    location: Optional[Location] = None

    def __repr__(self) -> str:
        # pylint: disable=no-member
        return generic_repr(self.__class__.__name__, self._asdict())

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.source == other.source
                and self.target == other.target
                and self.condition == other.condition
                and self.length == other.length
                and self.first == other.first
            )
        return NotImplemented

    def __hash__(self) -> int:
        return 0


def valid_message_field_types(message: "AbstractMessage") -> bool:
    for t in message.types.values():
        if not isinstance(t, (Scalar, Composite, AbstractMessage)):
            return False
    return True


class MessageState(Base):
    fields: Optional[Tuple[Field, ...]] = ()
    paths: Mapping[Field, Set[Tuple[Link, ...]]] = {}
    definite_predecessors: Mapping[Field, Tuple[Field, ...]] = {}
    field_condition: Mapping[Field, Expr] = {}


@invariant(lambda self: valid_message_field_types(self))
class AbstractMessage(Type):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
        location: Location = None,
        error: RecordFluxError = None,
        state: MessageState = None,
    ) -> None:
        super().__init__(identifier, location, error)

        self.structure = structure
        self.__types = types
        self.__has_unreachable = False
        self._state = state or MessageState()

        if not state and (structure or types):
            try:
                self.__verify()
                self._state.fields = self.__compute_topological_sorting()
                if self._state.fields:
                    self.__types = {f: self.__types[f] for f in self._state.fields}
                    self._state.paths = {f: self.__compute_paths(f) for f in self.all_fields}
                    self._state.definite_predecessors = {
                        f: self.__compute_definite_predecessors(f) for f in self.all_fields
                    }
                    self._state.field_condition = {
                        f: self.__compute_field_condition(f).simplified() for f in self.all_fields
                    }
                    self.__verify_conditions()
            except RecordFluxError:
                pass

    @abstractmethod
    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "AbstractMessage":
        raise NotImplementedError

    @abstractmethod
    def proven(self) -> "Message":
        raise NotImplementedError

    @property
    def fields(self) -> Tuple[Field, ...]:
        """Return fields topologically sorted."""
        return self._state.fields or ()

    @property
    def all_fields(self) -> Tuple[Field, ...]:
        return (INITIAL, *self.fields, FINAL)

    @property
    def definite_fields(self) -> Tuple[Field, ...]:
        """Return all fields which are part of all possible paths."""
        return self._state.definite_predecessors[FINAL]

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
        return self.fields[: self.fields.index(field)]

    def successors(self, field: Field) -> Tuple[Field, ...]:
        if field == INITIAL:
            return self.fields
        if field == FINAL:
            return ()
        return self.fields[self.fields.index(field) + 1 :]

    def direct_predecessors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.source for l in self.incoming(field)]))

    def direct_successors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.target for l in self.outgoing(field)]))

    def definite_predecessors(self, field: Field) -> Tuple[Field, ...]:
        """Return preceding fields which are part of all possible paths."""
        return self._state.definite_predecessors[field]

    def field_condition(self, field: Field) -> Expr:
        return self._state.field_condition[field]

    def field_size(self, field: Field) -> Expr:
        if field == FINAL:
            return Number(0)

        assert field in self.fields, f'field "{field.name}" not found'

        field_type = self.types[field]
        if isinstance(field_type, Scalar):
            return field_type.size

        raise NotImplementedError

    def prefixed(self, prefix: str) -> "AbstractMessage":
        fields = {f.identifier for f in self.fields}

        def prefixed_expression(expression: Expr) -> Expr:
            variables = {v.identifier for v in expression.variables()}
            literals = {l for l in variables - fields if len(l.parts) == 1}

            return expression.substituted(
                mapping={
                    **{
                        v: v.__class__(f"{prefix}{v.name}")
                        for v in expression.variables()
                        if v.identifier in fields
                    },
                    **{
                        v: v.__class__(f"{self.package}.{v.name}")
                        for v in expression.variables()
                        if v.identifier in literals
                    },
                }
            ).simplified()

        structure = []

        for l in self.structure:
            source = Field(prefix + l.source.identifier) if l.source != INITIAL else INITIAL
            target = Field(prefix + l.target.identifier) if l.target != FINAL else FINAL
            condition = prefixed_expression(l.condition)
            length = prefixed_expression(l.length)
            first = prefixed_expression(l.first)
            structure.append(Link(source, target, condition, length, first, l.location))

        types = {Field(prefix + f.identifier): t for f, t in self.types.items()}

        return self.copy(structure=structure, types=types)

    def is_possibly_empty(self, field: Field) -> bool:
        if isinstance(self.types[field], Scalar):
            return False

        for p in self._state.paths[FINAL]:
            conditions = [l.condition for l in p if l.condition != TRUE]
            lengths = [Equal(Length(l.target.name), l.length) for l in p if l.length != UNDEFINED]
            empty_field = Equal(Length(field.name), Number(0))
            proof = empty_field.check(
                [*self.__type_constraints(empty_field), *conditions, *lengths]
            )
            if proof.result == ProofResult.sat:
                return True

        return False

    # pylint: disable=too-many-branches
    def __verify(self) -> None:
        type_fields = self.__types.keys() | {INITIAL, FINAL}
        structure_fields = {l.source for l in self.structure} | {l.target for l in self.structure}

        for f in structure_fields - type_fields:
            self.error.append(
                f'missing type for field "{f.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                f.identifier.location,
            )

        for f in type_fields - structure_fields - {FINAL}:
            self.error.append(
                f'unused field "{f.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                f.identifier.location,
            )

        if len(self.outgoing(INITIAL)) != 1:
            self.error.append(
                f'ambiguous first field in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            self.error.extend(
                [
                    ("duplicate", Subsystem.MODEL, Severity.INFO, l.target.identifier.location)
                    for l in self.outgoing(INITIAL)
                    if l.target.identifier.location
                ]
            )

        name_conflicts = [
            (f, l)
            for f in type_fields
            for l in qualified_literals(self.types, self.package)
            if f.identifier == l
        ]

        if name_conflicts:
            conflicting_field, conflicting_literal = name_conflicts.pop(0)
            self.error.append(
                f'name conflict for field "{conflicting_field.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                conflicting_field.identifier.location,
            )
            self.error.append(
                "conflicting enumeration literal",
                Subsystem.MODEL,
                Severity.INFO,
                conflicting_literal.location,
            )

        self.error.propagate()

        for f in structure_fields:
            for l in self.structure:
                if f in (INITIAL, l.target):
                    break
            else:
                self.__has_unreachable = True
                self.error.append(
                    f'unreachable field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )

        duplicate_links = defaultdict(list)
        for link in self.structure:
            duplicate_links[(link.source, link.target, link.condition)].append(link)

        for links in duplicate_links.values():
            if len(links) > 1:
                self.error.append(
                    f'duplicate link from "{links[0].source.identifier}"'
                    f' to "{links[0].target.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    links[0].source.identifier.location,
                )
                self.error.extend(
                    [("duplicate link", Subsystem.MODEL, Severity.INFO, l.location,) for l in links]
                )

        for l in self.structure:
            exponentiations = And(l.condition, l.first, l.length).findall(
                lambda x: isinstance(x, Pow)
            )
            for e in exponentiations:
                assert isinstance(e, Pow)
                variables = e.right.findall(lambda x: isinstance(x, Variable))
                if variables:
                    self.error.append(
                        f'unsupported expression in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        e.location,
                    )
                    for v in variables:
                        self.error.append(
                            f'variable "{v}" in exponent',
                            Subsystem.MODEL,
                            Severity.INFO,
                            v.location,
                        )

    def __verify_conditions(self) -> None:
        literals = qualified_literals(self.types, self.package)
        variables = {f.identifier for f in self.fields}
        seen = {ID("Message")}
        for f in (INITIAL, *self.fields):
            seen.add(f.identifier)
            for l in self.outgoing(f):
                state = (variables, literals, seen)
                self.__check_vars(l.condition, state, l.condition.location)
                self.__check_vars(l.length, state, l.length.location)
                self.__check_vars(l.first, state, l.first.location)
                self.__check_attributes(l.condition, l.condition.location)
                self.__check_relations(l.condition, literals)
                self.__check_first_expression(l, l.first.location)
                self.__check_length_expression(l)
        self.error.propagate()

    def __check_vars(
        self,
        expression: Expr,
        state: Tuple[Set[ID], Dict[ID, Enumeration], Set[ID]],
        location: Location = None,
    ) -> None:
        variables, literals, seen = state
        for v in expression.variables():
            if v.identifier not in literals and v.identifier not in seen:
                if v.identifier in variables:
                    message = f'subsequent field "{v}" referenced'
                else:
                    message = f'undefined variable "{v}" referenced'
                self.error.append(message, Subsystem.MODEL, Severity.ERROR, location or v.location)

    def __check_attributes(self, expression: Expr, location: Location = None) -> None:
        for a in expression.findall(lambda x: isinstance(x, Attribute)):
            if isinstance(a, Length) and not (
                isinstance(a.prefix, Variable)
                and (
                    a.prefix.name == "Message"
                    or (
                        Field(a.prefix.name) in self.fields
                        and isinstance(self.types[Field(a.prefix.name)], Composite)
                    )
                )
            ):
                self.error.append(
                    f'invalid use of length attribute for "{a.prefix}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )

    def __check_relations(self, expression: Expr, literals: Dict[ID, Enumeration]) -> None:

        TypeExpr = Union[Type, Expr]

        def check_composite_element_range(
            relation: Relation, aggregate: Aggregate, composite: Composite
        ) -> None:
            first: Expr
            last: Expr
            if isinstance(composite, Opaque):
                first = Number(0)
                last = Number(255)

            if isinstance(composite, Array):
                if not isinstance(composite.element_type, Integer):
                    self.error.append(
                        f'invalid array element type "{composite.element_type.identifier}"'
                        " for aggregate comparison",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        relation.location,
                    )
                    return
                first = composite.element_type.first.simplified()
                last = composite.element_type.last.simplified()

            for element in aggregate.elements:
                if not first <= element <= last:
                    self.error.append(
                        f"aggregate element out of range {first} .. {last}",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        element.location,
                    )

        def check_enumeration(relation: Relation, left: Enumeration, right: Enumeration) -> None:
            if left != right:
                self.error.append(
                    "comparison of incompatible enumeration literals",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    relation.location,
                )
                self.error.append(
                    f'of type "{left.identifier}"', Subsystem.MODEL, Severity.INFO, left.location,
                )
                self.error.append(
                    f'and type "{right.identifier}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    right.location,
                )

        def relation_error(relation: Relation, left: TypeExpr, right: TypeExpr) -> None:
            self.error.append(
                f'invalid relation "{relation.symbol}" between {left.__class__.__name__} '
                f"and {right.__class__.__name__}",
                Subsystem.MODEL,
                Severity.ERROR,
                relation.location,
            )

        def resolve_types(
            left: Expr, right: Expr, literals: Dict[ID, Enumeration],
        ) -> Tuple[TypeExpr, TypeExpr]:
            def resolve_type(expr: Expr) -> Optional[TypeExpr]:
                if not isinstance(expr, Variable):
                    return expr
                if expr.identifier in literals:
                    return literals[expr.identifier]
                if Field(expr.name) in self.types:
                    return self.types[Field(expr.name)]
                return None

            lefttype = resolve_type(left)
            righttype = resolve_type(right)
            self.error.propagate()
            assert lefttype
            assert righttype
            return (lefttype, righttype)

        def invalid_relation(left: TypeExpr, right: TypeExpr) -> bool:
            return (
                (isinstance(left, Opaque) and not isinstance(right, (Opaque, Aggregate)))
                or (isinstance(left, Array) and not isinstance(right, (Array, Aggregate)))
                or (isinstance(left, Aggregate) and not isinstance(right, Composite))
            )

        for relation in expression.findall(lambda x: isinstance(x, Relation)):
            assert isinstance(relation, Relation)
            left, right = resolve_types(
                relation.left.simplified(), relation.right.simplified(), literals
            )
            if isinstance(relation, (Less, LessEqual, Greater, GreaterEqual)):
                if (isinstance(left, Aggregate) and isinstance(right, Type)) or (
                    isinstance(right, Aggregate) and isinstance(left, Type)
                ):
                    relation_error(relation, left, right)
            elif isinstance(relation, (Equal, NotEqual)):
                if invalid_relation(left, right):
                    relation_error(relation, left, right)
                elif invalid_relation(left=right, right=left):
                    relation_error(relation, left=right, right=left)
                elif isinstance(left, Aggregate) and isinstance(right, Composite):
                    check_composite_element_range(relation, left, right)
                elif isinstance(left, Composite) and isinstance(right, Aggregate):
                    check_composite_element_range(relation, right, left)
                elif isinstance(left, Enumeration) and isinstance(right, Enumeration):
                    check_enumeration(relation, left, right)

    def __check_first_expression(self, link: Link, location: Location = None) -> None:
        if link.first != UNDEFINED and not isinstance(link.first, First):
            self.error.append(
                f'invalid First for field "{link.target.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

    def __check_length_expression(self, link: Link) -> None:
        if link.target == FINAL and link.length != UNDEFINED:
            self.error.append(
                f'length attribute for final field in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                link.length.location,
            )
        if link.target != FINAL and link.target in self.types:
            t = self.types[link.target]
            unconstrained = isinstance(t, (Opaque, Array))
            if not unconstrained and link.length != UNDEFINED:
                self.error.append(
                    f'fixed size field "{link.target.name}" with length expression',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    link.target.identifier.location,
                )
            if unconstrained and link.length == UNDEFINED:
                self.error.append(
                    f'unconstrained field "{link.target.name}" without length expression',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    link.target.identifier.location,
                )

    def __type_constraints(self, expr: Expr) -> Sequence[Expr]:
        def get_constraints(aggregate: Aggregate, field: Variable) -> Sequence[Expr]:
            comp = self.types[Field(field.name)]
            assert isinstance(comp, Composite)
            result = Equal(
                Mul(aggregate.length, comp.element_size), Length(field), location=expr.location
            )
            if isinstance(comp, Array) and isinstance(comp.element_type, Scalar):
                return [
                    result,
                    *comp.element_type.constraints(name=comp.element_type.name, proof=True),
                ]
            return [result]

        literals = qualified_literals(self.types, self.package)
        scalar_types = [
            (f.name, t)
            for f, t in self.types.items()
            if isinstance(t, Scalar)
            and f.name not in literals
            and f.name not in ["Message", "Final"]
        ]

        aggregate_constraints: List[Expr] = []
        for r in expr.findall(lambda x: isinstance(x, (Equal, NotEqual))):
            assert isinstance(r, (Equal, NotEqual))
            if isinstance(r.left, Aggregate) and isinstance(r.right, Variable):
                aggregate_constraints.extend(get_constraints(r.left, r.right))
            if isinstance(r.left, Variable) and isinstance(r.right, Aggregate):
                aggregate_constraints.extend(get_constraints(r.right, r.left))

        message_constraints: List[Expr] = [
            Equal(Mod(First("Message"), Number(8)), Number(1)),
            Equal(Mod(Length("Message"), Number(8)), Number(0)),
        ]

        scalar_constraints = [
            c
            for n, t in scalar_types
            for c in t.constraints(name=n, proof=True, same_package=self.package == t.package)
        ]

        return [*message_constraints, *aggregate_constraints, *scalar_constraints]

    def __prove_conflicting_conditions(self) -> None:
        for f in (INITIAL, *self.fields):
            for i1, c1 in enumerate(self.outgoing(f)):
                for i2, c2 in enumerate(self.outgoing(f)):
                    if i1 < i2:
                        conflict = And(c1.condition, c2.condition)
                        proof = conflict.check(self.__type_constraints(conflict))
                        if proof.result == ProofResult.sat:
                            c1_message = str(c1.condition).replace("\n", " ")
                            c2_message = str(c2.condition).replace("\n", " ")
                            self.error.append(
                                f'conflicting conditions for field "{f.name}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                f.identifier.location,
                            )
                            self.error.append(
                                f"condition {i1} ({f.identifier} -> {c1.target.identifier}):"
                                f" {c1_message}",
                                Subsystem.MODEL,
                                Severity.INFO,
                                c1.condition.location,
                            )
                            self.error.append(
                                f"condition {i2} ({f.identifier} -> {c2.target.identifier}):"
                                f" {c2_message}",
                                Subsystem.MODEL,
                                Severity.INFO,
                                c2.condition.location,
                            )

    def __prove_reachability(self) -> None:
        def has_final(field: Field) -> bool:
            if field == FINAL:
                return True
            for o in self.outgoing(field):
                if has_final(o.target):
                    return True
            return False

        for f in (INITIAL, *self.fields):
            if not has_final(f):
                self.error.append(
                    f'no path to FINAL for field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )

        for f in (*self.fields, FINAL):
            paths = []
            for path in self._state.paths[f]:
                facts = [fact for link in path for fact in self.__link_expression(link)]
                outgoing = self.outgoing(f)
                if f != FINAL and outgoing:
                    facts.append(
                        Or(*[o.condition for o in outgoing], location=f.identifier.location)
                    )
                proof = TRUE.check(facts)
                if proof.result == ProofResult.sat:
                    break

                paths.append((path, proof.error))
            else:
                self.error.append(
                    f'unreachable field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )
                for index, (path, errors) in enumerate(sorted(paths)):
                    self.error.append(
                        f"path {index} (" + " -> ".join([l.target.name for l in path]) + "):",
                        Subsystem.MODEL,
                        Severity.INFO,
                        f.identifier.location,
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                            for m, l in errors
                        ]
                    )

    def __prove_contradictions(self) -> None:
        for f in (INITIAL, *self.fields):
            contradictions = []
            paths = 0
            for path in self._state.paths[f]:
                facts = [fact for link in path for fact in self.__link_expression(link)]
                for c in self.outgoing(f):
                    paths += 1
                    contradiction = c.condition
                    constraints = self.__type_constraints(contradiction)
                    proof = contradiction.check([*constraints, *facts])
                    if proof.result == ProofResult.sat:
                        continue

                    contradictions.append((path, c.condition, proof.error))

            if paths == len(contradictions):
                for path, cond, errors in sorted(contradictions):
                    self.error.append(
                        f'contradicting condition in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        cond.location,
                    )
                    self.error.extend(
                        [
                            (
                                f'on path: "{l.target.identifier}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.target.identifier.location,
                            )
                            for l in path
                        ]
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                            for m, l in errors
                        ]
                    )

    @staticmethod
    def __target_first(link: Link) -> Expr:
        if link.source == INITIAL:
            return First("Message")
        if link.first != UNDEFINED:
            return link.first
        return Add(Last(link.source.name), Number(1), location=link.location)

    def __target_length(self, link: Link) -> Expr:
        if link.length != UNDEFINED:
            return link.length
        return self.field_size(link.target)

    def __target_last(self, link: Link) -> Expr:
        return Sub(
            Add(self.__target_first(link), self.__target_length(link)),
            Number(1),
            link.target.identifier.location,
        )

    def __link_expression(self, link: Link) -> Sequence[Expr]:
        name = link.target.name
        target_first = self.__target_first(link)
        target_length = self.__target_length(link)
        target_last = self.__target_last(link)
        return [
            Equal(First(name), target_first, target_first.location or self.location),
            Equal(Length(name), target_length, target_length.location or self.location),
            Equal(Last(name), target_last, target_last.location or self.location),
            GreaterEqual(First("Message"), Number(0), self.location),
            GreaterEqual(Last("Message"), Last(name), self.location),
            GreaterEqual(Last("Message"), First("Message"), self.location),
            Equal(
                Length("Message"),
                Add(Sub(Last("Message"), First("Message")), Number(1)),
                self.location,
            ),
            *expression_list(link.condition),
        ]

    def __prove_field_positions(self) -> None:
        for f in (*self.fields, FINAL):
            for path in self._state.paths[f]:

                last = path[-1]
                negative = Less(self.__target_length(last), Number(0), last.length.location)
                start = GreaterEqual(self.__target_first(last), First("Message"), last.location)

                facts = [fact for link in path for fact in self.__link_expression(link)]

                outgoing = self.outgoing(f)
                if f != FINAL and outgoing:
                    facts.append(
                        Or(*[o.condition for o in outgoing], location=f.identifier.location)
                    )

                facts.extend(self.__type_constraints(negative))
                facts.extend(self.__type_constraints(start))

                proof = TRUE.check(facts)

                # Only check positions of reachable paths
                if proof.result != ProofResult.sat:
                    continue

                proof = negative.check(facts)
                if proof.result != ProofResult.unsat:
                    path_message = " -> ".join([l.target.name for l in path])
                    self.error.append(
                        f'negative length for field "{f.name}" ({path_message})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        f.identifier.location,
                    )
                    return

                proof = start.check(facts)
                if proof.result != ProofResult.sat:
                    path_message = " -> ".join([last.target.name for last in path])
                    self.error.append(
                        f'negative start for field "{f.name}" ({path_message})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.identifier.location,
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, locn)
                            for m, locn in proof.error
                        ]
                    )
                    return

                if f in self.__types:
                    t = self.__types[f]
                    if not isinstance(t, Opaque):
                        continue
                    element_size = t.element_size
                    start_aligned = Not(
                        Equal(
                            Mod(self.__target_first(last), element_size), Number(1), last.location
                        )
                    )
                    proof = start_aligned.check([*facts, *self.__type_constraints(start_aligned)])
                    if proof.result != ProofResult.unsat:
                        path_message = " -> ".join([p.target.name for p in path])
                        self.error.append(
                            f'opaque field "{f.name}" not aligned to {element_size} bit boundary'
                            f" ({path_message})",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        )
                        return

                    length_multiple_element_size = Not(
                        Equal(
                            Mod(self.__target_length(last), element_size), Number(0), last.location
                        )
                    )
                    proof = length_multiple_element_size.check(
                        [*facts, *self.__type_constraints(length_multiple_element_size)]
                    )
                    if proof.result != ProofResult.unsat:
                        path_message = " -> ".join([p.target.name for p in path])
                        self.error.append(
                            f'length of opaque field "{f.name}" not multiple of {element_size} bit'
                            f" ({path_message})",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        )
                        return

    def __prove_coverage(self) -> None:
        """
        Prove that the fields of a message cover all message bits, i.e. there are no holes in the
        message definition.

        Idea: Let f be the bits covered by the message. By definition
            (1) f >= Message'First and f <= Message'Last
        holds. For every field add a conjunction of the form
            (2) Not(f >= Field'First and f <= Field'Last),
        effectively pruning the range that this field covers from the bit range of the message. For
        the overall expression, prove that it is false for all f, i.e. no bits are left.
        """
        for path in [p[:-1] for p in self._state.paths[FINAL] if p]:

            facts: Sequence[Expr]

            # Calculate (1)
            facts = [
                GreaterEqual(Variable("f"), First("Message")),
                LessEqual(Variable("f"), Last("Message")),
            ]
            # Calculate (2) for all fields
            facts.extend(
                [
                    Not(
                        And(
                            GreaterEqual(Variable("f"), self.__target_first(l)),
                            LessEqual(Variable("f"), self.__target_last(l)),
                            location=l.location,
                        )
                    )
                    for l in path
                ]
            )

            # Define that the end of the last field of a path is the end of the message
            facts.append(Equal(self.__target_last(path[-1]), Last("Message"), self.location))

            # Constraints for links and types
            facts.extend([f for l in path for f in self.__link_expression(l)])

            # Coverage expression must be False, i.e. no bits left
            proof = TRUE.check(facts)
            if proof.result == ProofResult.sat:
                self.error.append(
                    "path does not cover whole message",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.identifier.location,
                )
                self.error.extend(
                    [
                        (
                            f'on path: "{l.target.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.target.identifier.location,
                        )
                        for l in path
                    ]
                )
                return

    def __prove_overlays(self) -> None:
        for f in (INITIAL, *self.fields):
            for p, l in [(p, p[-1]) for p in self._state.paths[f] if p]:
                if l.first != UNDEFINED and isinstance(l.first, First):
                    facts = [f for l in p for f in self.__link_expression(l)]
                    overlaid = Equal(self.__target_last(l), Last(l.first.prefix), l.location)
                    proof = overlaid.check(facts)
                    if proof.result != ProofResult.sat:
                        self.error.append(
                            f'field "{f.name}" not congruent with'
                            f' overlaid field "{l.first.prefix}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.identifier.location,
                        )
                        self.error.extend(
                            [
                                (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                                for m, l in proof.error
                            ]
                        )

    def _prove(self) -> None:
        self.__prove_conflicting_conditions()
        self.__prove_reachability()
        self.__prove_contradictions()
        self.__prove_coverage()
        self.__prove_overlays()
        self.__prove_field_positions()

    def __compute_topological_sorting(self) -> Optional[Tuple[Field, ...]]:
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
        if not self.__has_unreachable and set(self.structure) - visited:
            self.error.append(
                f'structure of "{self.identifier}" contains cycle',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            # ISSUE: Componolit/RecordFlux#256
            return None
        return tuple(f for f in result if f not in [INITIAL, FINAL])

    def __compute_paths(self, final: Field) -> Set[Tuple[Link, ...]]:
        if final == INITIAL:
            return {()}
        return set(
            itertools.chain.from_iterable(
                ((p + (l,) for p in self.__compute_paths(l.source)) for l in self.incoming(final))
            )
        )

    def __compute_definite_predecessors(self, final: Field) -> Tuple[Field, ...]:
        return tuple(
            f
            for f in self.fields
            if all(any(f == pf.source for pf in p) for p in self._state.paths[final])
        )

    def __compute_field_condition(self, final: Field) -> Expr:
        if final == INITIAL:
            return TRUE
        return Or(
            *[
                And(self.__compute_field_condition(l.source), l.condition)
                for l in self.incoming(final)
            ],
            location=final.identifier.location,
        )


class Message(AbstractMessage):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
        location: Location = None,
        error: RecordFluxError = None,
        state: MessageState = None,
    ) -> None:
        super().__init__(identifier, structure, types, location, error, state)

        if not self.error.check() and (structure or types):
            self._prove()

        self.error.propagate()

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "Message":
        return Message(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self) -> "Message":
        return copy(self)


class DerivedMessage(Message):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            location if location else base.location,
            error if error else base.error,
        )
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "DerivedMessage":
        return DerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self) -> "DerivedMessage":
        return copy(self)


class UnprovenMessage(AbstractMessage):
    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "UnprovenMessage":
        return UnprovenMessage(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self) -> Message:
        return Message(
            identifier=self.identifier,
            structure=self.structure,
            types=self.types,
            location=self.location,
            error=self.error,
            state=self._state,
        )

    @ensure(lambda result: valid_message_field_types(result))
    def merged(self) -> "UnprovenMessage":
        message = self

        while True:
            inner_messages = [
                (f, t) for f, t in message.types.items() if isinstance(t, AbstractMessage)
            ]

            if not inner_messages:
                break

            field, inner_message = inner_messages.pop(0)
            inner_message = inner_message.prefixed(f"{field.name}_")

            name_conflicts = [
                f for f in message.fields for g in inner_message.fields if f.name == g.name
            ]

            if name_conflicts:
                conflicting = name_conflicts.pop(0)
                self.error.append(
                    f'name conflict for "{conflicting.identifier}" in "{message.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    conflicting.identifier.location,
                )
                self.error.append(
                    f'when merging message "{inner_message.identifier}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    inner_message.location,
                )
                self.error.append(
                    f'into field "{field.name}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    field.identifier.location,
                )

            structure = []

            for link in message.structure:
                if link.target == field:
                    initial_link = inner_message.outgoing(INITIAL)[0]
                    structure.append(
                        Link(
                            link.source,
                            initial_link.target,
                            link.condition,
                            initial_link.length,
                            link.first,
                            link.location,
                        )
                    )
                elif link.source == field:
                    for final_link in inner_message.incoming(FINAL):
                        structure.append(
                            Link(
                                final_link.source,
                                link.target,
                                And(link.condition, final_link.condition).simplified(),
                                link.length,
                                link.first,
                                link.location,
                            )
                        )
                else:
                    structure.append(link)

            structure.extend(
                l for l in inner_message.structure if l.target != FINAL and l.source != INITIAL
            )

            types = {
                **{f: t for f, t in message.types.items() if f != field},
                **inner_message.types,
            }

            message = message.copy(structure=structure, types=types)

        return message


class UnprovenDerivedMessage(UnprovenMessage):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            location if location else base.location,
            error if error else base.error,
        )
        self.error.extend(base.error)
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "UnprovenDerivedMessage":
        return UnprovenDerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self) -> DerivedMessage:
        return DerivedMessage(
            self.identifier, self.base, self.structure, self.types, self.location, self.error
        )


class Refinement(Type):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        package: StrID,
        pdu: Message,
        field: Field,
        sdu: Message,
        condition: Expr = TRUE,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:
        package = ID(package)

        super().__init__(
            package * "__REFINEMENT__"
            f"{flat_name(sdu.full_name)}__{flat_name(pdu.full_name)}__{field.name}__",
            location,
            error,
        )

        self.error = error or RecordFluxError()
        if len(package.parts) != 1:
            self.error.append(
                f'unexpected format of package name "{package}"',
                Subsystem.MODEL,
                Severity.ERROR,
                package.location,
            )

        if not isinstance(pdu.types[field], Opaque):
            self.error.append(
                f'invalid type of field "{field.name}" in refinement of "{pdu.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                field.identifier.location,
            )
            self.error.append(
                "expected field of type Opaque",
                Subsystem.MODEL,
                Severity.INFO,
                next(f for f in pdu.fields if f == field).identifier.location,
            )

        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.package == other.package
                and self.pdu == other.pdu
                and self.field == other.field
                and self.sdu == other.sdu
            )
        return NotImplemented


class Model(Base):
    def __init__(self, types: Sequence[Type]) -> None:
        self.types = types
        self.__check_types()

    @property
    def messages(self) -> Sequence[Message]:
        return [m for m in self.types if isinstance(m, Message)]

    @property
    def refinements(self) -> Sequence[Refinement]:
        return [m for m in self.types if isinstance(m, Refinement)]

    def __check_types(self) -> None:
        error = RecordFluxError()
        for e1, e2 in [
            (e1, e2)
            for i1, e1 in enumerate(self.types)
            for i2, e2 in enumerate(self.types)
            if (
                isinstance(e1, Enumeration)
                and isinstance(e2, Enumeration)
                and i1 < i2
                and (
                    e1.package == e2.package
                    or e1.package == BUILTINS_PACKAGE
                    or e2.package == BUILTINS_PACKAGE
                )
            )
        ]:
            identical_literals = set(e2.literals) & set(e1.literals)

            if identical_literals:
                literals_message = ", ".join([f"{l}" for l in sorted(identical_literals)])
                error.append(
                    f"conflicting literals: {literals_message}",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    e2.location,
                )
                error.extend(
                    [
                        (
                            f'previous occurrence of "{l}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.location,
                        )
                        for l in sorted(identical_literals)
                    ]
                )

        literals = {l: t for t in self.types if isinstance(t, Enumeration) for l in t.literals}
        type_set = {t.identifier.name for t in self.types}
        name_conflicts = [n for n in literals.keys() if n in type_set]
        for name in sorted(name_conflicts):
            error.append(
                f'literal conflicts with type "{name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                name.location,
            )
            type_location = [t.location for t in self.types if t.identifier.name == name][0]
            error.append(
                "conflicting type declaration", Subsystem.MODEL, Severity.INFO, type_location,
            )

        error.propagate()


def qualified_literals(types: Mapping[Field, Type], package: ID) -> Dict[ID, Enumeration]:
    literals = {}

    for t in types.values():
        if isinstance(t, Enumeration):
            for l in t.literals:
                if t.package == BUILTINS_PACKAGE or t.package == package:
                    literals[l] = t
                if t.package != BUILTINS_PACKAGE:
                    literals[t.package * l] = t

    return literals


def qualified_type_name(name: ID, package: ID) -> ID:

    if is_builtin_type(name):
        return BUILTINS_PACKAGE * name

    if is_internal_type(name):
        return INTERNAL_PACKAGE * name

    if len(name.parts) == 1:
        return package * name

    return name


OPAQUE = Opaque(location=Location((0, 0), Path(str(BUILTINS_PACKAGE)), (0, 0)))

INTERNAL_TYPES = {
    OPAQUE.identifier: OPAQUE,
}

BOOLEAN = Enumeration(
    BUILTINS_PACKAGE * "Boolean",
    [
        (ID("False", Location((0, 0), Path(str(BUILTINS_PACKAGE)), (0, 0))), Number(0)),
        (ID("True", Location((0, 0), Path(str(BUILTINS_PACKAGE)), (0, 0))), Number(1)),
    ],
    Number(1),
    False,
    Location((0, 0), Path(str(BUILTINS_PACKAGE)), (0, 0)),
)

BUILTIN_TYPES = {
    BOOLEAN.identifier: BOOLEAN,
}


def is_internal_type(name: StrID) -> bool:
    return ID(name) in INTERNAL_TYPES or any(
        ID(name) == ID(t.name) for t in INTERNAL_TYPES.values()
    )


def is_builtin_type(name: StrID) -> bool:
    return ID(name) in BUILTIN_TYPES or any(ID(name) == ID(t.name) for t in BUILTIN_TYPES.values())


def expression_list(expr: Expr) -> Sequence[Expr]:
    if isinstance(expr, And):
        return expr.terms
    return [expr]
