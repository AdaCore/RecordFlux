# pylint: disable=too-many-lines
import itertools
from abc import ABC, abstractmethod
from collections import defaultdict
from copy import copy
from pathlib import Path
from typing import Dict, List, Mapping, NamedTuple, Sequence, Set, Tuple

from rflx.common import flat_name, generic_repr
from rflx.contract import ensure, invariant
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
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
    GreaterEqual,
    Last,
    Length,
    Less,
    LessEqual,
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
    def __init__(self, identifier: StrID, location: Location = None) -> None:
        identifier = ID(identifier)

        if len(identifier.parts) != 2:
            fail(
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
    def constraints(self, name: str, proof: bool = False) -> Sequence[Expr]:
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
        error = RecordFluxError()

        if not isinstance(modulus_num, Number):
            fail(
                f'modulus of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        else:
            modulus_int = int(modulus_num)

        if modulus_int > 2 ** 64:
            error.append(
                f'modulus of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                modulus.location,
            )
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            error.append(
                f'modulus of "{self.name}" not power of two',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        error.propagate()
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

    def constraints(self, name: str, proof: bool = False) -> Sequence[Expr]:
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
        error = RecordFluxError()

        if not isinstance(first_num, Number):
            error.append(
                f'first of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        last_num = last.simplified()

        if not isinstance(last_num, Number):
            error.append(
                f'last of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return
        if int(last_num) >= 2 ** 63:
            error.append(
                f'last of "{self.name}" exceeds limit (2**63 - 1)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        if first_num < Number(0):
            error.append(
                f'first of "{self.name}" negative', Subsystem.MODEL, Severity.ERROR, self.location,
            )
        if first_num > last_num:
            error.append(
                f'range of "{self.name}" negative', Subsystem.MODEL, Severity.ERROR, self.location,
            )

        size_num = size.simplified()

        if not isinstance(size_num, Number):
            fail(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        error.propagate()

        assert isinstance(size_num, Number)
        assert isinstance(last_num, Number)

        if int(last_num).bit_length() > int(size_num):
            error.append(
                "size too small", Subsystem.MODEL, Severity.ERROR, self.location,
            )
        if int(size_num) > 64:
            error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        error.propagate()
        self.__first = first
        self.__last = last

    @property
    def first(self) -> Expr:
        return self.__first

    @property
    def last(self) -> Expr:
        return self.__last

    def constraints(self, name: str, proof: bool = False) -> Sequence[Expr]:
        if proof:
            return [
                GreaterEqual(Variable(name), self.first, location=self.location),
                LessEqual(Variable(name), self.last, location=self.location),
                Equal(Length(name), self.size, location=self.location),
            ]

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
        literals: Dict[StrID, Number],
        size: Expr,
        always_valid: bool,
        location: Location = None,
    ) -> None:
        super().__init__(identifier, size, location)

        size_num = size.simplified()
        error = RecordFluxError()

        if not isinstance(size_num, Number):
            fail(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        assert isinstance(size_num, Number)

        if max(map(int, literals.values())).bit_length() > int(size_num):
            error.append(
                "size too small", Subsystem.MODEL, Severity.ERROR, self.location,
            )
        if int(size_num) > 64:
            error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        for i1, v1 in enumerate(literals.values()):
            for i2, v2 in enumerate(literals.values()):
                if i1 < i2 and v1 == v2:
                    error.append(
                        f'duplicate enumeration value "{v1}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        v2.location,
                    )
                    error.append("previous occurrence", Subsystem.MODEL, Severity.INFO, v1.location)
        for l in literals:
            if " " in str(l) or "." in str(l):
                error.append(
                    f'invalid literal name "{l}"', Subsystem.MODEL, Severity.ERROR, self.location,
                )
        if always_valid and len(literals) == 2 ** int(size_num):
            error.append(
                f'unnecessary always-valid aspect on "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        error.propagate()
        self.literals = {ID(k): v for k, v in literals.items()}
        self.always_valid = always_valid

    def constraints(self, name: str, proof: bool = False) -> Sequence[Expr]:
        if proof:
            result: List[Expr] = [
                Or(
                    *[
                        Equal(Variable(name), Variable(l), self.location)
                        for l in self.literals.keys()
                    ],
                    self.location,
                )
            ]
            result.extend([Equal(Variable(l), v, self.location) for l, v in self.literals.items()])
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

    @property
    def element_size(self) -> Expr:
        return Length(self.element_type.name)


class Opaque(Composite):
    def __init__(self) -> None:
        super().__init__(INTERNAL_PACKAGE * "Opaque")

    @property
    def element_size(self) -> Expr:
        return Number(8)


class Field(Base):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __hash__(self) -> int:
        return hash(self.identifier)

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

    def __repr__(self) -> str:
        # pylint: disable=no-member
        return generic_repr(self.__class__.__name__, self._asdict())


def valid_message_field_types(message: "AbstractMessage") -> bool:
    for t in message.types.values():
        if not isinstance(t, (Scalar, Composite, AbstractMessage)):
            return False
    return True


@invariant(lambda self: valid_message_field_types(self))
class AbstractMessage(Type):
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
        location: Location = None,
    ) -> None:
        super().__init__(identifier, location)

        self.structure = structure
        self.__types = types

        if structure or types:
            self.__verify()
            self.__fields = self.__compute_topological_sorting()
            self.__types = {f: self.__types[f] for f in self.__fields}
            self.__paths = {f: self.__compute_paths(f) for f in self.all_fields}
            self.__definite_predecessors = {
                f: self.__compute_definite_predecessors(f) for f in self.all_fields
            }
            self.__field_condition = {
                f: self.__compute_field_condition(f).simplified() for f in self.all_fields
            }
            self.__verify_conditions()
        else:
            self.__fields = ()
            self.__paths = {}
            self.__definite_predecessors = {}
            self.__field_condition = {}

    @abstractmethod
    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> "AbstractMessage":
        raise NotImplementedError

    @abstractmethod
    def proven(self) -> "Message":
        raise NotImplementedError

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
        return self.__definite_predecessors[field]

    def field_condition(self, field: Field) -> Expr:
        return self.__field_condition[field]

    def field_size(self, field: Field) -> Expr:
        if field == FINAL:
            return Number(0)

        if field not in self.fields:
            fail(
                f'field "{field.name}" not found',
                Subsystem.INTERNAL,
                Severity.ERROR,
                self.location,
            )

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
            structure.append(Link(source, target, condition, length, first))

        types = {Field(prefix + f.identifier): t for f, t in self.types.items()}

        return self.copy(structure=structure, types=types)

    def __verify(self) -> None:
        type_fields = self.__types.keys() | {INITIAL, FINAL}
        structure_fields = {l.source for l in self.structure} | {l.target for l in self.structure}
        error = RecordFluxError()

        for f in structure_fields - type_fields:
            error.append(
                f'missing type for field "{f.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        for f in type_fields - structure_fields:
            error.append(
                f'unused field "{f.name}"', Subsystem.MODEL, Severity.ERROR, f.identifier.location,
            )

        if len(self.outgoing(INITIAL)) != 1:
            error.append(
                "ambiguous first field", Subsystem.MODEL, Severity.ERROR, self.location,
            )
            error.extend(
                [
                    ("duplicate", Subsystem.MODEL, Severity.INFO, l.target.identifier.location)
                    for l in self.outgoing(INITIAL)
                    if l.target.identifier.location
                ]
            )

        error.propagate()

        for f in structure_fields:
            for l in self.structure:
                if f in (INITIAL, l.target):
                    break
            else:
                print(f.identifier)
                error.append(
                    f'unreachable field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )

        duplicate_links = defaultdict(list)
        for link in self.structure:
            duplicate_links[(link.source, link.target, link.condition)].append(link)

        for _, links in duplicate_links.items():
            if len(links) > 1:
                error.append(
                    f'duplicate link from "{links[0].source.identifier}"'
                    f' to "{links[0].target.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                error.extend(
                    [
                        (
                            "duplicate link",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.target.identifier.location,
                        )
                        for l in links
                    ]
                )

        error.propagate()

    def __verify_conditions(self) -> None:
        literals = qualified_literals(self.types, self.package)
        variables = {f.identifier for f in self.fields}
        seen = {ID("Message")}
        error = RecordFluxError()
        for f in (INITIAL, *self.fields):
            seen.add(f.identifier)
            for index, l in enumerate(self.outgoing(f)):

                def location(part: str) -> str:
                    # pylint: disable=cell-var-from-loop
                    return (
                        f' in {part} {index} from field "{l.source.name}" to "{l.target.name}"'
                        f' in "{self.identifier}"'
                    )

                state = (variables, literals, seen)
                self.__check_vars(l.condition, state, error, l.condition.location)
                self.__check_vars(l.length, state, error, l.length.location)
                self.__check_vars(l.first, state, error, l.first.location)
                self.__check_attributes(l.condition, error, l.condition.location)
                self.__check_relations(l.condition, error, l.condition.location)
                self.__check_first_expression(l, location("First expression"))
                self.__check_length_expression(l)
        error.propagate()

    @staticmethod
    def __check_vars(
        expression: Expr,
        state: Tuple[Set[ID], Set[ID], Set[ID]],
        error: RecordFluxError,
        location: Location = None,
    ) -> None:
        variables, literals, seen = state
        for v in expression.variables(True):
            if v.identifier not in literals and v.identifier not in seen:
                if v.identifier in variables:
                    message = f'subsequent field "{v}" referenced'
                else:
                    message = f'undefined variable "{v}" referenced'
                error.append(message, Subsystem.MODEL, Severity.ERROR, location)

    def __check_attributes(
        self, expression: Expr, error: RecordFluxError, location: Location = None
    ) -> None:
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
                error.append(
                    f'invalid use of length attribute for "{a.prefix}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )

    def __check_relations(
        self, expression: Expr, error: RecordFluxError, location: Location = None
    ) -> None:
        for r in expression.findall(lambda x: isinstance(x, Relation)):
            if (
                isinstance(r, Relation)
                and not isinstance(r, (Equal, NotEqual))
                and (isinstance(r.left, Aggregate) or isinstance(r.right, Aggregate))
            ):
                error.append(
                    f'invalid relation "{r.symbol}" to aggregate',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )

            if isinstance(r, (Equal, NotEqual)) and (
                isinstance(r.left, Aggregate) or isinstance(r.right, Aggregate)
            ):
                if isinstance(r.left, Aggregate):
                    other = r.right
                elif isinstance(r.right, Aggregate):
                    other = r.left
                if not (
                    isinstance(other, Variable)
                    and Field(other.name) in self.fields
                    and isinstance(self.types[Field(other.name)], Composite)
                ):
                    error.append(
                        f'invalid relation between "{other}" and aggregate',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        location,
                    )

    @staticmethod
    def __check_first_expression(link: Link, location: str) -> None:
        if link.first != UNDEFINED and not isinstance(link.first, First):
            raise ModelError(f'invalid First for field "{link.target.name}"{location}')

    def __check_length_expression(self, link: Link) -> None:
        if link.target == FINAL and link.length != UNDEFINED:
            raise ModelError(f'length attribute for final field in "{self.identifier}"')
        if link.target != FINAL:
            t = self.types[link.target]
            unconstrained = isinstance(t, (Opaque, Array))
            if not unconstrained and link.length != UNDEFINED:
                raise ModelError(
                    f'fixed size field "{link.target.name}" with length'
                    f' expression in "{self.identifier}"'
                )
            if unconstrained and link.length == UNDEFINED:
                raise ModelError(
                    f'unconstrained field "{link.target.name}" without length'
                    f' expression in "{self.identifier}"'
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
            if isinstance(t, Scalar) and f.name not in [*literals, "Message", "Final"]
        ]

        aggregate_constraints: List[Expr] = []
        for r in expr.findall(lambda x: isinstance(x, (Equal, NotEqual))):
            assert isinstance(r, (Equal, NotEqual))
            if isinstance(r.left, Aggregate) and isinstance(r.right, Variable):
                aggregate_constraints.extend(get_constraints(r.left, r.right))
            if isinstance(r.left, Variable) and isinstance(r.right, Aggregate):
                aggregate_constraints.extend(get_constraints(r.right, r.left))

        return aggregate_constraints + [
            c for n, t in scalar_types for c in t.constraints(name=n, proof=True)
        ]

    def __prove_conflicting_conditions(self) -> None:
        for f in (INITIAL, *self.__fields):
            for i1, c1 in enumerate(self.outgoing(f)):
                for i2, c2 in enumerate(self.outgoing(f)):
                    if i1 < i2:
                        conflict = And(c1.condition, c2.condition)
                        proof = conflict.check(self.__type_constraints(conflict))
                        if proof.result == ProofResult.sat:
                            raise ModelError(
                                f'conflicting conditions {i1} and {i2} for field "{f.name}"'
                                f' in "{self.identifier}"'
                            )

    def __prove_reachability(self) -> None:
        def has_final(field: Field) -> bool:
            if field == FINAL:
                return True
            for o in self.outgoing(field):
                if has_final(o.target):
                    return True
            return False

        for f in (INITIAL, *self.__fields):
            if not has_final(f):
                raise ModelError(f'no path to FINAL for field "{f.name}"')

        for f in (*self.__fields, FINAL):
            errors = []
            for path in self.__paths[f]:
                facts = [fact for link in path for fact in self.__link_expression(link)]
                conditions = [link.condition for link in path]
                if f != FINAL:
                    facts.extend(
                        expression_list(Or(*[o.condition for o in self.outgoing(f)]).simplified())
                    )
                proof = TRUE.check([*facts, *conditions])
                if proof.result == ProofResult.sat:
                    break

                path_message = " -> ".join([l.target.name for l in path])
                errors.append(f"[{path_message}]:\n   {proof.error}")
            else:
                error_message = "\n   ".join(errors)
                raise ModelError(
                    f'unreachable field "{f.name}" in "{self.identifier}"\n{error_message}'
                )

    def __prove_contradictions(self) -> None:
        for f in (INITIAL, *self.__fields):
            for path in self.__paths[f]:
                facts = [fact for link in path for fact in self.__link_expression(link)]
                for c in self.outgoing(f):
                    contradiction = c.condition
                    constraints = self.__type_constraints(contradiction)
                    proof = contradiction.check([*constraints, *facts])
                    if proof.result == ProofResult.unsat:
                        error = RecordFluxError()
                        error.append(
                            f'contradicting condition in "{self.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            c.condition.location,
                        )
                        error.extend(
                            [
                                (
                                    f'on path "{l.target.identifier}"',
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    l.target.identifier.location,
                                )
                                for l in path
                            ]
                        )
                        error.extend(
                            [(m, Subsystem.MODEL, Severity.INFO, l) for m, l in proof.error]
                        )
                        error.propagate()

    @staticmethod
    def __target_first(link: Link) -> Expr:
        if link.source == INITIAL:
            return First("Message")
        if link.first != UNDEFINED:
            return link.first
        return Add(Last(link.source.name), Number(1))

    def __target_length(self, link: Link) -> Expr:
        if link.length != UNDEFINED:
            return link.length
        return self.field_size(link.target)

    def __target_last(self, link: Link) -> Expr:
        return Sub(Add(self.__target_first(link), self.__target_length(link)), Number(1))

    def __link_expression(self, link: Link) -> Sequence[Expr]:
        name = link.target.name
        target_first = self.__target_first(link)
        target_length = self.__target_length(link)
        target_last = self.__target_last(link)
        return [
            Equal(First(name), target_first, target_first.location),
            Equal(Length(name), target_length, target_length.location),
            Equal(Last(name), target_last, target_last.location),
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
        for f in self.__fields:
            for p, l in [(p, p[-1]) for p in self.__paths[f] if p]:
                positive = GreaterEqual(self.__target_length(l), Number(0))
                facts = [f for l in p for f in self.__link_expression(l)]
                facts.extend(self.__type_constraints(positive))
                proof = positive.check(facts)
                if proof.result != ProofResult.sat:
                    path_message = " -> ".join([l.target.name for l in p])
                    raise ModelError(
                        f'negative length for field "{f.name}" on path {path_message}'
                        f' in "{self.identifier}" ({proof.error})'
                    )

                start = GreaterEqual(self.__target_first(l), First("Message"))
                facts.extend(self.__type_constraints(start))
                proof = start.check(facts)
                if proof.result != ProofResult.sat:
                    path_message = " -> ".join([l.target.name for l in p])
                    raise ModelError(
                        f'start of field "{f.name}" on path {path_message} before'
                        f' message start in "{self.identifier} ({proof.error})'
                    )

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
        for path in [p[:-1] for p in self.__paths[FINAL] if p]:

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
                        )
                    )
                    for l in path
                ]
            )

            # Define that the end of the last field of a path is the end of the message
            facts.append(Equal(self.__target_last(path[-1]), Last("Message")))

            # Constraints for links and types
            facts.extend([f for l in path for f in self.__link_expression(l)])

            # Coverage expression must be False, i.e. no bits left
            proof = TRUE.check(facts)
            if proof.result == ProofResult.sat:
                path_message = " -> ".join([l.target.name for l in path])
                raise ModelError(
                    f'path {path_message} does not cover whole message in "{self.identifier}"'
                )

    def __prove_overlays(self) -> None:
        for f in (INITIAL, *self.__fields):
            for p, l in [(p, p[-1]) for p in self.__paths[f] if p]:
                if l.first != UNDEFINED and isinstance(l.first, First):
                    facts = [f for l in p for f in self.__link_expression(l)]
                    overlaid = Equal(self.__target_last(l), Last(l.first.prefix))
                    proof = overlaid.check(facts)
                    if proof.result != ProofResult.sat:
                        raise ModelError(
                            f'field "{f.name}" not congruent with overlaid field '
                            f'"{l.first.prefix}" in "{self.identifier}"'
                            f" ({proof.error})"
                        )

    def _prove(self) -> None:
        self.__prove_conflicting_conditions()
        self.__prove_reachability()
        self.__prove_contradictions()
        self.__prove_coverage()
        self.__prove_overlays()
        self.__prove_field_positions()

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
            fail(
                f'structure of "{self.identifier}" contains cycle',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            # We do not identify the cycles in the model, c.f. Componolit/RecordFlux#256
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
            for f in self.__fields
            if all(any(f == pf.source for pf in p) for p in self.__paths[final])
        )

    def __compute_field_condition(self, final: Field) -> Expr:
        if final == INITIAL:
            return TRUE
        return Or(
            *[
                And(self.__compute_field_condition(l.source), l.condition)
                for l in self.incoming(final)
            ]
        )


class Message(AbstractMessage):
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
        location: Location = None,
    ) -> None:
        super().__init__(identifier, structure, types, location)

        if structure or types:
            self._prove()

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> "Message":
        return Message(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
        )

    def proven(self) -> "Message":
        return copy(self)


class DerivedMessage(Message):
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            location if location else base.location,
        )
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> "DerivedMessage":
        return DerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
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
    ) -> "UnprovenMessage":
        return UnprovenMessage(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
        )

    def proven(self) -> Message:
        return Message(self.identifier, self.structure, self.types, self.location)

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
                error = RecordFluxError()
                error.append(
                    f'name conflict for "{conflicting.identifier}" in "{message.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    conflicting.identifier.location,
                )
                error.append(
                    f'when merging message "{inner_message.identifier}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    inner_message.location,
                )
                error.append(
                    f'into field "{field.name}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    field.identifier.location,
                )
                error.propagate()

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
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            location if location else base.location,
        )
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
        location: Location = None,
    ) -> "UnprovenDerivedMessage":
        return UnprovenDerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            location if location else self.location,
        )

    def proven(self) -> DerivedMessage:
        return DerivedMessage(self.identifier, self.base, self.structure, self.types, self.location)


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
    ) -> None:
        package = ID(package)

        if len(package.parts) != 1:
            fail(
                f'unexpected format of package name "{package}"',
                Subsystem.MODEL,
                Severity.ERROR,
                package.location,
            )

        super().__init__(
            package * "__REFINEMENT__"
            f"{flat_name(sdu.full_name)}__{flat_name(pdu.full_name)}__{field.name}__",
            location,
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

    @property
    def messages(self) -> Sequence[Message]:
        return [m for m in self.types if isinstance(m, Message)]

    @property
    def refinements(self) -> Sequence[Refinement]:
        return [m for m in self.types if isinstance(m, Refinement)]


class ModelError(Exception):
    pass


def qualified_literals(types: Mapping[Field, Type], package: ID) -> Set[ID]:
    literals = set()

    for t in types.values():
        if isinstance(t, Enumeration):
            for l in t.literals:
                if t.package == BUILTINS_PACKAGE or t.package == package:
                    literals.add(l)
                if t.package != BUILTINS_PACKAGE:
                    literals.add(t.package * l)

    return literals


def qualified_type_name(name: ID, package: ID) -> ID:

    if is_builtin_type(name):
        return BUILTINS_PACKAGE * name

    if is_internal_type(name):
        return INTERNAL_PACKAGE * name

    if len(name.parts) == 1:
        return package * name

    return name


INTERNAL_TYPES = {
    Opaque().identifier: Opaque(),
}

BOOLEAN = Enumeration(
    BUILTINS_PACKAGE * "Boolean",
    {
        ID("False", Location((1, 18), Path(str(BUILTINS_PACKAGE)), (1, 21))): Number(0),
        ID("True", Location((1, 24), Path(str(BUILTINS_PACKAGE)), (1, 28))): Number(1),
    },
    Number(1),
    False,
    Location((1, 1), Path(str(BUILTINS_PACKAGE)), (1, 30)),
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
