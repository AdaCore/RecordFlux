# pylint: disable=too-many-lines
import itertools
from abc import ABC, abstractmethod, abstractproperty
from copy import copy
from math import log
from typing import Dict, Mapping, NamedTuple, Sequence, Set, Tuple

from rflx.common import generic_repr
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    And,
    Attribute,
    Equal,
    Expr,
    First,
    GreaterEqual,
    If,
    Last,
    Length,
    Less,
    LessEqual,
    Not,
    Number,
    Or,
    Pow,
    ProofResult,
    Sub,
    Variable,
)

BUILTINS_PACKAGE = "__BUILTINS__"


class Element(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)


class Type(Element):
    def __init__(self, full_name: str) -> None:
        if full_name.count(".") != 1:
            raise ModelError(f'unexpected format of type name "{full_name}"')
        self.full_name = full_name

    @property
    def name(self) -> str:
        return self.full_name.rsplit(".", 1)[1]

    @property
    def package(self) -> str:
        return self.full_name.rsplit(".", 1)[0]


class Scalar(Type):
    @property
    def size(self) -> Expr:
        raise NotImplementedError

    @abstractmethod
    def constraints(self, name: str, proof: bool = False) -> Expr:
        raise NotImplementedError


class Integer(Scalar):
    @abstractproperty
    def first(self) -> Expr:
        raise NotImplementedError

    @abstractproperty
    def last(self) -> Expr:
        raise NotImplementedError


class ModularInteger(Integer):
    def __init__(self, full_name: str, modulus: Expr) -> None:
        super().__init__(full_name)
        modulus_num = modulus.simplified()
        if not isinstance(modulus_num, Number):
            raise ModelError(f'modulus of "{self.name}" contains variable')
        modulus_int = int(modulus_num)
        if modulus_int > 2 ** 64:
            raise ModelError(f'modulus of "{self.name}" exceeds limit (2**64)')
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            raise ModelError(f'modulus of "{self.name}" not power of two')
        self.__modulus = modulus
        self.__size = Number(int(log(modulus_int) / log(2)))

    @property
    def modulus(self) -> Expr:
        return self.__modulus

    @property
    def first(self) -> Expr:
        return Number(0)

    @property
    def last(self) -> Expr:
        return Sub(self.modulus, Number(1))

    @property
    def size(self) -> Expr:
        return self.__size

    def constraints(self, name: str, proof: bool = False) -> Expr:
        if proof:
            return And(
                Less(Variable(name), self.__modulus), GreaterEqual(Variable(name), Number(0))
            )
        return TRUE


class RangeInteger(Integer):
    def __init__(self, full_name: str, first: Expr, last: Expr, size: Expr) -> None:
        super().__init__(full_name)
        first_num = first.simplified()
        if not isinstance(first_num, Number):
            raise ModelError(f'first of "{self.name}" contains variable')
        last_num = last.simplified()
        if not isinstance(last_num, Number):
            raise ModelError(f'last of "{self.name}" contains variable')
        if first_num < Number(0):
            raise ModelError(f'first of "{self.name}" negative')
        if first_num > last_num:
            raise ModelError(f'range of "{self.name}" negative')
        size_num = size.simplified()
        if not isinstance(size_num, Number):
            raise ModelError(f'size of "{self.name}" contains variable')
        if log(int(last_num) + 1) / log(2) > int(size_num):
            raise ModelError(f'size for "{self.name}" too small')
        self.__first = first
        self.__last = last
        self.__size = size

    @property
    def first(self) -> Expr:
        return self.__first

    @property
    def last(self) -> Expr:
        return self.__last

    @property
    def size(self) -> Expr:
        return self.__size

    def constraints(self, name: str, proof: bool = False) -> Expr:
        if proof:
            return And(
                GreaterEqual(Variable(name), self.first), LessEqual(Variable(name), self.last)
            )

        c: Expr = TRUE
        if self.first.simplified() != self.base_first.simplified():
            c = GreaterEqual(Variable(name), self.first)
        if self.last.simplified() != self.base_last.simplified():
            c = And(c, LessEqual(Variable(name), self.last))
        return c.simplified()

    @property
    def base_first(self) -> Expr:
        return Number(0)

    @property
    def base_last(self) -> Expr:
        return Sub(Pow(Number(2), self.size), Number(1))


class Enumeration(Scalar):
    def __init__(
        self, full_name: str, literals: Dict[str, Number], size: Number, always_valid: bool
    ) -> None:
        super().__init__(full_name)
        if log(max(map(int, literals.values())) + 1) / log(2) > int(size):
            raise ModelError(f'size for "{self.name}" too small')
        if len(set(literals.values())) < len(literals.values()):
            raise ModelError(f'"{self.name}" contains elements with same value')
        self.literals = literals
        self.__size = size
        self.always_valid = always_valid

    def constraints(self, name: str, proof: bool = False) -> Expr:
        if proof:
            return And(
                And(*[Equal(Variable(l), v) for l, v in self.literals.items()]),
                Or(*[Equal(Variable(name), Variable(l)) for l in self.literals.keys()]),
            )
        return TRUE

    @property
    def size(self) -> Number:
        return self.__size

    @property
    def enum_name(self) -> str:
        return f"{self.name}_Enum"

    @property
    def full_enum_name(self) -> str:
        return f"{self.full_name}_Enum"


class Composite(Type):
    pass


class Array(Composite):
    def __init__(self, full_name: str, element_type: Type) -> None:
        super().__init__(full_name)
        self.element_type = element_type


class Opaque(Composite):
    def __init__(self) -> None:
        super().__init__(f"__PACKAGE__.Opaque")


class Reference(Type):
    pass


class Field(NamedTuple):
    name: str

    def __repr__(self) -> str:
        # pylint: disable=no-member
        return generic_repr(self.__class__.__name__, self._asdict())

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


class AbstractMessage(Type):
    def __init__(
        self, full_name: str, structure: Sequence[Link], types: Mapping[Field, Type]
    ) -> None:
        super().__init__(full_name)

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
        full_name: str = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
    ) -> "AbstractMessage":
        raise NotImplementedError

    @abstractmethod
    def proven_message(self) -> "Message":
        raise NotImplementedError

    @property
    def name(self) -> str:
        return self.full_name.rsplit(".", 1)[1]

    @property
    def package(self) -> str:
        return self.full_name.rsplit(".", 1)[0]

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
            raise ValueError(f'field "{field.name}" not found')

        field_type = self.types[field]
        if isinstance(field_type, Scalar):
            return field_type.size

        raise NotImplementedError

    def __verify(self) -> None:
        type_fields = self.__types.keys() | {INITIAL, FINAL}
        structure_fields = {l.source for l in self.structure} | {l.target for l in self.structure}

        for f in structure_fields - type_fields:
            raise ModelError(f'missing type for field "{f.name}" of "{self.full_name}"')

        for f in type_fields - structure_fields:
            raise ModelError(f'superfluous field "{f.name}" in field types of "{self.full_name}"')

        if len(self.outgoing(INITIAL)) != 1:
            raise ModelError(f'ambiguous first field in "{self.full_name}"')

        for f in structure_fields:
            for l in self.structure:
                if f in (INITIAL, l.target):
                    break
            else:
                raise ModelError(f'unreachable field "{f.name}" in "{self.full_name}"')

        duplicate_links = set(l for l in self.structure if self.structure.count(l) > 1)
        if duplicate_links:
            raise ModelError(
                f'duplicate links in "{self.full_name}": '
                + ", ".join(f"{l.source.name} -> {l.target.name}" for l in duplicate_links)
            )

    @staticmethod
    def __check_vars(
        expression: Expr,
        state: Tuple[Set[str], Set[str], Set[str]],
        link: Link,
        index: int,
        location: Tuple[str, str],
    ) -> None:
        variables, literals, seen = state
        message, part = location
        for v in expression.variables(True):
            if v.name not in literals and v.name not in seen:
                if v.name in variables:
                    raise ModelError(
                        f'subsequent field "{v}" referenced in {part} '
                        f'{index} from field "{link.source.name}" to '
                        f'"{link.target.name}" in "{message}"'
                    )
                raise ModelError(
                    f'undefined variable "{v}" referenced in {part} '
                    f'{index} from field "{link.source.name}" to '
                    f'"{link.target.name}" in "{message}"'
                )

    def __verify_conditions(self) -> None:
        literals = qualified_literals(self.types, self.package)
        variables = {
            v
            for f in self.fields
            if isinstance(f.name, str)
            for v in [f.name, f"{f.name}'First", f"{f.name}'Length", f"{f.name}'Last"]
        }
        seen = set({"Message'First", "Message'Last", "Message'Length"})
        variables.update(*seen)
        for f in (INITIAL, *self.fields):
            for v in [f.name, f"{f.name}'Length", f"{f.name}'First", f"{f.name}'Last"]:
                seen.add(v)
            for index, l in enumerate(self.outgoing(f)):
                self.__check_vars(
                    l.condition,
                    (variables, literals, seen),
                    l,
                    index,
                    (self.full_name, "condition"),
                )
                self.__check_vars(
                    l.length,
                    (variables, literals, seen),
                    l,
                    index,
                    (self.full_name, "Length expression"),
                )
                self.__check_vars(
                    l.first,
                    (variables, literals, seen),
                    l,
                    index,
                    (self.full_name, "First expression"),
                )

                if l.first != UNDEFINED and not isinstance(l.first, First):
                    raise ModelError(
                        f'invalid First for field "{l.target.name}" in First'
                        f' expression {index} from field "{f.name}" to'
                        f' "{l.target.name}" in "{self.full_name}"'
                    )

                if l.target != FINAL:
                    t = self.types[l.target]
                    unconstrained = isinstance(t, (Opaque, Array))
                    if not unconstrained and l.length != UNDEFINED:
                        raise ModelError(
                            f'fixed size field "{l.target.name}" with length'
                            f' expression in "{self.full_name}"'
                        )
                    if unconstrained and l.length == UNDEFINED:
                        raise ModelError(
                            f'unconstrained field "{l.target.name}" without length'
                            f' expression in "{self.full_name}"'
                        )

    def __type_constraints(self, expr: Expr) -> Expr:
        literals = qualified_literals(self.types, self.package)
        return And(
            *[
                t.constraints(name=n, proof=True)
                for n, t in [
                    (v.name, self.types[Field(v.name)])
                    for v in expr.variables()
                    if not isinstance(v, Attribute)
                    and isinstance(v.name, str)
                    and v.name not in literals
                ]
                if isinstance(t, Scalar)
            ]
        )

    def __with_constraints(self, expr: Expr) -> Expr:
        return And(self.__type_constraints(expr), expr)

    def __prove_conflicting_conditions(self) -> None:
        for f in (INITIAL, *self.__fields):
            conflict = LessEqual(
                Add(
                    *[
                        If([(self.__with_constraints(c.condition), Number(1))], Number(0))
                        for c in self.outgoing(f)
                    ]
                ),
                Number(1),
            )
            result = conflict.forall()
            if result != ProofResult.sat:
                message = str(conflict).replace("\n", "")
                raise ModelError(
                    f'conflicting conditions for field "{f.name}"'
                    f' in "{self.full_name}" ({result}: {message})'
                )

    def __prove_reachability(self) -> None:
        for f in (*self.__fields, FINAL):
            reachability = Or(
                *[
                    And(*[self.__with_constraints(l.condition) for l in path])
                    for path in self.__paths[f]
                ]
            )
            result = reachability.exists()
            if result != ProofResult.sat:
                message = str(reachability).replace("\n", "")
                raise ModelError(
                    f'unreachable field "{f.name}" in "{self.full_name}"' f" ({result}: {message})"
                )

    def __prove_contradictions(self) -> None:
        for f in (INITIAL, *self.__fields):
            for index, c in enumerate(self.outgoing(f)):
                contradiction = Equal(self.__with_constraints(c.condition), FALSE)
                result = contradiction.forall()
                if result == ProofResult.sat:
                    message = str(contradiction).replace("\n", "")
                    raise ModelError(
                        f'contradicting condition {index} from field "{f.name}" to'
                        f' "{c.target.name}" in "{self.full_name}"'
                        f" ({result}: {message})"
                    )

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

    def __link_expression(self, link: Link) -> Expr:
        name = link.target.name
        return And(
            *[
                Equal(First(name), self.__target_first(link)),
                Equal(Length(name), self.__target_length(link)),
                Equal(Last(name), self.__target_last(link)),
                GreaterEqual(First("Message"), Number(0)),
                GreaterEqual(Last("Message"), Last(name)),
                GreaterEqual(Last("Message"), First("Message")),
                Equal(Length("Message"), Add(Sub(Last("Message"), First("Message")), Number(1))),
                link.condition,
            ]
        )

    def __prove_field_positions(self) -> None:
        for f in self.__fields:
            for p, l in [(p, p[-1]) for p in self.__paths[f] if p]:
                path_expressions = And(*[self.__link_expression(l) for l in p])
                length = self.__target_length(l)
                positive = If(
                    [
                        (
                            And(
                                self.__type_constraints(And(path_expressions, length)),
                                path_expressions,
                            ),
                            GreaterEqual(length, Number(0)),
                        )
                    ],
                    TRUE,
                )
                result = positive.forall()
                if result != ProofResult.sat:
                    path_message = " -> ".join([l.target.name for l in p])
                    message = str(positive.simplified()).replace("\n\t", "")
                    raise ModelError(
                        f'negative length for field "{f.name}" on path {path_message}'
                        f' in "{self.full_name}" ({result}: {message})'
                    )

                first = self.__target_first(l)
                start = If(
                    [
                        (
                            And(
                                self.__type_constraints(And(path_expressions, first)),
                                path_expressions,
                            ),
                            GreaterEqual(first, First("Message")),
                        )
                    ],
                    TRUE,
                )
                result = start.forall()
                if result != ProofResult.sat:
                    path_message = " -> ".join([l.target.name for l in p])
                    message = str(start.simplified()).replace("\n\t", "")
                    raise ModelError(
                        f'start of field "{f.name}" on path {path_message} before'
                        f' message start in "{self.full_name} ({result}: {message})'
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
            # Calculate (1)
            message_range = And(
                GreaterEqual(Variable("f"), First("Message")),
                LessEqual(Variable("f"), Last("Message")),
            )
            # Calculate (2) for all fields
            fields = And(
                *[
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
            last_field = Equal(self.__target_last(path[-1]), Last("Message"))
            # Constraints for links and types
            path_expressions = self.__with_constraints(
                And(*[self.__link_expression(l) for l in path])
            )

            # Coverage expression must be False, i.e. no bits left
            coverage = Not(And(*[fields, last_field, path_expressions, message_range]))
            result = coverage.forall()
            if result != ProofResult.sat:
                path_message = " -> ".join([l.target.name for l in path])
                message = str(coverage).replace("\n\t", "")
                raise ModelError(
                    f"path {path_message} does not cover whole message"
                    f' in "{self.full_name}" ({result}: {message})'
                )

    def __prove_overlays(self) -> None:
        for f in (INITIAL, *self.__fields):
            for p, l in [(p, p[-1]) for p in self.__paths[f] if p]:
                if l.first != UNDEFINED and isinstance(l.first, First):
                    path_expressions = And(*[self.__link_expression(l) for l in p])
                    overlaid = If(
                        [(path_expressions, Equal(self.__target_last(l), Last(l.first.name)))], TRUE
                    )
                    result = overlaid.forall()
                    if result != ProofResult.sat:
                        message = str(overlaid).replace("\n", "")
                        raise ModelError(
                            f'field "{f.name}" not congruent with overlaid field '
                            f'"{l.first.name}" in "{self.full_name}"'
                            f" ({result}: {message})"
                        )

    def _prove(self) -> None:
        self.__prove_field_positions()
        self.__prove_conflicting_conditions()
        self.__prove_reachability()
        self.__prove_contradictions()
        self.__prove_coverage()
        self.__prove_overlays()

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
        self, full_name: str, structure: Sequence[Link], types: Mapping[Field, Type]
    ) -> None:

        super().__init__(full_name, structure, types)

        if structure or types:
            self._prove()

    def copy(
        self,
        full_name: str = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
    ) -> "Message":
        return Message(
            full_name if full_name else self.full_name,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
        )

    def proven_message(self) -> "Message":
        return copy(self)


class DerivedMessage(Message):
    def __init__(
        self,
        full_name: str,
        full_base_name: str,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
    ) -> None:

        check_message_name(full_base_name)

        super().__init__(full_name, structure, types)
        self.full_base_name = full_base_name

    def copy(
        self,
        full_name: str = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
    ) -> "DerivedMessage":
        return DerivedMessage(
            full_name if full_name else self.full_name,
            self.full_base_name,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
        )

    def proven_message(self) -> "DerivedMessage":
        return copy(self)

    @property
    def base_name(self) -> str:
        return self.full_base_name.rsplit(".", 1)[1]

    @property
    def base_package(self) -> str:
        return self.full_base_name.rsplit(".", 1)[0]


class UnprovenMessage(AbstractMessage):
    def copy(
        self,
        full_name: str = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
    ) -> "UnprovenMessage":
        return UnprovenMessage(
            full_name if full_name else self.full_name,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
        )

    def proven_message(self) -> Message:
        return Message(self.full_name, self.structure, self.types)


class UnprovenDerivedMessage(UnprovenMessage):
    def __init__(
        self,
        full_name: str,
        full_base_name: str,
        structure: Sequence[Link],
        types: Mapping[Field, Type],
    ) -> None:

        check_message_name(full_base_name)

        super().__init__(full_name, structure, types)
        self.full_base_name = full_base_name

    def copy(
        self,
        full_name: str = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, Type] = None,
    ) -> "UnprovenDerivedMessage":
        return UnprovenDerivedMessage(
            full_name if full_name else self.full_name,
            self.full_base_name,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
        )

    def proven_message(self) -> DerivedMessage:
        return DerivedMessage(self.full_name, self.full_base_name, self.structure, self.types)


class Refinement(Type):
    # pylint: disable=too-many-arguments
    def __init__(
        self, package: str, pdu: str, field: Field, sdu: str, condition: Expr = TRUE
    ) -> None:
        super().__init__(f"{package}.")
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


class Model(Element):
    def __init__(self, messages: Sequence[Message], refinements: Sequence[Refinement]) -> None:
        self.messages = messages
        self.refinements = refinements


class ModelError(Exception):
    pass


def prefixed_message(message: AbstractMessage, prefix: str) -> AbstractMessage:
    def prefixed_expression(expression: Expr) -> Expr:
        return expression.simplified(
            {v: v.__class__(f"{prefix}{v.name}") for v in expression.variables()}
        )

    structure = []

    for l in message.structure:
        source = Field(f"{prefix}{l.source.name}") if l.source != INITIAL else INITIAL
        target = Field(f"{prefix}{l.target.name}") if l.target != FINAL else FINAL
        condition = prefixed_expression(l.condition)
        length = prefixed_expression(l.length) if l.length != UNDEFINED else UNDEFINED
        first = prefixed_expression(l.first) if l.first != UNDEFINED else UNDEFINED
        structure.append(Link(source, target, condition, length, first))

    types = {Field(f"{prefix}{f.name}"): t for f, t in message.types.items()}

    return message.copy(structure=structure, types=types)


def merged_message(name: str, messages: Mapping[str, AbstractMessage]) -> AbstractMessage:
    assert name in messages, f'unknown message "{name}"'

    check_message_references(name, messages)

    message = messages[name]

    while True:
        references = [(f, t) for f, t in message.types.items() if isinstance(t, Reference)]

        if not references:
            break

        ref_field, ref_type = references.pop(0)
        inner_message = prefixed_message(messages[ref_type.full_name], f"{ref_field.name}_")

        name_conflicts = [
            f.name for f in message.fields for g in inner_message.fields if f.name == g.name
        ]

        if name_conflicts:
            raise ModelError(
                f'name conflict for "{name_conflicts.pop(0)}" in "{name}" caused by reference'
                f' "{ref_field.name}" to "{ref_type.full_name}"'
            )

        structure = []

        for link in message.structure:
            if link.target == ref_field:
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
            elif link.source == ref_field:
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
            **{f: t for f, t in message.types.items() if f != ref_field},
            **inner_message.types,
        }

        message = message.copy(structure=structure, types=types)

    return message


def check_message_name(full_name: str) -> None:
    if full_name.count(".") != 1:
        raise ModelError(f'unexpected format of message name "{full_name}"')


def check_message_references(name: str, messages: Mapping[str, AbstractMessage]) -> None:
    nodes = [messages[name]]
    edges: Set[Tuple[str, str, str]] = set()

    while nodes:
        message = nodes.pop(0)
        for e in [
            (message.full_name, f.name, t.full_name)
            for f, t in message.types.items()
            if isinstance(t, Reference)
        ]:
            if e in edges:
                raise ModelError(f'references in "{name}" contain cycle')

            if e[2] not in messages:
                raise ModelError(f'reference to unknown message "{e[2]}"')

            edges.add(e)
            nodes.append(messages[e[2]])


def qualified_literals(types: Mapping[Field, Type], package: str) -> Set[str]:
    return {
        l if t.package == package or t.package == BUILTINS_PACKAGE else f"{t.package}.{l}"
        for t in types.values()
        if isinstance(t, Enumeration)
        for l in t.literals
    }


BOOLEAN = Enumeration(
    f"{BUILTINS_PACKAGE}.Boolean", {"False": Number(0), "True": Number(1)}, Number(1), False
)

BUILTIN_TYPES = {
    Opaque().name: Opaque(),
    BOOLEAN.name: BOOLEAN,
}
