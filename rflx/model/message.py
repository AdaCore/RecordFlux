from __future__ import annotations

import itertools
from collections import defaultdict
from collections.abc import Iterable, Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from copy import copy
from dataclasses import dataclass, field as dataclass_field
from enum import Enum
from functools import cached_property
from typing import Optional, Union

import rflx.typing_ as rty
from rflx import expression as expr
from rflx.common import Base, indent, indent_next, unique, verbose_repr
from rflx.const import MP_CONTEXT
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail, fatal_fail
from rflx.identifier import ID, StrID
from rflx.model.top_level_declaration import TopLevelDeclaration

from . import type_ as mty


class ByteOrder(Enum):
    HIGH_ORDER_FIRST = 1
    LOW_ORDER_FIRST = 2


class Field(Base):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __repr__(self) -> str:
        return f'Field("{self.identifier}")'

    def __lt__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.identifier < other.identifier
        return NotImplemented

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def affixed_name(self) -> str:
        return f"F_{self.name}"


INITIAL = Field("Initial")
FINAL = Field("Final")


@dataclass(order=True)
class Link(Base):
    source: Field
    target: Field
    condition: expr.Expr = expr.TRUE
    size: expr.Expr = expr.UNDEFINED
    first: expr.Expr = expr.UNDEFINED
    location: Optional[Location] = dataclass_field(default=None, repr=False)

    def __str__(self) -> str:
        condition = indent_next(
            f"\nif {indent_next(str(self.condition), 3)}" if self.condition != expr.TRUE else "",
            3,
        )
        aspects = []
        if self.size != expr.UNDEFINED:
            aspects.append(f"Size => {self.size}")
        if self.first != expr.UNDEFINED:
            aspects.append(f"First => {self.first}")
        with_clause = indent_next("\nwith " + ", ".join(aspects) if aspects else "", 3)
        target_name = self.target.name if self.target != FINAL else "null"
        return f"then {target_name}{with_clause}{condition}"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.source == other.source
                and self.target == other.target
                and self.condition == other.condition
                and self.size == other.size
                and self.first == other.first
            )
        return NotImplemented

    def __hash__(self) -> int:
        return 0

    @property
    def has_implicit_size(self) -> bool:
        return bool(self.size.findall(lambda x: x in [expr.Size("Message"), expr.Last("Message")]))


class Message(mty.Type):
    def __init__(  # noqa: PLR0913
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, mty.Type],
        checksums: Optional[Mapping[ID, Sequence[expr.Expr]]] = None,
        byte_order: Optional[Union[ByteOrder, Mapping[Field, ByteOrder]]] = None,
        location: Optional[Location] = None,
        skip_verification: bool = False,
        workers: int = 1,
    ) -> None:
        assert not types if not structure else True

        super().__init__(identifier, location)

        self.error.propagate()

        if not structure:
            structure = [Link(INITIAL, FINAL)]

        self._structure = sorted(structure)
        self._checksums = checksums or {}
        self._byte_order = {}

        self._field_types = {}
        self._parameter_types = {}
        self._first: dict[Link, tuple[Field, expr.Expr]] = {}
        dependencies = _dependencies(types)
        self._unqualified_enum_literals = mty.unqualified_enum_literals(dependencies, self.package)
        self._qualified_enum_literals = mty.qualified_enum_literals(dependencies)
        self._type_names = mty.qualified_type_names(dependencies)
        self._paths_cache: dict[Field, set[tuple[Link, ...]]] = {}
        self._definite_predecessors_cache: dict[Field, tuple[Field, ...]] = {}
        self._path_condition_cache: dict[Field, expr.Expr] = {}

        try:
            if not self.is_null:
                self._has_unreachable = self._validate(self._structure, types)
                self._structure, self._checksums = self._normalize(
                    self._structure,
                    types,
                    self._checksums,
                )
                fields = self._compute_topological_sorting(self._has_unreachable)
                if fields:
                    # The fields of `types` are used to preserve the locations of the field
                    # definitions. `fields` cannot be used directly, as it contains the field
                    # identifiers of link targets.
                    ft = {f: (f, t) for f, t in types.items()}
                    self._field_types = dict(ft[f] for f in fields)
                    self._parameter_types = dict(ft[f] for f in types if f not in fields)
                self._set_types()
            byte_order = byte_order if byte_order else ByteOrder.HIGH_ORDER_FIRST
            if not isinstance(byte_order, dict):
                assert isinstance(byte_order, ByteOrder)
                self._byte_order = {f: byte_order for f in self.fields}
            else:
                assert all(f in byte_order for f in self.fields)
                assert (
                    all(f in self.fields for f in byte_order) if not self._has_unreachable else True
                )
                self._byte_order = byte_order
        except RecordFluxError:
            pass

        self.error.propagate()

        self._refinements: list[Refinement] = []
        self._skip_verification = skip_verification
        self._workers = workers

        if not self.error.errors and not skip_verification:
            self._verify()

        self.error.propagate()

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.identifier == other.identifier
                and self.structure == other.structure
                and self.types == other.types
                and self.byte_order == other.byte_order
                and self.checksums == other.checksums
            )
        return NotImplemented

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "structure", "types", "checksums", "byte_order"])

    def __str__(self) -> str:
        if self.is_null:
            return f"type {self.name} is null message"

        parameters = "; ".join(
            [
                f"{parameter_field.identifier} : {parameter_type_identifier}"
                for parameter_field, parameter_type in self.parameter_types.items()
                for parameter_type_identifier in (parameter_type.qualified_identifier,)
            ],
        )
        if parameters:
            parameters = f" ({parameters})"

        fields = ""
        field_list = [INITIAL, *self.fields]
        for i, field in enumerate(field_list):
            if field != INITIAL:
                fields += "\n" if fields else ""
                field_type_identifier = self.types[field].qualified_identifier
                fields += f"{field.name} : {field_type_identifier}"
            outgoing = self.outgoing(field)
            if not (
                len(outgoing) == 1
                and outgoing[0].condition == expr.TRUE
                and outgoing[0].size == expr.UNDEFINED
                and outgoing[0].first == expr.UNDEFINED
                and (i >= len(field_list) - 1 or field_list[i + 1] == outgoing[0].target)
            ):
                if field == INITIAL:
                    fields += "null"
                fields += "\n" + indent("\n".join(str(o) for o in outgoing), 3)
            if fields:
                fields += ";"

        checksum_aspect = ""
        if self.checksums:
            checksums = ", ".join(
                [
                    f'{identifier} => ({", ".join(str(e) for e in expressions)})'
                    for identifier, expressions in self.checksums.items()
                ],
            )
            checksum_aspect = f"Checksum => ({checksums})"

        aspects = ""
        if checksum_aspect:
            aspects = f" with\n{indent(checksum_aspect, 6)}"

        return (
            f"type {self.name}{parameters} is\n   message\n{indent(fields, 6)}\n   end message"
            f"{aspects}"
        )

    @property
    def is_null(self) -> bool:
        return self._structure == [Link(INITIAL, FINAL)] and not self.types

    @property
    def direct_dependencies(self) -> list[mty.Type]:
        return [*self.types.values(), self]

    @property
    def dependencies(self) -> list[mty.Type]:
        return [*_dependencies(self.types), self]

    @property
    def byte_order(self) -> Mapping[Field, ByteOrder]:
        return self._byte_order

    def copy(  # noqa: PLR0913
        self,
        identifier: Optional[StrID] = None,
        structure: Optional[Sequence[Link]] = None,
        types: Optional[Mapping[Field, mty.Type]] = None,
        checksums: Optional[Mapping[ID, Sequence[expr.Expr]]] = None,
        byte_order: Optional[Union[ByteOrder, Mapping[Field, ByteOrder]]] = None,
        location: Optional[Location] = None,
        skip_verification: Optional[bool] = None,
    ) -> Message:
        return Message(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            checksums if checksums else copy(self.checksums),
            byte_order if byte_order else copy(self.byte_order),
            location if location else self.location,
            skip_verification if skip_verification else self._skip_verification,
        )

    @property
    def parameters(self) -> tuple[Field, ...]:
        return tuple(self._parameter_types or {})

    @property
    def fields(self) -> tuple[Field, ...]:
        """Return fields topologically sorted."""
        return tuple(self._field_types or {})

    @property
    def all_fields(self) -> tuple[Field, ...]:
        return (INITIAL, *self.fields, FINAL)

    @property
    def parameter_types(self) -> Mapping[Field, mty.Type]:
        """Return parameters and corresponding types."""
        return self._parameter_types

    @property
    def field_types(self) -> Mapping[Field, mty.Type]:
        """Return fields and corresponding types topologically sorted."""
        return self._field_types

    @property
    def structure(self) -> Sequence[Link]:
        return self._structure

    @property
    def types(self) -> Mapping[Field, mty.Type]:
        """Return parameters, fields and corresponding types topologically sorted."""
        return {**self._parameter_types, **self._field_types}

    @property
    def checksums(self) -> Mapping[ID, Sequence[expr.Expr]]:
        return self._checksums

    def incoming(self, field: Field) -> list[Link]:
        return [l for l in self.structure if l.target == field]

    def outgoing(self, field: Field) -> list[Link]:
        return [l for l in self.structure if l.source == field]

    def predecessors(self, field: Field) -> tuple[Field, ...]:
        if field == INITIAL:
            return ()
        if field == FINAL:
            return self.fields
        return self.fields[: self.fields.index(field)]

    def successors(self, field: Field) -> tuple[Field, ...]:
        if field == INITIAL:
            return self.fields
        if field == FINAL:
            return ()
        return self.fields[self.fields.index(field) + 1 :]

    def direct_predecessors(self, field: Field) -> list[Field]:
        return list(dict.fromkeys([l.source for l in self.incoming(field)]))

    def direct_successors(self, field: Field) -> list[Field]:
        return list(dict.fromkeys([l.target for l in self.outgoing(field)]))

    def definite_predecessors(self, field: Field) -> tuple[Field, ...]:
        """Return preceding fields which are part of all possible paths."""
        try:
            return self._definite_predecessors_cache[field]
        except KeyError:
            result = tuple(
                f
                for f in self.fields
                if all(any(f == pf.source for pf in p) for p in self.paths(field))
            )

            self._definite_predecessors_cache[field] = result

            return result

    def path_condition(self, field: Field) -> expr.Expr:
        """Return conjunction of all conditions on path from INITIAL to field."""
        try:
            return self._path_condition_cache[field]
        except KeyError:
            if field == INITIAL:
                return expr.TRUE

            result = expr.Or(
                *[
                    expr.And(self.path_condition(l.source), l.condition)
                    for l in self.incoming(field)
                ],
                location=field.identifier.location,
            ).simplified()

            self._path_condition_cache[field] = result

            return result

    def field_size_opt(self, field: Field) -> Optional[expr.Number]:
        """Return field size if field size is fixed and None otherwise."""
        if field == FINAL:
            return expr.Number(0)

        assert field in self.fields, f'field "{field.name}" not found'

        field_type = self.types[field]

        if isinstance(field_type, mty.Scalar):
            return field_type.size

        sizes = [
            l.size.substituted(mapping=to_mapping(self.message_constraints)).simplified()
            for l in self.incoming(field)
        ]
        size = sizes[0]
        if isinstance(size, expr.Number) and all(size == s for s in sizes):
            return size
        return None

    def field_size(self, field: Field) -> expr.Number:
        """Return field size if field size is fixed and fail otherwise."""
        result = self.field_size_opt(field)
        if result is not None:
            return result
        fail(
            f'unable to calculate size of field "{field.name}" of message "{self.identifier}"',
            Subsystem.MODEL,
            Severity.ERROR,
            field.identifier.location,
        )

    def paths(self, field: Field) -> set[tuple[Link, ...]]:
        try:
            return self._paths_cache[field]
        except KeyError:
            if field == INITIAL:
                return set()

            result = set()

            for l in self.incoming(field):
                source = self.paths(l.source)
                for s in source:
                    result.add((*s, l))
                if not source:
                    result.add((l,))

            self._paths_cache[field] = result

            return result

    def prefixed(self, prefix: str) -> Message:
        fields = {f.identifier for f in self.fields}

        def prefixed_expression(expression: expr.Expr) -> expr.Expr:
            variables = {v.identifier for v in expression.variables()}
            literals = {l for l in variables - fields if len(l.parts) == 1}

            return expression.substituted(
                mapping={
                    **{
                        v: v.__class__(ID(prefix) + v.name)
                        for v in expression.variables()
                        if v.identifier in fields
                    },
                    **{
                        v: v.__class__(self.package * v.name)
                        for v in expression.variables()
                        if v.identifier in literals
                        and v.identifier not in mty.BUILTIN_LITERALS
                        and v.identifier != ID("Message")
                        and Field(v.identifier) not in self.parameters
                    },
                },
            ).simplified()

        structure = []

        for l in self.structure:
            source = Field(prefix + l.source.identifier) if l.source != INITIAL else INITIAL
            target = Field(prefix + l.target.identifier) if l.target != FINAL else FINAL
            condition = prefixed_expression(l.condition)
            size = prefixed_expression(l.size)
            first = prefixed_expression(l.first)
            structure.append(Link(source, target, condition, size, first, l.location))

        types = {
            **{Field(f.identifier): t for f, t in self.parameter_types.items()},
            **{Field(prefix + f.identifier): t for f, t in self.field_types.items()},
        }

        checksums = (
            {prefix + i: [prefixed_expression(e) for e in e] for i, e in self.checksums.items()}
            if self.checksums
            else self.checksums
        )
        byte_order = {Field(prefix + f.identifier): t for f, t in self.byte_order.items()}

        return self.copy(
            structure=structure,
            types=types,
            checksums=checksums,
            byte_order=byte_order,
            skip_verification=True,
        )

    def typed_expression(self, expression: expr.Expr, types: Mapping[Field, mty.Type]) -> expr.Expr:
        return typed_expression(expression, types, self._qualified_enum_literals, self._type_names)

    @cached_property
    def message_constraints(self) -> list[expr.Expr]:
        return message_constraints(self.types, self._qualified_enum_literals, self._type_names)

    def aggregate_constraints(self, expression: expr.Expr) -> list[expr.Expr]:
        return aggregate_constraints(expression, self.types)

    def is_possibly_empty(self, field: Field) -> bool:
        if isinstance(self.types[field], mty.Scalar):
            return False

        for p in self.paths(FINAL):
            if not any(l.target == field for l in p):
                continue
            empty_field = expr.Equal(expr.Size(field.name), expr.Number(0))
            proof = self._prove_path_property(empty_field, p)
            if proof.result == expr.ProofResult.SAT:
                return True

        return False

    def set_refinements(self, refinements: list[Refinement]) -> None:
        if any(r.pdu != self for r in refinements):
            fatal_fail("setting refinements for different message", Subsystem.MODEL)
        self._refinements = refinements

    @property
    def type_(self) -> rty.Message:
        return rty.Message(
            self.full_name,
            {
                (
                    *(p.name for p in self.parameters),
                    *(l.target.name for l in p if l.target != FINAL),
                )
                for p in self.paths(FINAL)
            }
            if not self.is_null
            else set(),
            {f.identifier: t.type_ for f, t in self._parameter_types.items()},
            {f.identifier: t.type_ for f, t in self._field_types.items()},
            [rty.Refinement(r.field.identifier, r.sdu.type_, r.package) for r in self._refinements],
            self.is_definite,
        )

    @property
    def has_fixed_size(self) -> bool:
        return len(self.paths(FINAL)) <= 1 and not (
            {v.identifier for l in self.structure for v in l.size.variables()}
            - set(self._type_names.keys())
        )

    @property
    def has_implicit_size(self) -> bool:
        return any(l.has_implicit_size for l in self.structure)

    @property
    def is_definite(self) -> bool:
        """
        Return true if the message has an explicit size, no optional fields and no parameters.

        Messages with a First or Last attribute in a condition or size aspect are not yet supported
        and therefore considered as not definite. This is also the case for messages containing
        sequences.
        """
        return (
            len(self.paths(FINAL)) <= 1
            and not self.has_implicit_size
            and all(
                not l.condition.findall(lambda x: isinstance(x, (expr.First, expr.Last)))
                for l in self.structure
                for v in l.condition.variables()
            )
            and all(
                not l.size.findall(lambda x: isinstance(x, (expr.First, expr.Last)))
                for l in self.structure
                for v in l.size.variables()
            )
            and not self.parameters
            and not any(isinstance(t, mty.Sequence) for t in self.types.values())
        )

    def size(
        self,
        field_values: Optional[Mapping[Field, expr.Expr]] = None,
        message_instance: Optional[ID] = None,
        subpath: bool = False,
    ) -> expr.Expr:
        """
        Determine the size of the message based on the given field values.

        If field values are represented by variables, the returned size expression may contain
        if-expressions to represent these dependencies. Only message paths which contain all given
        fields are considered. The evaluation of the returned size expression may result in a size
        greater than zero, even if the field values do not lead to a valid message.

        The message fields can be prefixed by the message instance.

        The size calculation can be restricted to the size of a subpath. The subpath is defined by
        the given field values.
        """

        field_values = field_values if field_values else {}

        if subpath:
            if not field_values:
                return expr.Number(0)

            fields = list(field_values)
            possible_paths = [
                p
                for p in sorted(self.paths(FINAL))
                if any(
                    fields == [l.target for l in p[i : len(fields) + i]]
                    for i in range(len(p) - len(fields) + 1)
                )
            ]

            if not possible_paths:
                subpath_str = " -> ".join(f.name for f in fields)
                fail(
                    f'unable to calculate size of invalid subpath "{subpath_str}"'
                    f' of message "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
        else:
            if self.is_null:
                return expr.Number(0)

            fields = list(self.fields)
            possible_paths = [
                path
                for path in sorted(self.paths(FINAL))
                if not (
                    set(field_values)
                    - set(self.parameters)
                    - {l.target for l in path if l.target != FINAL}
                )
            ]

        def add_message_prefix(expression: expr.Expr) -> expr.Expr:
            if (
                message_instance
                and isinstance(expression, expr.Variable)
                and Field(expression.identifier) in self.types
            ):
                return expr.Selected(
                    expr.Variable(message_instance, type_=self.type_),
                    expression.identifier,
                    negative=expression.negative,
                    type_=expression.type_,
                    location=expression.location,
                )
            return expression

        def remove_variable_prefix(expression: expr.Expr) -> expr.Expr:
            """
            Remove prefix from variables.

            The prefix is used to prevent name conflicts between field values and field names.
            """
            if isinstance(expression, expr.Variable) and expression.name.startswith("RFLX_"):
                return expression.copy(identifier=expression.name[5:])
            if (
                isinstance(expression, expr.Selected)
                and isinstance(expression.prefix, expr.Variable)
                and expression.prefix.name.startswith("RFLX_")
            ):
                return expression.copy(
                    prefix=expression.prefix.copy(identifier=expression.prefix.name[5:]),
                )
            return expression

        values = [
            expr.Equal(expr.Variable(f.name), v, location=v.location)
            for f, v in field_values.items()
        ]
        aggregate_sizes = [
            expr.Equal(expr.Size(f.name), expr.Number(len(v.elements) * 8), location=v.location)
            for f, v in field_values.items()
            if isinstance(v, expr.Aggregate)
        ]
        composite_sizes = []
        for f, v in field_values.items():
            if isinstance(self.types[f], mty.Composite):
                if isinstance(v, expr.Variable):
                    composite_sizes.append(
                        expr.Equal(
                            expr.Size(f.name),
                            expr.Size(v.copy(identifier="RFLX_" + v.identifier)),
                        ),
                    )

                if isinstance(v, expr.Selected) and isinstance(v.prefix, expr.Variable):
                    composite_sizes.append(
                        expr.Equal(
                            expr.Size(f.name),
                            expr.Size(
                                v.copy(
                                    prefix=v.prefix.copy(identifier="RFLX_" + v.prefix.identifier),
                                ),
                            ),
                        ),
                    )
        facts: list[expr.Expr] = [*values, *aggregate_sizes, *composite_sizes]
        type_constraints = to_mapping(self._type_size_constraints())
        definite_fields = set.intersection(*[{l.target for l in path} for path in possible_paths])
        optional_fields = set(fields) - definite_fields
        conditional_field_size = []

        for field in fields:
            overlay_condition = expr.Not(
                expr.Or(*[l.condition for l in self.incoming(field) if l.first != expr.UNDEFINED]),
            ).simplified()
            paths_to_field = sorted(
                {
                    path[
                        : {f: i for i, f in enumerate(l.target for l in path if l.target != FINAL)}[
                            field
                        ]
                        + 1
                    ]
                    for path in possible_paths
                    if any(l.target == field for l in path)
                },
            )

            for path in paths_to_field:
                link_size_expressions = [
                    fact for link in path for fact in self._link_size_constraints(link)
                ]

                path_condition = (
                    expr.And(
                        *[l.condition for l in path if l.condition != expr.TRUE],
                        overlay_condition,
                    )
                    .substituted(mapping=to_mapping(link_size_expressions + facts))
                    .substituted(mapping=type_constraints)
                    .substituted(add_message_prefix)
                    .substituted(remove_variable_prefix)
                    .simplified()
                )
                field_size = (
                    expr.Size(expr.Variable(field.name, type_=self.types[field].type_))
                    .substituted(mapping=to_mapping(link_size_expressions + facts))
                    .substituted(mapping=type_constraints)
                    .substituted(add_message_prefix)
                    .substituted(remove_variable_prefix)
                )

                conditional_field_size.append(
                    (
                        expr.TRUE
                        if len(paths_to_field) == 1
                        and (
                            path_condition == expr.TRUE
                            or (field not in optional_fields and overlay_condition == expr.TRUE)
                        )
                        else path_condition,
                        field_size,
                    ),
                )

        return (
            expr.Add(
                *[
                    expr.IfExpr([(path_condition, field_size)], expr.Number(0))
                    if path_condition != expr.TRUE
                    else field_size
                    for path_condition, groups in itertools.groupby(
                        sorted(conditional_field_size),
                        lambda x: x[0],
                    )
                    for field_size in [expr.Add(*(s for _, s in groups))]
                ],
            )
            .substituted(mapping=to_mapping(values))
            .simplified()
        )

    def max_size(self) -> expr.Number:
        if self.is_null:
            return expr.Number(0)

        if self.has_implicit_size:
            fail(
                "unable to calculate maximum size of message with implicit size",
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        max_size = expr.Number(0)

        for path in self.paths(FINAL):
            max_size = max(max_size, self._max_value(expr.Size("Message"), path))

        return max_size

    def max_field_sizes(self) -> dict[Field, expr.Number]:
        if self.is_null:
            return {}

        if self.has_implicit_size:
            fail(
                "unable to calculate maximum field sizes of message with implicit size",
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        result = {f: expr.Number(0) for f in self.fields}

        for path in self.paths(FINAL):
            for l in path[:-1]:
                result[l.target] = max(
                    result[l.target],
                    self._max_value(expr.Size(l.target.name), path),
                )

        return result

    def link_first(self, link: Link) -> tuple[Field, expr.Expr]:
        """
        Express position of link target based on previous links.

        Compute a pair (node, distance) to express the position of `link.target`,
        assuming `link` was followed. The meaning is

          link.target'first = node'first + distance

        The distance is an expression that might contain references to sizes of
        other fields, using `expr.Size(fld.affixed_name)`. These references are
        to be replaced by the size of the corresponding field.
        """
        if link not in self._first:
            result = self._compute_first(link)
            self._first[link] = result
            return result
        return self._first[link]

    def field_first(self, fld: Field) -> tuple[Field, expr.Expr]:
        """
        Express position of field based on predecessor fields.

        Compute a pair (node, distance) to express the position of `fld`. See
        the comment of `link_first` for the exact meaning of the distance and
        contents of the distance expression. This function returns (fld, 0) if
        `fld` has several incoming links.
        """
        incoming = [l for l in self.structure if l.target == fld]
        if len(incoming) == 1:
            return self.link_first(incoming[0])
        return (fld, expr.Number(0))

    def _type_size_constraints(self) -> list[expr.Expr]:
        return _type_size_constraints(self._type_names)

    def _validate(self, structure: Sequence[Link], types: Mapping[Field, mty.Type]) -> bool:
        type_fields = {*types, INITIAL, FINAL}
        structure_fields = {l.source for l in structure} | {l.target for l in structure}

        self._validate_types(types, type_fields, structure_fields)
        self._validate_initial_link()
        self._validate_names(type_fields)

        self.error.propagate()

        has_unreachable = self._validate_structure(structure_fields)
        self._validate_link_aspects()

        return has_unreachable

    def _validate_types(
        self,
        types: Mapping[Field, mty.Type],
        type_fields: set[Field],
        structure_fields: set[Field],
    ) -> None:
        parameters = type_fields - structure_fields - {INITIAL, FINAL}

        for f, t in types.items():
            if f in structure_fields and not isinstance(t, (mty.Scalar, mty.Composite, Message)):
                self.error.extend(
                    [
                        (
                            "message fields must have a scalar or composite type",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        ),
                    ],
                )

            if f in parameters:
                if not isinstance(t, mty.Scalar):
                    self.error.extend(
                        [
                            (
                                "parameters must have a scalar type",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                f.identifier.location,
                            ),
                        ],
                    )
                elif isinstance(t, mty.Enumeration) and t.always_valid:
                    self.error.extend(
                        [
                            (
                                "always valid enumeration types not allowed as parameters",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                f.identifier.location,
                            ),
                        ],
                    )

        for f in structure_fields - type_fields:
            self.error.extend(
                [
                    (
                        f'missing type for field "{f.name}" in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        f.identifier.location,
                    ),
                ],
            )

    def _validate_initial_link(self) -> None:
        initial_links = self.outgoing(INITIAL)

        if len(initial_links) != 1:
            self.error.extend(
                [
                    (
                        f'ambiguous first field in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        ("duplicate", Subsystem.MODEL, Severity.INFO, l.target.identifier.location)
                        for l in self.outgoing(INITIAL)
                        if l.target.identifier.location
                    ],
                ],
            )

        if initial_links[0].first != expr.UNDEFINED:
            self.error.extend(
                [
                    (
                        "illegal first aspect at initial link",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        initial_links[0].first.location,
                    ),
                ],
            )

    def _validate_names(self, type_fields: set[Field]) -> None:
        name_conflicts = [
            (f, l)
            for f in type_fields
            for l in self._unqualified_enum_literals
            if f.identifier == l
        ]

        if name_conflicts:
            conflicting_field, conflicting_literal = name_conflicts.pop(0)
            self.error.extend(
                [
                    (
                        f'name conflict for field "{conflicting_field.name}" in'
                        f' "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        conflicting_field.identifier.location,
                    ),
                    (
                        "conflicting enumeration literal",
                        Subsystem.MODEL,
                        Severity.INFO,
                        conflicting_literal.location,
                    ),
                ],
            )

    def _validate_structure(self, structure_fields: set[Field]) -> bool:
        has_unreachable = False

        for f in structure_fields:
            for l in self.structure:
                if f in (INITIAL, l.target):
                    break
            else:
                has_unreachable = True
                self.error.extend(
                    [
                        (
                            f'unreachable field "{f.name}" in "{self.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        ),
                    ],
                )

        def has_final(field: Field, seen: Optional[set[Field]] = None) -> bool:
            """Return True if the field has a path to the final field or a cycle was found."""

            if seen is None:
                seen = set()

            if field in seen:
                return True

            seen = {field, *seen}

            if field == FINAL:
                return True

            return any(has_final(o.target, seen) for o in self.outgoing(field))

        for f in (INITIAL, *sorted(structure_fields)):
            if not has_final(f):
                self.error.extend(
                    [
                        (
                            f'no path to FINAL for field "{f.name}" in "{self.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        ),
                    ],
                )

        duplicate_links = defaultdict(list)
        for link in self.structure:
            duplicate_links[(link.source, link.target, link.condition)].append(link)

        for links in duplicate_links.values():
            if len(links) > 1:
                self.error.extend(
                    [
                        (
                            f'duplicate link from "{links[0].source.identifier}"'
                            f' to "{links[0].target.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            links[0].source.identifier.location,
                        ),
                        *[
                            (
                                "duplicate link",
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.location,
                            )
                            for l in links
                        ],
                    ],
                )

        return has_unreachable

    def _validate_link_aspects(self) -> None:
        for link in self.structure:
            exponentiations = itertools.chain.from_iterable(
                e.findall(lambda x: isinstance(x, expr.Pow))
                for e in [link.condition, link.first, link.size]
            )
            for e in exponentiations:
                assert isinstance(e, expr.Pow)
                variables = e.right.findall(lambda x: isinstance(x, expr.Variable))
                if variables:
                    self.error.extend(
                        [
                            (
                                f'unsupported expression in "{self.identifier}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                e.location,
                            ),
                            *[
                                (
                                    f'variable "{v}" in exponent',
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    v.location,
                                )
                                for v in variables
                            ],
                        ],
                    )

            if link.has_implicit_size:
                if any(l.target != FINAL for l in self.outgoing(link.target)):
                    self.error.extend(
                        [
                            (
                                '"Message" must not be used in size aspects',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                link.size.location,
                            ),
                        ],
                    )
                else:
                    valid_definitions = (
                        [
                            expr.Add(expr.Last("Message"), -expr.Last(link.source.name)),
                            expr.Sub(expr.Last("Message"), expr.Last(link.source.name)),
                        ]
                        if link.source != INITIAL
                        else [
                            expr.Size("Message"),
                            expr.Sub(expr.Last("Message"), expr.Last(INITIAL.name)),
                        ]
                    )
                    if link.size not in valid_definitions:
                        self.error.extend(
                            [
                                (
                                    'invalid use of "Message" in size aspect',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    link.size.location,
                                ),
                                (
                                    "remove size aspect to define field with implicit size",
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    link.size.location,
                                ),
                            ],
                        )

    def _normalize(
        self,
        structure: Sequence[Link],
        types: Mapping[Field, mty.Type],
        checksums: Mapping[ID, Sequence[expr.Expr]],
    ) -> tuple[list[Link], dict[ID, Sequence[expr.Expr]]]:
        """
        Normalize structure of message.

        - Replace variables by literals where necessary.
        - Qualify enumeration literals in conditions to prevent ambiguities.
        - Add size expression for fields with implicit size. The distinction between variables and
          literals is not possible in the parser, as both are syntactically identical.
        """

        def substitute(expression: expr.Expr) -> expr.Expr:
            return substitute_variables(
                expression,
                self._unqualified_enum_literals,
                self._qualified_enum_literals,
                self._type_names,
                self.package,
            )

        structure = [
            Link(
                l.source,
                l.target,
                l.condition.substituted(substitute),
                l.size.substituted(substitute),
                l.first.substituted(substitute),
                l.location,
            )
            for l in structure
        ]

        for link in structure:
            if link.size == expr.UNDEFINED and link.target in types:
                t = types[link.target]
                if isinstance(t, (mty.Opaque, mty.Sequence)) and all(
                    l.target == FINAL for l in self.outgoing(link.target)
                ):
                    if link.source == INITIAL:
                        link.size = expr.Size(ID("Message", location=link.location))
                    else:
                        link.size = expr.Sub(
                            expr.Last("Message"),
                            expr.Last(link.source.identifier),
                            location=link.location,
                        )

        checksums = {
            i: [e.substituted(substitute) for e in expressions]
            for i, expressions in self.checksums.items()
        }

        return (structure, checksums)

    def _compute_topological_sorting(self, has_unreachable: bool) -> Optional[tuple[Field, ...]]:
        """Return fields topologically sorted (Kahn's algorithm)."""
        result: tuple[Field, ...] = ()
        fields = [INITIAL]
        visited = set()
        while fields:
            n = fields.pop(0)
            result += (n,)
            for e in self.outgoing(n):
                visited.add(e)
                if set(self.incoming(e.target)) <= visited:
                    fields.append(e.target)
        if not has_unreachable and set(self.structure) - visited:
            self.error.extend(
                [
                    (
                        f'structure of "{self.identifier}" contains cycle',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )
            # Eng/RecordFlux/RecordFlux#256
            return None
        return tuple(f for f in result if f not in [INITIAL, FINAL])

    def _set_types(self) -> None:
        def set_types(expression: expr.Expr) -> expr.Expr:
            return self.typed_expression(expression, self.types)

        for link in self.structure:
            link.condition = link.condition.substituted(set_types)
            link.size = link.size.substituted(set_types)
            link.first = link.first.substituted(set_types)

    def _verify(self) -> None:
        if not self.is_null:
            self._verify_parameters()
            self._verify_use_of_literals()
            self._verify_use_of_type_names()
            self._verify_message_types()
            self._verify_links()

            self.error.propagate()

            valid_paths = self._determine_valid_paths()

            self._verify_expression_types(valid_paths)
            self._verify_checksums()

            self.error.propagate()

            proofs = expr.ParallelProofs(self._workers)

            self._prove_static_conditions(proofs)
            self._prove_conflicting_conditions(proofs)
            self._prove_overlays(proofs)
            self._prove_field_positions(proofs, valid_paths)
            self._prove_message_size(proofs)

            proofs.check(self.error)

            self._prove_reachability(valid_paths)
            self.error.propagate()

    def _determine_valid_paths(self) -> set[tuple[Link, ...]]:
        """Return all paths without contradictions."""

        paths = []
        facts = []

        for field in (*self.fields, FINAL):
            for path in self.paths(field):
                paths.append(path)
                facts.append(
                    [
                        *self._path_constraints(path),
                        *self.message_constraints,
                    ],
                )

        result = set()

        with ProcessPoolExecutor(max_workers=self._workers, mp_context=MP_CONTEXT) as executor:
            for i, e in enumerate(executor.map(prove, facts)):
                if e == expr.ProofResult.SAT:
                    result.add(paths[i])

        return result

    def _max_value(self, target: expr.Expr, path: tuple[Link, ...]) -> expr.Number:
        message_size = expr.Add(
            *[
                expr.Size(link.target.name)
                for link in path
                if link.target != FINAL and link.first == expr.UNDEFINED
            ],
        )
        return expr.max_value(
            target,
            [
                expr.Equal(expr.Size("Message"), message_size),
                *self._path_constraints(path),
                *self.message_constraints,
            ],
        )

    def _verify_parameters(self) -> None:
        variables = [
            v.identifier
            for l in self.structure
            for e in [l.condition, l.size, l.first]
            for v in e.findall(lambda x: isinstance(x, expr.Variable))
            if isinstance(v, expr.Variable)
        ]
        for p in self.parameters:
            if p.identifier not in variables:
                self.error.extend(
                    [
                        (
                            f'unused parameter "{p.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            p.identifier.location,
                        ),
                    ],
                )

    def _verify_use_of_literals(self) -> None:
        for link in self.structure:
            for expression in [link.condition, link.size, link.first]:
                self.error.extend(
                    [
                        (
                            f'invalid use of enum literal "{l}" in expression',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            l.location,
                        )
                        for l in [
                            *(
                                [expression]
                                if isinstance(expression, expr.Literal) and expression != expr.TRUE
                                else []
                            ),
                            *[
                                e
                                for ass_expr in expression.findall(
                                    lambda x: isinstance(x, expr.AssExpr),
                                )
                                if isinstance(ass_expr, expr.AssExpr)
                                for e in ass_expr.terms
                                if isinstance(e, expr.Literal) and e not in [expr.TRUE, expr.FALSE]
                            ],
                        ]
                    ],
                )

    def _verify_use_of_type_names(self) -> None:
        for link in self.structure:
            for expression in [link.condition, link.size, link.first]:
                self.error.extend(
                    [
                        (
                            f'invalid use of type name "{l}" in expression',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            l.location,
                        )
                        for l in [
                            *([expression] if isinstance(expression, expr.TypeName) else []),
                            *[
                                e
                                for ass_expr in expression.findall(
                                    lambda x: isinstance(x, expr.AssExpr),
                                )
                                if isinstance(ass_expr, expr.AssExpr)
                                for e in ass_expr.terms
                                if isinstance(e, expr.TypeName)
                            ],
                            *[
                                e
                                for relation in expression.findall(
                                    lambda x: isinstance(x, expr.Relation),
                                )
                                if isinstance(relation, expr.Relation)
                                for e in [relation.left, relation.right]
                                if isinstance(e, expr.TypeName)
                            ],
                        ]
                    ],
                )

    def _verify_message_types(self) -> None:
        for l in self.structure:
            for expression in [l.condition, l.size, l.first]:
                if expression == expr.UNDEFINED:
                    continue
                for var in expression.variables():
                    if var.type_ == rty.Undefined():
                        self.error.extend(
                            [
                                (
                                    f'undefined variable "{var.identifier}"',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    var.location,
                                ),
                            ],
                        )

    def _verify_expression_types(self, valid_paths: set[tuple[Link, ...]]) -> None:
        types: dict[Field, mty.Type] = {}

        def typed_variable(expression: expr.Expr) -> expr.Expr:
            return self.typed_expression(expression, types)

        def remove_types(expression: expr.Expr) -> expr.Expr:
            if isinstance(expression, expr.Variable):
                expression = copy(expression)
                expression.type_ = rty.Undefined()
            return expression

        for p in self.paths(FINAL):
            if p not in valid_paths:
                # Skip type checking on paths with contradictions to prevent false positives about
                # undefined variables in cases where an optional field F is accessed on an
                # independent optional path that is present only if F is present.
                continue

            types = {f: t for f, t in self.types.items() if f in self.parameters}
            path = []

            for l in [
                Link(
                    source=l.source,
                    target=l.target,
                    condition=l.condition.substituted(remove_types),
                    first=l.first.substituted(remove_types),
                    size=l.size.substituted(remove_types),
                )
                for l in p
            ]:
                path.append(l.target)

                if l.source in self.types:
                    types[l.source] = self.types[l.source]

                for expression in [
                    l.condition.substituted(typed_variable),
                    l.size.substituted(typed_variable),
                    l.first.substituted(typed_variable),
                ]:
                    if expression == expr.UNDEFINED:
                        continue

                    error = expression.check_type(rty.Any())

                    self.error.extend(error)

                    if error.errors:
                        self.error.extend(
                            [
                                (
                                    "on path " + " -> ".join(f.name for f in path),
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    expression.location,
                                ),
                            ],
                        )

    def _verify_links(self) -> None:
        for link in self.structure:
            self._verify_attributes(link.condition)
            self._verify_link_first(link)
            self._verify_link_size(link)

    def _verify_attributes(self, expression: expr.Expr) -> None:
        for a in expression.findall(lambda x: isinstance(x, expr.Attribute)):
            if isinstance(a, expr.Size) and not (
                (
                    isinstance(a.prefix, expr.Variable)
                    and (a.prefix.name == "Message" or Field(a.prefix.name) in self.fields)
                )
                or (
                    isinstance(a.prefix, expr.TypeName)
                    and (a.prefix.identifier in self._type_names)
                )
            ):
                self.error.extend(
                    [
                        (
                            f'invalid use of size attribute for "{a.prefix}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            expression.location,
                        ),
                    ],
                )

    def _verify_link_first(self, link: Link) -> None:
        if link.first not in (expr.UNDEFINED, expr.First(link.source.identifier)):
            self.error.extend(
                [
                    (
                        f'invalid First for field "{link.target.name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        link.first.location,
                    ),
                ],
            )

    def _verify_link_size(self, link: Link) -> None:
        if link.target == FINAL and link.size != expr.UNDEFINED:
            self.error.extend(
                [
                    (
                        f'size aspect for final field in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        link.size.location,
                    ),
                ],
            )
        if link.target != FINAL and link.target in self.types:
            t = self.types[link.target]
            unconstrained = isinstance(t, (mty.Opaque, mty.Sequence))
            if not unconstrained and link.size != expr.UNDEFINED:
                self.error.extend(
                    [
                        (
                            f'fixed size field "{link.target.name}" with size aspect',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            link.target.identifier.location,
                        ),
                    ],
                )
            if unconstrained and link.size == expr.UNDEFINED:
                self.error.extend(
                    [
                        (
                            f'unconstrained field "{link.target.name}" without size aspect',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            link.target.identifier.location,
                        ),
                    ],
                )

    def _verify_checksums(self) -> None:
        def valid_lower(expression: expr.Expr) -> bool:
            return isinstance(expression, expr.First) or (
                isinstance(expression, expr.Add)
                and len(expression.terms) == 2
                and isinstance(expression.terms[0], expr.Last)
                and expression.terms[1] == expr.Number(1)
            )

        def valid_upper(expression: expr.Expr) -> bool:
            return isinstance(expression, expr.Last) or (
                isinstance(expression, expr.Sub)
                and isinstance(expression.left, expr.First)
                and expression.right == expr.Number(1)
            )

        for name, expressions in self.checksums.items():
            if Field(name) not in self.fields:
                self.error.extend(
                    [
                        (
                            f'checksum definition for unknown field "{name}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            name.location,
                        ),
                    ],
                )

            for e in expressions:
                if not (
                    isinstance(e, (expr.Variable, expr.Size))
                    or (
                        isinstance(e, expr.ValueRange)
                        and valid_lower(e.lower)
                        and valid_upper(e.upper)
                    )
                ):
                    self.error.extend(
                        [
                            (
                                f'unsupported expression "{e}" in definition of checksum "{name}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                e.location,
                            ),
                        ],
                    )

                for v in e.findall(lambda x: isinstance(x, expr.Variable)):
                    assert isinstance(v, expr.Variable)

                    if Field(v.name) not in self.fields:
                        self.error.extend(
                            [
                                (
                                    f'unknown field "{v.name}" referenced'
                                    f' in definition of checksum "{name}"',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    v.location,
                                ),
                            ],
                        )

                if isinstance(e, expr.ValueRange):
                    lower = e.lower.findall(lambda x: isinstance(x, expr.Variable))[0]
                    upper = e.upper.findall(lambda x: isinstance(x, expr.Variable))[0]

                    assert isinstance(lower, expr.Variable)
                    assert isinstance(upper, expr.Variable)

                    if lower != upper:
                        upper_field = (
                            Field(upper.name) if upper.name.lower() != "message" else FINAL
                        )
                        lower_field = (
                            Field(lower.name) if lower.name.lower() != "message" else INITIAL
                        )
                        for p in self.paths(upper_field):
                            if not any(lower_field == l.source for l in p):
                                self.error.extend(
                                    [
                                        (
                                            f'invalid range "{e}" in definition of checksum'
                                            f' "{name}"',
                                            Subsystem.MODEL,
                                            Severity.ERROR,
                                            e.location,
                                        ),
                                    ],
                                )

        checked = {
            e.prefix.identifier
            for e in self.path_condition(FINAL).findall(lambda x: isinstance(x, expr.ValidChecksum))
            if isinstance(e, expr.ValidChecksum) and isinstance(e.prefix, expr.Variable)
        }
        for name in set(self.checksums) - checked:
            self.error.extend(
                [
                    (
                        f'no validity check of checksum "{name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        name.location,
                    ),
                ],
            )
        for name in checked - set(self.checksums):
            self.error.extend(
                [
                    (
                        f'validity check for undefined checksum "{name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        name.location,
                    ),
                ],
            )

    def _prove_static_conditions(self, proofs: expr.ParallelProofs) -> None:
        for l in self._structure:
            if l.condition == expr.TRUE:
                continue
            facts = [
                expr.Equal(l.condition, expr.FALSE),
                self.path_condition(l.source),
                *self.message_constraints,
                *self.aggregate_constraints(l.condition),
            ]
            unsat_error = RecordFluxError(
                [
                    (
                        f'condition "{l.condition}" on transition "{l.source.identifier}"'
                        f' -> "{l.target.identifier}" is always true',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        l.source.identifier.location,
                    ),
                ],
            )
            unknown_error = RecordFluxError(
                [
                    (
                        f'condition "{l.condition}" on transition "{l.source.identifier}"'
                        f' -> "{l.target.identifier}" might be always true',
                        Subsystem.MODEL,
                        Severity.WARNING,
                        l.source.identifier.location,
                    ),
                ],
            )
            proofs.add(
                expr.Equal(l.condition, expr.FALSE),
                facts,
                unsat_error=unsat_error,
                unknown_error=unknown_error,
            )

    def _prove_conflicting_conditions(self, proofs: expr.ParallelProofs) -> None:
        for f in (INITIAL, *self.fields):
            for i1, c1 in enumerate(self.outgoing(f)):
                for i2, c2 in enumerate(self.outgoing(f)):
                    if i1 < i2:
                        conflict = expr.And(c1.condition, c2.condition)
                        c1_message = str(c1.condition).replace("\n", " ")
                        c2_message = str(c2.condition).replace("\n", " ")
                        error = RecordFluxError(
                            [
                                (
                                    f'conflicting conditions for field "{f.name}"',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    f.identifier.location,
                                ),
                                (
                                    f"condition {i1} ({f.identifier} ->"
                                    f" {c1.target.identifier}): {c1_message}",
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    c1.condition.location,
                                ),
                                (
                                    f"condition {i2} ({f.identifier} ->"
                                    f" {c2.target.identifier}): {c2_message}",
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    c2.condition.location,
                                ),
                            ],
                        )
                        for path in self.paths(f):
                            facts = [
                                *self.message_constraints,
                                *self.aggregate_constraints(conflict),
                                *self._path_constraints(path),
                            ]
                            proofs.add(
                                conflict,
                                facts,
                                sat_error=error,
                                unknown_error=error,
                            )

    def _prove_reachability(self, valid_paths: set[tuple[Link, ...]]) -> None:
        """
        Find all fields that are unreachable due to contradictions on all paths to the field.

        Fields that can only be reached via an unreachable field, and are therefore unreachable due
        to the same problem, are not mentioned in the resulting error message.
        """

        def starts_with(path: tuple[Link, ...], paths: set[tuple[Link, ...]]) -> bool:
            """Check if any path of `paths` is a proper prefix of `path`."""
            return any(
                p for p in paths if len(p) < len(path) and all(a == b for a, b in zip(p, path))
            )

        def is_proper_prefix(path: tuple[Link, ...], paths: set[tuple[Link, ...]]) -> bool:
            """Check if `path` is a proper prefix of any path of `paths`."""
            return any(
                p for p in paths if len(p) > len(path) and all(a == b for a, b in zip(p, path))
            )

        unreachable_paths: set[tuple[Link, ...]] = set()

        for f in self.fields:
            paths = []
            for path in self.paths(f):
                if starts_with(path, unreachable_paths):
                    continue

                if is_proper_prefix(path, valid_paths):
                    break

                facts = self._path_constraints(path)
                outgoing = self.outgoing(path[-1].target)
                condition = expr.Or(*[o.condition for o in outgoing]) if outgoing else expr.TRUE
                proof = condition.check(
                    [
                        *facts,
                        *self.aggregate_constraints(condition),
                        *self.message_constraints,
                    ],
                )
                assert proof.result != expr.ProofResult.SAT

                paths.append((path, proof.error))
                unreachable_paths.add(path)
            else:
                if paths:
                    error = []
                    error.append(
                        (
                            f'unreachable field "{f.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        ),
                    )
                    for path, errors in sorted(paths):
                        error.extend(
                            (
                                f'on path: "{l.target.identifier}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.target.identifier.location,
                            )
                            for l in path
                        )
                        error.extend(
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                            for m, l in errors
                        )
                    self.error.extend(error)

    def _prove_overlays(self, proofs: expr.ParallelProofs) -> None:
        for f in (INITIAL, *self.fields):
            for p, l in [(p, p[-1]) for p in self.paths(f) if p]:
                if l.first != expr.UNDEFINED and isinstance(l.first, expr.First):
                    facts = self._path_constraints(p)
                    overlaid = expr.Equal(
                        self._target_last(l),
                        expr.Last(l.first.prefix),
                        l.location,
                    )
                    error = RecordFluxError(
                        [
                            (
                                f'field "{f.name}" not congruent with'
                                f' overlaid field "{l.first.prefix}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                self.identifier.location,
                            ),
                        ],
                    )
                    proofs.add(
                        overlaid,
                        facts,
                        unsat_error=error,
                        unknown_error=error,
                        add_unsat=True,
                    )

    def _prove_field_positions(
        self,
        proofs: expr.ParallelProofs,
        valid_paths: set[tuple[Link, ...]],
    ) -> None:
        for f in (*self.fields, FINAL):
            for path in self.paths(f):
                if path not in valid_paths:
                    continue

                last = path[-1]
                field_size = self._target_size(last)
                negative = expr.Less(field_size, expr.Number(0), last.size.location)
                start = expr.GreaterEqual(
                    self._target_first(last),
                    expr.First("Message"),
                    last.source.identifier.location,
                )

                facts = [
                    *self._path_constraints(path),
                    *self.message_constraints,
                    *self.aggregate_constraints(negative),
                    *self.aggregate_constraints(start),
                ]

                path_message = " -> ".join([l.target.name for l in path])
                error = RecordFluxError(
                    [
                        (
                            f'negative size for field "{f.name}" ({path_message})',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field_size.location,
                        ),
                    ],
                )
                proofs.add(negative, facts, sat_error=error, unknown_error=error)

                path_message = " -> ".join([last.target.name for last in path])
                error = RecordFluxError(
                    [
                        (
                            f'negative start for field "{f.name}" ({path_message})',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.identifier.location,
                        ),
                    ],
                )
                proofs.add(start, facts, unsat_error=error, unknown_error=error, add_unsat=True)

                if f in self.types:
                    t = self.types[f]
                    if isinstance(t, mty.Opaque):
                        element_size = t.element_size
                        start_aligned = expr.Not(
                            expr.Equal(
                                expr.Mod(self._target_first(last), element_size),
                                expr.Number(1),
                                last.location,
                            ),
                        )

                        path_message = " -> ".join([p.target.name for p in path])
                        error = RecordFluxError(
                            [
                                (
                                    f'opaque field "{f.name}" not aligned to {element_size} '
                                    f"bit boundary ({path_message})",
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    f.identifier.location,
                                ),
                            ],
                        )
                        proofs.add(
                            start_aligned,
                            [
                                *facts,
                                *self.message_constraints,
                                *self.aggregate_constraints(start_aligned),
                            ],
                            sat_error=error,
                            unknown_error=error,
                        )

                        is_multiple_of_element_size = expr.Not(
                            expr.Equal(
                                expr.Mod(field_size, element_size),
                                expr.Number(0),
                                last.location,
                            ),
                        )

                        path_message = " -> ".join([p.target.name for p in path])
                        error = RecordFluxError(
                            [
                                (
                                    f'size of opaque field "{f.name}" not multiple'
                                    f" of {element_size} bit ({path_message})",
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    field_size.location,
                                ),
                            ],
                        )
                        proofs.add(
                            is_multiple_of_element_size,
                            [
                                *facts,
                                *self.message_constraints,
                                *self.aggregate_constraints(is_multiple_of_element_size),
                            ],
                            sat_error=error,
                            unknown_error=error,
                        )

    def _prove_message_size(self, proofs: expr.ParallelProofs) -> None:
        """Prove that all paths lead to a message with a size that is a multiple of 8 bit."""
        message_constraints = self.message_constraints
        field_size_constraints = [
            expr.Equal(expr.Mod(expr.Size(f.name), expr.Number(8)), expr.Number(0))
            for f, t in self.types.items()
            if isinstance(t, (mty.Opaque, mty.Sequence))
        ]

        for path in [p[:-1] for p in self.paths(FINAL) if p]:
            message_size = expr.Add(
                *[
                    expr.Size(link.target.name)
                    for link in path
                    if link.target != FINAL and link.first == expr.UNDEFINED
                ],
            )
            facts = [
                *self._path_constraints(path),
                *message_constraints,
                *field_size_constraints,
            ]
            error = RecordFluxError(
                [
                    (
                        "message size must be multiple of 8 bit",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.identifier.location,
                    ),
                    (
                        "on path " + " -> ".join(l.target.name for l in path),
                        Subsystem.MODEL,
                        Severity.INFO,
                        self.identifier.location,
                    ),
                ],
            )
            proofs.add(
                expr.NotEqual(expr.Mod(message_size, expr.Number(8)), expr.Number(0)),
                facts,
                sat_error=error,
                unknown_error=error,
            )

    def _prove_path_property(self, prop: expr.Expr, path: Sequence[Link]) -> expr.Proof:
        conditions = [l.condition for l in path if l.condition != expr.TRUE]
        sizes = [
            expr.Equal(expr.Size(l.target.name), l.size) for l in path if l.size != expr.UNDEFINED
        ]
        return prop.check(
            [*self.message_constraints, *self.aggregate_constraints(prop), *conditions, *sizes],
        )

    @staticmethod
    def _target_first(link: Link) -> expr.Expr:
        if link.source == INITIAL:
            return expr.First("Message")
        if link.first != expr.UNDEFINED:
            return link.first
        return expr.Add(expr.Last(link.source.name), expr.Number(1), location=link.location)

    def _target_size(self, link: Link) -> expr.Expr:
        if link.size != expr.UNDEFINED:
            return link.size
        return self.field_size(link.target)

    def _target_size_opt(self, link: Link) -> Optional[expr.Expr]:
        if link.size != expr.UNDEFINED:
            return link.size
        return self.field_size_opt(link.target)

    def _target_last(self, link: Link) -> expr.Expr:
        return expr.Sub(
            expr.Add(self._target_first(link), self._target_size(link)),
            expr.Number(1),
            link.target.identifier.location,
        )

    def _target_last_opt(self, link: Link) -> Optional[expr.Expr]:
        size = self._target_size_opt(link)
        if not size:
            return None
        return expr.Sub(
            expr.Add(self._target_first(link), size),
            expr.Number(1),
            link.target.identifier.location,
        )

    def _link_size_constraints(
        self,
        link: Link,
    ) -> list[expr.Expr]:
        name = link.target.name
        target_first = self._target_first(link)
        target_size = self._target_size_opt(link)
        target_last = self._target_last_opt(link)
        include_sizes = not (
            target_size
            and (expr.Size("Message") in target_size or expr.Last("Message") in target_size)
        )
        return [
            expr.Equal(expr.First(name), target_first, target_first.location or self.location),
            *(
                [
                    expr.Equal(
                        expr.Size(name),
                        target_size,
                        target_size.location or self.location,
                    ),
                ]
                if include_sizes and target_size
                else []
            ),
            *(
                [
                    expr.Equal(expr.Last(name), target_last, target_last.location or self.location),
                ]
                if include_sizes and target_last
                else []
            ),
            expr.GreaterEqual(expr.First("Message"), expr.Number(0), self.location),
            expr.GreaterEqual(expr.Last("Message"), expr.Last(name), self.location),
            expr.GreaterEqual(expr.Last("Message"), expr.First("Message"), self.location),
            expr.Equal(
                expr.Size("Message"),
                expr.Add(expr.Sub(expr.Last("Message"), expr.First("Message")), expr.Number(1)),
                self.location,
            ),
        ]

    def _path_constraints(self, path: tuple[Link, ...]) -> list[expr.Expr]:
        return [fact for link in path for fact in self._link_constraints(link)]

    def _link_constraints(self, link: Link) -> list[expr.Expr]:
        return [
            *self._link_size_constraints(link),
            *expression_list(link.condition),
            *self.aggregate_constraints(link.condition),
        ]

    def _compute_first(self, lnk: Link) -> tuple[Field, expr.Expr]:
        if lnk.source == INITIAL:
            return (INITIAL, expr.Number(0))
        if lnk.first != expr.UNDEFINED:
            return self.field_first(lnk.source)
        root, dist = self.field_first(lnk.source)
        source_size = self.field_size_opt(lnk.source)
        if source_size is None:
            return (lnk.source, expr.Size(lnk.source.affixed_name))
        return (root, expr.Add(dist, source_size).simplified())


class DerivedMessage(Message):
    def __init__(  # noqa: PLR0913
        self,
        identifier: StrID,
        base: Message,
        structure: Optional[Sequence[Link]] = None,
        types: Optional[Mapping[Field, mty.Type]] = None,
        checksums: Optional[Mapping[ID, Sequence[expr.Expr]]] = None,
        byte_order: Optional[Union[ByteOrder, Mapping[Field, ByteOrder]]] = None,
        location: Optional[Location] = None,
        skip_verification: bool = False,
        workers: int = 1,
    ) -> None:
        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            checksums if checksums else copy(base.checksums),
            byte_order if byte_order else copy(base.byte_order),
            location if location else base.location,
            skip_verification,
            workers,
        )
        self.base = base

        if isinstance(base, DerivedMessage):
            RecordFluxError(
                [
                    (
                        f'illegal derivation "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    (
                        f'illegal base message type "{base.identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        base.location,
                    ),
                ],
            ).propagate()

    def copy(  # noqa: PLR0913
        self,
        identifier: Optional[StrID] = None,
        structure: Optional[Sequence[Link]] = None,
        types: Optional[Mapping[Field, mty.Type]] = None,
        checksums: Optional[Mapping[ID, Sequence[expr.Expr]]] = None,
        byte_order: Optional[Union[ByteOrder, Mapping[Field, ByteOrder]]] = None,
        location: Optional[Location] = None,
        skip_verification: Optional[bool] = None,
    ) -> DerivedMessage:
        return DerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            checksums if checksums else copy(self.checksums),
            byte_order if byte_order else copy(self.byte_order),
            location if location else self.location,
            skip_verification if skip_verification else self._skip_verification,
        )


class Refinement(mty.Type):
    def __init__(  # noqa: PLR0913
        self,
        package: StrID,
        pdu: Message,
        field: Field,
        sdu: Message,
        condition: expr.Expr = expr.TRUE,
        location: Optional[Location] = None,
        skip_verification: bool = False,
    ) -> None:
        super().__init__(
            ID(package) * "__REFINEMENT__"
            f"{sdu.identifier.flat}__{pdu.identifier.flat}__{field.name}__",
            location,
        )

        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

        self._normalize()

        if not skip_verification:
            self._verify()

        self.error.propagate()

    def _normalize(self) -> None:
        """
        Normalize condition of refinement.

        - Replace variables by literals where necessary.
        - Qualify enumeration literals to prevent ambiguities.
        - Set expression types
        """

        unqualified_enum_literals = mty.unqualified_enum_literals(self.dependencies, self.package)
        qualified_enum_literals = mty.qualified_enum_literals(self.dependencies)
        type_names = mty.qualified_type_names(self.dependencies)

        self.condition = self.condition.substituted(
            lambda e: substitute_variables(
                e,
                unqualified_enum_literals,
                qualified_enum_literals,
                type_names,
                self.package,
            ),
        )

        def set_types(expression: expr.Expr) -> expr.Expr:
            return self.pdu.typed_expression(expression, self.pdu.types)

        self.condition = self.condition.substituted(set_types)

    def _check_identifier(self) -> None:
        if len(self.package.parts) != 1:
            self.error.extend(
                [
                    (
                        f'unexpected format of package name "{self.package}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.package.location,
                    ),
                ],
            )

    def _verify(self) -> None:
        self._verify_field()
        self._verify_condition()

    def _verify_field(self) -> None:
        for f, t in self.pdu.types.items():
            if f == self.field:
                if not isinstance(t, mty.Opaque):
                    self.error.extend(
                        [
                            (
                                f'invalid type of field "{self.field.name}" in refinement of'
                                f' "{self.pdu.identifier}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                self.field.identifier.location,
                            ),
                            (
                                "expected field of type Opaque",
                                Subsystem.MODEL,
                                Severity.INFO,
                                f.identifier.location,
                            ),
                        ],
                    )
                break
        else:
            self.error.extend(
                [
                    (
                        f'invalid field "{self.field.name}" in refinement of'
                        f' "{self.pdu.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.field.identifier.location,
                    ),
                ],
            )

    def _verify_condition(self) -> None:
        self.error.extend(self.condition.check_type(rty.Any()))

        if not self.error.errors and self.condition != expr.TRUE:
            for cond, val in [
                (expr.Equal(self.condition, expr.FALSE), "true"),
                (self.condition, "false"),
            ]:
                proof = expr.TRUE.check(
                    [
                        *self.pdu.message_constraints,
                        *self.pdu.aggregate_constraints(self.condition),
                        *self.pdu.aggregate_constraints(self.pdu.path_condition(self.field)),
                        self.pdu.path_condition(self.field),
                        cond,
                    ],
                )
                if proof.result == expr.ProofResult.UNSAT:
                    self.error.extend(
                        [
                            (
                                f'condition "{self.condition}" in refinement of'
                                f' "{self.pdu.identifier}" is always {val}',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                self.field.identifier.location,
                            ),
                        ],
                    )
                if proof.result == expr.ProofResult.UNKNOWN:
                    self.error.extend(
                        [
                            (
                                f'condition "{self.condition}" in refinement of'
                                f' "{self.pdu.identifier}" might be always {val}',
                                Subsystem.MODEL,
                                Severity.WARNING,
                                self.field.identifier.location,
                            ),
                        ],
                    )

    def __str__(self) -> str:
        condition = f"\n   if {self.condition}" if self.condition != expr.TRUE else ""
        return f"for {self.pdu.name} use ({self.field.name} => {self.sdu.name}){condition}"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.package == other.package
                and self.pdu == other.pdu
                and self.field == other.field
                and self.sdu == other.sdu
            )
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    @property
    def direct_dependencies(self) -> list[mty.Type]:
        return list(unique([self.pdu, self.sdu, self]))

    @property
    def dependencies(self) -> list[mty.Type]:
        return list(unique([*self.pdu.dependencies, *self.sdu.dependencies, self]))


@dataclass
class UncheckedMessage(mty.UncheckedType):
    identifier: ID
    structure: Sequence[Link]
    parameter_types: Sequence[tuple[Field, ID, Sequence[tuple[ID, expr.Expr]]]]
    field_types: Sequence[tuple[Field, ID, Sequence[tuple[ID, expr.Expr]]]]
    checksums: Mapping[ID, Sequence[expr.Expr]] = dataclass_field(default_factory=dict)
    byte_order: Union[
        Mapping[Field, ByteOrder],
        ByteOrder,
    ] = dataclass_field(  # type: ignore[assignment]
        # TODO(eng/recordflux/RecordFlux#1359): Fix type annotation
        # The type should be `dict[Field, ByteOrder]`, but the subscription of `dict` is not
        # supported by Python 3.8.
        default_factory=dict,
    )
    location: Optional[Location] = dataclass_field(default=None)

    @property
    def fields(self) -> list[Field]:
        return [f for f, _, _ in self.field_types]

    @property
    def byte_order_dict(self) -> dict[Field, ByteOrder]:
        if not self.byte_order:
            return {f: ByteOrder.HIGH_ORDER_FIRST for f, _, _ in self.field_types}
        if isinstance(self.byte_order, ByteOrder):
            return {f: self.byte_order for f, _, _ in self.field_types}
        return dict(self.byte_order)

    def incoming(self, field: Field) -> list[Link]:
        return [l for l in self.structure if l.target == field]

    def outgoing(self, field: Field) -> list[Link]:
        return [l for l in self.structure if l.source == field]

    def paths(self, field: Field) -> set[tuple[Link, ...]]:
        if field == INITIAL:
            return set()

        result = set()
        for l in self.incoming(field):
            source = self.paths(l.source)
            for s in source:
                result.add((*s, l))
            if not source:
                result.add((l,))

        return result

    def types(self, declarations: Sequence[TopLevelDeclaration]) -> dict[Field, mty.Type]:
        return {
            field: next(
                (
                    t
                    for t in declarations
                    if isinstance(t, mty.Type) and t.identifier == type_identifier
                ),
            )
            for field, type_identifier, type_arguments in (
                *self.parameter_types,
                *self.field_types,
            )
        }

    def checked(
        self,
        declarations: Sequence[TopLevelDeclaration],
        skip_verification: bool = False,
        workers: int = 1,
    ) -> Message:
        error = RecordFluxError()
        arguments = {}
        fields: list[Field] = []
        for field, type_identifier, type_arguments in (*self.parameter_types, *self.field_types):
            field_type = next(
                (
                    t
                    for t in declarations
                    if isinstance(t, mty.Type) and t.identifier == type_identifier
                ),
                None,
            )
            if field_type:
                if isinstance(field_type, Message):
                    self._check_message_arguments(
                        error,
                        field_type,
                        type_arguments,
                        type_identifier.location,
                    )
                    arguments[type_identifier] = dict(type_arguments)
                if field in fields:
                    error.extend(  # pragma: no branch
                        [
                            (
                                f'name conflict for "{field.identifier}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                field.identifier.location,
                            ),
                            (
                                "conflicting name",
                                Subsystem.MODEL,
                                Severity.INFO,
                                next(f.identifier for f in fields if f == field).location,
                            ),
                        ],
                    )
                fields.append(field)
            else:
                error.extend(
                    [
                        (
                            f'undefined type "{type_identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            type_identifier.location,
                        ),
                    ],
                )

        error.propagate()

        result = self.merged(declarations, arguments)

        return Message(
            identifier=result.identifier,
            structure=result.structure,
            types=result.types(declarations),
            checksums=result.checksums,
            byte_order=result.byte_order,
            location=result.location,
            skip_verification=skip_verification,
            workers=workers,
        )

    def merged(
        self,
        declarations: Sequence[TopLevelDeclaration],
        message_arguments: Optional[Mapping[ID, Mapping[ID, expr.Expr]]] = None,
    ) -> UncheckedMessage:
        assert all_types_declared(self, declarations)

        message_arguments = message_arguments or {}
        message = self

        message_types = {d.identifier: d for d in declarations if isinstance(d, Message)}

        while True:
            inner_message = next(
                ((f, message_types[i]) for f, i, _ in message.field_types if i in message_types),
                None,
            )

            if not inner_message:
                return message

            message = self._merge_inner_message(
                message,
                *inner_message,
                message_arguments,
                declarations,
            )

    def _check_message_arguments(
        self,
        argument_errors: RecordFluxError,
        message: Message,
        type_arguments: Sequence[tuple[ID, expr.Expr]],
        field_type_location: Optional[Location],
    ) -> None:
        for param, arg in itertools.zip_longest(message.parameter_types, type_arguments):
            if arg:
                arg_id, arg_expression = arg
                if not param or (arg_id != param.identifier):
                    argument_errors.extend(
                        [
                            (
                                f'unexpected argument "{arg_id}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                arg_id.location,
                            ),
                            (
                                "expected no argument"
                                if not param
                                else f'expected argument for parameter "{param.identifier}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                arg_id.location,
                            ),
                        ],
                    )
            else:
                argument_errors.extend(
                    [
                        (
                            "missing argument",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field_type_location,
                        ),
                        (
                            f'expected argument for parameter "{param.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            param.identifier.location,
                        ),
                    ],
                )

    def _merge_inner_message(
        self,
        message: UncheckedMessage,
        field: Field,
        inner_message: Message,
        message_arguments: Mapping[ID, Mapping[ID, expr.Expr]],
        declarations: Sequence[TopLevelDeclaration],
    ) -> UncheckedMessage:
        inner_message = self._replace_message_attributes(inner_message.prefixed(f"{field.name}_"))

        self._check_message_attributes(message, inner_message, field)
        self._check_name_conflicts(message, inner_message, field)

        substitution: Mapping[expr.Name, expr.Expr] = (
            {expr.Variable(a): e for a, e in message_arguments[inner_message.identifier].items()}
            if inner_message.identifier in message_arguments
            else {}
        )
        structure = []
        message_types = message.types(declarations)
        message_dependencies = _dependencies(message_types)
        message_unqualified_enum_literals = mty.unqualified_enum_literals(
            message_dependencies,
            message.package,
        )
        message_qualified_enum_literals = mty.qualified_enum_literals(message_dependencies)
        message_qualified_type_names = mty.qualified_type_names(message_dependencies)
        inner_message_types = inner_message.types
        inner_message_dependencies = _dependencies(inner_message_types)
        inner_message_qualified_enum_literals = mty.qualified_enum_literals(
            inner_message_dependencies,
        )
        inner_message_qualified_type_names = mty.qualified_type_names(inner_message_dependencies)

        def substitute_message_variables(expression: expr.Expr) -> expr.Expr:
            return substitute_variables(
                expression,
                message_unqualified_enum_literals,
                message_qualified_enum_literals,
                message_qualified_type_names,
                message.package,
            )

        def typed_variable(expression: expr.Expr) -> expr.Expr:
            return typed_expression(
                expression,
                {**message_types, **inner_message_types},
                {**message_qualified_enum_literals, **inner_message_qualified_enum_literals},
                inner_message_qualified_type_names,
            )

        for path in message.paths(FINAL):
            for link in path:
                if link.target == field:
                    substitution = {
                        **substitution,
                        expr.Variable(INITIAL.name): expr.Variable(link.source.name),
                    }
                    initial_link = inner_message.outgoing(INITIAL)[0]
                    structure.append(
                        Link(
                            link.source,
                            initial_link.target,
                            link.condition.substituted(mapping=substitution),
                            initial_link.size.substituted(mapping=substitution),
                            link.first.substituted(mapping=substitution),
                            link.location,
                        ),
                    )
                elif link.source == field:
                    for final_link in inner_message.incoming(FINAL):
                        merged_condition = (
                            expr.And(
                                link.condition,
                                final_link.condition,
                            )
                            .substituted(mapping=substitution)
                            .substituted(substitute_message_variables)
                            .substituted(typed_variable)
                        )
                        merged_condition.check_type(rty.Any()).propagate()
                        proof = merged_condition.check(
                            [
                                *message_constraints(
                                    inner_message_types,
                                    inner_message_qualified_enum_literals,
                                    inner_message_qualified_type_names,
                                ),
                                *aggregate_constraints(merged_condition, inner_message_types),
                                inner_message.path_condition(final_link.source),
                            ],
                        )
                        if proof.result != expr.ProofResult.UNSAT:
                            structure.append(
                                Link(
                                    final_link.source,
                                    link.target,
                                    merged_condition.simplified(),
                                    link.size.substituted(
                                        mapping={
                                            **substitution,
                                            expr.Last(field.identifier): expr.Last(
                                                final_link.source.identifier,
                                            ),
                                        },
                                    ),
                                    link.first.substituted(mapping=substitution),
                                    link.location,
                                ),
                            )
                else:
                    structure.append(link)

        structure = list(set(structure))
        structure.extend(
            Link(
                l.source,
                l.target,
                l.condition.substituted(mapping=substitution),
                l.size.substituted(mapping=substitution),
                l.first.substituted(mapping=substitution),
                l.location,
            )
            for l in inner_message.structure
            if l.target != FINAL and l.source != INITIAL
        )

        field_types = [
            *[(f, t, p) for f, t, p in message.field_types if f != field],
            *[
                (f, t.identifier, [])
                for f, t in inner_message.field_types.items()
                if inner_message.identifier not in message_arguments
                or f.identifier not in message_arguments[inner_message.identifier]
            ],
        ]
        checksums = {
            **(message.checksums if message.checksums is not None else {}),
            **(inner_message.checksums if inner_message.checksums is not None else {}),
        }
        byte_order = {
            **{f: b for f, b in message.byte_order_dict.items() if f != field},
            **inner_message.byte_order,
        }

        structure, field_types, byte_order = self._prune_dangling_fields(
            structure,
            field_types,
            byte_order,
        )
        if not structure or not field_types:
            fail(
                f'empty message type when merging field "{field.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                field.identifier.location,
            )

        return UncheckedMessage(
            self.identifier,
            sorted(structure),
            self.parameter_types,
            field_types,
            checksums,
            byte_order,
            self.location,
        )

    @staticmethod
    def _replace_message_attributes(message: Message) -> Message:
        first_field = message.outgoing(INITIAL)[0].target

        def replace(expression: expr.Expr) -> expr.Expr:
            if (
                not isinstance(expression, expr.Attribute)
                or not isinstance(expression.prefix, expr.Variable)
                or expression.prefix.name != "Message"
            ):
                return expression
            if isinstance(expression, expr.First):
                return expr.First(ID(first_field.identifier, location=expression.location))
            if isinstance(expression, expr.Last):
                return expression
            if isinstance(expression, expr.Size):
                return expr.Sub(
                    expr.Last(ID("Message", location=expression.location)),
                    expr.Last(INITIAL.name),
                    location=expression.location,
                )
            assert False

        return Message(
            message.identifier,
            [
                Link(
                    l.source,
                    l.target,
                    l.condition.substituted(replace),
                    l.size.substituted(replace),
                    l.first.substituted(replace),
                    l.location,
                )
                for l in message.structure
            ],
            message.types,
            message.checksums,
            message.byte_order,
            message.location,
            skip_verification=True,
        )

    @staticmethod
    def _check_message_attributes(
        message: UncheckedMessage,
        inner_message: Message,
        field: Field,
    ) -> None:
        error = RecordFluxError()

        if any(n.target != FINAL for n in message.outgoing(field)):
            for expressions, issue in [
                ((l.condition for l in inner_message.structure), 'reference to "Message"'),
                ((l.size for l in inner_message.structure), "implicit size"),
            ]:
                locations = [
                    m.location
                    for e in expressions
                    for m in e.findall(
                        lambda x: isinstance(x, expr.Variable) and x.identifier == ID("Message"),
                    )
                ]
                if locations:
                    error.extend(
                        [
                            (
                                f"messages with {issue} may only be used for last fields",
                                Subsystem.MODEL,
                                Severity.ERROR,
                                field.identifier.location,
                            ),
                            *[
                                (
                                    f'message field with {issue} in "{inner_message.identifier}"',
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    loc,
                                )
                                for loc in locations
                            ],
                        ],
                    )

        error.propagate()

    def _check_name_conflicts(
        self,
        message: UncheckedMessage,
        inner_message: Message,
        field: Field,
    ) -> None:
        error = RecordFluxError()
        name_conflicts = [
            f for f in message.fields for g in inner_message.fields if f.name == g.name
        ]

        if name_conflicts:
            conflicting = name_conflicts.pop(0)
            error.extend(
                [
                    (
                        f'name conflict for "{conflicting.identifier}" in'
                        f' "{message.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        conflicting.identifier.location,
                    ),
                    (
                        f'when merging message "{inner_message.identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        inner_message.location,
                    ),
                    (
                        f'into field "{field.name}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        field.identifier.location,
                    ),
                ],
            )

        error.propagate()

    @staticmethod
    def _prune_dangling_fields(
        structure: Sequence[Link],
        field_types: Sequence[tuple[Field, ID, Sequence[tuple[ID, expr.Expr]]]],
        byte_order: Mapping[Field, ByteOrder],
    ) -> tuple[
        list[Link],
        list[tuple[Field, ID, Sequence[tuple[ID, expr.Expr]]]],
        dict[Field, ByteOrder],
    ]:
        dangling = []
        progress = True
        while progress:
            progress = False
            fields = {x for l in structure for x in (l.source, l.target) if x != FINAL}
            for s in fields:
                if all(l.source != s for l in structure):
                    dangling.append(s)
                    progress = True
            structure = [l for l in structure if l.target not in dangling]

        return (
            list(structure),
            [(f, t, p) for f, t, p in field_types if f not in dangling],
            {k: v for k, v in byte_order.items() if k not in dangling},
        )


@dataclass
class UncheckedDerivedMessage(mty.UncheckedType):
    identifier: ID
    base_identifier: ID
    location: Optional[Location] = dataclass_field(default=None)

    def checked(
        self,
        declarations: Sequence[TopLevelDeclaration],
        skip_verification: bool = False,  # noqa: ARG002
        workers: int = 1,  # noqa: ARG002
    ) -> DerivedMessage:
        base_types = [
            t
            for t in declarations
            if isinstance(t, mty.Type) and t.identifier == self.base_identifier
        ]

        if not base_types:
            fail(
                f'undefined base message "{self.base_identifier}" in derived message',
                Subsystem.MODEL,
                Severity.ERROR,
                self.base_identifier.location,
            )

        base_messages = [t for t in base_types if isinstance(t, Message)]

        if not base_messages:
            RecordFluxError(
                [
                    (
                        f'illegal derivation "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.identifier.location,
                    ),
                    (
                        f'invalid base message type "{self.base_identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        base_types[0].identifier.location,
                    ),
                ],
            ).propagate()

        return DerivedMessage(
            self.identifier,
            base_messages[0],
            location=self.location,
            skip_verification=True,  # Prevent that the base message is verified again
        )


@dataclass
class UncheckedRefinement(mty.UncheckedType):
    pdu: ID
    field: Field
    sdu: ID
    condition: expr.Expr
    location: Optional[Location] = dataclass_field(default=None)

    def __init__(  # noqa: PLR0913
        self,
        package: ID,
        pdu: ID,
        field: Field,
        sdu: ID,
        condition: expr.Expr = expr.TRUE,
        location: Optional[Location] = None,
    ) -> None:
        super().__init__(
            ID(package) * f"__REFINEMENT__{sdu.flat}__{pdu.flat}__{field.name}__",
        )
        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition
        self.location = location

    def checked(
        self,
        declarations: Sequence[TopLevelDeclaration],
        skip_verification: bool = False,
        workers: int = 1,  # noqa: ARG002
    ) -> Refinement:
        error = RecordFluxError()
        messages = {t.identifier: t for t in declarations if isinstance(t, Message)}

        if self.pdu not in messages:
            error.extend(
                [
                    (
                        f'undefined type "{self.pdu}" in refinement',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.pdu.location,
                    ),
                ],
            )

        if self.sdu not in messages:
            error.extend(
                [
                    (
                        f'undefined type "{self.sdu}" in refinement of "{self.pdu}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.sdu.location,
                    ),
                ],
            )

        error.propagate()

        try:
            result = Refinement(
                self.package,
                messages[self.pdu],
                self.field,
                messages[self.sdu],
                self.condition,
                self.location,
                skip_verification,
            )
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()

        return result


def typed_expression(
    expression: expr.Expr,
    types: Mapping[Field, mty.Type],
    qualified_enum_literals: Mapping[ID, mty.Enumeration],
    qualified_type_names: Mapping[ID, mty.Type],
) -> expr.Expr:
    expression = copy(expression)
    if isinstance(expression, expr.Variable):
        assert expression.identifier not in {
            *qualified_enum_literals,
            *qualified_type_names,
        }, f'variable "{expression.identifier}" has the same name as a literal'
        if expression.name.lower() == "message":
            expression.type_ = rty.OPAQUE
        elif Field(expression.identifier) in types:
            expression.type_ = types[Field(expression.identifier)].type_
    if isinstance(expression, expr.Literal) and expression.identifier in qualified_enum_literals:
        expression.type_ = qualified_enum_literals[expression.identifier].type_
    if isinstance(expression, expr.TypeName):
        expression.type_ = qualified_type_names[expression.identifier].type_
    return expression


def message_constraints(
    types: Mapping[Field, mty.Type],
    qualified_enum_literals: Mapping[ID, mty.Enumeration],
    qualified_type_names: Mapping[ID, mty.Type],
) -> list[expr.Expr]:
    return [
        *_message_size_and_position_constraints(),
        *_scalar_constraints(types, qualified_enum_literals),
        *_type_size_constraints(qualified_type_names),
    ]


def aggregate_constraints(
    expression: expr.Expr,
    types: Mapping[Field, mty.Type],
) -> list[expr.Expr]:
    """Return resulting field sizes for all relations that compare a field with an aggregate."""

    def get_constraints(
        aggregate: expr.Aggregate,
        field: expr.Variable,
        location: Optional[Location],
    ) -> Sequence[expr.Expr]:
        comp = types[Field(field.name)]

        if not isinstance(comp, mty.Composite):
            return []

        result = expr.Equal(
            expr.Mul(aggregate.length, comp.element_size),
            expr.Size(field),
            location=location,
        )
        if isinstance(comp, mty.Sequence) and isinstance(comp.element_type, mty.Scalar):
            return [
                result,
                *comp.element_type.constraints(name=comp.element_type.name, proof=True),
            ]
        return [result]

    aggregate_constraints: list[expr.Expr] = []
    for r in expression.findall(lambda x: isinstance(x, (expr.Equal, expr.NotEqual))):
        assert isinstance(r, (expr.Equal, expr.NotEqual))
        if isinstance(r.left, expr.Aggregate) and isinstance(r.right, expr.Variable):
            aggregate_constraints.extend(get_constraints(r.left, r.right, r.location))
        if isinstance(r.left, expr.Variable) and isinstance(r.right, expr.Aggregate):
            aggregate_constraints.extend(get_constraints(r.right, r.left, r.location))

    return aggregate_constraints


def _scalar_constraints(
    types: Mapping[Field, mty.Type],
    qualified_enum_literals: Mapping[ID, mty.Enumeration],
) -> list[expr.Expr]:
    scalar_types = [
        (f.name, t)
        for f, t in types.items()
        if isinstance(t, mty.Scalar)
        and t != mty.BOOLEAN
        and ID(f.name) not in qualified_enum_literals
        and f.name not in ["Message", "Final"]
    ]

    return [
        c for n, t in scalar_types for c in t.constraints(name=n, proof=True, same_package=False)
    ]


def _type_size_constraints(qualified_type_names: Mapping[ID, mty.Type]) -> list[expr.Expr]:
    return [
        expr.Equal(expr.Size(l), t.size)
        for l, t in qualified_type_names.items()
        if isinstance(t, mty.Scalar)
    ]


def _message_size_and_position_constraints() -> list[expr.Expr]:
    return [
        expr.Equal(expr.Mod(expr.First("Message"), expr.Number(8)), expr.Number(1)),
        expr.Equal(expr.Mod(expr.Size("Message"), expr.Number(8)), expr.Number(0)),
    ]


def _dependencies(types: Mapping[Field, mty.Type]) -> list[mty.Type]:
    return [*unique(a for t in types.values() for a in t.dependencies)]


def expression_list(expression: expr.Expr) -> Sequence[expr.Expr]:
    if isinstance(expression, expr.And):
        return expression.terms
    return [expression]


def to_mapping(facts: Sequence[expr.Expr]) -> dict[expr.Name, expr.Expr]:
    return {
        f.left: f.right
        for f in facts
        if isinstance(f, expr.Equal) and isinstance(f.left, expr.Name)
    }


def substitute_variables(
    expression: expr.Expr,
    unqualified_enum_literals: Iterable[ID],
    qualified_enum_literals: Iterable[ID],
    type_names: Iterable[ID],
    package: ID,
) -> expr.Expr:
    if isinstance(expression, expr.Variable) and expression.identifier in {
        *unqualified_enum_literals,
        *qualified_enum_literals,
    }:
        return expr.Literal(
            package * expression.identifier
            if expression.identifier in unqualified_enum_literals
            else expression.identifier,
            expression.type_,
            location=expression.location,
        )
    if isinstance(expression, expr.Variable) and expression.identifier in type_names:
        return expr.TypeName(
            package * expression.identifier
            if expression.identifier in unqualified_enum_literals
            else expression.identifier,
            expression.type_,
            location=expression.location,
        )
    return expression


def all_types_declared(
    message: UncheckedMessage,
    declarations: Sequence[TopLevelDeclaration],
) -> bool:
    undeclared_types = sorted(
        {
            str(type_identifier)
            for field, type_identifier, type_arguments in (
                *message.parameter_types,
                *message.field_types,
            )
            if not any(d.identifier == type_identifier for d in declarations)
        },
    )
    assert (
        not undeclared_types
    ), f'undeclared types for message "{message.identifier}": {", ".join(undeclared_types)}'
    return True


def prove(
    facts: list[expr.Expr],
) -> expr.ProofResult:
    return expr.TRUE.check(facts).result
