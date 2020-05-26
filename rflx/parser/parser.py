import logging
from collections import deque
from pathlib import Path
from typing import Deque, Dict, List, Mapping, Set, Tuple

from pyparsing import ParseException, ParseFatalException

from rflx.error import (
    RecordFluxError,
    Severity,
    Subsystem,
    fail,
    parser_location,
    pop_source,
    push_source,
)
from rflx.expression import UNDEFINED, Number
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    BUILTINS_PACKAGE,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Array,
    DerivedMessage,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    Refinement,
    Scalar,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
    qualified_type_name,
)

from . import grammar
from .ast import Component, DerivationSpec, MessageSpec, RefinementSpec, Specification

log = logging.getLogger(__name__)


class Parser:
    def __init__(self) -> None:
        self.__specifications: Deque[Specification] = deque()
        self.__evaluated_specifications: Set[str] = set()
        self.__types: Dict[ID, Type] = {**BUILTIN_TYPES, **INTERNAL_TYPES}

    def parse(self, specfile: Path) -> None:
        self.__parse(specfile)

    def __parse(self, specfile: Path, transitions: List[Tuple[ID, ID]] = None) -> None:
        log.info("Parsing %s", specfile)

        if not transitions:
            transitions = []

        with open(specfile, "r") as filehandle:
            push_source(specfile)
            try:
                for specification in grammar.unit().parseFile(filehandle):
                    check_naming(specfile.name, specification.package.identifier)
                    self.__specifications.appendleft(specification)
                    for item in specification.context.items:
                        transition = (specification.package.identifier, item)
                        if transition in transitions:
                            error = RecordFluxError()
                            error.append(
                                f'dependency cycle when including "{transitions[0][1]}"',
                                Subsystem.PARSER,
                                Severity.ERROR,
                                transitions[0][1].location,
                            )
                            error.extend(
                                [
                                    (
                                        f'when including "{i}"',
                                        Subsystem.PARSER,
                                        Severity.INFO,
                                        i.location,
                                    )
                                    for _, i in transitions[1:]
                                ]
                            )
                            error.propagate()
                        transitions.append(transition)
                        self.__parse(specfile.parent / f"{str(item).lower()}.rflx", transitions)
            except (ParseException, ParseFatalException) as e:
                if isinstance(e.msg, RecordFluxError):
                    # ISSUE: https://www.logilab.org/ticket/3207
                    raise e.msg  # pylint: disable=raising-bad-type
                fail(
                    e.msg,
                    Subsystem.PARSER,
                    Severity.ERROR,
                    parser_location(e.loc, e.loc, e.pstr, specfile),
                )
            finally:
                pop_source()

    def parse_string(self, string: str) -> None:
        try:
            for specification in grammar.unit().parseString(string):
                self.__specifications.appendleft(specification)
        except (ParseException, ParseFatalException) as e:
            if isinstance(e.msg, RecordFluxError):
                # ISSUE: https://www.logilab.org/ticket/3207
                raise e.msg  # pylint: disable=raising-bad-type
            fail(
                e.msg, Subsystem.PARSER, Severity.ERROR, parser_location(e.loc, e.loc, e.pstr),
            )

    def create_model(self) -> Model:
        for specification in self.__specifications:
            if str(specification.package.identifier) in self.__evaluated_specifications:
                continue
            self.__evaluated_specifications.add(str(specification.package.identifier))
            self.__evaluate_specification(specification)
        return Model(list(self.__types.values()))

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {str(s.package.identifier): s for s in self.__specifications}

    def __evaluate_specification(self, specification: Specification) -> None:
        log.info("Processing %s", specification.package.identifier)

        self.__evaluate_types(specification)
        check_types(self.__types)

    def __evaluate_types(self, spec: Specification) -> None:
        for t in spec.package.types:
            t.identifier = ID(f"{spec.package.identifier}.{t.name}", t.identifier.location)

            if t.identifier in self.__types:
                error = RecordFluxError()
                error.append(
                    f'duplicate type "{t.identifier}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    t.location,
                )
                error.append(
                    f'previous occurrence of "{t.identifier}"',
                    Subsystem.PARSER,
                    Severity.INFO,
                    self.__types[t.identifier].location,
                )
                error.propagate()

            if isinstance(t, Scalar):
                self.__types[t.identifier] = t

            elif isinstance(t, Array):
                self.__types[t.identifier] = create_array(t, self.__types)

            elif isinstance(t, MessageSpec):
                self.__types[t.identifier] = create_message(t, self.__types)

            elif isinstance(t, DerivationSpec):
                self.__types[t.identifier] = create_derived_message(t, self.__types)

            elif isinstance(t, RefinementSpec):
                self.__types[t.identifier] = create_refinement(t, self.__types)

            else:
                raise NotImplementedError(f'unsupported type "{type(t).__name__}"')


def message_types(types: Mapping[ID, Type]) -> Mapping[ID, Message]:
    return {n: m for n, m in types.items() if isinstance(m, Message)}


def check_types(types: Mapping[ID, Type]) -> None:
    error = RecordFluxError()
    for e1, e2 in [
        (e1, e2)
        for e1 in types.values()
        for e2 in types.values()
        if (
            isinstance(e1, Enumeration)
            and isinstance(e2, Enumeration)
            and e1 != e2
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
                Subsystem.PARSER,
                Severity.ERROR,
                e2.location,
            )
            error.extend(
                [
                    (f'previous occurrence of "{l}"', Subsystem.PARSER, Severity.INFO, l.location)
                    for l in sorted(identical_literals)
                ]
            )

    literals = {l: t for t in types.values() if isinstance(t, Enumeration) for l in t.literals}
    type_set = {t.name for t in types.keys() if t.parent != BUILTINS_PACKAGE}
    name_conflicts = type_set & set(literals.keys())
    for name in sorted(name_conflicts):
        error.append(
            f'literal conflicts with type "{name}"',
            Subsystem.PARSER,
            Severity.ERROR,
            name.location,
        )
        type_location = [
            v.location for k, v in types.items() if k.parent != BUILTINS_PACKAGE and k.name == name
        ][0]
        error.append(
            "conflicting type declaration", Subsystem.PARSER, Severity.INFO, type_location,
        )

    error.propagate()


def create_array(array: Array, types: Mapping[ID, Type]) -> Array:
    array.element_type.identifier = ID(
        array.element_type.full_name.replace("__PACKAGE__", str(array.package)), array.location
    )

    if array.element_type.identifier in types:
        element_type = types[array.element_type.identifier]
    else:
        fail(
            f'undefined element type "{array.element_type.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            array.element_type.location,
        )

    if isinstance(element_type, Scalar):
        element_type_size = element_type.size.simplified()
        if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
            error = RecordFluxError()
            error.append(
                "unsupported element type size", Subsystem.PARSER, Severity.ERROR, array.location
            )
            error.append(
                f'type "{element_type.identifier}" has size {element_type_size},'
                r" must be multiple of 8",
                Subsystem.PARSER,
                Severity.INFO,
                element_type.location,
            )
            error.propagate()

    return Array(array.identifier, element_type)


def create_message(message: MessageSpec, types: Mapping[ID, Type]) -> Message:
    components = list(message.components)

    if components and not components[0].name.null:
        components.insert(0, Component(ID(), ID()))

    field_types: Dict[Field, Type] = {}

    error = RecordFluxError()

    for component in components:
        if not component.name.null:
            type_name = qualified_type_name(component.type_name, message.package)
            if type_name not in types:
                error.append(
                    "undefined component type",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    component.type_name.location,
                )
            else:
                field_types[Field(component.name)] = types[type_name]

    error.propagate()
    structure: List[Link] = []

    for i, component in enumerate(components):
        if component.name.null:
            error.extend(
                [
                    (
                        "invalid first expression",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        then.first.location,
                    )
                    for then in component.thens
                    if then.first != UNDEFINED
                ]
            )

        source_node = Field(component.name) if not component.name.null else INITIAL

        if not component.thens:
            target_node = Field(components[i + 1].name) if i + 1 < len(components) else FINAL
            structure.append(Link(source_node, target_node))

        for then in component.thens:
            target_node = Field(then.name) if not then.name.null else FINAL
            if target_node not in field_types.keys() | {FINAL}:
                error.append(
                    f'undefined component "{then.name}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    then.name.location,
                )
            else:
                structure.append(
                    Link(source_node, target_node, then.condition, then.length, then.first)
                )

    error.propagate()

    return (
        UnprovenMessage(message.identifier, structure, field_types, message.location)
        .merged()
        .proven()
    )


def create_derived_message(derivation: DerivationSpec, types: Mapping[ID, Type]) -> Message:
    base_name = qualified_type_name(derivation.base, derivation.package)
    messages = message_types(types)
    error = RecordFluxError()

    if base_name not in types:
        fail(
            f'undefined base message "{base_name}" in derived message',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )

    if base_name not in messages:
        error.append(
            f'illegal derivation "{derivation.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )
        error.append(
            f'invalid base message type "{base_name}"',
            Subsystem.PARSER,
            Severity.INFO,
            types[base_name].location,
        )
        error.propagate()

    base = messages[base_name]

    if isinstance(base, DerivedMessage):
        error.append(
            f'illegal derivation "{derivation.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )
        error.append(
            f'invalid base message "{base_name}"', Subsystem.PARSER, Severity.INFO, base.location
        )
        error.propagate()

    return (
        UnprovenDerivedMessage(derivation.identifier, base, location=derivation.location)
        .merged()
        .proven()
    )


def create_refinement(refinement: RefinementSpec, types: Mapping[ID, Type]) -> Refinement:
    messages = message_types(types)

    refinement.pdu = qualified_type_name(refinement.pdu, refinement.package)
    if refinement.pdu not in messages:
        fail(
            f'undefined type "{refinement.pdu}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.location,
        )

    pdu = messages[refinement.pdu]

    if Field(refinement.field) not in pdu.fields:
        fail(
            f'invalid field "{refinement.field}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.field.location,
        )

    refinement.sdu = qualified_type_name(refinement.sdu, refinement.package)
    if refinement.sdu not in messages:
        fail(
            f'undefined type "{refinement.sdu}" in refinement of "{refinement.pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.sdu.location,
        )

    sdu = messages[refinement.sdu]

    for variable in refinement.condition.variables():
        literals = [
            l for e in pdu.types.values() if isinstance(e, Enumeration) for l in e.literals.keys()
        ] + [
            e.package * l
            for e in types.values()
            if isinstance(e, Enumeration)
            for l in e.literals.keys()
        ]

        if Field(str(variable.name)) not in pdu.fields and variable.identifier not in literals:
            fail(
                f'unknown field or literal "{variable.identifier}" in refinement'
                f' condition of "{refinement.pdu}"',
                Subsystem.PARSER,
                Severity.ERROR,
                variable.location,
            )

    result = Refinement(
        refinement.package,
        pdu,
        Field(refinement.field),
        sdu,
        refinement.condition,
        refinement.location,
    )

    if result in types.values():
        error = RecordFluxError()
        error.append(
            f'duplicate refinement with "{refinement.sdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.location,
        )
        error.append(
            "previous occurrence",
            Subsystem.PARSER,
            Severity.INFO,
            types[result.identifier].location,
        )
        error.propagate()

    return result


def check_naming(filename: str, package_identifier: ID) -> None:
    expected_filename = f"{str(package_identifier).lower()}.rflx"
    if filename != expected_filename:
        fail(
            f'file name does not match unit name "{package_identifier}",'
            f' should be "{expected_filename}"',
            Subsystem.PARSER,
            Severity.ERROR,
            package_identifier.location,
        )
