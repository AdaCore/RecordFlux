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
from rflx.expression import UNDEFINED
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
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
    is_builtin_type,
    qualified_type_name,
)

from . import grammar
from .ast import (
    ArraySpec,
    Component,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    RefinementSpec,
    Specification,
)

log = logging.getLogger(__name__)


class Parser:
    def __init__(self) -> None:
        self.__specifications: Deque[Specification] = deque()
        self.__evaluated_specifications: Set[ID] = set()
        self.__types: Dict[ID, Type] = {**BUILTIN_TYPES, **INTERNAL_TYPES}

    def parse(self, specfile: Path) -> None:
        self.__parse(specfile)

    def __parse(self, specfile: Path, transitions: List[Tuple[ID, ID]] = None) -> None:
        error = RecordFluxError()
        log.info("Parsing %s", specfile)

        if not transitions:
            transitions = []

        with open(specfile, "r") as filehandle:
            push_source(specfile)
            try:
                for specification in grammar.unit().parseFile(filehandle):
                    check_naming(error, specification.package, specfile.name)
                    self.__specifications.appendleft(specification)
                    for item in specification.context.items:
                        transition = (specification.package.identifier, item)
                        if transition in transitions:
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
                            continue
                        transitions.append(transition)
                        self.__parse(specfile.parent / f"{str(item).lower()}.rflx", transitions)
            except (ParseException, ParseFatalException) as e:
                error.append(
                    e.msg,
                    Subsystem.PARSER,
                    Severity.ERROR,
                    parser_location(e.loc, e.loc, e.pstr, specfile),
                )
            finally:
                pop_source()

        error.propagate()

    def parse_string(self, string: str) -> None:
        error = RecordFluxError()
        try:
            for specification in grammar.unit().parseString(string):
                self.__specifications.appendleft(specification)
                check_naming(error, specification.package)
        except (ParseException, ParseFatalException) as e:
            error.append(
                e.msg, Subsystem.PARSER, Severity.ERROR, parser_location(e.loc, e.loc, e.pstr),
            )
        error.propagate()

    def create_model(self) -> Model:
        error = RecordFluxError()
        for specification in self.__specifications:
            if specification.package.identifier in self.__evaluated_specifications:
                continue
            self.__evaluated_specifications.add(specification.package.identifier)
            try:
                self.__evaluate_specification(specification)
            except RecordFluxError as e:
                error.extend(e)
        try:
            result = Model(list(self.__types.values()))
        except RecordFluxError as e:
            error.extend(e)
        error.propagate()
        return result

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {str(s.package.identifier): s for s in self.__specifications}

    def __evaluate_specification(self, specification: Specification) -> None:
        log.info("Processing %s", specification.package.identifier)

        error = RecordFluxError()
        self.__evaluate_types(specification, error)
        error.propagate()

    def __evaluate_types(self, spec: Specification, error: RecordFluxError) -> None:
        for t in spec.package.types:
            t.identifier = ID(f"{spec.package.identifier}.{t.name}", t.identifier.location)

            if t.identifier in self.__types:
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
                continue

            new_type: Type

            try:
                if isinstance(t, Scalar):
                    new_type = t

                elif isinstance(t, ArraySpec):
                    new_type = create_array(t, self.__types)

                elif isinstance(t, MessageSpec):
                    new_type = create_message(t, self.__types)

                elif isinstance(t, DerivationSpec):
                    new_type = create_derived_message(t, self.__types)

                elif isinstance(t, RefinementSpec):
                    new_type = create_refinement(t, self.__types)

                else:
                    raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

                self.__types[t.identifier] = new_type
                error.extend(new_type.error)

            except RecordFluxError as e:
                error.extend(e)


def message_types(types: Mapping[ID, Type]) -> Mapping[ID, Message]:
    return {n: m for n, m in types.items() if isinstance(m, Message)}


def create_array(array: ArraySpec, types: Mapping[ID, Type]) -> Array:
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

    return Array(array.identifier, element_type, array.location)


def create_message(message: MessageSpec, types: Mapping[ID, Type]) -> Message:
    components = list(message.components)

    if components and components[0].name:
        components.insert(0, Component())

    field_types: Dict[Field, Type] = {}

    error = RecordFluxError()

    for component in components:
        if component.name and component.type_name:
            type_name = qualified_type_name(component.type_name, message.package)
            if type_name not in types:
                continue
            field_types[Field(component.name)] = types[type_name]

    structure: List[Link] = []

    for i, component in enumerate(components):
        if not component.name:
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

        source_node = Field(component.name) if component.name else INITIAL

        if not component.thens:
            name = components[i + 1].name if i + 1 < len(components) else None
            target_node = Field(name) if name else FINAL
            structure.append(Link(source_node, target_node))

        for then in component.thens:
            target_node = Field(then.name) if then.name else FINAL
            if then.name and target_node not in field_types.keys():
                error.append(
                    f'undefined field "{then.name}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    then.name.location if then.name else None,
                )
                continue
            structure.append(
                Link(
                    source_node, target_node, then.condition, then.length, then.first, then.location
                )
            )

    return (
        UnprovenMessage(message.identifier, structure, field_types, message.location, error)
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

    error = RecordFluxError()
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
            error.append(
                f'unknown field or literal "{variable.identifier}" in refinement'
                f' condition of "{refinement.pdu}"',
                Subsystem.PARSER,
                Severity.ERROR,
                variable.location,
            )

    if Field(refinement.field) not in pdu.fields:
        error.append(
            f'invalid field "{refinement.field}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.field.location,
        )
        error.propagate()

    refinement.sdu = qualified_type_name(refinement.sdu, refinement.package)
    if refinement.sdu not in messages:
        error.append(
            f'undefined type "{refinement.sdu}" in refinement of "{refinement.pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.sdu.location,
        )
        error.propagate()

    sdu = messages[refinement.sdu]

    result = Refinement(
        refinement.package,
        pdu,
        Field(refinement.field),
        sdu,
        refinement.condition,
        refinement.location,
    )

    result.error.extend(error)
    if result in types.values():
        result.error.append(
            f'duplicate refinement with "{refinement.sdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.location,
        )
        result.error.append(
            "previous occurrence",
            Subsystem.PARSER,
            Severity.INFO,
            types[result.identifier].location,
        )

    return result


def check_naming(error: RecordFluxError, package: PackageSpec, filename: str = None) -> None:
    if str(package.identifier).startswith("RFLX"):
        error.append(
            f'illegal prefix "RFLX" in package identifier "{package.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            package.identifier.location,
        )
    if package.identifier != package.end_identifier:
        error.append(
            f'inconsistent package identifier "{package.end_identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            package.end_identifier.location,
        )
        error.append(
            f'previous identifier was "{package.identifier}"',
            Subsystem.PARSER,
            Severity.INFO,
            package.identifier.location,
        )
    if filename:
        expected_filename = f"{str(package.identifier).lower()}.rflx"
        if filename != expected_filename:
            error.append(
                f'file name does not match unit name "{package.identifier}",'
                f' should be "{expected_filename}"',
                Subsystem.PARSER,
                Severity.ERROR,
                package.identifier.location,
            )
    for t in package.types:
        if is_builtin_type(t.identifier.name):
            error.append(
                f'illegal redefinition of built-in type "{t.identifier.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                t.location,
            )
