import logging
import traceback
from collections import deque
from pathlib import Path
from typing import Deque, Dict, List, Mapping, MutableMapping, Set, Tuple

from pyparsing import ParseException, ParseFatalException

from rflx.expression import UNDEFINED, Number
from rflx.model import (
    BUILTIN_TYPES,
    BUILTINS_PACKAGE,
    FINAL,
    INITIAL,
    AbstractMessage,
    Array,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    ModelError,
    Refinement,
    Scalar,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
)

from . import grammar
from .ast import Component, DerivationSpec, MessageSpec, RefinementSpec, Specification

log = logging.getLogger(__name__)


class Parser:
    def __init__(self) -> None:
        self.__specifications: Deque[Specification] = deque()
        self.__evaluated_specifications: Set[str] = set()
        self.__types: Dict[str, Type] = dict(BUILTIN_TYPES)

    def parse(self, specfile: Path) -> None:
        self.__parse(specfile)

    def __parse(self, specfile: Path, transitions: Set[Tuple[str, str]] = None) -> None:
        log.info("Parsing %s", specfile)

        if not transitions:
            transitions = set()

        with open(specfile, "r") as filehandle:
            try:
                for specification in grammar.unit().parseFile(filehandle):
                    check_naming(specfile.name, specification.package.identifier)
                    self.__specifications.appendleft(specification)
                    for item in specification.context.items:
                        transition = (specification.package.identifier, item)
                        if transition in transitions:
                            raise ParserError(
                                f'dependency cycle due to context item "{item}"'
                                f' in "{specification.package.identifier}"'
                            )
                        transitions.add(transition)
                        self.__parse(specfile.parent / f"{item.lower()}.rflx", transitions)
            except (ParseException, ParseFatalException) as e:
                raise ParserError("\n" + ParseException.explain(e, 0))

    def parse_string(self, string: str) -> None:
        for specification in grammar.unit().parseString(string):
            self.__specifications.appendleft(specification)

    def create_model(self) -> Model:
        for specification in self.__specifications:
            if specification.package.identifier in self.__evaluated_specifications:
                continue
            self.__evaluated_specifications.add(specification.package.identifier)
            self.__evaluate_specification(specification)
        return Model(list(self.__types.values()))

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {s.package.identifier: s for s in self.__specifications}

    def __evaluate_specification(self, specification: Specification) -> None:
        log.info("Processing %s", specification.package.identifier)

        try:
            self.__evaluate_types(specification)
            check_types(self.__types)
        except (ParserError, ModelError) as e:
            raise e
        except Exception:
            raise ParserError(traceback.format_exc())

    def __evaluate_types(self, spec: Specification) -> None:
        for t in spec.package.types:
            t.full_name = f"{spec.package.identifier}.{t.name}"

            if t.full_name in self.__types:
                raise ParserError(f'duplicate type "{t.full_name}"')

            if isinstance(t, Scalar):
                self.__types[t.full_name] = t

            elif isinstance(t, Array):
                self.__types[t.full_name] = create_array(t, self.__types)

            elif isinstance(t, MessageSpec):
                self.__types[t.full_name] = create_message(t, self.__types)

            elif isinstance(t, DerivationSpec):
                self.__types[t.full_name] = create_derived_message(t, message_types(self.__types))

            elif isinstance(t, RefinementSpec):
                self.__types[t.full_name] = create_refinement(t, self.__types)

            else:
                raise NotImplementedError(f'unsupported type "{type(t).__name__}"')


class ParserError(Exception):
    pass


def message_types(types: Mapping[str, Type]) -> Mapping[str, Message]:
    return {n: m for n, m in types.items() if isinstance(m, Message)}


def check_types(types: Mapping[str, Type]) -> None:
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
                or e2.package in BUILTINS_PACKAGE
            )
        )
    ]:
        identical_literals = set(e1.literals) & set(e2.literals)

        if identical_literals:
            raise ParserError(
                f'"{e2.full_name}" contains identical literals as "{e1.full_name}": '
                + ", ".join(sorted(identical_literals))
            )


def create_array(array: Array, types: Mapping[str, Type]) -> Array:
    array.element_type.full_name = array.element_type.full_name.replace(
        "__PACKAGE__", array.package
    )

    if array.element_type.full_name in types:
        element_type = types[array.element_type.full_name]
    else:
        raise ParserError(
            f'undefined element type "{array.element_type.full_name}" in array "{array.full_name}"'
        )

    if isinstance(element_type, Scalar):
        element_type_size = element_type.size.simplified()
        if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
            raise ParserError(
                f"unsupported size ({element_type_size}) of element type "
                f'"{array.element_type.name}" in "{array.name}" '
                "(no multiple of 8)"
            )

    return Array(array.full_name, element_type)


def create_message(message: MessageSpec, types: Mapping[str, Type]) -> Message:
    components = list(message.components)

    if components and components[0].name != "null":
        components.insert(0, Component("null", ""))

    field_types: MutableMapping[Field, Type] = {}

    for component in components:
        if component.name != "null":
            type_name = (
                component.type
                if "." in component.type or component.type in BUILTIN_TYPES
                else f"{message.package}.{component.type}"
            )
            if type_name not in types:
                raise ParserError(
                    f'undefined component type "{type_name}" in message "{message.full_name}"'
                )
            field_types[Field(component.name)] = types[type_name]

    structure: List[Link] = []

    for i, component in enumerate(components):
        if component.name == "null" and any(then.first != UNDEFINED for then in component.thens):
            raise ParserError(
                f'invalid first expression in initial node of message "{message.full_name}"'
            )

        source_node = Field(component.name) if component.name != "null" else INITIAL

        if not component.thens:
            target_node = Field(components[i + 1].name) if i + 1 < len(components) else FINAL
            structure.append(Link(source_node, target_node))

        for then in component.thens:
            target_node = Field(then.name) if then.name != "null" else FINAL
            if target_node not in field_types.keys() | {FINAL}:
                raise ParserError(
                    f'undefined component "{then.name}" in message "{message.full_name}"'
                )
            structure.append(
                Link(source_node, target_node, then.condition, then.length, then.first)
            )

    return UnprovenMessage(message.full_name, structure, field_types).merged().proven()


def create_derived_message(
    derivation: DerivationSpec, messages: Mapping[str, AbstractMessage],
) -> Message:
    base = derivation.base if "." in derivation.base else f"{derivation.package}.{derivation.base}"

    if base not in messages:
        raise ParserError(f'undefined message "{base}" in derived message "{derivation.full_name}"')

    return (
        UnprovenDerivedMessage(
            derivation.full_name, base, messages[base].structure, messages[base].types
        )
        .merged()
        .proven()
    )


def create_refinement(refinement: RefinementSpec, types: Mapping[str, Type]) -> Refinement:
    messages = message_types(types)

    if "." not in refinement.pdu:
        refinement.pdu = f"{refinement.package}.{refinement.pdu}"
    if refinement.pdu not in messages:
        raise ParserError(f'undefined type "{refinement.pdu}" in refinement')

    pdu = messages[refinement.pdu]

    if Field(refinement.field) not in pdu.fields:
        raise ParserError(f'invalid field "{refinement.field}" in refinement of "{refinement.pdu}"')

    if "." not in refinement.sdu:
        refinement.sdu = f"{refinement.package}.{refinement.sdu}"
    if refinement.sdu not in messages:
        raise ParserError(f'undefined type "{refinement.sdu}" in refinement of "{refinement.pdu}"')

    sdu = messages[refinement.sdu]

    for variable in refinement.condition.variables():
        literals = [
            l for e in pdu.types.values() if isinstance(e, Enumeration) for l in e.literals.keys()
        ] + [
            f"{e.package}.{l}"
            for e in types.values()
            if isinstance(e, Enumeration)
            for l in e.literals.keys()
        ]

        if Field(str(variable.name)) not in pdu.fields and str(variable.name) not in literals:
            raise ParserError(
                f'unknown field or literal "{variable.name}" in refinement'
                f' condition of "{refinement.pdu}"'
            )

    result = Refinement(refinement.package, pdu, Field(refinement.field), sdu, refinement.condition)

    if result in types.values():
        raise ParserError(
            f'duplicate refinement of field "{refinement.field}" with "{refinement.sdu}"'
            f' for "{refinement.pdu}"'
        )

    return result


def check_naming(filename: str, package_identifier: str) -> None:
    expected_filename = f"{package_identifier.lower()}.rflx"
    if filename != expected_filename:
        raise ParserError(
            f'file name "{filename}" does not match unit name "{package_identifier}",'
            f' should be "{expected_filename}"'
        )
