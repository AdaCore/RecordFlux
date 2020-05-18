import logging
import traceback
from collections import deque
from pathlib import Path
from typing import Deque, Dict, List, Mapping, Set, Tuple

from pyparsing import ParseException, ParseFatalException

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
    ModelError,
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
            t.identifier = ID(f"{spec.package.identifier}.{t.name}")

            if t.identifier in self.__types:
                raise ParserError(f'duplicate type "{t.identifier}"')

            if isinstance(t, Scalar):
                self.__types[t.identifier] = t

            elif isinstance(t, Array):
                self.__types[t.identifier] = create_array(t, self.__types)

            elif isinstance(t, MessageSpec):
                self.__types[t.identifier] = create_message(t, self.__types)

            elif isinstance(t, DerivationSpec):
                self.__types[t.identifier] = create_derived_message(t, message_types(self.__types))

            elif isinstance(t, RefinementSpec):
                self.__types[t.identifier] = create_refinement(t, self.__types)

            else:
                raise NotImplementedError(f'unsupported type "{type(t).__name__}"')


class ParserError(Exception):
    pass


def message_types(types: Mapping[ID, Type]) -> Mapping[ID, Message]:
    return {n: m for n, m in types.items() if isinstance(m, Message)}


def check_types(types: Mapping[ID, Type]) -> None:
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
        identical_literals = set(e1.literals) & set(e2.literals)

        if identical_literals:
            raise ParserError(
                f'"{e2.identifier}" contains identical literals as "{e1.identifier}": '
                + ", ".join(sorted(identical_literals))
            )

    literals = {l: t for t in types.values() if isinstance(t, Enumeration) for l in t.literals}
    name_conflicts = set(literals) & set(str(t.name) for t in types)
    for name in sorted(name_conflicts):
        raise ParserError(
            f'literal in enumeration "{literals[name].identifier}" conflicts with type "{name}"'
        )


def create_array(array: Array, types: Mapping[ID, Type]) -> Array:
    array.element_type.identifier = ID(
        array.element_type.full_name.replace("__PACKAGE__", str(array.package))
    )

    if array.element_type.identifier in types:
        element_type = types[array.element_type.identifier]
    else:
        raise ParserError(
            f'undefined element type "{array.element_type.identifier}"'
            f' in array "{array.identifier}"'
        )

    if isinstance(element_type, Scalar):
        element_type_size = element_type.size.simplified()
        if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
            raise ParserError(
                f"unsupported size ({element_type_size}) of element type "
                f'"{array.element_type.name}" in "{array.name}" '
                "(no multiple of 8)"
            )

    return Array(array.identifier, element_type)


def create_message(message: MessageSpec, types: Mapping[ID, Type]) -> Message:
    components = list(message.components)

    if components and components[0].name != "null":
        components.insert(0, Component("null", "null"))

    field_types: Dict[Field, Type] = {}

    for component in components:
        if component.name != "null":
            type_name = qualified_type_name(component.type_name, message.package)
            if type_name not in types:
                raise ParserError(
                    f'undefined component type "{type_name}" in message "{message.identifier}"'
                )
            field_types[Field(component.name)] = types[type_name]

    structure: List[Link] = []

    for i, component in enumerate(components):
        if component.name == "null" and any(then.first != UNDEFINED for then in component.thens):
            raise ParserError(
                f'invalid first expression in initial node of message "{message.identifier}"'
            )

        source_node = Field(component.name) if component.name != "null" else INITIAL

        if not component.thens:
            target_node = Field(components[i + 1].name) if i + 1 < len(components) else FINAL
            structure.append(Link(source_node, target_node))

        for then in component.thens:
            target_node = Field(then.name) if then.name != "null" else FINAL
            if target_node not in field_types.keys() | {FINAL}:
                raise ParserError(
                    f'undefined component "{then.name}" in message "{message.identifier}"'
                )
            structure.append(
                Link(source_node, target_node, then.condition, then.length, then.first)
            )

    return UnprovenMessage(message.identifier, structure, field_types).merged().proven()


def create_derived_message(derivation: DerivationSpec, messages: Mapping[ID, Message]) -> Message:
    base_name = qualified_type_name(derivation.base, derivation.package)

    if base_name not in messages:
        raise ParserError(
            f'undefined message "{base_name}" in derived message "{derivation.identifier}"'
        )

    base = messages[base_name]

    if isinstance(base, DerivedMessage):
        raise ParserError(
            f'illegal derivation "{derivation.identifier}" of derived message "{base_name}"'
        )

    return UnprovenDerivedMessage(derivation.identifier, base).merged().proven()


def create_refinement(refinement: RefinementSpec, types: Mapping[ID, Type]) -> Refinement:
    messages = message_types(types)

    refinement.pdu = qualified_type_name(refinement.pdu, refinement.package)
    if refinement.pdu not in messages:
        raise ParserError(f'undefined type "{refinement.pdu}" in refinement')

    pdu = messages[refinement.pdu]

    if Field(refinement.field) not in pdu.fields:
        raise ParserError(f'invalid field "{refinement.field}" in refinement of "{refinement.pdu}"')

    refinement.sdu = qualified_type_name(refinement.sdu, refinement.package)
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
