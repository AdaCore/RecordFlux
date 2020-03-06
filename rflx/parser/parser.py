import traceback
from pathlib import Path
from typing import Dict, Iterable, List, MutableMapping

from pyparsing import ParseException, ParseFatalException

from rflx.expression import UNDEFINED, Number
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    ModelError,
    ModularInteger,
    Opaque,
    RangeInteger,
    Reference,
    Refinement,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
    merged_message,
)

from . import grammar
from .ast import Component, DerivationSpec, MessageSpec, Specification


class Parser:
    def __init__(self) -> None:
        self.__specifications: Dict[str, Specification] = {}
        self.__messages: Dict[str, Message] = {}
        self.__refinements: List[Refinement] = []

    def parse(self, specfile: Path) -> None:
        with open(specfile, "r") as filehandle:
            try:
                for specification in grammar.unit().parseFile(filehandle):
                    check_naming(specfile.name, specification.package.identifier)
                    self.__process_specification(specification)
            except (ParseException, ParseFatalException) as e:
                raise ParserError("\n" + ParseException.explain(e, 0))

    def parse_string(self, string: str) -> None:
        for specification in grammar.unit().parseString(string):
            self.__process_specification(specification)

    def create_model(self) -> Model:
        for specification in self.__specifications.values():
            self.__evaluate_specification(specification)
        return Model(list(self.__messages.values()), self.__refinements)

    @property
    def specifications(self) -> Dict[str, Specification]:
        return self.__specifications

    def __process_specification(self, specification: Specification) -> None:
        identifier = specification.package.identifier
        if identifier in self.__specifications:
            raise ParserError(f'duplicate package "{identifier}"')
        self.__specifications[identifier] = specification

    def __evaluate_specification(self, specification: Specification) -> None:
        try:
            messages = convert_to_messages(specification)
            if messages:
                self.__messages.update(messages)
            refinements = convert_to_refinements(specification, self.__messages)
            if refinements:
                self.__refinements.extend(refinements)
        except (ParserError, ModelError) as e:
            raise e
        except Exception:
            raise ParserError(traceback.format_exc())


class ParserError(Exception):
    pass


def convert_to_messages(spec: Specification) -> Dict[str, Message]:
    boolean = Enumeration(
        "__BUILTINS__.Boolean", {"False": Number(0), "True": Number(1)}, Number(1), False
    )
    types: Dict[str, Type] = {
        Opaque().name: Opaque(),
        boolean.name: boolean,
    }
    messages: Dict[str, UnprovenMessage] = {}

    for t in spec.package.types:
        if t.name in types:
            raise ParserError(f'duplicate type "{t.name}"')

        t.full_name = f"{spec.package.identifier}.{t.name}"

        if isinstance(t, (ModularInteger, RangeInteger, Enumeration)):
            pass
        elif isinstance(t, Array):
            t.element_type.full_name = t.element_type.full_name.replace(
                "__PACKAGE__", spec.package.identifier
            )
            if t.element_type.name not in types:
                raise ParserError(f'undefined type "{t.element_type.name}" in "{t.name}"')
            if not isinstance(types[t.element_type.name], Reference):
                element_type_size = types[t.element_type.name].size.simplified()
                if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
                    raise ParserError(
                        f"unsupported size ({element_type_size}) of element type "
                        f'"{t.element_type.name}" in "{t.name}" '
                        "(no multiple of 8)"
                    )
                t = Array(t.full_name, types[t.element_type.name])
        elif isinstance(t, MessageSpec):
            messages[t.full_name] = create_message(t.full_name, types, t.components, t.name)
            t = Reference(t.full_name)
        elif isinstance(t, DerivationSpec):
            base = t.base
            if base not in types and base not in messages:
                raise ParserError(f'undefined type "{t.base}" in "{t.name}"')
            base = qualified_type_name(
                t.base,
                spec.package.identifier,
                messages,
                f'unsupported type "{t.base}" in "{t.name}"',
            )
            messages[t.full_name] = UnprovenDerivedMessage(
                t.full_name, base, messages[base].structure, messages[base].types
            )
            t = Reference(t.full_name)
        elif isinstance(t, Refinement):
            continue
        else:
            raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

        types[t.name] = t

    return proven_messages(messages)


def create_message(
    full_name: str, types: Dict[str, Type], components: List[Component], message_name: str
) -> UnprovenMessage:
    components = list(components)

    if components and components[0].name != "null":
        components.insert(0, Component("null", ""))

    field_types: MutableMapping[Field, Type] = {}

    for component in components:
        if component.name != "null":
            if component.type not in types:
                raise ParserError(f'undefined type "{component.type}" in "{message_name}"')
            field_types[Field(component.name)] = types[component.type]

    structure: List[Link] = []

    for i, component in enumerate(components):
        if component.name == "null" and any(then.first != UNDEFINED for then in component.thens):
            raise ParserError(f'invalid first expression in initial node in "{message_name}"')

        source_node = Field(component.name) if component.name != "null" else INITIAL

        if not component.thens:
            target_node = Field(components[i + 1].name) if i + 1 < len(components) else FINAL
            structure.append(Link(source_node, target_node))

        for then in component.thens:
            target_node = Field(then.name) if then.name != "null" else FINAL
            if target_node not in field_types.keys() | {FINAL}:
                raise ParserError(f'undefined component "{then.name}" in "{message_name}"')
            structure.append(
                Link(source_node, target_node, then.condition, then.length, then.first)
            )

    return UnprovenMessage(full_name, structure, field_types)


def proven_messages(messages: Dict[str, UnprovenMessage]) -> Dict[str, Message]:
    return {message: merged_message(message, messages).proven_message() for message in messages}


def convert_to_refinements(spec: Specification, messages: Dict[str, Message]) -> List[Refinement]:
    refinements: List[Refinement] = []
    for t in spec.package.types:
        if isinstance(t, Refinement):
            pdu = qualified_type_name(
                t.pdu,
                spec.package.identifier,
                messages.keys(),
                f'undefined type "{t.pdu}" in refinement',
            )
            if t.field not in messages[pdu].fields:
                raise ParserError(f'invalid field "{t.field.name}" in refinement of "{t.pdu}"')
            sdu = qualified_type_name(
                t.sdu,
                spec.package.identifier,
                messages.keys(),
                f'undefined type "{t.sdu}" in refinement of "{t.pdu}"',
            )
            refinement = Refinement(spec.package.identifier, pdu, t.field, sdu, t.condition)
            if refinement in refinements:
                raise ParserError(
                    f'duplicate refinement of field "{t.field.name}" with "{t.sdu}"'
                    f' in "{t.pdu}"'
                )
            refinements.append(refinement)
            for variable in t.condition.variables():
                literals = [
                    l
                    for e in messages[pdu].types.values()
                    if isinstance(e, Enumeration)
                    for l in e.literals.keys()
                ]
                if (
                    Field(str(variable.name)) not in messages[pdu].fields
                    and str(variable.name) not in literals
                ):
                    raise ParserError(
                        f'unknown field or literal "{variable.name}" in refinement'
                        f' condition of "{t.pdu}"'
                    )
        elif isinstance(t, DerivationSpec):
            for r in refinements:
                if r.pdu == f"{t.base}" or r.pdu == f"{spec.package.identifier}.{t.base}":
                    pdu = f"{spec.package.identifier}.{t.name}"
                    refinements.append(Refinement(spec.package.identifier, pdu, r.field, r.sdu))
    return refinements


def qualified_type_name(name: str, package: str, types: Iterable[str], error_message: str) -> str:
    if name in types:
        return name

    name = f"{package}.{name}"

    if name not in types:
        raise ParserError(error_message)

    return name


def check_naming(filename: str, package_identifier: str) -> None:
    expected_filename = f"{package_identifier.lower()}.rflx"
    if filename != expected_filename:
        raise ParserError(
            f'file name "{filename}" does not match unit name "{package_identifier}",'
            f' should be "{expected_filename}"'
        )
