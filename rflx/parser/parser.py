import traceback
from collections import deque
from pathlib import Path
from typing import Deque, Dict, Iterable, List, Mapping, MutableMapping, Set, Tuple

from pyparsing import ParseException, ParseFatalException

from rflx.expression import UNDEFINED, Number, Variable
from rflx.model import (
    BUILTIN_TYPES,
    BUILTINS_PACKAGE,
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
        self.__specifications: Deque[Specification] = deque()
        self.__evaluated_specifications: Set[str] = set()
        self.__messages: Dict[str, Message] = {}
        self.__types: Dict[str, Type] = {}
        self.__refinements: List[Refinement] = []

    def parse(self, specfile: Path) -> None:
        self.__parse(specfile)

    def __parse(self, specfile: Path, transitions: Set[Tuple[str, str]] = None) -> None:
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
            self.__evaluate_specification(specification, self.specifications)
        return Model(list(self.__messages.values()), self.__refinements)

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {s.package.identifier: s for s in self.__specifications}

    def __evaluate_specification(
        self, specification: Specification, specifications: Dict[str, Specification]
    ) -> None:
        try:
            messages, types = create_messages(
                specification, self.__messages, self.__types, specifications
            )

            if messages:
                self.__messages.update(messages)

            self.__types.update(types)
            check_types(self.__types)

            refinements = create_refinements(specification, self.__messages, self.__types)

            if refinements:
                self.__refinements.extend(refinements)
        except (ParserError, ModelError) as e:
            raise e
        except Exception:
            raise ParserError(traceback.format_exc())


class ParserError(Exception):
    pass


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


def create_messages(
    spec: Specification,
    messages: Mapping[str, Message],
    types: Mapping[str, Type],
    specifications: Dict[str, Specification],
) -> Tuple[Dict[str, Message], Dict[str, Type]]:

    spec_types: Dict[str, Type] = dict(BUILTIN_TYPES)
    spec_messages: Dict[str, UnprovenMessage] = {}

    literals = {
        name: {
            Variable(l)
            for t in spec.package.types + [v for _, v in spec_types.items()]
            if isinstance(t, Enumeration)
            for l in t.literals
        }
        for name, spec in specifications.items()
    }

    for t in spec.package.types:
        t.full_name = f"{spec.package.identifier}.{t.name}"

        if t.full_name in spec_types:
            raise ParserError(f'duplicate type "{t.full_name}"')

        if isinstance(t, (ModularInteger, RangeInteger, Enumeration)):
            pass
        elif isinstance(t, Array):
            t = create_array(t, {**spec_types, **types})
        elif isinstance(t, MessageSpec):
            spec_messages[t.full_name] = create_message(t, {**spec_types, **types})
            t = Reference(t.full_name)
        elif isinstance(t, DerivationSpec):
            spec_messages[t.full_name] = create_derived_message(t, spec_messages, messages)
            t = Reference(t.full_name)
        elif isinstance(t, Refinement):
            continue
        else:
            raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

        spec_types[t.full_name] = t

    return (proven_messages(spec_messages, messages, literals), spec_types)


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
    if not isinstance(element_type, Reference):
        element_type_size = element_type.size.simplified()
        if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
            raise ParserError(
                f"unsupported size ({element_type_size}) of element type "
                f'"{array.element_type.name}" in "{array.name}" '
                "(no multiple of 8)"
            )
        return Array(array.full_name, element_type)
    return array


def create_message(message: MessageSpec, types: Mapping[str, Type]) -> UnprovenMessage:
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

    return UnprovenMessage(message.full_name, structure, field_types)


def create_derived_message(
    derivation: DerivationSpec,
    messages: Mapping[str, UnprovenMessage],
    known_messages: Mapping[str, Message],
) -> UnprovenMessage:
    base = derivation.base if "." in derivation.base else f"{derivation.package}.{derivation.base}"

    if base in messages:
        message_structure = messages[base].structure
        message_types = messages[base].types
    elif base in known_messages:
        message_structure = known_messages[base].structure
        message_types = known_messages[base].types
    else:
        raise ParserError(f'undefined message "{base}" in derived message "{derivation.full_name}"')

    return UnprovenDerivedMessage(derivation.full_name, base, message_structure, message_types,)


def proven_messages(
    messages: Mapping[str, UnprovenMessage],
    known_messages: Mapping[str, Message],
    literals: Dict[str, Set[Variable]],
) -> Dict[str, Message]:

    return {
        message: merged_message(message, {**messages, **known_messages}, literals).proven_message()
        for message in messages
    }


def create_refinements(
    spec: Specification, messages: Dict[str, Message], types: Dict[str, Type]
) -> List[Refinement]:
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
                ] + [
                    f"{e.package}.{l}"
                    for e in types.values()
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
