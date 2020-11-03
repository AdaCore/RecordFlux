import logging
from collections import OrderedDict
from pathlib import Path
from typing import Dict, List, Mapping, Optional, Sequence, Set, Tuple

from librecordfluxdsllang import (
    AnalysisContext,
    ArrayTypeDef,
    ChecksumAspect,
    Component,
    Components,
    Diagnostic,
    Expr,
    MathematicalAspect,
    MessageTypeDef,
    ModularTypeDef,
    NullID,
    PackageSpec,
    RefinementSpec,
    RFLXNode,
    Specification,
    ThenNode,
    TypeDerivationDef,
)

import rflx.expression as rexpr
from rflx import common, expression as expr
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Array,
    Field,
    Link,
    Message,
    Model,
    ModularInteger,
    Refinement,
    Session,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
    is_builtin_type,
    qualified_type_identifier,
)

from .cache import Cache

log = logging.getLogger(__name__)


class ParseFatalException(Exception):
    pass


def node_location(node: RFLXNode, filename: str = None) -> Location:
    start = node.token_start.sloc_range
    end = node.token_end.sloc_range
    return Location(
        start=(start.start.line, start.start.column),
        source=filename,
        end=(end.end.line, end.end.column),
    )


def diagnostics_to_error(
    diagnostics: List[Diagnostic], error: RecordFluxError, specfile: Path = None
) -> bool:
    """
    Return langkit diagnostics to RecordFlux error. Return True if error occured.
    """

    if len(diagnostics) == 0:
        return False

    for diag in diagnostics:
        loc = diag.sloc_range
        error.append(
            diag.message,
            Subsystem.PARSER,
            Severity.ERROR,
            Location(
                start=(loc.start.line, loc.start.column),
                source=specfile,
                end=(loc.end.line, loc.end.column),
            ),
        )
    return True


class Parser:
    def __init__(self, skip_verification: bool = False, cached: bool = False) -> None:
        self.skip_verification = skip_verification
        self.__specifications: OrderedDict[Path, Optional[Specification]] = OrderedDict()
        self.__evaluated_specifications: Set[ID] = set()
        self.__types: List[Type] = [*BUILTIN_TYPES.values(), *INTERNAL_TYPES.values()]
        self.__sessions: List[Session] = []
        self.__cache = Cache(cached)

    def __parse_unit(
        self, spec: Specification, specfile: Path, transitions: List[Tuple[str, ID]] = None
    ) -> RecordFluxError:
        transitions = transitions or []
        error = RecordFluxError()

        self.__specifications[specfile] = spec
        if spec:
            for context in spec.f_context_clause:
                item = context.f_item.text
                transition = (specfile.name, item)
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
                error.extend(
                    self.__parse_specfile(specfile.parent / f"{item.lower()}.rflx", transitions)
                )

        return error

    def __parse_specfile(
        self, specfile: Path, transitions: List[Tuple[str, ID]] = None
    ) -> RecordFluxError:
        error = RecordFluxError()
        if specfile in self.__specifications:
            self.__specifications.move_to_end(specfile)
        else:
            self.__specifications[specfile] = None

        transitions = transitions or []

        log.info("Parsing %s", specfile)
        unit = AnalysisContext().get_from_file(str(specfile))
        if diagnostics_to_error(unit.diagnostics, error, specfile):
            return error
        return self.__parse_unit(unit.root, specfile, transitions)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        for f in specfiles:
            error.extend(self.__parse_specfile(f))

        for f, s in self.__specifications.items():
            if s:
                check_naming(error, s.f_package_declaration, f)
        error.propagate()

    def parse_string(self, string: str) -> None:
        error = RecordFluxError()
        unit = AnalysisContext().get_from_buffer("<stdin>", string)
        if not diagnostics_to_error(unit.diagnostics, error):
            specfile = Path(
                f"{common.file_name(unit.root.f_package_declaration.f_identifier.text)}.rflx"
            )
            error = self.__parse_unit(unit.root, specfile)
            for f, s in self.__specifications.items():
                if s:
                    check_naming(error, s.f_package_declaration, f)
        error.propagate()

    def create_model(self) -> Model:
        error = RecordFluxError()
        for filename, specification in reversed(self.__specifications.items()):
            if (
                specification
                and specification.f_package_declaration.f_identifier.text
                not in self.__evaluated_specifications
            ):
                self.__evaluated_specifications.add(
                    specification.f_package_declaration.f_identifier.text
                )
                try:
                    self.__evaluate_specification(filename, specification)
                except RecordFluxError as e:
                    error.extend(e)
        try:
            result = Model(self.__types, self.__sessions)
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()
        return result

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {
            s.f_package_declaration.f_identifier.text: s
            for s in self.__specifications.values()
            if s
        }

    def __evaluate_specification(self, filename: str, specification: Specification) -> None:
        log.info("Processing %s", specification.f_package_declaration.f_identifier.text)

        error = RecordFluxError()
        self.__evaluate_types(filename, specification, error)
        self.__evaluate_sessions(filename, specification)
        error.propagate()

    def __evaluate_types(self, filename: str, spec: Specification, error: RecordFluxError) -> None:
        package_id = create_id(spec.f_package_declaration.f_identifier, filename)
        for t in spec.f_package_declaration.f_declarations:
            identifier = package_id * create_id(t.f_identifier, filename).name
            if t.f_definition.kind_name == "ArrayTypeDef":
                new_type = create_array(identifier, t.f_definition, self.__types)
            elif t.f_definition.kind_name == "ModularTypeDef":
                new_type = create_modular(identifier, t.f_definition)
            elif t.f_definition.kind_name == "MessageTypeDef":
                new_type = create_message(
                    identifier, t.f_definition, self.__types, self.skip_verification, self.__cache
                )
            else:
                raise ParseFatalException(f"Unknown type {t.f_definition.kind_name}")
            self.__types.append(new_type)
            error.extend(new_type.error)

    def __evaluate_sessions(self, filename: str, spec: Specification) -> None:
        for s in spec.package.sessions:
            self.__sessions.append(
                Session(
                    ID(spec.package.identifier, s.identifier.location) * s.identifier.name,
                    s.initial,
                    s.final,
                    s.states,
                    s.declarations,
                    s.parameters,
                    self.__types,
                    s.location,
                )
            )


def create_id(identifier: NullID, filename: str = None) -> ID:
    if identifier.kind_name == "UnqualifiedID":
        return ID(identifier.text, location=node_location(identifier, filename))
    elif identifier.kind_name == "ID":
        name = ID(identifier.f_name.text, location=node_location(identifier.f_name, filename))
        if identifier.f_package:
            return (
                ID(
                    identifier.f_package.text,
                    location=node_location(identifier.f_package, filename),
                )
                * name
            )
        else:
            return name

    raise RecordFluxError(f"Invalid ID: {identifier.text}")


def create_array(identifier: ID, array: ArrayTypeDef, types: Sequence[Type]) -> Array:
    element_identifier = ID(
        identifier.parent * create_id(array.f_element_type), node_location(array.f_element_type)
    )

    try:
        element_type = [t for t in types if element_identifier == t.identifier][0]
    except IndexError:
        fail(
            f'undefined element type "{element_identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            element_identifier.location,
        )

    return Array(identifier, element_type, node_location(array))


def create_expression(expression: Expr) -> rexpr.Expr:
    if expression.kind_name == "MathematicalExpression":
        return create_expression(expression.f_data)
    elif expression.kind_name == "NumericLiteral":
        return rexpr.Number(int(expression.text))
    elif expression.kind_name == "BinOp":
        if expression.f_op.kind_name == "OpAnd":
            return rexpr.And(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        elif expression.f_op.kind_name == "OpLt":
            return rexpr.Less(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        elif expression.f_op.kind_name == "OpGt":
            return rexpr.Greater(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        elif expression.f_op.kind_name == "OpPow":
            return rexpr.Pow(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        elif expression.f_op.kind_name == "OpAdd":
            return rexpr.Add(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        elif expression.f_op.kind_name == "OpSub":
            return rexpr.Sub(
                create_expression(expression.f_left), create_expression(expression.f_right)
            )
        else:
            raise NotImplementedError(
                f"Invalid BinOp {expression.f_op.kind_name} => {expression.text}"
            )
    elif expression.kind_name == "QualifiedVariable":
        return expr.Variable(expression.f_identifier.text)
    elif expression.kind_name == "Attribute":
        if expression.f_kind.kind_name == "AttrLast":
            return expr.Last(create_expression(expression.f_expression))
        if expression.f_kind.kind_name == "AttrFirst":
            return expr.First(create_expression(expression.f_expression))
        else:
            raise NotImplementedError(
                f"Invalid Attribute {expression.f_kind.kind_name} => {expression.text}"
            )
    raise NotImplementedError(f"{expression.kind_name} => {expression.text}")


def create_modular(identifier: ID, modular: ModularTypeDef) -> ModularInteger:
    return ModularInteger(identifier, create_expression(modular.f_mod), node_location(modular))


def create_message(
    identifier: ID,
    message: MessageTypeDef,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
) -> Message:

    components = message.f_components

    error = RecordFluxError()

    field_types = create_message_types(identifier, message, types, components)
    structure = create_message_structure(components, error)
    aspects = create_message_aspects(message.f_checksums)

    return create_proven_message(
        UnprovenMessage(
            identifier, structure, field_types, aspects, node_location(message), error
        ).merged(),
        skip_verification,
        cache,
    )


def create_message_types(
    identifier: ID,
    message: MessageTypeDef,
    types: Sequence[Type],
    components: Components,
) -> Dict[Field, Type]:

    field_types: Dict[Field, Type] = {}

    for component in components.f_components:
        type_identifier = qualified_type_identifier(
            create_id(component.f_type_identifier), identifier.parent
        )
        field_type = [t for t in types if t.identifier == type_identifier]
        if field_type:
            field_types[Field(component.f_identifier.text)] = field_type[0]

    return field_types


def create_message_structure(components: Components, error: RecordFluxError) -> List[Link]:
    # pylint: disable=too-many-branches

    def extract_aspect(aspects: List[MathematicalAspect]) -> Tuple[rexpr.Expr, rexpr.Expr]:
        size = expr.UNDEFINED
        first = expr.UNDEFINED
        for aspect in aspects:
            if aspect.f_identifier.text == "Size":
                size = create_expression(aspect.f_value)
            elif aspect.f_identifier.text == "First":
                first = create_expression(aspect.f_value)
            else:
                raise ParseFatalException(f"Invalid aspect {aspect.f_identifier.text}")
        return size, first

    def extract_then(then: ThenNode) -> Tuple[Field, expr.Expr, expr.Expr, expr.Expr, Location]:
        target = FINAL if then.f_target.text == "null" else create_id(then.f_target)
        condition = create_expression(then.f_condition) if then.f_condition else expr.TRUE
        size, first = extract_aspect(then.aspects)
        return target, condition, size, first, node_location(then)

    structure: List[Link] = []

    if components.f_initial_component:
        structure.append(Link(INITIAL, *extract_then(components.f_initial_component.f_then)))

    for i, component in enumerate(components.f_components):
        source_node = Field(component.f_identifier.text) if component.f_identifier else INITIAL
        component_identifier = ID(component.f_identifier.text)

        if not component.f_thens:
            identifier = (
                components.f_components[i + 1].f_identifier.text
                if i + 1 < len(components.f_components)
                else None
            )
            target_node = Field(identifier) if identifier else FINAL
            structure.append(Link(source_node, target_node))

        condition = (
            create_expression(component.f_condition.f_data) if component.f_condition else expr.TRUE
        )
        size, first = extract_aspect(component.f_aspects)
        if first != expr.UNDEFINED or size != expr.UNDEFINED or condition != expr.TRUE:
            for l in (l for l in structure if l.target.identifier == component_identifier):
                if first != expr.UNDEFINED:
                    if l.first == expr.UNDEFINED:
                        l.first = first
                    else:
                        error.append(
                            f'first aspect of field "{component_identifier}"'
                            " conflicts with previous"
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            first.location,
                        )
                        error.append(
                            "previous specification of first",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.first.location,
                        )

                if size != expr.UNDEFINED:
                    if l.size == expr.UNDEFINED:
                        l.size = component_size
                    else:
                        error.append(
                            f'size aspect of field "{component_identifier}" conflicts with previous'
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            size.location,
                        )
                        error.append(
                            "previous specification of size",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.size.location,
                        )

                if condition != expr.TRUE:
                    l.condition = (
                        expr.And(condition, l.condition, location=l.condition.location)
                        if l.condition != expr.TRUE
                        else condition
                    )

        for then in component.f_thens:
            if then.f_target and not any(
                then.f_target == c.f_identifier for c in components.f_components
            ):
                error.append(
                    f'undefined field "{then.f_target}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(then.f_target) if then.f_target else None,
                )
                continue
            structure.append(Link(source_node, *extract_then(then)))

    return structure


def create_message_aspects(checksum: ChecksumAspect) -> Mapping[ID, Sequence[rexpr.Expr]]:
    result = {}
    if checksum:
        for assoc in checksum.f_associations:
            exprs = []
            for value in assoc.f_covered_fields:
                if value.kind_name == "ChecksumVal":
                    exprs.append(create_expression(value.f_data))
                elif value.kind_name == "ChecksumValueRange":
                    exprs.append(
                        rexpr.ValueRange(
                            create_expression(value.f_lower), create_expression(value.f_upper)
                        )
                    )
                else:
                    raise ParseFatalException(f"Invalid checksum association {value.kind_name}")
            result[create_id(assoc.f_identifier)] = exprs
    return result


def create_derived_message(
    identifier: ID,
    derivation: TypeDerivationDef,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
) -> Message:
    base_name = qualified_type_identifier(derivation.base, derivation.package)
    error = RecordFluxError()

    base_types = [t for t in types if t.identifier == base_name]

    if not base_types:
        fail(
            f'undefined base message "{base_name}" in derived message',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )

    base_messages = [t for t in base_types if isinstance(t, Message)]

    if not base_messages:
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
            base_types[0].location,
        )
        error.propagate()

    return create_proven_message(
        UnprovenDerivedMessage(
            derivation.identifier, base_messages[0], location=derivation.location
        ).merged(),
        skip_verification,
        cache,
    )


def create_proven_message(
    unproven_message: UnprovenMessage, skip_verification: bool, cache: Cache
) -> Message:
    proven_message = unproven_message.proven(
        skip_verification or cache.is_verified(unproven_message)
    )

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(
    identifie: ID, refinement: RefinementSpec, types: Sequence[Type]
) -> Refinement:
    messages = {t.identifier: t for t in types if isinstance(t, Message)}

    refinement.pdu = qualified_type_identifier(refinement.pdu, refinement.package)
    if refinement.pdu not in messages:
        fail(
            f'undefined type "{refinement.pdu}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.location,
        )

    refinement.sdu = qualified_type_identifier(refinement.sdu, refinement.package)
    if refinement.sdu not in messages:
        fail(
            f'undefined type "{refinement.sdu}" in refinement of "{refinement.pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.sdu.location,
        )

    return Refinement(
        refinement.package,
        messages[refinement.pdu],
        Field(refinement.field),
        messages[refinement.sdu],
        refinement.condition,
        refinement.location,
    )


def check_naming(error: RecordFluxError, package: PackageSpec, filename: Path = None) -> None:
    identifier = package.f_identifier.text
    if identifier.startswith("RFLX"):
        error.append(
            f'illegal prefix "RFLX" in package identifier "{identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_identifier, filename),
        )
    if identifier != identifier:
        error.append(
            f'inconsistent package identifier "{package.f_end_identifier.text}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_end_identifier, filename),
        )
        error.append(
            f'previous identifier was "{identifier}"',
            Subsystem.PARSER,
            Severity.INFO,
            node_location(package.f_identifier, filename),
        )
    if filename:
        expected_filename = f"{identifier.lower()}.rflx"
        if filename.name != expected_filename:
            error.append(
                f'file name does not match unit name "{identifier}",'
                f' should be "{expected_filename}"',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(package.f_identifier, filename),
            )
    for t in package.f_declarations:
        if is_builtin_type(create_id(t.f_identifier).name):
            error.append(
                f'illegal redefinition of built-in type "{t.f_identifier.text}"',
                Subsystem.MODEL,
                Severity.ERROR,
                t.location,
            )
