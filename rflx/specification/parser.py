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
    Description,
    Diagnostic,
    EnumerationTypeDef,
    Expr,
    GrammarRule,
    MathematicalAspect,
    MessageTypeDef,
    ModularTypeDef,
    NullID,
    PackageSpec,
    RangeTypeDef,
    RefinementSpec,
    RFLXNode,
    SessionDecl,
    SessionSpec,
    Specification,
    State,
    Statement,
    ThenNode,
    Transition,
    TypeDecl,
    TypeDerivationDef,
    TypeSpec,
)

import rflx.declaration as decl
import rflx.expression as rexpr
import rflx.model.session as rsess
import rflx.statement as stmt
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Array,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    ModularInteger,
    Private,
    RangeInteger,
    Refinement,
    Session,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
    is_builtin_type,
    qualified_type_identifier,
)
from rflx.specification.const import RESERVED_WORDS

from .cache import Cache

log = logging.getLogger(__name__)


def node_location(node: RFLXNode, filename: Path = None) -> Location:
    start = node.token_start.sloc_range
    end = node.token_end.sloc_range
    return Location(
        start=(start.start.line, start.start.column),
        source=str(filename) if filename else "<stdin>",
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

    def __convert_unit(
        self, spec: Specification, specfile: Path = None, transitions: List[ID] = None
    ) -> RecordFluxError:
        transitions = transitions or []
        error = RecordFluxError()

        if spec:
            parent = specfile.parent if specfile else Path(".")
            filename = parent / Path(f"{spec.f_package_declaration.f_identifier.text.lower()}.rflx")
            self.__specifications[filename] = (specfile, spec)
            for context in spec.f_context_clause:
                item = create_id(context.f_item, specfile)
                if item in transitions:
                    error.append(
                        f'dependency cycle when including "{transitions[0]}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        transitions[0].location,
                    )
                    error.extend(
                        [
                            (
                                f'when including "{i}"',
                                Subsystem.PARSER,
                                Severity.INFO,
                                i.location,
                            )
                            for i in transitions[1:] + [item]
                        ]
                    )
                    continue
                transitions.append(item)
                withed_file = filename.parent / f"{str(item).lower()}.rflx"
                error.extend(self.__parse_specfile(withed_file, transitions))

        return error

    def __parse_specfile(self, specfile: Path, transitions: List[ID] = None) -> RecordFluxError:
        error = RecordFluxError()
        if specfile in self.__specifications:
            self.__specifications.move_to_end(specfile)
            return error

        transitions = transitions or []

        log.info("Parsing %s", specfile)
        unit = AnalysisContext().get_from_file(str(specfile))
        if diagnostics_to_error(unit.diagnostics, error, specfile):
            return error
        return self.__convert_unit(unit.root, specfile, transitions)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        for f in specfiles:
            error.extend(self.__parse_specfile(f))

        for f, (origname, s) in self.__specifications.items():
            if s:
                check_naming(error, s.f_package_declaration, f, origname)
        error.propagate()

    def parse_string(
        self,
        string: str,
        rule: GrammarRule = GrammarRule.main_rule_rule,
    ) -> None:
        error = RecordFluxError()
        unit = AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)
        if not diagnostics_to_error(unit.diagnostics, error):
            error = self.__convert_unit(unit.root)
            for f, (origname, s) in self.__specifications.items():
                if s:
                    check_naming(error, s.f_package_declaration, f, origname)
        error.propagate()

    def create_model(self) -> Model:
        error = RecordFluxError()
        for filename, (origname, specification) in reversed(self.__specifications.items()):
            if (
                specification
                and specification.f_package_declaration.f_identifier.text
                not in self.__evaluated_specifications
            ):
                self.__evaluated_specifications.add(
                    specification.f_package_declaration.f_identifier.text
                )
                try:
                    self.__evaluate_specification(specification, origname)
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
            for _, s in self.__specifications.values()
            if s
        }

    def __evaluate_specification(self, specification: Specification, filename: str = Path) -> None:
        log.info("Processing %s", specification.f_package_declaration.f_identifier.text)

        error = RecordFluxError()
        self.__evaluate_types(specification, error, filename)
        error.propagate()

    def __evaluate_types(
        self, spec: Specification, error: RecordFluxError, filename: Path = None
    ) -> None:
        package_id = create_id(spec.f_package_declaration.f_identifier, filename)
        for t in spec.f_package_declaration.f_declarations:
            if isinstance(t, TypeSpec):
                identifier = qualified_type_identifier(
                    create_id(t.f_identifier, filename), package_id
                )
                if t.f_definition.kind_name == "ArrayTypeDef":
                    new_type = create_array(identifier, t.f_definition, self.__types, filename)
                elif t.f_definition.kind_name == "ModularTypeDef":
                    new_type = create_modular(identifier, t.f_definition, filename)
                elif t.f_definition.kind_name == "RangeTypeDef":
                    new_type = create_range(identifier, t.f_definition, filename)
                elif t.f_definition.kind_name == "MessageTypeDef":
                    new_type = create_message(
                        identifier,
                        t.f_definition,
                        self.__types,
                        self.skip_verification,
                        self.__cache,
                        filename,
                    )
                elif t.f_definition.kind_name == "NullMessageTypeDef":
                    new_type = Message(identifier, [], {}, location=node_location(t, filename))
                elif t.f_definition.kind_name == "TypeDerivationDef":
                    new_type = create_derived_message(
                        identifier,
                        t.f_definition,
                        filename,
                        self.__types,
                        self.skip_verification,
                        self.__cache,
                    )
                elif t.f_definition.kind_name == "EnumerationTypeDef":
                    new_type = create_enumeration(identifier, t.f_definition, filename)
                else:
                    fail(
                        f"Unknown type {t.f_definition.kind_name}",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(identifier, filename),
                    )
            else:
                if t.kind_name == "RefinementSpec":
                    new_type = create_refinement(t, package_id, self.__types, filename)
                elif t.kind_name == "SessionSpec":
                    session = create_session(t, package_id, self.__types, filename)
                    self.__sessions.append(session)
                    error.extend(session.error)
                    continue
                else:
                    fail(
                        f'Unknown type "{t.kind_name}"',
                        Subsystem.PARSER,
                        Severity.ERROR,
                        node_location(t, filename),
                    )
            self.__types.append(new_type)
            error.extend(new_type.error)


def create_session(
    session: SessionSpec, package: ID, types: Sequence[Type], filename: Path = None
) -> Refinement:
    def __create_description(description: Description = None) -> Optional[str]:
        if description:
            return description.text.split('"')[1]
        return None

    def __create_transition(transition: Transition) -> rsess.Transition:
        if transition.kind_name not in ("Transition", "ConditionalTransition"):
            raise NotImplementedError(f"Transition kind {transition.kind_name} unsupported")
        target = qualified_type_identifier(create_id(transition.f_target, filename), package)
        condition = rexpr.TRUE
        description = __create_description(transition.f_description)
        if transition.kind_name == "Conditional_Transition":
            condition = create_expression(declaration.f_condition, filename, package)
        return rsess.Transition(target, condition, description, node_location(transition, filename))

    def __create_statement(statement: Statement) -> stmt.Statement:
        identifier = qualified_type_identifier(create_id(statement.f_identifier, filename), package)
        location = node_location(statement, filename)
        if statement.kind_type == "Assignment":
            return stmt.Assignment(
                identifier, create_expression(statement.f_expression, filename, package), location
            )
        elif statement.kind_type == "ListAttribute":
            expression = create_expression(statement.f_expression, filename, package)
            if statement.f_attr.kind_name == "ListAttrAppend":
                return stmt.Append(identifier, expression, location)
            elif statement.f_attr.kind_name == "ListAttrExtend":
                return stmt.Extend(identifier, expression, location)
            elif statement.f_attr.kind_name == "ListAttrRead":
                return stmt.Read(identifier, expression, location)
            elif statement.f_attr.kind_name == "ListAttrWrite":
                return stmt.Write(identifier, expression, location)

            raise NotImplementedError(f"Attribute {statement.f_attr.kind_name} unsupported")

        elif statement.kind_type == "Reset":
            return stmt.Reset(identifier, location)

        raise NotImplementedError(f"Statement kind {statement.kind_name} unsupported")

    def __create_state(state: State) -> rsess.State:
        identifier = qualified_type_identifier(create_id(state.f_identifier, filename), package)
        if state.f_body.kind_name == "NullStateBody":
            return rsess.State(identifier)
        transitions = []
        for t in state.f_body.f_conditional_transitions:
            transitions.append(__create_transition(t))
        transitions.append(__create_transition(state.f_body.f_final_transition))
        actions = []
        if state.f_body.f_actions:
            for a in state.f_body.f_actions:
                actions.append(__create_statement(a))
        declarations = []
        if state.f_body.f_declarations:
            for d in state.f_body.f_declarations:
                declarations.append(__create_declarations(d))
        description = __create_description(state.f_description)
        return rsess.State(
            identifier=identifier,
            transitions=transitions,
            actions=actions,
            declarations=declarations,
            description=description,
        )

    def __create_declaration(declaration: SessionDecl) -> decl.BasicDeclaration:
        identifier = qualified_type_identifier(
            create_id(declaration.f_identifier, filename), package
        )
        type_identifier = create_id(declaration.f_type_identifier, filename)
        if declaration.kind_name == "VariableDecl":
            return decl.VariableDeclaration(
                identifier,
                type_identifier,
                create_expression(declaration.f_initializer, filename, package),
                node_location(declaration, filename),
            )
        elif declaration.kind_name == "RenamingDecl":
            return decl.RenamingDeclaration(
                identifier,
                type_identifier,
                create_expression(declaration.f_expression, filename, package),
                node_location(declaration, filename),
            )

        raise NotImplementedError(f"Declaration kind {declaration.kind_name} unsupported")

    def __create_parameter(declaration: TypeDecl) -> decl.FormalDeclaration:
        arguments = []
        identifier = qualified_type_identifier(
            create_id(declaration.f_identifier, filename), package
        )
        location = node_location(declaration, filename)
        if declaration.kind_name == "FunctionDecl":
            if declaration.f_parameters:
                for a in declaration.f_parameters.f_parameters:
                    arg_identifier = qualified_type_identifier(
                        create_id(a.f_identifier, filename), package
                    )
                    type_identifier = qualified_type_identifier(
                        create_id(a.f_type_identifier, filename), package
                    )
                    arguments.append(decl.Argument(arg_identifier, type_identifier))
            return_type = qualified_type_identifier(
                create_id(declaration.f_return_type_identifier, filename), package
            )
            return FunctionDeclaration(identifier, arguments, return_type, location)
        elif declaration.kind_name == "ChannelDecl":
            readable = False
            writable = False
            if declaration.f_parameters:
                for p in declaration.f_parameters:
                    if p.kind_type == "Readable":
                        readable = True
                    elif p.kind_type == "Writable":
                        writable = True
                    else:
                        raise NotImplementedError(f"Channel parameter {p.kind_name} unsupported")
            return ChannelDeclaration(identifier, readable, writable, location)
        elif declaration.kind_name == "PrivateTypeDecl":
            return Private(identifier, location)

        raise NotImplementedError(f"Parameter kind {declaration.kind_name} unsupported")

    return Session(
        qualified_type_identifier(create_id(session.f_identifier, filename), package),
        session.f_aspects.f_initial.text,
        session.f_aspects.f_final.text,
        [__create_state(s) for s in session.f_states],
        [__create_declaration(d) for d in session.f_declarations],
        [__create_parameter(p) for p in session.f_parameters],
        types,
        node_location(session, filename),
    )


def create_id(identifier: NullID, filename: Path = None) -> ID:
    if identifier.kind_name == "UnqualifiedID":
        if identifier.text.lower() in RESERVED_WORDS:
            fail(
                f'reserved word "{identifier.text}" used as identifier',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(identifier, filename),
            )
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

    fail(
        f"Invalid ID: {identifier.text}",
        Subsystem.PARSER,
        Severity.ERROR,
        node_location(identifier),
    )


def create_array(
    identifier: ID, array: ArrayTypeDef, types: Sequence[Type], filename: Path = None
) -> Array:
    element_identifier = qualified_type_identifier(
        create_id(array.f_element_type, filename), identifier.parent
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

    return Array(identifier, element_type, node_location(array, filename))


def create_expression(expression: Expr, filename: Path = None, package: ID = None) -> rexpr.Expr:
    location = node_location(expression, filename)
    if expression.kind_name == "MathematicalExpression":
        return create_expression(expression.f_data, filename, package)
    elif expression.kind_name == "NumericLiteral":
        num = expression.text.split("#")
        if len(num) == 1:
            return rexpr.Number(int(num[0]), location=location)
        elif len(num) == 3:
            base = int(num[0])
            return rexpr.Number(int(num[1], base), base=base, location=location)
        fail(
            f"Invalid numeric literal: {expression.text}",
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(identifier, filename),
        )
    elif expression.kind_name == "BinOp":
        if expression.f_op.kind_name == "OpAnd":
            return rexpr.And(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        if expression.f_op.kind_name == "OpOr":
            return rexpr.Or(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpLt":
            return rexpr.Less(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpGt":
            return rexpr.Greater(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpLe":
            return rexpr.LessEqual(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpGe":
            return rexpr.GreaterEqual(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpPow":
            return rexpr.Pow(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpAdd":
            return rexpr.Add(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpSub":
            return rexpr.Sub(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpMul":
            return rexpr.Mul(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpDiv":
            return rexpr.Div(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpEq":
            return rexpr.Equal(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpNeq":
            return rexpr.NotEqual(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpIn":
            return rexpr.In(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpNotin":
            return rexpr.NotIn(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        elif expression.f_op.kind_name == "OpMod":
            return rexpr.Mod(
                create_expression(expression.f_left, filename, package),
                create_expression(expression.f_right, filename, package),
                location=location,
            )
        else:
            raise NotImplementedError(
                f"Invalid BinOp {expression.f_op.kind_name} => {expression.text}"
            )
    elif expression.kind_name == "ParenExpression":
        return create_expression(expression.f_data, filename, package)
    elif expression.kind_name == "BooleanExpression":
        return create_expression(expression.f_data, filename, package)
    elif expression.kind_name == "Variable":
        if expression.f_identifier.text.lower() == "true":
            return rexpr.TRUE
        elif expression.f_identifier.text.lower() == "false":
            return rexpr.FALSE
        var_id = create_id(expression.f_identifier, filename)
        if package:
            return rexpr.Variable(qualified_type_identifier(var_id, package), location=location)
        return rexpr.Variable(var_id, location=location)
    elif expression.kind_name == "Attribute":
        attr_id = create_id(expression.f_identifier, filename)
        if expression.f_kind.kind_name == "AttrLast":
            return rexpr.Last(attr_id)
        elif expression.f_kind.kind_name == "AttrFirst":
            return rexpr.First(attr_id)
        elif expression.f_kind.kind_name == "AttrSize":
            return rexpr.Size(attr_id)
        elif expression.f_kind.kind_name == "AttrValidChecksum":
            return rexpr.ValidChecksum(attr_id)
        else:
            raise NotImplementedError(
                f"Invalid Attribute: {expression.f_kind.kind_name} => {expression.text}"
            )
    elif expression.kind_name == "ExpressionAttribute":
        inner = create_expression(expression.f_expression, filename, package)
        if expression.f_kind.kind_name == "ExprAttrHead":
            return rexpr.Head(inner)
        elif expression.f_kind.kind_name == "ExprAttrOpaque":
            return rexpr.Opaque(inner)
        elif expression.f_kind.kind_name == "ExprAttrPresent":
            return rexpr.Present(inner)
        elif expression.f_kind.kind_name == "ExprAttrValid":
            return rexpr.Valid(inner)
        else:
            raise NotImplementedError(
                f"Invalid ExprssionAttribute: {expression.f_kind.kind_name} => {expression.text}"
            )
    elif expression.kind_name == "ArrayAggregate":
        return rexpr.Aggregate(
            *[create_expression(v, filename, package) for v in expression.f_values],
            location=location,
        )
    elif expression.kind_name == "StringLiteral":
        return rexpr.String(
            expression.text.split('"')[1],
            location=location,
        )
    elif expression.kind_name == "Call":
        return rexpr.Call(
            create_id(expression.f_identifier, filename),
            [create_expression(a, filename, package) for a in expression.f_arguments],
            location=location,
        )
    elif expression.kind_name == "QuantifiedExpression":
        param_id = create_id(expression.f_parameter_identifier, filename)
        iterable = create_expression(expression.f_iterable, filename, package)
        predicate = create_expression(expression.f_predicate, filename, package)
        if expression.f_operation.kind_name == "QuantAll":
            return rexpr.ForAllIn(param_id, iterable, predicate, location)
        elif expression.f_operation.kind_name == "QuantSome":
            return rexpr.ForSomeIn(param_id, iterable, predicate, location)
        else:
            raise NotImplementedError(f"Invalid quantified: {rexpr.f_operation.text}")
    elif expression.kind_name == "Binding":
        bindings = {
            create_id(b.f_identifier, filename): create_expression(
                b.f_expression, filename, package
            )
            for b in expression.f_bindings
        }
        return rexpr.Binding(
            create_expression(expression.f_expression, filename, package), bindings, location
        )
    elif expression.kind_name == "ListAttribute":
        attrs = {
            "Append": stmt.Append,
            "Extend": stmt.Extend,
            "Read": stmt.Read,
            "Write": stmt.Write,
        }
        try:
            constructor = attrs[expression.f_attr.text]
        except KeyError:
            raise NotImplementedError(f"list attribute: {expression.f_attr.text}")

        return constructor(
            create_id(expression.f_identifier, filename),
            create_expression(expression.f_expression, filename, package),
            location=location,
        )
    elif expression.kind_name == "Reset":
        return stmt.Reset(create_id(expression.f_identifier, filename), location=location)
    elif expression.kind_name == "VariableDecl":
        initializer = (
            create_expression(expression.f_initializer, filename, package)
            if expression.f_initializer
            else None
        )
        return decl.VariableDeclaration(
            create_id(expression.f_identifier, filename),
            qualified_type_identifier(create_id(expression.f_type_identifier, filename), package),
            initializer,
            location=location,
        )
    elif expression.kind_name == "PrivateTypeDecl":
        return decl.TypeDeclaration(
            Private(create_id(expression.f_identifier, filename), location=location)
        )
    elif expression.kind_name == "ChannelDecl":
        readable = False
        writable = False
        for p in expression.f_parameters:
            if p.kind_name == "Readable":
                readable = True
            elif p.kind_name == "Writable":
                writable = True
            else:
                raise NotImplementedError(f"channel parameter: {p.kind_name}")
        return decl.ChannelDeclaration(
            create_id(expression.f_identifier, filename),
            readable=readable,
            writable=writable,
            location=location,
        )
    elif expression.kind_name == "RenamingDecl":
        return decl.RenamingDeclaration(
            create_id(expression.f_identifier, filename),
            qualified_type_identifier(create_id(expression.f_type_identifier, filename), package),
            create_expression(expression.f_expression, filename, package),
            location,
        )
    elif expression.kind_name == "FunctionDecl":
        arguments = []
        if expression.f_parameters:
            for p in expression.f_parameters.f_parameters:
                arguments.append(
                    decl.Argument(
                        create_id(p.f_identifier, filename),
                        qualified_type_identifier(
                            create_id(p.f_type_identifier, filename), package
                        ),
                    )
                )
        return decl.FunctionDeclaration(
            create_id(expression.f_identifier, filename),
            arguments,
            create_id(expression.f_return_type_identifier, filename),
            location,
        )
    elif expression.kind_name == "Negation":
        expr = create_expression(expression.f_data, filename, package)
        assert isinstance(expr, rexpr.Number)
        return rexpr.Number(-expr.value, expr.base, location)
    elif expression.kind_name == "Assignment":
        return stmt.Assignment(
            create_id(expression.f_identifier, filename),
            create_expression(expression.f_expression, filename, package),
            location,
        )
    elif expression.kind_name == "Concatenation":
        left = create_expression(expression.f_left, filename, package)
        right = create_expression(expression.f_right, filename, package)
        assert isinstance(left, rexpr.Aggregate)
        assert isinstance(right, rexpr.Aggregate)
        return rexpr.Aggregate(*(left.elements + right.elements), location=location)
    elif expression.kind_name == "Comprehension":
        condition = create_expression(expression.f_condition, filename, package) if expression.f_condition else rexpr.TRUE
        return rexpr.Comprehension(
            create_id(expression.f_iterator, filename),
            create_expression(expression.f_array, filename, package),
            create_expression(expression.f_selector, filename, package),
            condition,
            location,
        )
    elif expression.kind_name == "SelectNode":
        return rexpr.Selected(
            create_expression(expression.f_expression, filename, package),
            create_id(expression.f_selector, filename),
            location=location,
        )
    elif expression.kind_name == "Conversion":
        return rexpr.Conversion(
            create_id(expression.f_target_identifier, filename),
            create_expression(expression.f_argument, filename, package),
            location=location,
        )

    raise NotImplementedError(f"{expression.kind_name} => {expression.text}")


def create_modular(
    identifier: ID, modular: ModularTypeDef, filename: Path = None
) -> ModularInteger:
    return ModularInteger(
        identifier,
        create_expression(modular.f_mod, filename, identifier.parent),
        node_location(modular, filename),
    )


def create_range(identifier: ID, rangetype: RangeTypeDef, filename: Path = None) -> RangeInteger:
    if rangetype.f_size.f_identifier.text != "Size":
        fail(
            f"invalid aspect {rangetype.f_size.f_identifier.text} for range type {identifier}",
            Subsystem.PARSER,
            Severity.ERROR,
            base_name.location,
        )
    size = create_expression(rangetype.f_size.f_value, filename, identifier.parent)
    return RangeInteger(
        identifier,
        create_expression(rangetype.f_lower, filename, identifier.parent),
        create_expression(rangetype.f_upper, filename, identifier.parent),
        size,
        node_location(rangetype, filename),
    )


def create_message(
    identifier: ID,
    message: MessageTypeDef,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
    filename: Path = None,
) -> Message:

    components = message.f_components

    error = RecordFluxError()

    field_types = create_message_types(identifier, message, types, components, filename)
    structure = create_message_structure(components, identifier.parent, error, filename)
    aspects = {
        ID("Checksum"): create_message_aspects(message.f_checksums, identifier.parent, filename)
    }

    return create_proven_message(
        UnprovenMessage(
            identifier, structure, field_types, aspects, node_location(message, filename), error
        ).merged(),
        skip_verification,
        cache,
    )


def create_message_types(
    identifier: ID,
    message: MessageTypeDef,
    types: Sequence[Type],
    components: Components,
    filename: Path = None,
) -> Dict[Field, Type]:

    field_types: Dict[Field, Type] = {}

    for component in components.f_components:
        type_identifier = qualified_type_identifier(
            create_id(component.f_type_identifier, filename), identifier.parent
        )
        field_type = [t for t in types if t.identifier == type_identifier]
        if field_type:
            field_types[Field(component.f_identifier.text)] = field_type[0]

    return field_types


def create_message_structure(
    components: Components, package: ID, error: RecordFluxError, filename: Path = None
) -> List[Link]:
    # pylint: disable=too-many-branches

    def extract_aspect(aspects: List[MathematicalAspect]) -> Tuple[rexpr.Expr, rexpr.Expr]:
        size = rexpr.UNDEFINED
        first = rexpr.UNDEFINED
        for aspect in aspects:
            if aspect.f_identifier.text == "Size":
                size = create_expression(aspect.f_value, filename)
            elif aspect.f_identifier.text == "First":
                first = create_expression(aspect.f_value, filename)
            else:
                fail(
                    f"Invalid aspect {aspect.f_identifier.text}",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(aspect.f_identifier, filename),
                )
        return size, first

    def extract_then(then: ThenNode) -> Tuple[Field, rexpr.Expr, rexpr.Expr, rexpr.Expr, Location]:
        target = FINAL if then.f_target.text == "null" else Field(then.f_target.text)
        condition = create_expression(then.f_condition, filename) if then.f_condition else rexpr.TRUE
        size, first = extract_aspect(then.f_aspects)
        return target, condition, size, first, node_location(then, filename)

    structure: List[Link] = []

    if components.f_initial_component:
        structure.append(Link(INITIAL, *extract_then(components.f_initial_component.f_then)))
    else:
        structure.append(Link(INITIAL, Field(components.f_components[0].f_identifier.text)))

    for i, component in enumerate(components.f_components):
        source_node = Field(component.f_identifier.text) if component.f_identifier else INITIAL
        component_identifier = create_id(component.f_identifier, filename)
        if component.f_identifier.text.lower() == "message":
            fail(
                f'reserved word "Message" used as identifier',
                Subsystem.PARSER,
                Severity.ERROR,
                component_identifier.location,
            )

        if len(component.f_thens) == 0:
            target_name = (
                components.f_components[i + 1].f_identifier.text
                if i + 1 < len(components.f_components)
                else None
            )
            target_node = Field(target_name) if target_name else FINAL
            structure.append(Link(source_node, target_node))

        condition = (
            create_expression(component.f_condition, filename)
            if component.f_condition
            else rexpr.TRUE
        )
        size, first = extract_aspect(component.f_aspects)
        if first != rexpr.UNDEFINED or size != rexpr.UNDEFINED or condition != rexpr.TRUE:
            for l in (l for l in structure if l.target.identifier == component_identifier):
                if first != rexpr.UNDEFINED:
                    if l.first == rexpr.UNDEFINED:
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

                if size != rexpr.UNDEFINED:
                    if l.size == rexpr.UNDEFINED:
                        l.size = size
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

                if condition != rexpr.TRUE:
                    l.condition = (
                        rexpr.And(condition, l.condition, location=l.condition.location)
                        if l.condition != rexpr.TRUE
                        else condition
                    )

        for then in component.f_thens:
            if then.f_target.kind_name != "NullID" and not any(
                then.f_target.text == c.f_identifier.text for c in components.f_components
            ):
                error.append(
                    f'undefined field "{then.f_target.text}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    node_location(then.f_target, filename) if then.f_target else None,
                )
                continue
            structure.append(Link(source_node, *extract_then(then)))

    return structure


def create_message_aspects(
    checksum: ChecksumAspect, package: ID, filename: Path
) -> Mapping[ID, Sequence[rexpr.Expr]]:
    result = {}
    if checksum:
        for assoc in checksum.f_associations:
            exprs = []
            for value in assoc.f_covered_fields:
                if value.kind_name == "ChecksumVal":
                    exprs.append(create_expression(value.f_data, filename))
                elif value.kind_name == "ChecksumValueRange":
                    exprs.append(
                        rexpr.ValueRange(
                            create_expression(value.f_lower, filename),
                            create_expression(value.f_upper, filename),
                        )
                    )
                else:
                    fail(
                        f"Invalid checksum association {value.kind_name}",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        base_name.location,
                    )
            result[create_id(assoc.f_identifier, filename)] = exprs
    return result


def create_derived_message(
    identifier: ID,
    derivation: TypeDerivationDef,
    filename: Path,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
) -> Message:
    base_id = create_id(derivation.f_base, filename)
    base_name = qualified_type_identifier(base_id, identifier.parent)
    error = RecordFluxError()

    base_types = [t for t in types if t.identifier == base_name]

    if not base_types:
        fail(
            f'undefined base message "{base_name}" in derived message',
            Subsystem.PARSER,
            Severity.ERROR,
            base_name.location,
        )

    base_messages = [t for t in base_types if isinstance(t, Message)]

    if not base_messages:
        error.append(
            f'illegal derivation "{identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            identifier.location,
        )
        error.append(
            f'invalid base message type "{base_name}"',
            Subsystem.PARSER,
            Severity.INFO,
            base_types[0].identifier.location,
        )
        error.propagate()

    return create_proven_message(
        UnprovenDerivedMessage(
            identifier, base_messages[0], location=node_location(derivation, filename)
        ).merged(),
        skip_verification,
        cache,
    )


def create_enumeration(
    identifier: ID,
    enumeration: EnumerationTypeDef,
    filename: Path,
) -> Message:
    always_valid = False
    size = None
    literals = []

    if enumeration.f_elements.kind_name == "NamedEnumerationDef":
        for e in enumeration.f_elements.f_elements:
            element_identifier = create_id(e.f_identifier, filename)
            value = create_expression(e.f_literal, filename)
            literals.append((element_identifier, value))
    elif enumeration.f_elements.kind_name == "PositionalEnumerationDef":
        literals = [
            (create_id(e, filename), rexpr.Number(i))
            for i, e in enumerate(enumeration.f_elements.f_elements)
        ]
    else:
        raise NotImplementedError(
            f"Enumeration kind {enumeration.f_elements.kind_name}" " unsupported"
        )

    for a in enumeration.f_aspects:
        if a.f_identifier.text == "Size":
            size = create_expression(a.f_value, filename, identifier.parent)
        if a.f_identifier.text == "Always_Valid":
            if a.f_value:
                av_expr = create_expression(a.f_value, filename)
                if av_expr == rexpr.TRUE:
                    always_valid = True
                elif av_expr == rexpr.FALSE:
                    always_valid = False
                else:
                    fail(
                        f"Invalid Always_Valid expression: {av_expr}",
                        Subsystem.PARSER,
                        Severity.ERROR,
                        base_name.location,
                    )
            else:
                always_valid = True

    if not size:
        fail(
            f"No size set for {identifier}",
            Subsystem.PARSER,
            Severity.ERROR,
            base_name.location,
        )
    return Enumeration(identifier, literals, size, always_valid)


def create_proven_message(
    unproven_message: UnprovenMessage, skip_verification: bool, cache: Cache
) -> Message:
    proven_message = unproven_message.proven(
        skip_verification or cache.is_verified(unproven_message)
    )

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(
    refinement: RefinementSpec, package: ID, types: Sequence[Type], filename: Path
) -> Refinement:
    messages = {t.identifier: t for t in types if isinstance(t, Message)}

    pdu = qualified_type_identifier(create_id(refinement.f_pdu, filename), package)
    if pdu not in messages:
        fail(
            f'undefined type "{pdu}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(refinement, filename),
        )

    sdu = qualified_type_identifier(create_id(refinement.f_sdu, filename), package)
    if sdu not in messages:
        fail(
            f'undefined type "{sdu}" in refinement of "{pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            sdu.location,
        )

    if refinement.f_condition:
        condition = create_expression(refinement.f_condition.f_data, filename)
    else:
        condition = rexpr.TRUE

    return Refinement(
        package,
        messages[pdu],
        Field(refinement.f_field.text),
        messages[sdu],
        condition,
        node_location(refinement, filename),
    )


def check_naming(
    error: RecordFluxError, package: PackageSpec, filename: Path, origname: Path = None
) -> None:
    name = "<stdin>" or origname
    identifier = package.f_identifier.text
    if identifier.startswith("RFLX"):
        error.append(
            f'illegal prefix "RFLX" in package identifier "{identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_identifier, name),
        )
    if identifier != package.f_end_identifier.text:
        error.append(
            f'inconsistent package identifier "{package.f_end_identifier.text}"',
            Subsystem.PARSER,
            Severity.ERROR,
            node_location(package.f_end_identifier, name),
        )
        error.append(
            f'previous identifier was "{identifier}"',
            Subsystem.PARSER,
            Severity.INFO,
            node_location(package.f_identifier, name),
        )
    if origname:
        expected_filename = f"{identifier.lower()}.rflx"
        if origname.name != expected_filename:
            error.append(
                f'file name does not match unit name "{identifier}",'
                f' should be "{expected_filename}"',
                Subsystem.PARSER,
                Severity.ERROR,
                node_location(package.f_identifier, origname),
            )
    for t in package.f_declarations:
        if isinstance(t, TypeSpec) and is_builtin_type(create_id(t.f_identifier, name).name):
            error.append(
                f'illegal redefinition of built-in type "{t.f_identifier.text}"',
                Subsystem.MODEL,
                Severity.ERROR,
                node_location(t, name),
            )
