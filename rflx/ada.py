import itertools
from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from typing import List, Mapping, NamedTuple, Optional, Sequence, Tuple, Union

from rflx.common import indent, indent_next, unique, verify_identifier
from rflx.expression import Case, Expr, Number


class Ada(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self) -> int:
        return hash(repr(self))

    def __repr__(self) -> str:
        args = "\n\t" + ",\n\t".join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f"{self.__class__.__name__}({args})".replace("\t", "\t    ")

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class Declaration(Ada):
    def __init__(self, name: str) -> None:
        verify_identifier(name)
        self.name = name

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ContextItem(Ada):
    def __init__(self, *names: str) -> None:
        for name in names:
            verify_identifier(name)
        self.names = names

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class WithClause(ContextItem):
    def __str__(self) -> str:
        names = ", ".join(self.names)
        return f"with {names};"


class UsePackageClause(ContextItem, Declaration):
    def __str__(self) -> str:
        names = ", ".join(self.names)
        return f"use {names};"


class UseTypeClause(ContextItem, Declaration):
    def __str__(self) -> str:
        names = ", ".join(self.names)
        return f"use type {names};"


class Aspect(Ada):
    def __str__(self) -> str:
        if self.definition:
            return f"{self.mark} =>\n{indent(self.definition, 2)}"
        return f"{self.mark}"

    @abstractproperty
    def mark(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def definition(self) -> str:
        raise NotImplementedError


class Precondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Pre"

    @property
    def definition(self) -> str:
        return str(self.expr)


class Postcondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Post"

    @property
    def definition(self) -> str:
        return str(self.expr)


class ContractCases(Aspect):
    def __init__(self, *cases: Tuple[Expr, Expr]) -> None:
        self.cases = cases

    @property
    def mark(self) -> str:
        return "Contract_Cases"

    @property
    def definition(self) -> str:
        cases = indent_next(",\n".join(f"{p} =>\n{indent(str(q), 3)}" for p, q in self.cases), 1)
        return f"({cases})"


class DynamicPredicate(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Dynamic_Predicate"

    @property
    def definition(self) -> str:
        return str(self.expr)


class Size(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Size"

    @property
    def definition(self) -> str:
        return str(self.expr)


class DefaultInitialCondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Default_Initial_Condition"

    @property
    def definition(self) -> str:
        return str(self.expr)


class Ghost(Aspect):
    @property
    def mark(self) -> str:
        return "Ghost"

    @property
    def definition(self) -> str:
        return ""


class Import(Aspect):
    @property
    def mark(self) -> str:
        return "Import"

    @property
    def definition(self) -> str:
        return ""


class Annotate(Aspect):
    def __init__(self, *args: str) -> None:
        self.args = args

    @property
    def mark(self) -> str:
        return "Annotate"

    @property
    def definition(self) -> str:
        args = ", ".join(self.args)
        return f"({args})"


class FormalDeclaration(Ada):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class FormalSubprogramDeclaration(FormalDeclaration):
    def __init__(self, specification: "SubprogramSpecification") -> None:
        self.specification = specification

    def __str__(self) -> str:
        return f"with {self.specification};"


class FormalPackageDeclaration(FormalDeclaration):
    def __init__(self, name: str, generic_name: str) -> None:
        self.name = name
        self.generic_name = generic_name

    def __str__(self) -> str:
        return f"with package {self.name} is new {self.generic_name} (<>);"


class PackageDeclaration(Declaration):
    def __init__(
        self,
        name: str,
        declarations: List[Declaration] = None,
        private_declarations: List[Declaration] = None,
        formal_parameters: List[FormalDeclaration] = None,
    ) -> None:
        super().__init__(name)
        self.declarations = declarations or []
        self.private_declarations = private_declarations or []
        self.formal_parameters = formal_parameters

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"package {self.name} with\n  SPARK_Mode\nis\n\n"
            f"{declarative_items(self.declarations)}"
            f"{declarative_items(self.private_declarations, True)}"
            f"end {self.name};\n"
        )


class PackageBody(Declaration):
    def __init__(self, name: str, declarations: List[Declaration] = None) -> None:
        super().__init__(name)
        self.declarations = declarations or []

    def __str__(self) -> str:
        if not self.declarations:
            return ""

        return (
            f"package body {self.name} with\n  SPARK_Mode\nis\n\n"
            f"{declarative_items(self.declarations)}end {self.name};\n"
        )


class GenericPackageInstantiation(Declaration):
    def __init__(self, name: str, generic_package: str, associations: List[str] = None) -> None:
        super().__init__(name)
        self.generic_package = generic_package
        self.associations = associations or []

    def __str__(self) -> str:
        associations = ", ".join(self.associations)
        if associations:
            associations = f" ({associations})"
        return f"package {self.name} is new {self.generic_package}{associations};\n"


class ObjectDeclaration(Declaration):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifiers: Sequence[str],
        type_name: str,
        expression: Expr = None,
        constant: bool = False,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__("")
        self.identifiers = identifiers
        self.type_name = type_name
        self.expression = expression
        self.constant = constant
        self.aspects = aspects or []

    def __str__(self) -> str:
        identifiers = ", ".join(self.identifiers)
        constant = "constant " if self.constant else ""
        expression = f" := {self.expression}" if self.expression else ""
        return (
            f"{identifiers} : {constant}{self.type_name}{expression}"
            f"{aspect_specification(self.aspects)};"
        )


class Discriminant(Ada):
    def __init__(self, identifiers: Sequence[str], type_name: str, default: Expr = None) -> None:
        for name in identifiers:
            verify_identifier(name)
        verify_identifier(type_name)
        self.identifiers = identifiers
        self.type_name = type_name
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(self.identifiers)
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.type_name}{default}"


class TypeDeclaration(Declaration):
    def __init__(
        self,
        name: str,
        discriminants: Sequence[Discriminant] = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(name)
        self.discriminants = discriminants
        self.aspects = aspects or []

    def __str__(self) -> str:
        return (
            f"type {self.name}{self.discriminant_part} is{self.type_definition}"
            f"{aspect_specification(self.aspects)};{self.extra_declaration}"
        )

    @property
    def discriminant_part(self) -> str:
        return " (" + "; ".join(map(str, self.discriminants)) + ")" if self.discriminants else ""

    @abstractproperty
    def type_definition(self) -> str:
        raise NotImplementedError

    @property
    def extra_declaration(self) -> str:
        return ""


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: Expr) -> None:
        super().__init__(name)
        self.modulus = modulus

    @property
    def type_definition(self) -> str:
        return f" mod {self.modulus}"


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: Expr, last: Expr, size: Expr) -> None:
        super().__init__(name, aspects=[Size(size)])
        self.first = first
        self.last = last
        self.size = size

    @property
    def type_definition(self) -> str:
        return f" range {self.first} .. {self.last}"


class EnumerationType(TypeDeclaration):
    def __init__(
        self, name: str, literals: Mapping[str, Optional[Number]], size: Number = None
    ) -> None:
        super().__init__(name, aspects=([Size(size)] if size else []))
        self.literals = (
            OrderedDict(sorted(literals.items(), key=lambda t: t[1]))
            if None not in literals.values()
            else literals
        )
        self.size = size

    @property
    def type_definition(self) -> str:
        literal_specification = ", ".join(self.literals.keys())
        return f" ({literal_specification})"

    @property
    def extra_declaration(self) -> str:
        literal_representation = ", ".join(
            f"{k} => {v}" for k, v in self.literals.items() if v is not None
        )
        return (
            f"\nfor {self.name} use ({literal_representation});" if literal_representation else ""
        )


class Subtype(TypeDeclaration):
    def __init__(self, name: str, base_name: str) -> None:
        super().__init__(name)
        self.base_name = base_name

    def __str__(self) -> str:
        return "sub" + super().__str__()

    @property
    def type_definition(self) -> str:
        return f" {self.base_name}"


class RangeSubtype(Subtype):
    def __init__(self, name: str, base_name: str, first: Expr, last: Expr) -> None:
        super().__init__(name, base_name)
        self.first = first
        self.last = last

    @property
    def type_definition(self) -> str:
        return f" {self.base_name} range {self.first} .. {self.last}"


class DerivedType(TypeDeclaration):
    def __init__(self, name: str, type_name: str) -> None:
        super().__init__(name)
        self.type_name = type_name

    @property
    def type_definition(self) -> str:
        return f" new {self.type_name}"


class PrivateType(TypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " private"


class ArrayType(TypeDeclaration):
    def __init__(self, name: str, index_type: str, component_name: str) -> None:
        super().__init__(name)
        self.name = name
        self.index_type = index_type
        self.component_name = component_name

    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type}) of {self.component_name}"


class Component(Ada):
    def __init__(self, name: str, type_name: str, default: Expr = None) -> None:
        verify_identifier(name)
        verify_identifier(type_name)
        self.name = name
        self.type_name = type_name
        self.default = default

    def __str__(self) -> str:
        default = f" := {self.default}" if self.default else ""
        return f"{self.name} : {self.type_name}{default};"


class NullComponent(Component):
    def __init__(self) -> None:
        super().__init__("null", "null")

    def __str__(self) -> str:
        return f"null;"


class Variant(Ada):
    def __init__(self, discrete_choices: Sequence[Expr], components: Sequence[Component]) -> None:
        self.discrete_choices = discrete_choices
        self.components = components

    def __str__(self) -> str:
        choices = " | ".join(map(str, self.discrete_choices))
        components = indent("\n".join(map(str, self.components)), 6)
        return f"   when {choices} =>\n{components}"


class VariantPart(Ada):
    def __init__(self, discriminant_name: str, variants: Sequence[Variant]) -> None:
        verify_identifier(discriminant_name)
        self.discriminant_name = discriminant_name
        self.variants = variants

    def __str__(self) -> str:
        variants = "\n".join(map(str, self.variants))
        return f"case {self.discriminant_name} is\n{variants}\nend case;\n"


class RecordType(TypeDeclaration):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        name: str,
        components: List[Component],
        discriminants: Sequence[Discriminant] = None,
        variant_part: VariantPart = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(name, discriminants, aspects)
        self.components = components
        self.discriminants = discriminants or []
        self.variant_part = variant_part

    @property
    def type_definition(self) -> str:
        components = (
            (indent("\n".join(map(str, self.components)), 6) + "\n") if self.components else ""
        )
        variant_part = indent(str(self.variant_part), 6) if self.variant_part else ""
        return f"\n" f"   record\n" f"{components}" f"{variant_part}" f"   end record"


class Statement(Ada):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class NullStatement(Statement):
    def __init__(self) -> None:
        pass

    def __str__(self) -> str:
        return "null;"


class Assignment(Statement):
    def __init__(self, name: Union[str, Expr], expression: Expr) -> None:
        if isinstance(name, str):
            verify_identifier(name)
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.name} := {self.expression};"


class CallStatement(Statement):
    def __init__(self, name: str, arguments: Sequence[Expr]) -> None:
        verify_identifier(name)
        self.name = name
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ", ".join(map(str, self.arguments))
        return f"{self.name} ({arguments});"


class PragmaStatement(Statement):
    def __init__(self, name: str, parameters: Sequence[str]) -> None:
        verify_identifier(name)
        self.name = name
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(self.pragma_parameters)
            parameters = f" ({parameters})"
        return f"pragma {self.name}{parameters};"


class ReturnStatement(Statement):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if isinstance(self.expression, Case):
            return f"return ({self.expression});"
        return f"return {self.expression};"


class IfStatement(Statement):
    def __init__(
        self,
        condition_statements: Sequence[Tuple[Expr, Sequence[Statement]]],
        else_statements: Sequence[Statement] = None,
    ) -> None:
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def __str__(self) -> str:
        result = ""
        for condition, statements in self.condition_statements:
            if not result:
                result = f"if {condition} then\n"
            else:
                result += f"elsif {condition} then\n"
            for statement in statements:
                result += indent(str(statement), 3) + "\n"
        if self.else_statements:
            result += "else\n"
            for statement in self.else_statements:
                result += indent(str(statement), 3) + "\n"
        result += "end if;"
        return result


class CaseStatement(Statement):
    def __init__(
        self, control_expression: Expr, case_statements: Sequence[Tuple[Expr, Sequence[Statement]]]
    ) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements

    def __str__(self) -> str:
        grouped_cases = [
            (" | ".join(str(c) for c, _ in choices), statements)
            for statements, choices in itertools.groupby(self.case_statements, lambda x: x[1])
        ]
        cases = "".join(
            [
                "\nwhen {} =>\n{}".format(choice, indent("\n".join(str(s) for s in statements), 3))
                for choice, statements in grouped_cases
            ]
        )

        return f"case {self.control_expression} is{indent(cases, 3)}\nend case;"


class Parameter(Ada):
    def __init__(self, identifiers: Sequence[str], type_name: str, default: Expr = None) -> None:
        for name in identifiers:
            verify_identifier(name)
        verify_identifier(type_name)
        self.identifiers = identifiers
        self.type_name = type_name
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(self.identifiers)
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.mode}{self.type_name}{default}"

    @property
    def mode(self) -> str:
        return ""


class OutParameter(Parameter):
    @property
    def mode(self) -> str:
        return "out "


class InOutParameter(Parameter):
    @property
    def mode(self) -> str:
        return "in out "


class AccessParameter(Parameter):
    def __init__(
        self,
        identifiers: Sequence[str],
        type_name: str,
        default: Expr = None,
        constant: bool = False,
    ) -> None:
        super().__init__(identifiers, type_name, default)
        self.constant = constant

    @property
    def mode(self) -> str:
        return "access constant " if self.constant else "access "


class SubprogramSpecification(Ada):
    def __init__(self, name: str, parameters: Sequence[Parameter] = None) -> None:
        verify_identifier(name)
        self.name = name
        self.parameters = parameters or []

    def _parameters(self) -> str:
        return (" (" + "; ".join(map(str, self.parameters)) + ")") if self.parameters else ""

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ProcedureSpecification(SubprogramSpecification):
    def __str__(self) -> str:
        return f"procedure {self.name}{self._parameters()}"


class FunctionSpecification(SubprogramSpecification):
    def __init__(self, name: str, return_type: str, parameters: Sequence[Parameter] = None) -> None:
        super().__init__(name, parameters)
        self.return_type = return_type

    def __str__(self) -> str:
        return f"function {self.name}{self._parameters()} return {self.return_type}"


class Subprogram(Declaration):
    def __init__(self, specification: SubprogramSpecification) -> None:
        super().__init__(specification.name)
        self.specification = specification

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class SubprogramDeclaration(Subprogram):
    def __init__(
        self,
        specification: SubprogramSpecification,
        aspects: List[Aspect] = None,
        formal_parameters: Sequence[FormalDeclaration] = None,
    ) -> None:
        super().__init__(specification)
        self.aspects = aspects or []
        self.formal_parameters = formal_parameters

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"{self.specification}{aspect_specification(self.aspects)};"
        )


class SubprogramBody(Subprogram):
    def __init__(
        self,
        specification: SubprogramSpecification,
        declarations: Sequence[Declaration],
        statements: Sequence[Statement],
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(specification)
        self.declarations = declarations or []
        self.statements = statements or []
        self.aspects = aspects or []

    def _declarations(self) -> str:
        return "".join(indent(f"{declaration}\n", 3) for declaration in self.declarations)

    def _statements(self) -> str:
        return "\n".join(indent(str(s), 3) for s in self.statements)

    def __str__(self) -> str:
        aspects = f"{aspect_specification(self.aspects)}\n" if self.aspects else " "
        return (
            f"{self.specification}{aspects}is\n"
            f"{self._declarations()}"
            f"begin\n"
            f"{self._statements()}\n"
            f"end {self.specification.name};"
        )


class ExpressionFunctionDeclaration(Subprogram):
    def __init__(
        self, specification: FunctionSpecification, expression: Expr, aspects: List[Aspect] = None
    ) -> None:
        super().__init__(specification)
        self.expression = expression
        self.aspects = aspects or []

    def __str__(self) -> str:
        aspects = f"\n{aspect_specification(self.aspects)}" if self.aspects else ""
        return f"{self.specification} is\n" f"  ({self.expression!s}){aspects};"


class GenericProcedureInstantiation(Subprogram):
    def __init__(
        self, name: str, specification: ProcedureSpecification, associations: List[str] = None
    ) -> None:
        verify_identifier(name)
        super().__init__(specification)
        self.name = name
        self.parameters = specification.parameters
        self.associations = associations or []

    def __str__(self) -> str:
        associations = ", ".join(self.associations)
        if associations:
            associations = f" ({associations})"
        return f"procedure {self.name} is new {self.specification.name}{associations};"


class GenericFunctionInstantiation(Subprogram):
    def __init__(
        self, name: str, specification: FunctionSpecification, associations: List[str] = None
    ) -> None:
        verify_identifier(name)
        super().__init__(specification)
        self.name = name
        self.parameters = specification.parameters
        self.associations = associations or []

    def __str__(self) -> str:
        associations = ", ".join(self.associations)
        if associations:
            associations = f" ({associations})"
        return f"function {self.name} is new {self.specification.name}{associations};"


class SubprogramRenamingDeclaration(Subprogram):
    def __init__(self, specification: SubprogramSpecification, subprogram_name: str) -> None:
        verify_identifier(subprogram_name)
        super().__init__(specification)
        self.subprogram_name = subprogram_name

    def __str__(self) -> str:
        return f"{self.specification} renames {self.subprogram_name};"


class Pragma(Declaration, ContextItem):
    def __init__(self, name: str, parameters: List[str] = None) -> None:
        super().__init__(name)
        self.pragma_parameters = parameters or []

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return False
        return NotImplemented

    def __hash__(self) -> int:
        return hash(repr(self))

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(self.pragma_parameters)
            parameters = f" ({parameters})"
        return f"pragma {self.name}{parameters};"


class Unit(Ada):
    def __init__(self, context: List[ContextItem]) -> None:
        self.context = context

    def __str__(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def __iadd__(self, other: object) -> "Unit":
        raise NotImplementedError

    @abstractproperty
    def specification(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def body(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def name(self) -> str:
        raise NotImplementedError


class PackageUnit(Unit):
    def __init__(
        self, context: List[ContextItem], specification: PackageDeclaration, body: PackageBody
    ) -> None:
        assert specification.name == body.name, "name of specification and body differ"
        super().__init__(context)
        self.__specification = specification
        self.__body = body

    def __str__(self) -> str:
        raise NotImplementedError

    def __iadd__(self, other: object) -> "PackageUnit":
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.__specification.declarations = self.__specification.declarations + list(
                other.specification
            )
            self.__specification.private_declarations = (
                self.__specification.private_declarations + list(other.private)
            )
            self.__body.declarations = self.__body.declarations + list(other.body)
            return self
        return NotImplemented

    @property
    def specification(self) -> str:
        context_clause = ""
        if self.context:
            context_clause = "\n".join(map(str, unique(self.context)))
            context_clause = f"{context_clause}\n\n"
        return f"{context_clause}{self.__specification}"

    @property
    def body(self) -> str:
        return str(self.__body)

    @property
    def name(self) -> str:
        return self.__specification.name.lower().replace(".", "-")


class InstantiationUnit(Unit):
    def __init__(
        self, context: List[ContextItem], specification: GenericPackageInstantiation
    ) -> None:
        super().__init__(context)
        self.__specification = specification

    def __str__(self) -> str:
        raise NotImplementedError

    def __iadd__(self, other: object) -> Unit:
        return NotImplemented

    @property
    def specification(self) -> str:
        context_clause = ""
        if self.context:
            context_clause = "\n".join(map(str, unique(self.context)))
            context_clause = f"{context_clause}\n\n"
        return f"{context_clause}{self.__specification}"

    @property
    def body(self) -> str:
        return ""

    @property
    def name(self) -> str:
        return self.__specification.name.lower().replace(".", "-")


class UnitPart(NamedTuple):
    specification: Sequence[Declaration] = []
    body: Sequence[Declaration] = []
    private: Sequence[Declaration] = []


class SubprogramUnitPart(NamedTuple):
    specification: Sequence[Subprogram] = []
    body: Sequence[Subprogram] = []
    private: Sequence[Subprogram] = []


def generic_formal_part(parameters: Sequence[FormalDeclaration] = None) -> str:
    if parameters is None:
        return ""
    return "generic" + ("".join(f"\n   {p}" for p in parameters) if parameters else "") + "\n"


def declarative_items(declarations: List[Declaration], private: bool = False) -> str:
    result = "\n\n".join(indent(str(d), 3) for d in unique(declarations) if str(d))
    if result:
        result = f"private\n\n{result}" if private else result
        result += "\n\n"
    return result


def aspect_specification(aspects: Sequence[Aspect]) -> str:
    if not aspects:
        return ""
    return " with\n" + ",\n".join(indent(str(aspect), 2) for aspect in aspects)
