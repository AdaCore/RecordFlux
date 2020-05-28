# pylint: disable=too-many-lines
import itertools
from abc import ABC, abstractmethod
from collections import OrderedDict
from typing import List, Mapping, NamedTuple, Optional, Sequence, Tuple, Union

from rflx.common import file_name, generic_repr, indent, indent_next, unique
from rflx.contract import invariant
from rflx.expression import Case, Expr, Number, Variable
from rflx.identifier import ID, StrID


class Ada(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)


class Declaration(Ada):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ContextItem(Ada):
    def __init__(self, *names: StrID) -> None:
        self.names = list(map(ID, names))

    def __hash__(self) -> int:
        return hash(tuple(self.names))

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class WithClause(ContextItem):
    def __str__(self) -> str:
        names = ", ".join(map(str, self.names))
        return f"with {names};"


class UsePackageClause(ContextItem, Declaration):
    def __str__(self) -> str:
        names = ", ".join(map(str, self.names))
        return f"use {names};"


class UseTypeClause(ContextItem, Declaration):
    def __str__(self) -> str:
        names = ", ".join(map(str, self.names))
        return f"use type {names};"


class Aspect(Ada):
    def __str__(self) -> str:
        if self.definition:
            return f"{self.mark} =>\n{indent(self.definition, 2)}"
        return f"{self.mark}"

    @property
    @abstractmethod
    def mark(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
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


class Depends(Aspect):
    def __init__(self, dependencies: Mapping[StrID, Sequence[StrID]]) -> None:
        self.dependencies = dependencies

    @property
    def mark(self) -> str:
        return "Depends"

    @property
    def definition(self) -> str:
        def input_values(values: Sequence[StrID]) -> str:
            if len(values) == 0:
                return "null"
            if len(values) == 1:
                return str(values[0])
            return "(" + ", ".join(str(p) for p in values) + ")"

        dependencies = indent_next(
            ", ".join(f"{o} => {input_values(i)}" for o, i in self.dependencies.items()), 1
        )
        return f"({dependencies})"


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


class SparkMode(Aspect):
    @property
    def mark(self) -> str:
        return "SPARK_Mode"

    @property
    def definition(self) -> str:
        return ""


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

    def __hash__(self) -> int:
        return hash(self.specification)

    def __str__(self) -> str:
        return f"with {self.specification};"


class FormalPackageDeclaration(FormalDeclaration):
    def __init__(
        self, name: StrID, generic_name: StrID, associations: Sequence[StrID] = None
    ) -> None:
        self.name = ID(name)
        self.generic_name = ID(generic_name)
        self.associations = list(map(str, associations or []))

    def __hash__(self) -> int:
        return hash(self.name)

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations)) if self.associations else "<>"
        return f"with package {self.name} is new {self.generic_name} ({associations});"


class FormalTypeDeclaration(FormalDeclaration):
    def __init__(self, type_declaration: "TypeDeclaration") -> None:
        self.type_declaration = type_declaration

    def __hash__(self) -> int:
        return hash(self.type_declaration)

    def __str__(self) -> str:
        return str(self.type_declaration)


class PackageDeclaration(Declaration):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        declarations: List[Declaration] = None,
        private_declarations: List[Declaration] = None,
        formal_parameters: List[FormalDeclaration] = None,
        aspects: List[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.private_declarations = private_declarations or []
        self.formal_parameters = formal_parameters
        self.aspects = aspects or []

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"package {self.identifier}{aspect_specification(self.aspects)}\nis\n\n"
            f"{declarative_items(self.declarations)}"
            f"{declarative_items(self.private_declarations, True)}"
            f"end {self.identifier};\n"
        )


class PackageBody(Declaration):
    def __init__(
        self,
        identifier: StrID,
        declarations: List[Declaration] = None,
        aspects: List[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.aspects = aspects or []

    def __str__(self) -> str:
        if not self.declarations:
            return ""

        return (
            f"package body {self.identifier}{aspect_specification(self.aspects)}\nis\n\n"
            f"{declarative_items(self.declarations)}end {self.identifier};\n"
        )


class GenericPackageInstantiation(Declaration):
    def __init__(
        self, identifier: StrID, generic_package: StrID, associations: Sequence[StrID] = None
    ) -> None:
        self.identifier = ID(identifier)
        self.generic_package = ID(generic_package)
        self.associations = list(map(ID, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"package {self.identifier} is new {self.generic_package}{associations};\n"


class ObjectDeclaration(Declaration):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifiers: Sequence[StrID],
        type_name: StrID,
        expression: Expr = None,
        constant: bool = False,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_name = ID(type_name)
        self.expression = expression
        self.constant = constant
        self.aspects = aspects or []

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        constant = "constant " if self.constant else ""
        expression = f" := {self.expression}" if self.expression else ""
        return (
            f"{identifiers} : {constant}{self.type_name}{expression}"
            f"{aspect_specification(self.aspects)};"
        )


class Discriminant(Ada):
    def __init__(
        self, identifiers: Sequence[StrID], type_name: StrID, default: Expr = None
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_name = ID(type_name)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.type_name}{default}"


class TypeDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        discriminants: Sequence[Discriminant] = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.discriminants = discriminants
        self.aspects = aspects or []

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        return (
            f"type {self.identifier}{self.discriminant_part} is{self.type_definition}"
            f"{aspect_specification(self.aspects)};{self.extra_declaration}"
        )

    @property
    def discriminant_part(self) -> str:
        return " (" + "; ".join(map(str, self.discriminants)) + ")" if self.discriminants else ""

    @property
    @abstractmethod
    def type_definition(self) -> str:
        raise NotImplementedError

    @property
    def extra_declaration(self) -> str:
        return ""


class ModularType(TypeDeclaration):
    def __init__(self, identifier: StrID, modulus: Expr) -> None:
        super().__init__(identifier)
        self.modulus = modulus

    @property
    def type_definition(self) -> str:
        return f" mod {self.modulus}"


class RangeType(TypeDeclaration):
    def __init__(self, identifier: StrID, first: Expr, last: Expr, size: Expr) -> None:
        super().__init__(identifier, aspects=[Size(size)])
        self.first = first
        self.last = last
        self.size = size

    @property
    def type_definition(self) -> str:
        return f" range {self.first} .. {self.last}"


class EnumerationType(TypeDeclaration):
    def __init__(
        self, identifier: StrID, literals: Mapping[str, Optional[Number]], size: Expr = None,
    ) -> None:
        super().__init__(identifier, aspects=([Size(size)] if size else []))
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
            f"\nfor {self.identifier} use ({literal_representation});"
            if literal_representation
            else ""
        )


class Subtype(TypeDeclaration):
    def __init__(self, identifier: StrID, base_name: StrID) -> None:
        super().__init__(identifier)
        self.base_name = ID(base_name)

    def __str__(self) -> str:
        return "sub" + super().__str__()

    @property
    def type_definition(self) -> str:
        return f" {self.base_name}"


class RangeSubtype(Subtype):
    def __init__(self, identifier: StrID, base_name: StrID, first: Expr, last: Expr) -> None:
        super().__init__(identifier, base_name)
        self.first = first
        self.last = last

    @property
    def type_definition(self) -> str:
        return f" {self.base_name} range {self.first} .. {self.last}"


class DerivedType(TypeDeclaration):
    def __init__(self, identifier: StrID, type_name: StrID) -> None:
        super().__init__(identifier)
        self.type_name = ID(type_name)

    @property
    def type_definition(self) -> str:
        return f" new {self.type_name}"


class PrivateType(TypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " private"


class DiscreteType(TypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " (<>)"


class ArrayType(TypeDeclaration):
    def __init__(self, identifier: StrID, index_type: StrID, component_name: StrID) -> None:
        super().__init__(identifier)
        self.index_type = ID(index_type)
        self.component_name = ID(component_name)

    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type}) of {self.component_name}"


class UnconstrainedArrayType(ArrayType):
    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type} range <>) of {self.component_name}"


class AccessType(TypeDeclaration):
    def __init__(self, identifier: StrID, object_name: StrID) -> None:
        super().__init__(identifier)
        self.object_name = ID(object_name)

    @property
    def type_definition(self) -> str:
        return f" access {self.object_name}"


class Component(Ada):
    def __init__(self, name: StrID, type_name: StrID, default: Expr = None) -> None:
        self.name = ID(name)
        self.type_name = ID(type_name)
        self.default = default

    def __str__(self) -> str:
        default = f" := {self.default}" if self.default else ""
        return f"{self.name} : {self.type_name}{default};"


class NullComponent(Component):
    def __init__(self) -> None:
        super().__init__("null", "null")

    def __str__(self) -> str:
        return "null;"


class Variant(Ada):
    def __init__(self, discrete_choices: Sequence[Expr], components: Sequence[Component]) -> None:
        self.discrete_choices = discrete_choices
        self.components = components

    def __str__(self) -> str:
        choices = " | ".join(map(str, self.discrete_choices))
        components = indent("\n".join(map(str, self.components)), 6)
        return f"   when {choices} =>\n{components}"


class VariantPart(Ada):
    def __init__(self, discriminant_name: StrID, variants: Sequence[Variant]) -> None:
        self.discriminant_name = ID(discriminant_name)
        self.variants = variants

    def __str__(self) -> str:
        variants = "\n".join(map(str, self.variants))
        return f"case {self.discriminant_name} is\n{variants}\nend case;\n"


class RecordType(TypeDeclaration):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        components: List[Component],
        discriminants: Sequence[Discriminant] = None,
        variant_part: VariantPart = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(identifier, discriminants, aspects)
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
    def __init__(self, name: Union[StrID, Expr], expression: Expr) -> None:
        self.name = name if isinstance(name, Expr) else Variable(name)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.name} := {self.expression};"


class CallStatement(Statement):
    def __init__(self, identifier: StrID, arguments: Sequence[Expr]) -> None:
        self.identifier = ID(identifier)
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ", ".join(map(str, self.arguments))
        return f"{self.identifier} ({arguments});"


class PragmaStatement(Statement):
    def __init__(self, identifier: StrID, parameters: Sequence[str]) -> None:
        self.identifier = ID(identifier)
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(self.pragma_parameters)
            parameters = " (" + indent_next(str(parameters), len(str(self.identifier)) + 9) + ")"
        return f"pragma {self.identifier}{parameters};"


class ReturnStatement(Statement):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if isinstance(self.expression, Case):
            return "return (" + indent_next(str(self.expression), 8) + ");"
        return "return " + indent_next(str(self.expression), 7) + ";"


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
            c = (
                f" {condition} "
                if str(condition).count("\n") == 0
                else f"\n{indent(str(condition), 2)}\n"
            )
            if not result:
                result = f"if{c}then\n"
            else:
                result += f"elsif{c}then\n"
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
    def __init__(
        self, identifiers: Sequence[StrID], type_name: StrID, default: Expr = None
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_name = ID(type_name)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
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
        identifiers: Sequence[StrID],
        type_name: StrID,
        default: Expr = None,
        constant: bool = False,
    ) -> None:
        super().__init__(identifiers, type_name, default)
        self.constant = constant

    @property
    def mode(self) -> str:
        return "access constant " if self.constant else "access "


class SubprogramSpecification(Ada):
    def __init__(self, identifier: StrID, parameters: Sequence[Parameter] = None) -> None:
        self.identifier = ID(identifier)
        self.parameters = parameters or []

    def _parameters(self) -> str:
        return (" (" + "; ".join(map(str, self.parameters)) + ")") if self.parameters else ""

    def __hash__(self) -> int:
        return hash(self.identifier)

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ProcedureSpecification(SubprogramSpecification):
    def __str__(self) -> str:
        return f"procedure {self.identifier}{self._parameters()}"


class FunctionSpecification(SubprogramSpecification):
    def __init__(
        self, identifier: StrID, return_type: StrID, parameters: Sequence[Parameter] = None
    ) -> None:
        super().__init__(identifier, parameters)
        self.return_type = ID(return_type)

    def __str__(self) -> str:
        return f"function {self.identifier}{self._parameters()} return {self.return_type}"


class Subprogram(Declaration):
    def __init__(self, specification: SubprogramSpecification) -> None:
        self.specification = specification

    def __hash__(self) -> int:
        return hash(self.specification)

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
            f"end {self.specification.identifier};"
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
        return f"{self.specification} is\n  ({indent_next(str(self.expression), 3)}){aspects};"


class GenericProcedureInstantiation(Subprogram):
    def __init__(
        self, name: StrID, specification: ProcedureSpecification, associations: List[StrID] = None,
    ) -> None:
        super().__init__(specification)
        self.name = ID(name)
        self.parameters = specification.parameters
        self.associations = list(map(ID, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"procedure {self.name} is new {self.specification.identifier}{associations};"


class GenericFunctionInstantiation(Subprogram):
    def __init__(
        self, name: StrID, specification: FunctionSpecification, associations: List[StrID] = None,
    ) -> None:
        super().__init__(specification)
        self.name = ID(name)
        self.parameters = specification.parameters
        self.associations = list(map(ID, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"function {self.name} is new {self.specification.identifier}{associations};"


class SubprogramRenamingDeclaration(Subprogram):
    def __init__(self, specification: SubprogramSpecification, subprogram_name: StrID) -> None:
        super().__init__(specification)
        self.subprogram_name = ID(subprogram_name)

    def __str__(self) -> str:
        return f"{self.specification} renames {self.subprogram_name};"


class Pragma(Declaration, ContextItem):
    def __init__(self, identifier: StrID, parameters: List[str] = None) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.pragma_parameters = parameters or []

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return False
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(self.pragma_parameters)
            parameters = f" ({parameters})"
        return f"pragma {self.identifier}{parameters};"


class Unit(Ada):
    @abstractmethod
    def __iadd__(self, other: object) -> "Unit":
        raise NotImplementedError

    @property
    @abstractmethod
    def ads(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def adb(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError


@invariant(lambda self: self.declaration.identifier == self.body.identifier)
class PackageUnit(Unit):
    def __init__(
        self,
        declaration_context: List[ContextItem],
        declaration: PackageDeclaration,
        body_context: List[ContextItem],
        body: PackageBody,
    ) -> None:
        self.declaration_context = declaration_context
        self.declaration = declaration
        self.body_context = body_context
        self.body = body

    def __iadd__(self, other: object) -> "PackageUnit":
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.declaration.declarations = self.declaration.declarations + list(
                other.specification
            )
            self.declaration.private_declarations = self.declaration.private_declarations + list(
                other.private
            )
            self.body.declarations = self.body.declarations + list(other.body)
            return self
        return NotImplemented

    @property
    def ads(self) -> str:
        return f"{context_clause(self.declaration_context)}{self.declaration}"

    @property
    def adb(self) -> str:
        return f"{context_clause(self.body_context)}{self.body}" if str(self.body) else ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))


class InstantiationUnit(Unit):
    def __init__(
        self, context: List[ContextItem], declaration: GenericPackageInstantiation
    ) -> None:
        self.context = context
        self.declaration = declaration

    def __iadd__(self, other: object) -> Unit:
        return NotImplemented

    @property
    def ads(self) -> str:
        return f"{context_clause(self.context)}{self.declaration}"

    @property
    def adb(self) -> str:
        return ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))


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
    return (
        "generic" + ("".join(f"\n   {p}" for p in unique(parameters)) if parameters else "") + "\n"
    )


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


def context_clause(context: Sequence[ContextItem]) -> str:
    return ("\n".join(map(str, unique(context))) + "\n\n") if context else ""
