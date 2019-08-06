from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from typing import Dict, Iterator, List, Sequence, Set, Tuple, TypeVar, Union

from rflx.expression import Case, Expr, Number


class Ada(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self) -> int:
        return hash(repr(self))

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ContextItem(Ada):
    def __init__(self, names: List[str]) -> None:
        self.names = names

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class WithClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'with {names};'


class UsePackageClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'use {names};'


class UseTypeClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'use type {names};'


class Declaration(Ada):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class FormalDeclaration(Ada):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class FormalPackageDeclaration(FormalDeclaration):
    def __init__(self, name: str, generic_name: str) -> None:
        super().__init__(name)
        self.generic_name = generic_name

    def __str__(self) -> str:
        return f'with package {self.name} is new {self.generic_name} (<>);'


class PackageDeclaration(Declaration):
    def __init__(self, name: str, declarations: List[Declaration] = None,
                 private_declarations: List[Declaration] = None,
                 formal_parameters: List[FormalDeclaration] = None) -> None:
        super().__init__(name)
        self.declarations = declarations or []
        self.private_declarations = private_declarations or []
        self.formal_parameters = formal_parameters

    def __str__(self) -> str:
        aspect = '\n  with SPARK_Mode\n'

        return (f'{generic_formal_part(self.formal_parameters)}'
                f'package {self.name}{aspect}is\n\n'
                f'{declarative_items(self.declarations)}'
                f'{declarative_items(self.private_declarations, True)}'
                f'end {self.name};\n')


class PackageBody(Declaration):
    def __init__(self, name: str, declarations: List[Declaration] = None) -> None:
        super().__init__(name)
        self.declarations = declarations or []

    def __str__(self) -> str:
        if not self.declarations:
            return ''

        aspect = '\n  with SPARK_Mode\n'

        return (f'package body {self.name}{aspect}is\n\n'
                f'{declarative_items(self.declarations)}end {self.name};\n')


class GenericPackageInstantiation(Declaration):
    def __init__(self, name: str, generic_package: str, associations: List[str] = None) -> None:
        super().__init__(name)
        self.generic_package = generic_package
        self.associations = associations or []

    def __str__(self) -> str:
        associations = ', '.join(self.associations)
        if associations:
            associations = f' ({associations})'
        return f'package {self.name} is new {self.generic_package}{associations};\n'


class ObjectDeclaration(Declaration):
    def __init__(self, name: str, type_name: str, expression: Expr = None,
                 constant: bool = False) -> None:
        super().__init__(name)
        self.type_name = type_name
        self.expression = expression
        self.constant = constant

    def __str__(self) -> str:
        constant = 'constant ' if self.constant else ''
        expression = f' := {self.expression}' if self.expression else ''
        return f'{self.name} : {constant}{self.type_name}{expression};'


class TypeDeclaration(Declaration):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: Expr) -> None:
        super().__init__(name)
        self.modulus = modulus

    def __str__(self) -> str:
        return f'type {self.name} is mod {self.modulus};'


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: Expr, last: Expr, size: Expr) -> None:
        super().__init__(name)
        self.first = first
        self.last = last
        self.size = size

    def __str__(self) -> str:
        return (f'type {self.name} is range {self.first} .. {self.last}'
                f' with Size => {self.size};')


class EnumerationType(TypeDeclaration):
    def __init__(self, name: str, literals: Dict[str, Number], size: Number) -> None:
        super().__init__(name)
        self.literals = OrderedDict(sorted(literals.items(), key=lambda t: t[1]))
        self.size = size

    def __str__(self) -> str:
        literal_specification = ', '.join(self.literals.keys())
        literal_representation = ', '.join([f'{k} => {v}' for k, v in self.literals.items()])
        return (f'type {self.name} is ({literal_specification}) with Size => {self.size};\n'
                f'for {self.name} use ({literal_representation});')


class Subtype(TypeDeclaration):
    def __init__(self, name: str, base_name: str) -> None:
        super().__init__(name)
        self.base_name = base_name

    def __str__(self) -> str:
        return f'subtype {self.name} is {self.base_name};'


class RangeSubtype(Subtype):
    def __init__(self, name: str, base_name: str, first: Expr, last: Expr) -> None:
        super().__init__(name, base_name)
        self.first = first
        self.last = last

    def __str__(self) -> str:
        return f'subtype {self.name} is {self.base_name} range {self.first} .. {self.last};'


class DerivedType(TypeDeclaration):
    def __init__(self, name: str, type_name: str) -> None:
        super().__init__(name)
        self.type_name = type_name

    def __str__(self) -> str:
        return f'type {self.name} is new {self.type_name};'


class ComponentItem(Ada):
    def __init__(self, name: str, type_: str) -> None:
        self.name = name
        self.type = type_

    def __str__(self) -> str:
        return f'      {self.name} : {self.type};\n'


class VariantItem(Ada):
    def __init__(self, discrete_choice: str, components: List[ComponentItem]) -> None:
        self.discrete_choice = discrete_choice
        self.components = components

    def __str__(self) -> str:
        components = '\n'.join(f'      {component}' for component in self.components)
        return f'         when {self.discrete_choice} =>\n{components}'


class RecordType(TypeDeclaration):
    def __init__(self, name: str, components: List[ComponentItem]) -> None:
        super().__init__(name)
        self.components = components

    def __str__(self) -> str:
        components = ''.join(map(str, self.components))
        return (f'type {self.name} is\n'
                f'   record\n'
                f'{components}'
                f'   end record;')


class Discriminant(Ada):
    def __init__(self, name: str, type_: str, default: str = None) -> None:
        self.name = name
        self.type = type_
        self.default = default

    def __str__(self) -> str:
        default = ''
        if self.default:
            default = f' := {self.default}'
        return f'{self.name} : {self.type}{default}'


class VariantRecordType(TypeDeclaration):
    def __init__(self, name: str, discriminant: Discriminant,
                 variants: List[VariantItem]) -> None:
        super().__init__(name)
        self.discriminant = discriminant
        self.variants = variants

    def __str__(self) -> str:
        variants = ''.join(map(str, self.variants))
        return (f'type {self.name} ({self.discriminant}) is\n'
                f'   record\n'
                f'      case {self.discriminant.name} is\n'
                f'{variants}'
                f'      end case;\n'
                f'   end record;')


class Aspect(Ada):
    def __str__(self) -> str:
        if self.definition:
            return f'{self.mark} => {self.definition}'
        return f'{self.mark}'

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
        return 'Pre'

    @property
    def definition(self) -> str:
        return str(self.expr)


class Postcondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return 'Post'

    @property
    def definition(self) -> str:
        return str(self.expr)


class Ghost(Aspect):
    @property
    def mark(self) -> str:
        return 'Ghost'

    @property
    def definition(self) -> str:
        return ''


class Import(Aspect):
    @property
    def mark(self) -> str:
        return 'Import'

    @property
    def definition(self) -> str:
        return ''


class Statement(Ada):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class Assignment(Statement):
    def __init__(self, name: str, expression: Expr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'{self.name} := {self.expression};'


class CallStatement(Statement):
    def __init__(self, name: str, arguments: List[Expr]) -> None:
        self.name = name
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(map(str, self.arguments))
        return f'{self.name} ({arguments});'


class PragmaStatement(Statement):
    def __init__(self, name: str, parameters: List[str]) -> None:
        self.name = name
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'pragma {self.name}{parameters};'


class ReturnStatement(Statement):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if isinstance(self.expression, Case):
            return f'return ({self.expression});'
        return f'return {self.expression};'


class IfStatement(Statement):
    def __init__(self, condition_statements: List[Tuple[Expr, List[Statement]]],
                 else_statements: List[Statement] = None) -> None:
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def __str__(self) -> str:
        result = ''
        for condition, statements in self.condition_statements:
            if not result:
                result = f'if {condition} then\n'
            else:
                result += f'elsif {condition} then\n'
            for statement in statements:
                result += f'   {statement}\n'
        if self.else_statements:
            result += 'else\n'
            for statement in self.else_statements:
                result += f'{statement}\n'
        result += 'end if;'
        return result


class SubprogramSpecification(Ada):
    def __init__(self, name: str, parameters: List[Tuple[str, str]] = None) -> None:
        self.name = name
        self.parameters = parameters or []

    def _parameters(self) -> str:
        parameters = ''
        if self.parameters:
            parameters = '; '.join([f'{p_name} : {p_type}' for p_name, p_type in self.parameters])
            parameters = f' ({parameters})'
        return parameters

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ProcedureSpecification(SubprogramSpecification):
    def __str__(self) -> str:
        return f'procedure {self.name}{self._parameters()}'


class FunctionSpecification(SubprogramSpecification):
    def __init__(self, name: str, return_type: str,
                 parameters: List[Tuple[str, str]] = None) -> None:
        super().__init__(name, parameters)
        self.return_type = return_type

    def __str__(self) -> str:
        return f'function {self.name}{self._parameters()} return {self.return_type}'


class Subprogram(Declaration):
    def __init__(self, specification: SubprogramSpecification) -> None:
        super().__init__(specification.name)
        self.specification = specification

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class SubprogramDeclaration(Subprogram):
    def __init__(self, specification: SubprogramSpecification,
                 aspects: List[Aspect] = None) -> None:
        super().__init__(specification)
        self.aspects = aspects or []

    def __str__(self) -> str:
        return f'{self.specification}{with_clause(self.aspects)};'


class SubprogramBody(Subprogram):
    def __init__(self, specification: SubprogramSpecification, declarations: List[Declaration],
                 statements: List[Statement], aspects: List[Aspect] = None) -> None:
        super().__init__(specification)
        self.declarations = declarations or []
        self.statements = statements or []
        self.aspects = aspects or []

    def _declarations(self) -> str:
        return ''.join(indent(f'{declaration}\n', 3) for declaration in self.declarations)

    def _statements(self) -> str:
        return '\n'.join(indent(str(s), 3) for s in self.statements)

    def __str__(self) -> str:
        return (f'{self.specification}{with_clause(self.aspects)} is\n'
                f'{self._declarations()}'
                f'begin\n'
                f'{self._statements()}\n'
                f'end {self.specification.name};')


class ExpressionFunctionDeclaration(Subprogram):
    def __init__(self, specification: FunctionSpecification, expression: Expr,
                 aspects: List[Aspect] = None) -> None:
        super().__init__(specification)
        self.expression = expression
        self.aspects = aspects or []

    def __str__(self) -> str:
        return (f'{self.specification} is\n'
                f'   ({self.expression!s}){with_clause(self.aspects)};')


class GenericFunctionInstantiation(Subprogram):
    def __init__(self, name: str, specification: FunctionSpecification,
                 associations: List[str] = None) -> None:
        super().__init__(specification)
        self.name = name
        self.parameters = specification.parameters
        self.associations = associations or []

    def __str__(self) -> str:
        associations = ', '.join(self.associations)
        if associations:
            associations = f' ({associations})'
        return f'function {self.name} is new {self.specification.name}{associations};'


class SubprogramRenamingDeclaration(Subprogram):
    def __init__(self, specification: SubprogramSpecification, subprogram_name: str) -> None:
        super().__init__(specification)
        self.subprogram_name = subprogram_name

    def __str__(self) -> str:
        return f'{self.specification} renames {self.subprogram_name};'


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
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'pragma {self.name}{parameters};'


class Unit(Ada):
    def __init__(self, context: List[ContextItem]) -> None:
        self.context = context

    def __str__(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def __iadd__(self, other: object) -> 'Unit':
        raise NotImplementedError

    @abstractmethod
    def __imul__(self, other: object) -> 'Unit':
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
    def __init__(self, context: List[ContextItem], specification: PackageDeclaration,
                 body: PackageBody) -> None:
        assert specification.name == body.name, 'name of specification and body differ'
        super().__init__(context)
        self.__specification = specification
        self.__body = body

    def __str__(self) -> str:
        raise NotImplementedError

    def __iadd__(self, other: object) -> 'PackageUnit':
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.__specification.declarations = (self.__specification.declarations
                                                 + list(other.specification))
            self.__body.declarations = self.__body.declarations + list(other.body)
            return self
        return NotImplemented

    def __imul__(self, other: object) -> 'PackageUnit':
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.__specification.declarations = (list(other.specification)
                                                 + self.__specification.declarations)
            self.__body.declarations = list(other.body) + self.__body.declarations
            return self
        return NotImplemented

    @property
    def specification(self) -> str:
        context_clause = ''
        if self.context:
            context_clause = '\n'.join(map(str, unique(self.context)))
            context_clause = f'{context_clause}\n\n'
        return f'{context_clause}{self.__specification}'

    @property
    def body(self) -> str:
        return str(self.__body)

    @property
    def name(self) -> str:
        return self.__specification.name.lower().replace('.', '-')


class InstantiationUnit(Unit):
    def __init__(self, context: List[ContextItem],
                 specification: GenericPackageInstantiation) -> None:
        super().__init__(context)
        self.__specification = specification

    def __str__(self) -> str:
        raise NotImplementedError

    def __iadd__(self, other: object) -> Unit:
        return NotImplemented

    def __imul__(self, other: object) -> Unit:
        return NotImplemented

    @property
    def specification(self) -> str:
        context_clause = ''
        if self.context:
            context_clause = '\n'.join(map(str, unique(self.context)))
            context_clause = f'{context_clause}\n\n'
        return f'{context_clause}{self.__specification}'

    @property
    def body(self) -> str:
        return ''

    @property
    def name(self) -> str:
        return self.__specification.name.lower().replace('.', '-')


class UnitPart:
    def __init__(self, specification: Sequence[Declaration] = None,
                 body: Sequence[Declaration] = None) -> None:
        self.specification = specification or []
        self.body = body or []

    def __add__(self, other: object) -> 'UnitPart':
        if isinstance(other, Union[self.__class__, SubprogramUnitPart]):
            return UnitPart(self.specification + other.specification, self.body + other.body)
        return NotImplemented


class SubprogramUnitPart:
    def __init__(self, specification: List[Subprogram] = None,
                 body: List[Subprogram] = None) -> None:
        self.specification = specification or []
        self.body = body or []

    def __add__(self, other: object) -> 'SubprogramUnitPart':
        if isinstance(other, Union[self.__class__, UnitPart]):
            return SubprogramUnitPart(self.specification + other.specification,
                                      self.body + other.body)
        return NotImplemented


def indent(string: str, indentation: int) -> str:
    return '\n'.join((indentation * ' ' + l if l else '') for l in string.split('\n'))


def generic_formal_part(parameters: List[FormalDeclaration] = None) -> str:
    if parameters is None:
        return ''
    return 'generic' + (''.join(f'\n   {p}' for p in parameters) if parameters else '') + '\n'


def declarative_items(declarations: List[Declaration], private: bool = False) -> str:
    result = '\n\n'.join(indent(str(d), 3) for d in unique(declarations) if str(d))
    if result:
        result = '\nprivate\n{result}' if private else result
        result += '\n\n'
    return result


def with_clause(aspects: List[Aspect]) -> str:
    if not aspects:
        return ''
    result = '\n  with\n    '
    for i, aspect in enumerate(aspects):
        result += str(aspect)
        if i + 1 < len(aspects):
            result += ',\n    '
    return result


T = TypeVar('T')  # pylint: disable=invalid-name


def unique(iterable: List[T]) -> Iterator[T]:
    seen: Set[T] = set()
    for e in iterable:
        if e not in seen:
            seen.add(e)
            yield e
