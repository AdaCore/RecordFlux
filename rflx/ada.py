from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from typing import Callable, Dict, Iterator, List, Set, Tuple, TypeVar

from rflx.expression import Case, Expr, Number


class Ada(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')

    def __hash__(self) -> int:
        return hash(repr(self))


class MultiPartElement(Ada):
    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Unit(MultiPartElement):
    def __init__(self, context: List['ContextItem'], package: 'PackageDeclaration') -> None:
        self.context = context
        self.package = package

    def specification(self) -> str:
        context_clause = ''
        if self.context:
            context_clause = '\n'.join(map(str, unique(self.context)))
            context_clause = f'{context_clause}\n\n'
        return f'{context_clause}{self.package.specification()}\n'

    def definition(self) -> str:
        return f'{self.package.definition()}\n'


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


class PackageDeclaration(MultiPartElement):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Package(PackageDeclaration):
    def __init__(self, name: str, types: List['TypeDeclaration'] = None,
                 subprograms: List['Subprogram'] = None) -> None:
        super().__init__(name)
        self.types = types or []
        self.subprograms = subprograms or []

    def specification(self) -> str:
        return self._representation(lambda x: x.specification(), False)

    def definition(self) -> str:
        return self._representation(lambda x: x.definition(), True)

    def _representation(self, function: Callable, definition: bool) -> str:
        types = ''
        if not definition:
            types = '\n\n'.join(str(t) for t in unique(self.types) if str(t))
            if types:
                types += '\n\n'

        subprograms = '\n\n'.join([function(f) for f in unique(self.subprograms) if function(f)])
        if subprograms:
            subprograms += '\n\n'

        if definition and not types and not subprograms:
            return ''

        indicator = ' '
        aspect = '\n  with SPARK_Mode\n'
        if definition:
            indicator = ' body '

        return f'package{indicator}{self.name}{aspect}is\n\n{types}{subprograms}end {self.name};'


class GenericPackage(Package):
    def specification(self) -> str:
        return 'generic\n' + self._representation(lambda x: x.specification(), False)


class GenericPackageInstantiation(PackageDeclaration):
    def __init__(self, name: str, generic_package: str, associations: List[str] = None) -> None:
        super().__init__(name)
        self.generic_package = generic_package
        self.associations = associations or []

    def specification(self) -> str:
        associations = ', '.join(self.associations)
        if associations:
            associations = f' ({associations})'
        return f'package {self.name} is new {self.generic_package}{associations};'

    def definition(self) -> str:
        return ''


class TypeDeclaration(Ada):
    def __init__(self, name: str) -> None:
        self.name = name


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: Expr) -> None:
        super().__init__(name)
        self.modulus = modulus

    def __str__(self) -> str:
        return (f'   type {self.name} is mod {self.modulus};\n'
                f'   function Convert_To_{self.name} is new Types.Convert_To_Mod ({self.name});')


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: Expr, last: Expr, size: Expr) -> None:
        super().__init__(name)
        self.first = first
        self.last = last
        self.size = size

    def __str__(self) -> str:
        return (f'   type {self.name} is range {self.first} .. {self.last}'
                f' with Size => {self.size};\n'
                f'   function Convert_To_{self.name} is new Types.Convert_To_Int ({self.name});')


class EnumerationType(TypeDeclaration):
    def __init__(self, name: str, literals: Dict[str, Number], size: Number) -> None:
        super().__init__(name)
        self.literals = OrderedDict(sorted(literals.items(), key=lambda t: t[1]))
        self.size = size

    def __str__(self) -> str:
        literal_specification = ', '.join(self.literals.keys())
        literal_representation = ', '.join([f'{k} => {v}' for k, v in self.literals.items()])
        return (f'   type {self.name} is ({literal_specification}) with Size => {self.size};\n'
                f'   for {self.name} use ({literal_representation});')


class RangeSubtype(TypeDeclaration):
    def __init__(self, name: str, base_name: str, first: Expr, last: Expr) -> None:
        super().__init__(name)
        self.base_name = base_name
        self.first = first
        self.last = last

    def __str__(self) -> str:
        return f'   subtype {self.name} is {self.base_name} range {self.first} .. {self.last};'


class DerivedType(TypeDeclaration):
    def __init__(self, name: str, type_name: str) -> None:
        super().__init__(name)
        self.type_name = type_name

    def __str__(self) -> str:
        return f'   type {self.name} is new {self.type_name};'


class RecordType(TypeDeclaration):
    def __init__(self, name: str, components: List['ComponentItem']) -> None:
        super().__init__(name)
        self.components = components

    def __str__(self) -> str:
        components = ''.join(map(str, self.components))
        return (f'   type {self.name} is\n'
                f'      record\n'
                f'{components}'
                f'      end record;')


class VariantRecordType(TypeDeclaration):
    def __init__(self, name: str, discriminant: 'Discriminant',
                 variants: List['VariantItem']) -> None:
        super().__init__(name)
        self.discriminant = discriminant
        self.variants = variants

    def __str__(self) -> str:
        variants = ''.join(map(str, self.variants))
        return (f'   type {self.name} ({self.discriminant}) is\n'
                f'      record\n'
                f'         case {self.discriminant.name} is\n'
                f'{variants}'
                f'         end case;\n'
                f'      end record;')


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


class VariantItem(Ada):
    def __init__(self, discrete_choice: str, components: List['ComponentItem']) -> None:
        self.discrete_choice = discrete_choice
        self.components = components

    def __str__(self) -> str:
        components = '\n'.join(f'      {component}' for component in self.components)
        return f'            when {self.discrete_choice} =>\n{components}'


class ComponentItem(Ada):
    def __init__(self, name: str, type_: str) -> None:
        self.name = name
        self.type = type_

    def __str__(self) -> str:
        return f'         {self.name} : {self.type};\n'


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


class Subprogram(MultiPartElement):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, parameters: List[Tuple[str, str]] = None,
                 declarations: List['Declaration'] = None, body: List['Statement'] = None,
                 aspects: List[Aspect] = None) -> None:
        self.name = name
        self.parameters = parameters or []
        self.declarations = declarations or []
        self.body = body or []
        self.aspects = aspects or []

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError

    def _parameters(self) -> str:
        parameters = ''
        if self.parameters:
            parameters = '; '.join([f'{p_name} : {p_type}' for p_name, p_type in self.parameters])
            parameters = f' ({parameters})'
        return parameters

    def _declarations(self) -> str:
        return ''.join(f'      {declaration}\n' for declaration in self.declarations)

    def _body(self) -> str:
        return '\n'.join(map(str, self.body))

    def _with_clause(self) -> str:
        if not self.aspects:
            return ''
        with_clause = '\n     with\n       '
        for i, aspect in enumerate(self.aspects):
            with_clause += str(aspect)
            if i + 1 < len(self.aspects):
                with_clause += ',\n       '
        return with_clause


class Pragma(Subprogram):
    def __init__(self, name: str, parameters: List[str]) -> None:
        super().__init__(name)
        self.pragma_parameters = parameters

    def specification(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'   pragma {self.name}{parameters};'

    def definition(self) -> str:
        return ''


class Function(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, return_type: str, parameters: List[Tuple[str, str]] = None,
                 declarations: List['Declaration'] = None, body: List['Statement'] = None,
                 aspects: List[Aspect] = None) -> None:
        super().__init__(name, parameters, declarations, body, aspects)
        self.return_type = return_type

    def specification(self) -> str:
        return (f'   function {self.name}{self._parameters()} return {self.return_type}'
                f'{self._with_clause()};')

    def definition(self) -> str:
        return (f'   function {self.name}{self._parameters()} return {self.return_type} is\n'
                f'{self._declarations()}'
                f'   begin\n'
                f'{self._body()}\n'
                f'   end {self.name};')


class ExpressionFunction(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, return_type: str, parameters: List[Tuple[str, str]] = None,
                 expression: Expr = None, aspects: List[Aspect] = None) -> None:
        super().__init__(name, parameters, aspects=aspects)
        self.return_type = return_type
        self.expression = expression

    def specification(self) -> str:
        signature = f'   function {self.name}{self._parameters()} return {self.return_type}'
        if self.expression:
            return f'{signature} is\n      ({self.expression!s}){self._with_clause()};'
        return f'{signature}{self._with_clause()};'

    def definition(self) -> str:
        return ''


class Procedure(Subprogram):
    def specification(self) -> str:
        return f'   procedure {self.name}{self._parameters()}{self._with_clause()};'

    def definition(self) -> str:
        return (f'   procedure {self.name}{self._parameters()} is\n'
                f'{self._declarations()}'
                f'   begin\n'
                f'{self._body()}\n'
                f'   end {self.name};')


class Declaration(Ada):
    def __init__(self, name: str, type_: str, default: Expr = None) -> None:
        self.name = name
        self.type = type_
        self.default = default

    def __str__(self) -> str:
        default = ''
        if self.default:
            default = f' := {self.default}'
        return f'{self.name} : {self.type}{default};'


class Statement(Ada):
    pass


class Assignment(Statement):
    def __init__(self, name: str, expression: Expr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'      {self.name} := {self.expression};'


class CallStatement(Statement):
    def __init__(self, name: str, arguments: List[Expr]) -> None:
        self.name = name
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(map(str, self.arguments))
        return f'      {self.name} ({arguments});'


class PragmaStatement(Statement):
    def __init__(self, name: str, parameters: List[str]) -> None:
        self.name = name
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'      pragma {self.name}{parameters};'


class ReturnStatement(Statement):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if isinstance(self.expression, Case):
            return f'      return ({self.expression});'
        return f'      return {self.expression};'


class IfStatement(Statement):
    def __init__(self, condition_statements: List[Tuple[Expr, List[Statement]]],
                 else_statements: List[Statement] = None) -> None:
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def __str__(self) -> str:
        result = ''
        for condition, statements in self.condition_statements:
            if not result:
                result = f'      if {condition} then\n'
            else:
                result += f'      elsif {condition} then\n'
            for statement in statements:
                result += f'   {statement}\n'
        if self.else_statements:
            result += '      else\n'
            for statement in self.else_statements:
                result += f'   {statement}\n'
        result += '      end if;'
        return result


T = TypeVar('T')  # pylint: disable=invalid-name


def unique(iterable: List[T]) -> Iterator[T]:
    seen: Set[T] = set()
    for e in iterable:
        if e not in seen:
            seen.add(e)
            yield e
