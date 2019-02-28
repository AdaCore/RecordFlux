import itertools
from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from typing import Callable, Dict, List, Tuple

from rflx.expression import Attribute, Expr, LogExpr, MathExpr, Number


class AdaRepresentation(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Unit(AdaRepresentation):
    def __init__(self, context: List['ContextItem'], package: 'Package') -> None:
        self.context = context
        self.package = package

    def specification(self) -> str:
        context_clause = ''
        if self.context:
            context_clause = '\n'.join([str(c) for c in self.context])
            context_clause = f'{context_clause}\n\n'
        return f'{context_clause}{self.package.specification()}\n'

    def definition(self) -> str:
        return f'{self.package.definition()}\n'


class ContextItem:
    def __init__(self, names: List[str]) -> None:
        self.names = names

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented


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


class Package(AdaRepresentation):
    def __init__(self, name: str, types: List['TypeDeclaration'],
                 subprograms: List['Subprogram']) -> None:
        self.name = name
        self.types = types
        self.subprograms = subprograms

    def specification(self) -> str:
        return self.__representation(lambda x: x.specification(), False)

    def definition(self) -> str:
        return self.__representation(lambda x: x.definition(), True)

    def __representation(self, function: Callable, definition: bool) -> str:
        types = ''
        if not definition:
            types = '\n\n'.join([str(t) for t in self.types if str(t)])
            if types:
                types += '\n\n'

        subprograms = '\n\n'.join([function(f) for f in self.subprograms if function(f)])
        if subprograms:
            subprograms += '\n\n'

        if definition and not types and not subprograms:
            return ''

        indicator = ' '
        aspect = '\n  with SPARK_Mode\n'
        if definition:
            indicator = ' body '

        return f'package{indicator}{self.name}{aspect}is\n\n{types}{subprograms}end {self.name};'


class TypeDeclaration(ABC):
    def __init__(self, name: str) -> None:
        self.name = name


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: MathExpr) -> None:
        super().__init__(name)
        self.modulus = modulus

    def __str__(self) -> str:
        return (f'   type {self.name} is mod {self.modulus};\n'
                f'   function Convert_To_{self.name} is new Types.Convert_To_Mod ({self.name});')


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: MathExpr, last: MathExpr, size: MathExpr) -> None:
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
    def __init__(self, name: str, base_name: str, first: MathExpr, last: MathExpr) -> None:
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


class VariantRecordType(TypeDeclaration):
    def __init__(self, name: str, discriminant: 'Discriminant',
                 variants: List['VariantItem']) -> None:
        super().__init__(name)
        self.discriminant = discriminant
        self.variants = variants

    def __str__(self) -> str:
        variants = ''.join(str(variant) for variant in self.variants)
        return (f'   type {self.name} ({self.discriminant}) is\n'
                f'      record\n'
                f'         case {self.discriminant.name} is\n'
                f'{variants}'
                f'         end case;\n'
                f'      end record;')


class Discriminant:
    def __init__(self, name: str, type_: str, default: str = None) -> None:
        self.name = name
        self.type = type_
        self.default = default

    def __str__(self) -> str:
        default = ''
        if self.default:
            default = f' := {self.default}'
        return f'{self.name} : {self.type}{default}'


class VariantItem:
    def __init__(self, discrete_choice: str, components: List['ComponentItem']) -> None:
        self.discrete_choice = discrete_choice
        self.components = components

    def __str__(self) -> str:
        components = '   '.join(str(component) for component in self.components)
        return f'            when {self.discrete_choice} =>\n{components}'


class ComponentItem:
    def __init__(self, name: str, type_: str) -> None:
        self.name = name
        self.type = type_

    def __str__(self) -> str:
        return f'               {self.name} : {self.type};\n'


class Aspect(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

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
    def __init__(self, expr: LogExpr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return 'Pre'

    @property
    def definition(self) -> str:
        return str(self.expr)


class Postcondition(Aspect):
    def __init__(self, expr: LogExpr) -> None:
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


class Subprogram(AdaRepresentation):
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
        return '\n'.join([str(s) for s in self.body])

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


class Declaration:
    def __init__(self, name: str, type_: str, default: Expr = None) -> None:
        self.name = name
        self.type = type_
        self.default = default

    def __str__(self) -> str:
        default = ''
        if self.default:
            default = f' := {self.default}'
        return f'{self.name} : {self.type}{default};'


class IfExpression(LogExpr):
    def __init__(self, condition_expressions: List[Tuple[LogExpr, Expr]],
                 else_expression: str = '') -> None:
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __str__(self) -> str:
        result = ''
        for c, e in self.condition_expressions:
            if not result:
                result = f'(if {c} then {e}'
            else:
                result += f' elsif {c} then {e}'
        if self.else_expression:
            result += f' else {self.else_expression}'
        result += ')'
        return result

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        raise NotImplementedError


class CaseExpression(Expr):
    def __init__(self, control_expression: Expr,
                 case_statements: List[Tuple[Expr, Expr]]) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements

    def __str__(self) -> str:
        grouped_cases = [(' | '.join([str(c) for c, _ in choices]), expr)
                         for expr, choices in itertools.groupby(self.case_statements,
                                                                lambda x: x[1])]
        cases = ', '.join([f'when {choice} => {expr}'
                           for choice, expr in grouped_cases])
        return f'case {self.control_expression} is {cases}'


class Aggregate(Expr):
    def __init__(self, *expressions: Expr) -> None:
        self.expressions = expressions

    def __str__(self) -> str:
        expressions = ', '.join(str(expression) for expression in self.expressions)
        return f'({expressions})'


class Statement(ABC):
    pass


class Assignment(Statement):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'      {self.name} := {self.expression};'


class CallStatement(Statement):
    def __init__(self, name: str, arguments: List[str]) -> None:
        self.name = name
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(self.arguments)
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
        if isinstance(self.expression, CaseExpression):
            return f'      return ({self.expression});'
        return f'      return {self.expression};'


class IfStatement(Statement):
    def __init__(self, condition_statements: List[Tuple[LogExpr, List[Statement]]],
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


class Call(ABC):
    def __init__(self, call: str) -> None:
        self.call = call

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}({self.call})'

    def __str__(self) -> str:
        return self.call


class MathCall(Call, MathExpr):
    def __init__(self, call: str, negative: bool = False) -> None:
        super().__init__(call)
        self.negative = negative

    def __repr__(self) -> str:
        result = f'{self.__class__.__name__}({self.call})'
        if self.negative:
            return f'(-{result})'
        return result

    def __neg__(self) -> MathExpr:
        return self.__class__(self.call, not self.negative)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def to_bytes(self) -> MathExpr:
        return self

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return self


class LogCall(Call, LogExpr):
    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        raise NotImplementedError


class Convert(MathExpr):
    # pylint: disable=too-many-arguments
    def __init__(self, type_name: str, array_name: str, first: MathExpr, last: MathExpr,
                 offset: int = 0, negative: bool = False) -> None:
        self.type_name = type_name
        self.array_name = array_name
        self.first = first
        self.last = last
        self.offset = offset
        self.negative = negative

    def __str__(self) -> str:
        negative = '-1 * ' if self.negative else ''
        return (f'{negative}'
                f'Convert_To_{self.type_name} ({self.array_name} ({self.first} .. {self.last}), '
                f'{self.offset}'
                ')')

    def __neg__(self) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first,
                       self.last,
                       self.offset,
                       not self.negative)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.simplified(facts),
                       self.last.simplified(facts),
                       self.offset,
                       self.negative)

    def to_bytes(self) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.to_bytes(),
                       self.last.to_bytes(),
                       self.offset,
                       self.negative)


class Cast(MathExpr):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'{self.name} ({self.expression})'

    def __neg__(self) -> MathExpr:
        return Cast(self.name, -self.expression)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return Cast(self.name, self.expression.simplified(facts))

    def to_bytes(self) -> MathExpr:
        return Cast(self.name, self.expression.to_bytes())


class FalseExpr(LogExpr):
    def __repr__(self) -> str:
        return 'FALSE'

    def __str__(self) -> str:
        return 'False'

    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        raise NotImplementedError


FALSE = FalseExpr()
