from __future__ import annotations

from functools import reduce
from typing import Optional, Union

import lark.grammar

from rflx import ada
from rflx.identifier import ID

ADA_GRAMMAR = lark.Lark(
    r"""
        # 2.3 (2/2)
        identifier:                 /[a-zA-Z][a-zA-Z0-9_]*/

        # 2.4 (2)
        numeric_literal:            decimal_literal

        # 2.4.1 (2)
        decimal_literal:            numeral

        # 2.4.1 (3)
        numeral:                    /\d(_?\d)*/

        # 2.6 (2)
        string_literal:             /"(""|[^"])*"/

        # 2.8 (2)
        pragma:                     "pragma" identifier \
                                        pragma_arguments? \
                                        ";"

        pragma_arguments:           "(" \
                                        pragma_argument_association \
                                        ( "," pragma_argument_association )* \
                                    ")"

        # 2.8 (3/3)
        pragma_argument_association: \
                                    expression

        # 3.1 (3/3)
        basic_declaration: \
                                    type_declaration \
                                  | object_declaration \
                                  | subprogram_declaration \
                                  | unified_function_declaration \
                                  | pragma

        # 3.1 (4)
        defining_identifier:        identifier

        # 3.2.1 (2)
        type_declaration:           full_type_declaration

        # 3.2.1 (3/3)
        full_type_declaration: \
                                    "type" defining_identifier "is" type_definition \
                                        full_type_decl_aspects ";"

        full_type_decl_aspects:     aspect_specification?

        # 3.2.1 (4/2)
        type_definition: \
                                    integer_type_definition

        # 3.2.2 (3/2)
        subtype_indication:         subtype_mark

        # 3.2.2 (4)
        subtype_mark:               name

        # 3.3.1 (2/3)
        object_declaration: \
                                    defining_identifier_list ":" constant_opt subtype_indication \
                                        object_declaration_expr \
                                    ";"

        object_declaration_expr:    (":=" expression)?
        !constant_opt:              "constant"?

        # 3.3.1 (3)
        defining_identifier_list: \
                                    defining_identifier ("," defining_identifier)*

        # 3.5 (3)
        range: \
                                    simple_expression ".." simple_expression

        # 3.5.1 (2)
        integer_type_definition:    signed_integer_type_definition | modular_type_definition

        # 3.5.1 (3)
        signed_integer_type_definition: "range" expression ".." expression

        # 3.5.1 (4)
        modular_type_definition:    "mod" expression

        # 3.11 (2)
        declarative_part:           declarative_item*

        # 3.11 (3)
        declarative_item: \
                                    basic_declarative_item

        # 3.11 (4/1)
        basic_declarative_item: \
                                    basic_declaration

        # 4.1 (2/3)
        name:                       direct_name
                                  | attribute_reference

        # 4.1 (3)
        direct_name:                identifier

        # 4.1 (4)
        prefix:                     name

        # 4.1.3 (3)
        selector_name:              identifier

        # 4.1.4 (2)
        attribute_reference:        prefix "'" attribute_designator

        # 4.1.4 (3/2)
        attribute_designator: \
                                    identifier

        # 4.3 (2)
        aggregate:                  array_aggregate

        # 4.3.3 (2)
        array_aggregate: \
                                    positional_array_aggregate

        # 4.3.3 (3/2)
        positional_array_aggregate: \
                                    "(" expression "," expression ("," expression)* ")"

        # 4.4 (2)
        expression:                 relation (relation_operator relation)*

        !relation_operator:         "and" | "and then" | "or" | "or else"

        # 4.4 (3/3)
        relation: \
                                    simple_expression (relational_operator simple_expression)?
                                  | simple_expression in_operator membership_choice_list

        !in_operator:               "in"

        # 4.4 (3.1/3)
        membership_choice_list:     membership_choice

        # 4.4 (3.2/3)
        membership_choice:          range

        # 4.4 (4)
        simple_expression:          term (binary_adding_operator term)*

        # 4.4 (5)
        term:                       factor

        # 4.4 (6)
        factor:                     primary ("**" primary)?

        # 4.4 (7/3)
        primary: \
                                    true | false
                                  | numeric_literal | string_literal | aggregate
                                  | function_call
                                  | name | "(" expression ")"
                                  | conditional_expression

        true:                       "True"
        false:                      "False"

        # 4.5 (3)
        !relational_operator:       "=" | "/=" | "<" | "<=" | ">" | ">="

        # 4.5 (4)
        !binary_adding_operator:    "+" | "-" | "&"

        # 4.5.7 (2/3)
        conditional_expression:     if_expression

        # 4.5.7 (3/3)
        if_expression: \
                                    "if" condition "then" expression \
                                    if_expr_elsif \
                                    if_expr_else

        if_expr_elsif:              ("elsif" condition "then" expression)*
        if_expr_else:               ("else" expression)?

        # 4.5.7 (4/3)
        condition:                  expression

        # 5.1 (2/3)
        sequence_of_statements:     statement statement*

        # 5.1 (3)
        statement: \
                                    simple_statement

        # 5.1 (4/2)
        simple_statement:           pragma_statement
                                  | simple_return_statement

        # 6.1 (2/3)
        # Function specifications is special-cased in unified_function_declaration
        subprogram_declaration: \
                                    subprogram_specification \
                                    subprogram_declaration_aspects ";"

        subprogram_declaration_aspects: \
                                    aspect_specification?

        # 6.1 (4/2)
        subprogram_specification: \
                                    procedure_specification

        # 6.1 (4.1/2)
        procedure_specification:    "procedure" defining_program_unit_name parameter_profile

        # 6.1 (4.2/2)
        function_specification:     "function" defining_designator parameter_and_result_profile

        # 6.1 (5)
        designator:                 ( parent_unit_name "." )* identifier

        # 6.1 (6)
        defining_designator:        defining_program_unit_name

        # 6.1 (7)
        defining_program_unit_name: ( parent_unit_name "." )* defining_identifier

        # 6.1 (12)
        parameter_profile:          parameter_profile_formal_part

        parameter_profile_formal_part: formal_part?

        # 6.1 (13/2)
        parameter_and_result_profile: \
                                    parameter_and_result_profile_formal_part "return" subtype_mark

        parameter_and_result_profile_formal_part: formal_part?

        # 6.1 (14)
        formal_part: \
                                    "(" parameter_specification (";" parameter_specification)* ")"

        # 6.1 (15/3)
        parameter_specification: \
                                    defining_identifier_list ":" mode subtype_mark

        # 6.1 (16)
        !mode:                       "in"? | "in" "out" | "out"

        # 6.3 (2/3)
        # subprogram_body is special-cased in unified_function_declaration.

        # 6.4 (3)
        function_call: \
                                    prefix actual_parameter_part

        # 6.4 (4)
        actual_parameter_part: \
                                    "(" parameter_association ("," parameter_association)* ")"

        # 6.4 (5)
        parameter_association: \
                                    (selector_name "=>")? explicit_actual_parameter

        # 6.4 (6)
        explicit_actual_parameter:  expression

        # 6.5 (2/2)
        simple_return_statement:    "return" expression? ";"

        # 6.8 (2/3)
        # expression_function_declaration is special-cased in unified_function_declaration.

        # 7.1 (2)
        package_declaration:        package_specification ";"

        # 7.1 (3/3)
        package_specification:      "package" defining_program_unit_name \
                                    package_spec_aspects "is" \
                                    package_spec_declarations \
                                    "end" defining_program_unit_name

        package_spec_aspects:       aspect_specification?
        package_spec_declarations:  basic_declarative_item*

        # 8.4 (2)
        use_clause:                 use_package_clause | use_type_clause

        # 8.4 (3)
        use_package_clause:         "use" name ("," name)* ";"

        # 8.4 (4/3)
        use_type_clause:            "use" "all"? "type" subtype_mark ("," subtype_mark)* ";"

        # 10.1.1 (3)
        compilation_unit: \
                                    context_clause library_item

        # 10.1.1 (4)
        library_item:               "private"? library_unit_declaration
                                  | library_unit_body

        # 10.1.1 (5)
        library_unit_declaration:   \
                                    package_declaration

        # 10.1.1 (7)
        library_unit_body:          package_body

        # 10.1.1 (8)
        parent_unit_name:           name

        # 10.1.2 (2)
        context_clause:             context_item*

        # 10.1.2 (3)
        context_item:               with_clause | use_clause | pragma

        # 10.1.2 (4/2)
        with_clause:                limited_with_clause | nonlimited_with_clause

        # 10.1.2 (4.1/2)
        limited_with_clause:        "limited" "private"? "with" name ("," name)* ";"

        # 10.1.2 (4.2/2)
        nonlimited_with_clause:     "private"? "with" name ("," name)* ";"

        # 11.2 (2)
        handled_sequence_of_statements: \
                                    sequence_of_statements

        # 12 (2/3)
        package_body: \
                                    "package" "body" defining_program_unit_name \
                                    package_body_aspects "is" \
                                    declarative_part \
                                    "end" defining_program_unit_name ";"

        package_body_aspects:       aspect_specification?

        # 13.1.1 (2/3)
        aspect_specification:       "with"  aspect_part ( "," aspect_part )*

        aspect_part:                aspect_mark ( "=>" aspect_definition )?

        # 13.1.1 (3/3)
        aspect_mark:                identifier

        # 13.1.1 (4/3)
        aspect_definition:          expression

        # Custom rules
        pragma_statement:           "pragma" identifier \
                                        pragma_arguments? \
                                        ";"

        file:                       compilation_unit* /\0/

        unified_function_declaration: \
                                    function_specification \
                                        ( function_declaration_part \
                                        | function_body_part  \
                                        | expression_function_part ) \
                                    ";"

        optional_aspect_specification: aspect_specification?

        function_declaration_part: optional_aspect_specification

        function_body_part:         optional_aspect_specification \
                                    "is" \
                                        declarative_part \
                                    "begin" \
                                        handled_sequence_of_statements \
                                    "end" designator

        expression_function_part:   /is\s*\(/ expression ")" optional_aspect_specification

        # Skip whitespace
        %import common.WS
        %ignore WS

        # Skip comments
        %ignore /--.*/
    """,
    start="file",
    parser="lalr",
)


class FunctionPart:
    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        raise NotImplementedError


class FunctionDeclPart(FunctionPart):
    def __init__(self, aspects: Optional[list[ada.Aspect]]):
        self.aspects = aspects

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.SubprogramDeclaration(specification=specification, aspects=self.aspects)


class FunctionBodyPart(FunctionPart):
    def __init__(
        self,
        aspects: Optional[list[ada.Aspect]],
        declarations: list[ada.Declaration],
        statements: list[ada.Statement],
    ):
        self.aspects = aspects
        self.declarations = declarations
        self.statements = statements

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.SubprogramBody(
            specification=specification,
            declarations=self.declarations,
            statements=self.statements,
            aspects=self.aspects,
        )


class ExpressionFunctionPart(FunctionPart):
    def __init__(self, expression: ada.Expr, aspects: Optional[list[ada.Aspect]]):
        self.expression = expression
        self.aspects = aspects

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.ExpressionFunctionDeclaration(
            specification=specification,
            expression=self.expression,
            aspects=self.aspects,
        )


class TreeToAda(lark.Transformer[lark.lexer.Token, ada.PackageUnit]):

    def identifier(self, data: list[lark.lexer.Token]) -> ID:
        return ID(data[0])

    def numeric_literal(self, data: list[ada.Number]) -> ada.Number:
        return data[0]

    def decimal_literal(self, data: list[ada.Number]) -> ada.Number:
        return data[0]

    def numeral(self, data: list[lark.Token]) -> ada.Number:
        assert isinstance(data[0].value, str)
        return ada.Number(int(data[0].value))

    def string_literal(self, data: list[lark.lexer.Token]) -> ada.String:
        return ada.String(data[0][1:-1])

    def pragma(self, data: tuple[ID, list[ada.Expr]]) -> ada.Pragma:
        return ada.Pragma(identifier=data[0], parameters=data[1])

    def pragma_arguments(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def pragma_argument_association(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def basic_declaration(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def defining_identifier(self, data: list[ID]) -> ID:
        return data[0]

    def type_declaration(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def full_type_declaration(
        self,
        data: tuple[ID, Union[ada.ModularType, ada.RangeType], list[ada.Aspect]],
    ) -> ada.TypeDeclaration:
        identifier, definition, aspects = data
        if isinstance(definition, ada.ModularType):
            assert definition.identifier == ID("__INVALID__")
            return ada.ModularType(
                identifier=identifier,
                modulus=definition.modulus,
                aspects=aspects,
            )
        if isinstance(definition, ada.RangeType):
            assert definition.identifier == ID("__INVALID__")
            return ada.RangeType(
                identifier=identifier,
                first=definition.first,
                last=definition.last,
                aspects=aspects,
            )

    def full_type_decl_aspects(self, data: list[list[ada.Aspect]]) -> list[ada.Aspect]:
        if len(data) == 0:
            return []
        return data[0]

    def type_definition(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def subtype_indication(self, data: list[ID]) -> ID:
        return data[0]

    def subtype_mark(self, data: list[ID]) -> ID:
        return data[0]

    def object_declaration(
        self,
        data: tuple[list[ID], bool, ID, Optional[ada.Expr]],
    ) -> ada.ObjectDeclaration:
        return ada.ObjectDeclaration(
            identifiers=data[0],
            type_identifier=data[2],
            expression=data[3],
            constant=data[1],
        )

    def object_declaration_expr(self, data: list[ada.Expr]) -> Optional[ada.Expr]:
        if not data:
            return None
        return data[0]

    def constant_opt(self, data: list[lark.Token]) -> bool:
        return len(data) > 0 and data[0] == "constant"

    def defining_identifier_list(self, data: list[ID]) -> list[ID]:
        return data

    def range(self, data: tuple[ada.Number, ada.Number]) -> ada.ValueRange:
        return ada.ValueRange(lower=data[0], upper=data[1])

    def integer_type_definition(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def signed_integer_type_definition(
        self,
        data: tuple[ada.Number, ada.Number],
    ) -> ada.Declaration:
        return ada.RangeType(identifier="__INVALID__", first=data[0], last=data[1])

    def modular_type_definition(self, data: list[ada.Expr]) -> ada.Declaration:
        return ada.ModularType(identifier="__INVALID__", modulus=data[0])

    def declarative_part(self, data: list[ada.Declaration]) -> list[ada.Declaration]:
        return data

    def declarative_item(
        self,
        data: list[Union[ada.Declaration, ada.SubprogramBody]],
    ) -> Union[ada.Declaration, ada.SubprogramBody]:
        return data[0]

    def basic_declarative_item(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def name(self, data: list[Union[ID, ada.Attribute]]) -> Union[ID, ada.Attribute]:
        return data[0]

    def direct_name(self, data: list[ID]) -> ID:
        return data[0]

    def prefix(self, data: list[ID]) -> ID:
        return data[0]

    def attribute_reference(self, data: tuple[ID, str]) -> ada.Attribute:
        attributes: dict[str, type[ada.Attribute]] = {
            "Size": ada.Size,
            "Length": ada.Length,
            "First": ada.First,
            "Last": ada.Last,
            "LoopEntry": ada.LoopEntry,
            "Range": ada.Range,
            "Old": ada.Old,
            "Result": ada.Result,
            "Constrained": ada.Constrained,
            "Valid": ada.Valid,
            "Access": ada.Access,
            "Initialized": ada.Initialized,
            "Image": ada.Image,
            "Class": ada.Class,
            "UnrestrictedAccess": ada.UnrestrictedAccess,
        }
        return attributes[data[1]](data[0])

    def attribute_designator(self, data: list[ID]) -> str:
        return data[0].ada_str

    def aggregate(self, data: list[list[ada.Expr]]) -> ada.Aggregate:
        return ada.Aggregate(*data[0])

    def array_aggregate(self, data: list[list[ada.Expr]]) -> list[ada.Expr]:
        return data[0]

    def positional_array_aggregate(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def expression(self, data: list[Union[ada.Expr, str]]) -> ada.Expr:

        operators: dict[str, type[ada.BoolAssExpr]] = {
            "and": ada.And,
            "and then": ada.AndThen,
            "or": ada.Or,
            "or else": ada.OrElse,
        }

        assert isinstance(data[0], ada.Expr)

        if len(data) == 1:
            return data[0]

        assert len(data) > 2
        assert isinstance(data[1], str)

        remainder = self.expression(data[2:])
        operation = operators[data[1]]

        terms = remainder.terms if isinstance(remainder, operation) else [remainder]
        return operation(data[0], *terms)

    def relation_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def relation(self, data: list[Union[ada.Expr, str]]) -> ada.Expr:

        operators: dict[str, type[ada.Relation]] = {
            "=": ada.Equal,
            "/=": ada.NotEqual,
            "<": ada.Less,
            "<=": ada.LessEqual,
            ">": ada.Greater,
            ">=": ada.GreaterEqual,
            "in": ada.In,
        }

        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return data[0]

        left, operator, right = data
        assert isinstance(left, ada.Expr)
        assert isinstance(operator, str)
        assert isinstance(right, ada.Expr)
        return operators[operator](left, right)

    def in_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def membership_choice_list(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def membership_choice(self, data: list[ada.ValueRange]) -> ada.Expr:
        return data[0]

    def simple_expression(self, data: list[Union[ada.Expr, str]]) -> ada.Expr:

        assert isinstance(data[0], ada.Expr), data

        if len(data) == 1:
            return data[0]

        assert len(data) > 2
        assert isinstance(data[1], str)

        if data[1] == "-":
            return ada.Sub(data[0], self.simple_expression(data[2:]))

        remainder = self.simple_expression(data[2:])

        if data[1] == "+":
            terms = remainder.terms if isinstance(remainder, ada.Add) else [remainder]
            return ada.Add(data[0], *terms)

        if data[1] == "&":
            terms = remainder.terms if isinstance(remainder, ada.Concatenation) else [remainder]
            return ada.Concatenation(data[0], *terms)

        raise NotImplementedError(f"simple expression with operator {data[1]}")

    def term(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def factor(self, data: list[ada.Expr]) -> ada.Expr:
        if len(data) == 1:
            return data[0]
        return ada.Pow(data[0], data[1])

    def primary(self, data: list[Union[ada.Expr, ID]]) -> ada.Expr:
        # TODO(senier): How exactly do we distinguish ID and Variable?
        if isinstance(data[0], ID):
            return ada.Variable(data[0])
        return data[0]

    def true(self, _: list[lark.Token]) -> ada.Expr:
        return ada.TRUE

    def false(self, _: list[lark.Token]) -> ada.Expr:
        return ada.FALSE

    def relational_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def binary_adding_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def conditional_expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def if_expression(
        self,
        data: tuple[
            ada.Expr,
            ada.Expr,
            Optional[list[tuple[ada.Expr, ada.Expr]]],
            Optional[ada.Expr],
        ],
    ) -> ada.IfExpr:
        condition, expression, elif_expressions, else_expression = data
        condition_expressions = [(condition, expression)]
        if elif_expressions:
            condition_expressions.extend(elif_expressions)
        return ada.IfExpr(
            condition_expressions=condition_expressions,
            else_expression=else_expression,
        )

    def if_expr_elsif(
        self,
        data: list[tuple[ada.Expr, ada.Expr]],
    ) -> Optional[list[tuple[ada.Expr, ada.Expr]]]:
        if not data:
            return None
        return data

    def if_expr_else(self, data: list[ada.Expr]) -> Optional[ada.Expr]:
        if not data:
            return None
        return data[0]

    def condition(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def sequence_of_statements(self, data: list[ada.Statement]) -> list[ada.Statement]:
        return data

    def statement(self, data: list[ada.Statement]) -> ada.Statement:
        return data[0]

    def simple_statement(self, data: list[ada.Statement]) -> ada.Statement:
        return data[0]

    def subprogram_declaration(
        self,
        data: tuple[ada.SubprogramSpecification, list[ada.Aspect]],
    ) -> ada.SubprogramDeclaration:
        specification, aspects = data
        return ada.SubprogramDeclaration(specification=specification, aspects=aspects)

    def subprogram_declaration_aspects(
        self,
        data: list[list[ada.Aspect]],
    ) -> Optional[list[ada.Aspect]]:
        return data[0]

    def subprogram_specification(
        self,
        data: list[ada.ProcedureSpecification],
    ) -> ada.ProcedureSpecification:
        return data[0]

    def procedure_specification(
        self,
        data: tuple[ID, Optional[list[ada.Parameter]]],
    ) -> ada.ProcedureSpecification:
        identifier, parameters = data
        return ada.ProcedureSpecification(
            identifier=identifier,
            parameters=parameters,
        )

    def function_specification(
        self,
        data: tuple[ID, tuple[ID, Optional[list[ada.Parameter]]]],
    ) -> ada.FunctionSpecification:
        identifier, (return_type, parameters) = data
        return ada.FunctionSpecification(
            identifier=identifier,
            return_type=return_type,
            parameters=parameters,
        )

    def designator(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def defining_designator(self, data: list[ID]) -> ID:
        return data[0]

    def defining_program_unit_name(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def parameter_profile(
        self,
        data: list[Optional[list[ada.Parameter]]],
    ) -> Optional[list[ada.Parameter]]:
        return data[0]

    def parameter_profile_formal_part(
        self,
        data: list[list[ada.Parameter]],
    ) -> Optional[list[ada.Parameter]]:
        if not data:
            return None
        return data[0]

    def parameter_and_result_profile(
        self,
        data: tuple[Optional[list[ada.Parameter]], ID],
    ) -> tuple[ID, Optional[list[ada.Parameter]]]:
        return data[1], data[0]

    def parameter_and_result_profile_formal_part(
        self,
        data: list[list[ada.Parameter]],
    ) -> Optional[list[ada.Parameter]]:
        if not data:
            return None
        return data[0]

    def formal_part(self, data: list[ada.Parameter]) -> list[ada.Parameter]:
        return data

    def parameter_specification(self, data: tuple[list[ID], str, ID]) -> ada.Parameter:
        identifiers, mode, type_identifier = data

        if mode == "in out":
            return ada.InOutParameter(identifiers=identifiers, type_identifier=type_identifier)

        if mode == "out":
            return ada.OutParameter(identifiers=identifiers, type_identifier=type_identifier)

        if mode == "in":
            return ada.Parameter(identifiers=identifiers, type_identifier=type_identifier)

        assert False, f"Unexpected mode {mode}"

    def mode(self, data: list[lark.Token]) -> str:
        if not data:
            return "in"
        if "in" in data:
            if "out" in data:
                return "in out"
            return "in"
        assert data == ["out"]
        return "out"

    def function_call(
        self,
        data: tuple[ID, tuple[Optional[list[ada.Expr]], Optional[dict[ID, ada.Expr]]]],
    ) -> ada.Call:
        identifier, (arguments, named_arguments) = data
        return ada.Call(identifier=identifier, arguments=arguments, named_arguments=named_arguments)

    def actual_parameter_part(
        self,
        data: list[tuple[Optional[ID], ada.Expr]],
    ) -> tuple[Optional[list[ada.Expr]], Optional[list[tuple[ID, ada.Expr]]]]:
        arguments = [e for i, e in data if not i]
        named_arguments = [(i, e) for i, e in data if i]
        return arguments or None, named_arguments or None

    def parameter_association(
        self,
        data: list[tuple[Optional[ID], ada.Expr]],
    ) -> tuple[Optional[ID], ada.Expr]:
        return data[0]

    def explicit_actual_parameter(self, data: list[ada.Expr]) -> tuple[Optional[ID], ada.Expr]:
        return None, data[0]

    def simple_return_statement(self, data: list[ada.Expr]) -> ada.ReturnStatement:
        return ada.ReturnStatement(data[0] if data else None)

    def package_declaration(self, data: list[ada.PackageDeclaration]) -> ada.PackageDeclaration:
        return data[0]

    def package_specification(
        self,
        data: tuple[ID, list[ada.Aspect], list[ada.Declaration], ID],
    ) -> ada.PackageDeclaration:
        identifier, aspects, declarations, end_identifier = data
        assert identifier == end_identifier
        return ada.PackageDeclaration(
            identifier=identifier,
            declarations=declarations,
            aspects=aspects,
        )

    def package_spec_aspects(self, data: list[list[ada.Aspect]]) -> list[ada.Aspect]:
        if len(data) > 0:
            return data[0]
        return []

    def package_spec_declarations(self, data: list[ada.Declaration]) -> list[ada.Declaration]:
        return data

    def use_clause(
        self,
        data: list[Union[ada.UsePackageClause, ada.UseTypeClause]],
    ) -> Union[ada.UsePackageClause, ada.UseTypeClause]:
        return data[0]

    def use_package_clause(self, data: list[ID]) -> ada.UsePackageClause:
        assert len(data) == 1
        return ada.UsePackageClause(identifier=data[0])

    def use_type_clause(self, data: list[ID]) -> ada.UseTypeClause:
        assert len(data) == 1
        return ada.UseTypeClause(identifier=data[0])

    def compilation_unit(
        self,
        data: tuple[list[ada.ContextItem], Union[ada.PackageDeclaration, ada.PackageBody]],
    ) -> tuple[list[ada.ContextItem], Union[ada.PackageDeclaration, ada.PackageBody]]:
        return (data[0], data[1])

    def library_item(
        self,
        data: list[Union[ada.PackageDeclaration, ada.PackageBody]],
    ) -> Union[ada.PackageDeclaration, ada.PackageBody]:
        return data[0]

    def library_unit_declaration(
        self,
        data: list[ada.PackageDeclaration],
    ) -> ada.PackageDeclaration:
        return data[0]

    def library_unit_body(
        self,
        data: list[ada.PackageBody],
    ) -> ada.PackageBody:
        return data[0]

    def parent_unit_name(self, data: list[ID]) -> ID:
        return data[0]

    def context_clause(self, data: list[ada.ContextItem]) -> list[ada.ContextItem]:
        return data

    def context_item(self, data: list[ada.ContextItem]) -> ada.ContextItem:
        return data[0]

    def with_clause(self, data: list[ada.WithClause]) -> ada.WithClause:
        return data[0]

    def handled_sequence_of_statements(
        self,
        data: list[list[ada.Statement]],
    ) -> list[ada.Statement]:
        return data[0]

    def package_body(
        self,
        data: tuple[ID, Optional[list[ada.Aspect]], Optional[list[ada.Declaration]], ID],
    ) -> ada.PackageBody:
        identifier, aspects, declarations, end_identifier = data
        assert identifier == end_identifier

        return ada.PackageBody(
            identifier=identifier,
            declarations=declarations,
            statements=None,
            aspects=aspects,
        )

    def package_body_aspects(self, data: list[list[ada.Aspect]]) -> list[ada.Aspect]:
        if len(data) > 0:
            return data[0]
        return []

    def aspect_specification(self, data: list[ada.Aspect]) -> list[ada.Aspect]:
        return data

    def aspect_part(self, data: list[Union[ID, ada.Expr]]) -> ada.Aspect:
        assert isinstance(data[0], ID)
        name = data[0].parts[0]
        definition = data[1] if len(data) > 1 and isinstance(data[1], ada.Expr) else None

        if name == "Annotate":
            assert isinstance(definition, ada.Aggregate), definition
            return ada.Annotate(
                *[e.name for e in definition.elements if isinstance(e, ada.Variable)],
            )

        if name == "SPARK_Mode":
            if definition is None:
                return ada.SparkMode()
            assert isinstance(definition, ada.Variable)
            return ada.SparkMode(off=definition.name == "off")

        if name == "Always_Terminates":
            return ada.AlwaysTerminates(expression=definition)

        assert definition is not None

        if name == "Post":
            return ada.Postcondition(definition)

        if name == "Pre":
            return ada.Precondition(definition)

        raise NotImplementedError(data[0].name)

    def aspect_mark(self, data: list[ID]) -> ID:
        return data[0]

    def aspect_definition(self, data: list[Union[ID, ada.Expr]]) -> Union[ID, ada.Expr]:
        return data[0]

    def optional_aspect_specification(
        self,
        data: list[list[ada.Aspect]],
    ) -> Optional[list[ada.Aspect]]:
        if len(data) == 0:
            return None
        return data[0]

    def unified_function_declaration(
        self,
        data: tuple[ada.FunctionSpecification, FunctionPart],
    ) -> ada.Declaration:
        return data[1].declaration(data[0])

    def function_declaration_part(self, data: list[Optional[list[ada.Aspect]]]) -> FunctionDeclPart:
        return FunctionDeclPart(aspects=data[0])

    def function_body_part(
        self,
        data: tuple[Optional[list[ada.Aspect]], list[ada.Declaration], list[ada.Statement]],
    ) -> FunctionBodyPart:
        return FunctionBodyPart(declarations=data[1], statements=data[2], aspects=data[0])

    def expression_function_part(
        self,
        data: tuple[lark.Token, ada.Expr, Optional[list[ada.Aspect]]],
    ) -> ExpressionFunctionPart:
        return ExpressionFunctionPart(expression=data[1], aspects=data[2])

    def file(
        self,
        data: list[tuple[list[ada.ContextItem], Union[ada.PackageDeclaration, ada.PackageBody]]],
    ) -> ada.PackageUnit:
        data_len = len(data[:-1])
        assert data_len in [1, 2], data

        declaration_context, declaration = data[0]
        assert isinstance(declaration, ada.PackageDeclaration)

        if data_len == 1:
            body_context: list[ada.ContextItem] = []
            body = ada.PackageBody(identifier=declaration.identifier)
        elif data_len == 2:
            body_context, tmp = data[1]
            assert isinstance(tmp, ada.PackageBody)
            body = tmp

        return ada.PackageUnit(
            declaration_context=declaration_context,
            declaration=declaration,
            body_context=body_context,
            body=body,
        )


def parse(text: str) -> ada.PackageUnit:
    return TreeToAda().transform(ADA_GRAMMAR.parse(f"{text}\0"))
