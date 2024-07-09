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
                         |          expression_function_declaration

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

        # 3.2.2 (4)
        subtype_mark:               name

        # 3.3.1 (3)
        defining_identifier_list: \
                                    defining_identifier ("," defining_identifier)*

        # 3.5.1 (2)
        integer_type_definition:    signed_integer_type_definition | modular_type_definition

        # 3.5.1 (3)
        signed_integer_type_definition: "range" expression ".." expression

        # 3.5.1 (4)
        modular_type_definition:    "mod" expression

        # 3.11 (4/1)
        basic_declarative_item: \
                                    basic_declaration

        # 4.1 (2/3)
        name:                       direct_name

        # 4.1 (3)
        direct_name:                identifier

        # 4.3 (2)
        aggregate:                  array_aggregate

        # 4.3.3 (2)
        array_aggregate: \
                                    positional_array_aggregate

        # 4.3.3 (3/2)
        positional_array_aggregate: \
                                    "(" expression "," expression ("," expression)* ")"

        # 4.4 (2)
        expression:                 relation

        # 4.4 (3/3)
        relation: \
                                    simple_expression (relational_operator simple_expression)?

        # 4.4 (4)
        simple_expression:          term

        # 4.4 (5)
        term:                       factor

        # 4.4 (6)
        factor:                     primary

        # 4.4 (7/3)
        primary: \
                                    true
               |                    false
               |                    numeric_literal
               |                    string_literal
               |                    aggregate
               |                    name
               |                    "(" conditional_expression ")"

        true:                       "True"
        false:                      "False"

        # 4.5 (3)
        !relational_operator:       "=" | "/=" | "<" | "<=" | ">" | ">="

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

        # 6.1 (4.2/2)
        function_specification:     "function" defining_designator parameter_and_result_profile

        # 6.1 (6)
        defining_designator:        defining_program_unit_name

        # 6.1 (7)
        defining_program_unit_name: ( parent_unit_name "." )* defining_identifier

        # 6.1 (13/2)
        parameter_and_result_profile: \
                                    formal_part? "return" subtype_mark

        # 6.1 (14)
        formal_part: \
                                    "(" parameter_specification (";" parameter_specification)* ")"

        # 6.1 (15/3)
        parameter_specification: \
                                    defining_identifier_list ":" mode subtype_mark

        # 6.1 (16)
        mode:                       "in"? | "in" "out" | "out"

        # 6.8 (2/3)
        expression_function_declaration: \
                                    function_specification "is" \
                                    "(" expression ")" \
                                    expression_function_aspects ";"

        expression_function_aspects:    aspect_specification?

        # 7.1 (2)
        package_declaration:        package_specification ";"

        # 7.1 (3/3)
        package_specification:      "package" defining_program_unit_name \
                                    package_spec_aspects "is" \
                                    package_spec_declarations \
                                    "end" defining_program_unit_name

        package_spec_aspects:       aspect_specification?
        package_spec_declarations:  basic_declarative_item?

        # 8.4 (2)
        use_clause:                 use_package_clause | use_type_clause

        # 8.4 (3)
        use_package_clause:         "use" name ("," name)* ";"

        # 8.4 (4/3)
        use_type_clause:            "use" "all"? "type" subtype_mark ("," subtype_mark)* ";"

        # 10.1.1 (3)
        compilation_unit: \
                                    context_clause library_item /\0/

        # 10.1.1 (4)
        library_item:               "private"? library_unit_declaration

        # 10.1.1 (5)
        library_unit_declaration:   \
                                    package_declaration

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

        # 13.1.1 (2/3)
        aspect_specification:       "with"  aspect_part ( "," aspect_part )*

        aspect_part:                aspect_mark ( "=>" aspect_definition )?

        # 13.1.1 (3/3)
        aspect_mark:                identifier

        # 13.1.1 (4/3)
        aspect_definition:          expression

        # Skip whitespace
        %import common.WS
        %ignore WS
    """,
    start="compilation_unit",
    parser="lalr",
)


class TreeToAda(lark.Transformer[lark.lexer.Token, ada.Unit]):

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

    def subtype_mark(self, data: list[ID]) -> ID:
        return data[0]

    def defining_identifier_list(self, data: list[ID]) -> list[ID]:
        return data

    def integer_type_definition(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def signed_integer_type_definition(
        self,
        data: tuple[ada.Number, ada.Number],
    ) -> ada.Declaration:
        assert isinstance(data[0], ada.Number)
        assert isinstance(data[1], ada.Number)
        return ada.RangeType(identifier="__INVALID__", first=data[0], last=data[1])

    def modular_type_definition(self, data: list[ada.Expr]) -> ada.Declaration:
        assert isinstance(data[0], ada.Number)
        return ada.ModularType(identifier="__INVALID__", modulus=data[0])

    def basic_declarative_item(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def name(self, data: list[ID]) -> ID:
        return data[0]

    def direct_name(self, data: list[ID]) -> ID:
        return data[0]

    def aggregate(self, data: list[list[ada.Expr]]) -> ada.Aggregate:
        return ada.Aggregate(*data[0])

    def array_aggregate(self, data: tuple[list[ID]]) -> list[ID]:
        return data[0]

    def positional_array_aggregate(self, data: list[ID]) -> list[ID]:
        return data

    def expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def relation(self, data: list[Union[ada.Expr, str]]) -> ada.Expr:
        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return data[0]

        relations: dict[str, type[ada.Relation]] = {
            "=": ada.Equal,
            "/=": ada.NotEqual,
            "<": ada.Less,
            "<=": ada.LessEqual,
            ">": ada.Greater,
            ">=": ada.GreaterEqual,
        }
        left, operator, right = data
        assert isinstance(left, ada.Expr)
        assert isinstance(operator, str)
        assert isinstance(right, ada.Expr)
        return relations[operator](left, right)

    def simple_expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def term(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def factor(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def primary(self, data: list[ada.Expr]) -> ada.Expr:
        # TODO(senier): How exactly do we distinguish ID and Variable?
        if isinstance(data[0], ID):
            return ada.Variable(data[0])
        return data[0]

    def true(self, _: lark.Token) -> ada.Expr:
        return ada.TRUE

    def false(self, _: lark.Token) -> ada.Expr:
        return ada.FALSE

    def relational_operator(self, data: list[lark.Token]) -> Optional[str]:
        if not data:
            return None
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

    def function_specification(
        self,
        data: tuple[ID, tuple[ID, list[ada.Parameter]]],
    ) -> ada.FunctionSpecification:
        identifier, (return_type, parameters) = data
        return ada.FunctionSpecification(
            identifier=identifier,
            return_type=return_type,
            parameters=parameters,
        )

    def defining_designator(self, data: list[ID]) -> ID:
        return data[0]

    def defining_program_unit_name(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def parameter_and_result_profile(
        self,
        data: tuple[Optional[list[ada.Parameter]], ID],
    ) -> tuple[ID, list[ada.Parameter]]:
        return data[1], data[0] or []

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
        assert False, f"mode: {data}"

    def expression_function_declaration(
        self,
        data: tuple[ada.FunctionSpecification, ada.Expr, Optional[list[ada.Aspect]]],
    ) -> ada.ExpressionFunctionDeclaration:
        return ada.ExpressionFunctionDeclaration(
            specification=data[0],
            expression=data[1],
            aspects=data[2],
        )

    def expression_function_aspects(self, data: list[list[ada.Aspect]]) -> list[ada.Aspect]:
        return data[0]

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
        data: tuple[list[ada.ContextItem], ada.PackageDeclaration],
    ) -> ada.Unit:
        package_declaration = data[1]
        return ada.PackageUnit(
            declaration_context=data[0],
            declaration=package_declaration,
            body_context=[],
            body=ada.PackageBody(identifier=package_declaration.identifier),
        )

    def library_item(self, data: list[ada.PackageDeclaration]) -> ada.PackageDeclaration:
        return data[0]

    def library_unit_declaration(
        self,
        data: list[ada.PackageDeclaration],
    ) -> ada.PackageDeclaration:
        return data[0]

    def parent_unit_name(self, data: list[ID]) -> ID:
        return data[0]

    def context_clause(self, data: list[ada.ContextItem]) -> list[ada.ContextItem]:
        return data

    def context_item(self, data: list[ada.ContextItem]) -> ada.ContextItem:
        return data[0]

    def with_clause(self, data: list[ada.WithClause]) -> ada.WithClause:
        return data[0]

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

        raise NotImplementedError(data[0].name)

    def aspect_mark(self, data: list[ID]) -> ID:
        return data[0]

    def aspect_definition(self, data: list[ID]) -> Union[ID, ada.Expr]:
        return data[0]


def parse(text: str) -> ada.Unit:
    return TreeToAda().transform(ADA_GRAMMAR.parse(f"{text}\0"))
