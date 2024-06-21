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

        # 2.6 (2)
        string_literal:             /"(""|[^"])*"/

        # 2.8 (2)
        pragma:                     "pragma" identifier \
                                        ( "(" \
                                            pragma_argument_association \
                                            ( "," pragma_argument_association )* \
                                          ")" \
                                        )? \
                                        ";"

        # 2.8 (3/3)
        pragma_argument_association: \
                                     name
                                   | expression

        # 3.1 (4)
        defining_identifier:        identifier

        # 3.2.2 (4)
        subtype_mark:               name

        # 4.1 (2/3)
        name:                       direct_name

        # 4.1 (3)
        direct_name:                identifier

        # 4.4 (2)
        expression:                 relation

        # 4.4 (3/3)
        relation:                   simple_expression

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
               |                    string_literal

        true:                       "True"
        false:                      "False"

        # 6.1 (7)
        defining_program_unit_name: ( parent_unit_name "." )* defining_identifier

        # 7.1 (2)
        package_declaration:        package_specification ";"

        # 7.1 (3/3)
        package_specification:      "package" defining_program_unit_name \
                                    package_specification_aspect "is" \
                                    "end" defining_program_unit_name

        package_specification_aspect: aspect_specification?

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
        aspect_definition:          name | expression

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

    def string_literal(self, data: list[lark.lexer.Token]) -> ada.String:
        return ada.String(data[0][1:-1])

    def pragma(self, data: list[lark.lexer.Token]) -> ada.Pragma:
        return ada.Pragma(identifier=data[0], parameters=data[1:])

    def pragma_argument_association(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def defining_identifier(self, data: list[ID]) -> ID:
        return data[0]

    def name(self, data: list[ID]) -> ID:
        return data[0]

    def direct_name(self, data: list[ID]) -> ID:
        return data[0]

    def expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def relation(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def simple_expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def term(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def factor(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def primary(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def true(self, _) -> ada.Expr:
        return ada.TRUE

    def false(self, _) -> ada.Expr:
        return ada.FALSE

    def defining_program_unit_name(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def package_declaration(self, data: list[ada.PackageDeclaration]) -> ada.PackageDeclaration:
        return data[0]

    def package_specification(
        self,
        data: tuple[ID, list[ada.Aspect], ID],
    ) -> ada.PackageDeclaration:
        assert data[0] == data[2]
        return ada.PackageDeclaration(identifier=data[0], aspects=data[1])

    def package_specification_aspect(self, data: list[list[ada.Aspect]]) -> list[ada.Aspect]:
        if len(data) > 0:
            return data[0]
        return []

    def use_clause(
        self,
        data: list[Union[ada.UsePackageClause, ada.UseTypeClause]],
    ) -> Union[ada.UsePackageClause, ada.UseTypeClause]:
        return data[0]

    def use_package_clause(self, data: list[ID]) -> ada.UsePackageClause:
        assert len(data) == 1
        return ada.UsePackageClause(identifier=data[0])

    def use_type_clause(self, data: list[ID]) -> ada.UsePackageClause:
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

    def aspect_part(self, data: tuple[ID, Optional[ID]]) -> ada.Aspect:
        argument = data[1] if len(data) > 1 else None
        if data[0].parts == ["SPARK_Mode"]:
            return ada.SparkMode(off=argument and argument.name == "off")
        if data[0].parts == ["Always_Terminates"]:
            return ada.AlwaysTerminates(expression=argument)
        raise NotImplementedError(data[0].name)

    def aspect_mark(self, data: list[ID]) -> ID:
        return data[0]

    def aspect_definition(self, data: list[ID]) -> Union[ID, ada.Expr]:
        return data[0]


def parse(text: str) -> ada.Unit:
    return TreeToAda().transform(ADA_GRAMMAR.parse(f"{text}\0"))
