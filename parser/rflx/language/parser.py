from langkit.dsl import ASTNode, Field, abstract  # type: ignore
from langkit.parsers import Grammar, List, Opt, Or, Pick  # type: ignore

from rflx.language.lexer import rflx_lexer as lexer


@abstract
class RFLXNode(ASTNode):
    pass


class ID(RFLXNode):
    token_node = True


class PackageDeclarationNode(RFLXNode):
    name_start = Field()
    content = Field()
    name_end = Field()


@abstract
class IntegerTypeDef(RFLXNode):
    pass


class RangeTypeDef(IntegerTypeDef):
    lower = Field()
    upper = Field()
    size = Field()


class ModularTypeDef(IntegerTypeDef):
    mod = Field()


class Type(RFLXNode):
    identifier = Field()
    type_definition = Field(type=IntegerTypeDef)


class BasedLiteral(RFLXNode):
    # base = Field()
    # value = Field()
    pass


class NumericLiteral(RFLXNode):
    value = Field()


class SizeAspect(RFLXNode):
    size = Field()


rflx_grammar = Grammar("main_rule")
G = rflx_grammar

rflx_grammar.add_rules(
    main_rule=Opt(G.package_declaration),
    unqualified_identifier=ID(lexer.UnqualifiedIdentifier),
    based_literal=BasedLiteral(Pick(lexer.Numeral, "#", lexer.Numeral, "#")),
    num_literal=Or(G.based_literal, BasedLiteral(lexer.Numeral)),
    numeric_literal=NumericLiteral(G.num_literal),
    mathematical_expression=Or(G.numeric_literal),
    size_aspect=SizeAspect("Size", "=>", G.mathematical_expression),
    range_type_definition=RangeTypeDef(
        "range", G.mathematical_expression, "..", G.mathematical_expression, "with", G.size_aspect
    ),
    modular_type_definition=ModularTypeDef("mod", G.mathematical_expression),
    integer_type_definition=Or(G.range_type_definition, G.modular_type_definition),
    type_declaration=Type("type", G.unqualified_identifier, "is", Or(G.integer_type_definition)),
    basic_declaration=Or(G.type_declaration),
    package_declaration=PackageDeclarationNode(
        "package",
        G.unqualified_identifier,
        "is",
        Opt(List(G.basic_declaration, sep=";"), ";"),
        "end",
        G.unqualified_identifier,
        ";",
    ),
)
