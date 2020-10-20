from langkit.dsl import ASTNode, Field, abstract  # type: ignore
from langkit.parsers import Grammar, Opt, Pick  # type: ignore

from rflx.language.lexer import rflx_lexer as lexer


@abstract
class RFLXNode(ASTNode):
    pass


class ID(RFLXNode):
    token_node = True


class PackageDeclarationNode(RFLXNode):
    name_start = Field()
    name_end = Field()


rflx_grammar = Grammar("main_rule")
G = rflx_grammar

rflx_grammar.add_rules(
    main_rule=Opt(G.package_declaration),
    unqualified_identifier=ID(lexer.UnqualifiedIdentifier),
    package_declaration=PackageDeclarationNode(
        "package", G.unqualified_identifier, "is", "end", G.unqualified_identifier, ";"
    ),
)
