from langkit.parsers import Grammar, List, Opt, Or, Pick  # type: ignore

import rflx.language.ast as ast
from rflx.language.lexer import rflx_lexer as lexer

rflx_grammar = Grammar("main_rule")
grammar = rflx_grammar

rflx_grammar.add_rules(
    main_rule=Opt(grammar.package_declaration),
    unqualified_identifier=ast.ID(lexer.UnqualifiedIdentifier),
    based_literal=ast.BasedLiteral(Pick(lexer.Numeral, "#", lexer.Numeral, "#")),
    num_literal=Or(grammar.based_literal, ast.BasedLiteral(lexer.Numeral)),
    numeric_literal=ast.NumericLiteral(grammar.num_literal),
    mathematical_expression=Or(grammar.numeric_literal),
    size_aspect=ast.SizeAspect("Size", "=>", grammar.mathematical_expression),
    range_type_definition=ast.RangeTypeDef(
        "range",
        grammar.mathematical_expression,
        "..",
        grammar.mathematical_expression,
        "with",
        grammar.size_aspect,
    ),
    modular_type_definition=ast.ModularTypeDef("mod", grammar.mathematical_expression),
    integer_type_definition=Or(grammar.range_type_definition, grammar.modular_type_definition),
    type_declaration=ast.Type(
        "type", grammar.unqualified_identifier, "is", Or(grammar.integer_type_definition)
    ),
    basic_declaration=Or(grammar.type_declaration),
    package_declaration=ast.PackageDeclarationNode(
        "package",
        grammar.unqualified_identifier,
        "is",
        Opt(List(grammar.basic_declaration, sep=";"), ";"),
        "end",
        grammar.unqualified_identifier,
        ";",
    ),
)
