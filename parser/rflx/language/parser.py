from langkit.parsers import Grammar, List, NoBacktrack as cut, Opt, Or, Pick  # type: ignore

import rflx.language.ast as ast
from rflx.language.lexer import rflx_lexer as lexer

rflx_grammar = Grammar("main_rule")
grammar = rflx_grammar

rflx_grammar.add_rules(
    main_rule=Opt(grammar.package_declaration),
)

rflx_grammar.add_rules(
    unqualified_identifier=ast.UnqualifiedID(lexer.UnqualifiedIdentifier),
    qualified_identifier=ast.ID(List(grammar.unqualified_identifier, sep=".")),
    based_literal=ast.BasedLiteral(Pick(lexer.Numeral, "#", lexer.Numeral, "#")),
    num_literal=Or(grammar.based_literal, ast.BasedLiteral(lexer.Numeral)),
    numeric_literal=ast.NumericLiteral(grammar.num_literal),
    qualified_variable=ast.Variable(grammar.qualified_identifier),
    primary=Or(grammar.numeric_literal, grammar.qualified_variable, grammar.paren_expression),
    binop=Or(
        # pylint: disable=no-member
        ast.Op.alt_pow("**"),
        ast.Op.alt_mul("*"),
        ast.Op.alt_div("/"),
        ast.Op.alt_add("+"),
        ast.Op.alt_sub("-"),
        ast.Op.alt_eq("="),
        ast.Op.alt_neq("/="),
        ast.Op.alt_le("<="),
        ast.Op.alt_lt("<"),
        ast.Op.alt_ge(">="),
        ast.Op.alt_gt(">"),
        ast.Op.alt_and("and"),
        ast.Op.alt_or("or"),
    ),
    paren_expression=ast.ParenExpression("(", grammar.expression, ")"),
    expression=Or(
        ast.BinOp(
            grammar.expression,
            grammar.binop,
            cut(),
            grammar.primary,
        ),
        grammar.primary,
    ),
    mathematical_expression=ast.MathematicalExpression(grammar.expression),
    boolean_expression=ast.BooleanExpression(grammar.expression),
)

rflx_grammar.add_rules(
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
    first_aspect=ast.First("First", "=>", grammar.mathematical_expression),
    last_aspect=ast.Last("Last", "=>", grammar.mathematical_expression),
    if_condition=ast.If("if", grammar.boolean_expression),
    then=ast.Then(
        "then",
        Or(ast.NullID("null"), grammar.unqualified_identifier),
        Opt("with", List(grammar.first_aspect or grammar.last_aspect, sep=",")),
        Opt(grammar.if_condition),
    ),
    null_component_item=ast.NullComponent("null", grammar.then, ";"),
    component_item=ast.Component(
        grammar.unqualified_identifier,
        ":",
        grammar.qualified_identifier,
        List(grammar.then, empty_valid=True),
        ";",
    ),
    component_list=ast.Components(Opt(grammar.null_component_item), List(grammar.component_item)),
    value_range=ast.ValueRange(
        grammar.mathematical_expression, "..", grammar.mathematical_expression
    ),
    checksum_association=ast.ChecksumAssoc(
        grammar.unqualified_identifier,
        "=>",
        "(",
        List(Or(grammar.value_range, grammar.mathematical_expression)),
        ")",
    ),
    checksum_aspect=ast.ChecksumAspect(
        "Checksum", "=>", "(", List(grammar.checksum_association), ")"
    ),
    message_type_definition=Or(
        ast.Message(
            "message",
            grammar.component_list,
            "end",
            "message",
            Opt("with", grammar.checksum_aspect),
        ),
        ast.NullMessage("null", "message"),
    ),
    type_declaration=ast.Type(
        "type",
        grammar.unqualified_identifier,
        "is",
        Or(grammar.integer_type_definition, grammar.message_type_definition),
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
