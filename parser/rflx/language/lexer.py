from langkit.lexer import (  # type: ignore
    Ignore,
    Lexer,
    LexerToken,
    Literal,
    Pattern,
    WithSymbol,
    WithText,
    WithTrivia,
)


class Token(LexerToken):
    Package = WithText()
    Is = WithText()
    End = WithText()
    Semicolon = WithText()

    # Identifiers
    UnqualifiedIdentifier = WithSymbol()

    # Comment
    Comment = WithTrivia()


rflx_lexer = Lexer(Token)
rflx_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"--.*"), Token.Comment),
    (Literal("package"), Token.Package),
    (Literal("is"), Token.Is),
    (Literal("end"), Token.End),
    (Literal(";"), Token.Semicolon),
    (Pattern("[a-zA-Z]\w*"), Token.UnqualifiedIdentifier),
)
