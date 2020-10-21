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
    # Identifiers
    UnqualifiedIdentifier = WithSymbol()

    # Keywords
    Package = WithText()
    Is = WithText()
    End = WithText()
    Semicolon = WithText()
    Type = WithText()
    Range = WithText()
    With = WithText()
    Mod = WithText()
    Size = WithText()

    DoubleDot = WithText()
    Hash = WithText()
    Minus = WithText()
    Arrow = WithText()

    # Comment
    Comment = WithTrivia()

    # Numeric
    Numeral = WithText()


rflx_lexer = Lexer(Token)
rflx_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"--.*"), Token.Comment),
    (Literal("package"), Token.Package),
    (Literal("is"), Token.Is),
    (Literal("end"), Token.End),
    (Literal("type"), Token.Type),
    (Literal("range"), Token.Range),
    (Literal("with"), Token.With),
    (Literal("mod"), Token.Mod),
    (Literal("Size"), Token.Size),
    (Literal(";"), Token.Semicolon),
    (Literal(".."), Token.DoubleDot),
    (Literal("#"), Token.Hash),
    (Literal("-"), Token.Minus),
    (Literal("=>"), Token.Arrow),
    (Pattern(r"[0-9A-F]+(_?[0-9A-F]+)*"), Token.Numeral),
    (Pattern(r"[a-zA-Z][a-zA-Z0-9_]*"), Token.UnqualifiedIdentifier),
)
