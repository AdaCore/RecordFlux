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
    If = WithText()
    End = WithText()
    Null = WithText()
    Semicolon = WithText()
    Colon = WithText()
    Type = WithText()
    Range = WithText()
    With = WithText()
    Mod = WithText()
    Message = WithText()
    Then = WithText()

    # Aspect names
    First = WithText()
    Length = WithText()
    Last = WithText()
    Checksum = WithText()

    # Symbols
    Dot = WithText()
    Comma = WithText()
    DoubleDot = WithText()
    Hash = WithText()
    Minus = WithText()
    Arrow = WithText()
    LPar = WithText()
    RPar = WithText()
    Exp = WithText()
    Mul = WithText()
    Div = WithText()
    Add = WithText()
    Sub = WithText()
    Eq = WithText()
    Neq = WithText()
    Leq = WithText()
    Lt = WithText()
    Le = WithText()
    Gt = WithText()
    Ge = WithText()
    And = WithText()
    Or = WithText()

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
    (Literal("if"), Token.Is),
    (Literal("end"), Token.End),
    (Literal("null"), Token.Null),
    (Literal("type"), Token.Type),
    (Literal("range"), Token.Range),
    (Literal("with"), Token.With),
    (Literal("mod"), Token.Mod),
    (Literal("message"), Token.Message),
    (Literal("then"), Token.Then),
    (Literal("Checksum"), Token.Checksum),
    (Literal(";"), Token.Semicolon),
    (Literal(":"), Token.Colon),
    (Literal("("), Token.LPar),
    (Literal(")"), Token.RPar),
    (Literal(".."), Token.DoubleDot),
    (Literal("."), Token.Dot),
    (Literal(","), Token.Comma),
    (Literal("#"), Token.Hash),
    (Literal("-"), Token.Minus),
    (Literal("**"), Token.Exp),
    (Literal("*"), Token.Mul),
    (Literal("/="), Token.Neq),
    (Literal("/"), Token.Div),
    (Literal("+"), Token.Add),
    (Literal("-"), Token.Sub),
    (Literal("="), Token.Eq),
    (Literal("<="), Token.Le),
    (Literal("<"), Token.Lt),
    (Literal(">="), Token.Ge),
    (Literal(">"), Token.Gt),
    (Literal("and"), Token.And),
    (Literal("or"), Token.Or),
    (Literal("=>"), Token.Arrow),
    (Pattern(r"[0-9]+(_?[0-9]+)*"), Token.Numeral),
    (Pattern(r"[0-9]+#[0-9A-F]+(_?[0-9A-F]+)*#"), Token.Numeral),
    (Pattern(r"[a-zA-Z][a-zA-Z0-9_]*"), Token.UnqualifiedIdentifier),
)
