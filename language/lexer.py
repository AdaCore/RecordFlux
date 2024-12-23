from langkit.lexer import (
    Alt,
    Case as Lexer_Case,
    Ignore,
    Lexer,
    LexerToken,
    Literal,
    Pattern,
    WithSymbol,
    WithText,
    WithTrivia,
)


class Token(LexerToken):  # type: ignore[misc]
    # Identifiers
    UnqualifiedIdentifier = WithSymbol()

    # Keywords
    Package = WithText()
    Is = WithText()
    If = WithText()
    End = WithText()
    Null = WithText()
    Type = WithText()
    Range = WithText()
    Unsigned = WithText()
    With = WithText()
    Mod = WithText()
    Message = WithText()
    Then = WithText()
    Sequence = WithText()
    Of = WithText()
    In = WithText()
    Not = WithText()
    New = WithText()
    For = WithText()
    When = WithText()
    Where = WithText()
    Use = WithText()
    All = WithText()
    Some = WithText()
    Generic = WithText()
    Session = WithText()
    Begin = WithText()
    Return = WithText()
    Function = WithText()
    State = WithText()
    Machine = WithText()
    Transition = WithText()
    Goto = WithText()
    Exception = WithText()
    Renames = WithText()
    Channel = WithText()
    Readable = WithText()
    Writable = WithText()
    Desc = WithText()
    Append = WithText()
    Extend = WithText()
    Read = WithText()
    Write = WithText()
    Reset = WithText()
    HighOrderFirst = WithText()
    LowOrderFirst = WithText()
    Case = WithText()

    # Aspect names
    First = WithText()
    Size = WithText()
    Last = WithText()
    ByteOrder = WithText()
    Checksum = WithText()
    ValidChecksum = WithText()
    HasData = WithText()
    Head = WithText()
    Opaque = WithText()
    Present = WithText()
    Valid = WithText()

    # Symbols
    Dot = WithText()
    Comma = WithText()
    DoubleDot = WithText()
    Tick = WithText()
    Hash = WithText()
    Minus = WithText()
    Arrow = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrack = WithText()
    RBrack = WithText()
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
    Ampersand = WithText()
    Semicolon = WithText()
    DoubleColon = WithText()
    Assignment = WithText()
    Colon = WithText()
    Pipe = WithText()

    # Comment
    Comment = WithTrivia()

    # Numeric
    Numeral = WithText()

    # String
    StringLiteral = WithText()


rflx_lexer = Lexer(Token)

rflx_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"--.*"), Token.Comment),
)

# Workaround to support keywords that equal attributes
# Inspired by Libadalang grammar (ada/language/lexer.py)
rflx_lexer.add_rules(
    *[
        Lexer_Case(
            Literal(text),
            Alt(
                prev_token_cond=(Token.Tick,),
                send=token,
                match_size=len(text),
            ),
            Alt(send=Token.UnqualifiedIdentifier, match_size=len(text)),
        )
        for text, token in [
            ("First", Token.First),
            ("Last", Token.Last),
            ("Size", Token.Size),
            ("Valid_Checksum", Token.ValidChecksum),
            ("Has_Data", Token.HasData),
            ("Head", Token.Head),
            ("Opaque", Token.Opaque),
            ("Present", Token.Present),
            ("Valid", Token.Valid),
        ]
    ],
)

rflx_lexer.add_rules(
    *[
        Lexer_Case(
            Literal(text),
            Alt(
                prev_token_cond=(Token.With,),
                send=token,
                match_size=len(text),
            ),
            Alt(send=Token.UnqualifiedIdentifier, match_size=len(text)),
        )
        for text, token in [
            ("Checksum", Token.Checksum),
        ]
    ],
)

rflx_lexer.add_rules(
    (Literal("package"), Token.Package),
    (Literal("is"), Token.Is),
    (Literal("if"), Token.If),
    (Literal("end"), Token.End),
    (Literal("null"), Token.Null),
    (Literal("type"), Token.Type),
    (Literal("range"), Token.Range),
    (Literal("unsigned"), Token.Unsigned),
    (Literal("with"), Token.With),
    (Literal("mod"), Token.Mod),
    (Literal("message"), Token.Message),
    (Literal("then"), Token.Then),
    (Literal("sequence"), Token.Sequence),
    (Literal("of"), Token.Of),
    (Literal("in"), Token.In),
    (Literal("not"), Token.Not),
    (Literal("new"), Token.New),
    (Literal("for"), Token.For),
    (Literal("when"), Token.When),
    (Literal("where"), Token.Where),
    (Literal("use"), Token.Use),
    (Literal("all"), Token.All),
    (Literal("some"), Token.Some),
    (Literal("generic"), Token.Generic),
    (Literal("session"), Token.Session),
    (Literal("begin"), Token.Begin),
    (Literal("return"), Token.Return),
    (Literal("function"), Token.Function),
    (Literal("state"), Token.State),
    (Literal("machine"), Token.Machine),
    (Literal("transition"), Token.Transition),
    (Literal("goto"), Token.Goto),
    (Literal("exception"), Token.Exception),
    (Literal("renames"), Token.Renames),
    (Literal("case"), Token.Case),
    (Literal("Channel"), Token.Channel),
    (Literal("Readable"), Token.Readable),
    (Literal("Writable"), Token.Writable),
    (Literal("Desc"), Token.Desc),
    (Literal("Append"), Token.Append),
    (Literal("Extend"), Token.Extend),
    (Literal("Read"), Token.Read),
    (Literal("Write"), Token.Write),
    (Literal("Reset"), Token.Reset),
    (Literal("Checksum"), Token.Checksum),
    (Literal("Byte_Order"), Token.ByteOrder),
    (Literal("High_Order_First"), Token.HighOrderFirst),
    (Literal("Low_Order_First"), Token.LowOrderFirst),
    (Literal("Valid_Checksum"), Token.ValidChecksum),
    (Literal("Has_Data"), Token.HasData),
    (Literal("Head"), Token.Head),
    (Literal("Opaque"), Token.Opaque),
    (Literal("Present"), Token.Present),
    (Literal("Valid"), Token.Valid),
    (Literal(";"), Token.Semicolon),
    (Literal("::"), Token.DoubleColon),
    (Literal(":="), Token.Assignment),
    (Literal(":"), Token.Colon),
    (Literal("("), Token.LPar),
    (Literal(")"), Token.RPar),
    (Literal("["), Token.LBrack),
    (Literal("]"), Token.RBrack),
    (Literal(".."), Token.DoubleDot),
    (Literal("."), Token.Dot),
    (Literal(","), Token.Comma),
    (Literal("'"), Token.Tick),
    (Literal("#"), Token.Hash),
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
    (Literal("|"), Token.Pipe),
    (Literal("and"), Token.And),
    (Literal("or"), Token.Or),
    (Literal("&"), Token.Ampersand),
    (Literal("=>"), Token.Arrow),
    (Pattern(r'"[^"]*"'), Token.StringLiteral),
    (Pattern(r"[0-9]+(_?[0-9]+)*"), Token.Numeral),
    (Pattern(r"2#[01]+(_?[01]+)*#"), Token.Numeral),
    (Pattern(r"8#[0-7]+(_?[0-7]+)*#"), Token.Numeral),
    (Pattern(r"10#[0-9]+(_?[0-9]+)*#"), Token.Numeral),
    (Pattern(r"16#[0-9A-F]+(_?[0-9A-F]+)*#"), Token.Numeral),
    (Pattern(r"[a-zA-Z](_?[a-zA-Z0-9]+)*"), Token.UnqualifiedIdentifier),
)
