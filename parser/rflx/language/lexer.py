from langkit.lexer import Lexer, LexerToken, Literal, WithText  # type: ignore


class Token(LexerToken):
    Example = WithText()


rflx_lexer = Lexer(Token)
rflx_lexer.add_rules(
    (Literal("example"), Token.Example),
)
