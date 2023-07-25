from pathlib import Path, PosixPath

import pytest
import rflx.model
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.specification.parser import Parser
from rflx_ls.lexer import LSLexer, Token
from rflx_ls.model import LSModel, Symbol, SymbolCategory


@pytest.fixture
def model() -> LSModel:
    parser = Parser()

    with open("tests/data/universal.rflx") as file:
        parser.parse_string(file.read())

    return LSModel(parser.create_unchecked_model())


def test_tokenization_with_empty_model() -> None:
    lexer = LSLexer(LSModel(rflx.model.UncheckedModel([], RecordFluxError())))
    lexer.tokenize(Path("tests/data/universal.rflx").read_text())
    lexer.tokenize(Path("tests/data/messages.rflx").read_text())
    tokens = lexer.tokens

    assert len(tokens) == 167
    # assert tokens[...] == ...


def test_tokenization_with_model(model: LSModel) -> None:  # noqa: PLR0915
    lexer = LSLexer(model)
    lexer.tokenize(Path("tests/data/universal.rflx").read_text())
    lexer.tokenize(Path("tests/data/messages.rflx").read_text())

    tokens = lexer.tokens

    assert len(tokens) == 167
    # assert tokens[...] == ...


def test_search_token(model: LSModel) -> None:
    lexer = LSLexer(model)

    assert lexer.search_token(5, 1) is None

    lexer.tokenize(Path("tests/data/universal.rflx").read_text())

    assert lexer.search_token(0, 10) == Token(
        symbol=Symbol(
            ID("Universal"),
            SymbolCategory.PACKAGE,
            Location((3, 9), PosixPath("<stdin>"), (3, 21)),
            None,
        ),
        lexeme="Universal",
        line_number=0,
        character_offset=8,
    )
    lexer.search_token(21, 12) == Token(
        symbol=Symbol(
            identifier=ID("Universal::Option"),
            category=SymbolCategory.MESSAGE,
            definition_location=Location(
                (22, 9),
                PosixPath("<stdin>"),
                (33, 18),
            ),
            parent=None,
        ),
        lexeme="Option",
        line_number=21,
        character_offset=8,
    )
    assert lexer.search_token(1, 0) is None
    assert lexer.search_token(1, 10) is None
    assert lexer.search_token(3, 0) is None
    assert lexer.search_token(3, 200) is None
