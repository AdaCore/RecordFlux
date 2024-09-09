from pathlib import Path, PosixPath

import pytest

from rflx.identifier import ID
from rflx.ls.lexer import LSLexer, Token
from rflx.ls.model import LSModel, Symbol, SymbolCategory
from rflx.model import UncheckedModel
from rflx.rapidflux import Location, RecordFluxError
from rflx.specification.parser import Parser

DATA_DIR = Path("tests/unit/ls/data")


@pytest.fixture()
def model() -> LSModel:
    parser = Parser()
    parser.parse_string((DATA_DIR / "message.rflx").read_text())
    return LSModel(parser.create_unchecked_model())


def test_tokenization_with_empty_model() -> None:
    lexer = LSLexer(LSModel(UncheckedModel([], RecordFluxError())))
    lexer.tokenize((DATA_DIR / "message.rflx").read_text())
    lexer.tokenize((DATA_DIR / "state_machine.rflx").read_text())
    tokens = lexer.tokens

    assert len(tokens) == 274


def test_tokenization_with_model(model: LSModel) -> None:
    lexer = LSLexer(model)
    lexer.tokenize((DATA_DIR / "message.rflx").read_text())
    lexer.tokenize((DATA_DIR / "state_machine.rflx").read_text())

    tokens = lexer.tokens

    assert len(tokens) == 274


def test_search_token(model: LSModel) -> None:
    lexer = LSLexer(model)

    assert lexer.search_token(5, 1) is None

    lexer.tokenize((DATA_DIR / "message.rflx").read_text())

    assert lexer.search_token(0, 10) == Token(
        symbol=Symbol(
            ID("Message"),
            SymbolCategory.PACKAGE,
            Location((3, 9), PosixPath("<stdin>"), (3, 21)),
            None,
        ),
        lexeme="Message",
        line_number=0,
        character_offset=8,
    )
    assert lexer.search_token(21, 12) == Token(
        symbol=Symbol(
            identifier=ID("Message::Option"),
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
