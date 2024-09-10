from pathlib import Path

import pytest

import tools.rflxlexer
from rflx.rapidflux import RecordFluxError
from tests.const import FEATURE_DIR

# ruff: noqa: SLF001


def test_calculate_offset_valid() -> None:
    assert tools.rflxlexer._calculate_offset("a", 1, 1) == 0
    assert tools.rflxlexer._calculate_offset("abc", 1, 2) == 1
    assert tools.rflxlexer._calculate_offset("abc\ndef", 2, 2) == 5
    assert tools.rflxlexer._calculate_offset("abc\n\ndef", 3, 2) == 6


def test_calculate_offset_invalid() -> None:
    with pytest.raises(RecordFluxError, match="^<stdin>:2:1: error: line out of range$"):
        tools.rflxlexer._calculate_offset("", 2, 1)

    with pytest.raises(RecordFluxError, match="^<stdin>:2:4: error: column out of range$"):
        tools.rflxlexer._calculate_offset("1234232\n1", 2, 4)


@pytest.mark.parametrize(
    "file_path",
    [
        FEATURE_DIR / "shared" / "universal.rflx",
        FEATURE_DIR / "fsm_functions" / "test.rflx",
    ],
)
def test_tokenization(file_path: Path) -> None:
    lexer = tools.rflxlexer.RFLXLexer()
    spec = file_path.read_text()
    tokens = list(lexer.get_tokens_unprocessed(spec))
    assert "".join([s for _, _, s in tokens]) == spec


def test_fragment_tokenization() -> None:
    lexer = tools.rflxlexer.RFLXLexer()
    spec = "type T is range 5 .. 127 with Size => 8"
    tokens = list(lexer.get_tokens_unprocessed(spec))
    assert "".join([s for _, _, s in tokens]) == spec
