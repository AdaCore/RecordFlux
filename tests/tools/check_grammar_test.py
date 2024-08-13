from __future__ import annotations

import sys
from pathlib import Path

import pytest

from rflx.rapidflux import RecordFluxError
from tests.const import DATA_DIR as BASE_DATA_DIR
from tools.check_grammar import check_spec, main

DATA_DIR = BASE_DATA_DIR / "lrm_grammar"


def check_rst(filename: Path, invalid: bool = False, examples: list[Path] | None = None) -> None:
    errors = RecordFluxError()
    check_spec(
        filename=filename,
        examples=examples or [],
        invalid=invalid,
        verbal_map={},
        max_prod_line_len=65,
        errors=errors,
    )
    errors.propagate()


def test_missing_grammar() -> None:
    with pytest.raises(
        RecordFluxError,
        match=rf"^{DATA_DIR}/without_grammar.rst:1:1: error: No grammar found$",
    ):
        check_rst(DATA_DIR / "without_grammar.rst")


def test_multiple_start_rules() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf"{DATA_DIR}/unused_grammar_rule.rst:1:1: error: "
            r"Multiple start rules: root, unused$"
        ),
    ):
        check_rst(DATA_DIR / "unused_grammar_rule.rst")


def test_missing_mapping() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf"{DATA_DIR}/complex_grammar.rst:4:13: error: "
            r"No mapping for verbal: 'some characters'$"
        ),
    ):
        check_rst(filename=DATA_DIR / "complex_grammar.rst")


def test_specification_not_rejected() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(rf"{DATA_DIR}/simple_grammar.txt:1:1: error: Specification should be rejected$"),
    ):
        check_rst(
            filename=DATA_DIR / "simple_grammar.rst",
            examples=[DATA_DIR / "simple_grammar.txt"],
            invalid=True,
        )


def test_reject_invalid_example() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf"{DATA_DIR}/empty.txt:1:1: error: "
            r"Unexpected end-of-input. Expected one of: \n"
            rf"{DATA_DIR}/empty.txt:1:1: info: \t[*] FOO$"
        ),
    ):
        check_rst(
            filename=DATA_DIR / "simple_grammar.rst",
            examples=[DATA_DIR / "empty.txt"],
        )


def test_simple_grammar() -> None:
    check_rst(DATA_DIR / "simple_grammar.rst", examples=[DATA_DIR / "simple_grammar.txt"])


def test_accept_invalid_example() -> None:
    check_rst(
        DATA_DIR / "simple_grammar.rst",
        examples=[DATA_DIR / "empty.txt"],
        invalid=True,
    )


def test_simple_grammar_multiple_examples() -> None:
    check_rst(
        DATA_DIR / "simple_grammar.rst",
        examples=[
            DATA_DIR / "simple_grammar.txt",
            DATA_DIR / "simple_grammar.txt",
        ],
    )


def test_invalid_grammar() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: error: "
            r"No terminal matches '/' in the current parser context, at line 4 col 9\n"
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: info: \n"
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: info:    start/\n"
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: info:         \^\n"
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: info: Expected one of: \n"
            rf"{DATA_DIR}/invalid_grammar.rst:4:9: info: \t\* COLON$"
        ),
    ):
        check_rst(DATA_DIR / "invalid_grammar.rst")


def test_cli_simple_grammar(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "",
            "--document",
            str(DATA_DIR / "simple_grammar.rst"),
            str(DATA_DIR / "simple_grammar.txt"),
        ],
    )
    main()


def test_cli_complex_grammar(monkeypatch: pytest.MonkeyPatch, tmpdir: Path) -> None:
    verbal_map_file = tmpdir / "mapping.json"
    with verbal_map_file.open("w") as mf:
        mf.write('{"some characters": ".+"}')
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "",
            "--document",
            str(DATA_DIR / "complex_grammar.rst"),
            "--verbal-map",
            str(verbal_map_file),
            str(DATA_DIR / "simple_grammar.txt"),
        ],
    )
    main()


def test_cli_error(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "",
            "--document",
            str(DATA_DIR / "without_grammar.rst"),
            str(DATA_DIR / "simple_grammar.txt"),
        ],
    )
    with pytest.raises(
        SystemExit,
        match=rf"^{DATA_DIR}/without_grammar.rst:1:1: error: No grammar found$",
    ):
        main()
