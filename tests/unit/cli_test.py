from __future__ import annotations

import re
from pathlib import Path
from typing import Callable

import pytest
from _pytest.monkeypatch import MonkeyPatch

import rflx.specification
from rflx import cli, generator, validator
from rflx.error import Location, Severity, Subsystem, fail, fatal_fail
from rflx.pyrflx import PyRFLXError
from tests.const import SPEC_DIR

MESSAGE_SPEC_FILE = str(SPEC_DIR / "tlv.rflx")
SESSION_SPEC_FILE = str(SPEC_DIR / "session.rflx")


def raise_parser_error() -> None:
    fail("TEST", Subsystem.PARSER, Severity.ERROR, Location((8, 22)))


def raise_model_error() -> None:
    fail("TEST", Subsystem.MODEL, Severity.ERROR, Location((8, 22)))


def raise_pyrflx_error() -> None:
    raise PyRFLXError("TEST")


def raise_validation_error() -> None:
    raise validator.ValidationError("TEST")


def raise_unexpected_exception() -> None:
    raise NotImplementedError("Not implemented")


def raise_fatal_error() -> None:
    fatal_fail("TEST", Subsystem.CLI)


def test_main_noarg() -> None:
    assert cli.main(["rflx"]) == 2


def test_main_help() -> None:
    with pytest.raises(SystemExit):
        cli.main(["rflx", "-h"])


def test_main_version() -> None:
    assert cli.main(["rflx", "--version"]) == 0


def test_main_check() -> None:
    assert cli.main(["rflx", "check", MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]) == 0


def test_main_check_quiet() -> None:
    assert cli.main(["rflx", "-q", "check", MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]) == 0
    assert cli.main(["rflx", "--quiet", "check", MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]) == 0


def test_main_check_parser_error(monkeypatch: MonkeyPatch) -> None:
    monkeypatch.setattr(cli, "check", lambda x: raise_parser_error())
    assert "<stdin>:8:22: parser: error: TEST" in str(cli.main(["rflx", "check", "README.md"]))


def test_main_check_model_error_parse(monkeypatch: MonkeyPatch) -> None:
    monkeypatch.setattr(cli, "check", lambda x: raise_model_error())
    assert "<stdin>:8:22: model: error: TEST" in str(cli.main(["rflx", "check", "README.md"]))


def test_main_check_model_error_create_model(monkeypatch: MonkeyPatch) -> None:
    monkeypatch.setattr(rflx.specification.Parser, "create_model", lambda x: raise_model_error())
    assert "<stdin>:8:22: model: error: TEST" in str(cli.main(["rflx", "check", "README.md"]))


def test_main_check_non_existent_file() -> None:
    assert 'error: file not found: "non-existent file"' in str(
        cli.main(["rflx", "check", "non-existent file"])
    )


def test_main_generate(tmp_path: Path) -> None:
    assert (
        cli.main(["rflx", "generate", "-d", str(tmp_path), MESSAGE_SPEC_FILE, SESSION_SPEC_FILE])
        == 0
    )
    top_level_package = Path(tmp_path) / (cli.DEFAULT_PREFIX.lower() + ".ads")
    assert top_level_package.exists()


def test_main_generate_no_library_files(tmp_path: Path) -> None:
    assert (
        cli.main(
            [
                "rflx",
                "generate",
                "-d",
                str(tmp_path),
                "-p",
                "",
                "-n",
                str(SPEC_DIR / "empty_package.rflx"),
            ]
        )
        == 0
    )
    assert len(list(tmp_path.glob("*"))) == 0


def test_main_generate_prefix(tmp_path: Path) -> None:
    for prefix in ["", " ", "A", "A.B", "A.B.C"]:
        assert (
            cli.main(
                [
                    "rflx",
                    "generate",
                    "-d",
                    str(tmp_path),
                    "-p",
                    prefix,
                    MESSAGE_SPEC_FILE,
                    SESSION_SPEC_FILE,
                ]
            )
            == 0
        )
        top_level_package = Path(tmp_path) / (prefix.replace(".", "-").lower() + ".ads")
        assert not top_level_package.exists()


def test_main_generate_invalid_prefix(tmp_path: Path) -> None:
    for prefix in [".", "A.B.", ".A.B", "A..B"]:
        assert rf'cli: error: invalid prefix: "{prefix}"' in str(
            cli.main(
                [
                    "rflx",
                    "generate",
                    "-d",
                    str(tmp_path),
                    "-p",
                    prefix,
                    MESSAGE_SPEC_FILE,
                    SESSION_SPEC_FILE,
                ]
            )
        )


def test_main_generate_no_output_files(tmp_path: Path) -> None:
    assert (
        cli.main(["rflx", "generate", "-d", str(tmp_path), str(SPEC_DIR / "empty_package.rflx")])
        == 0
    )


def test_main_generate_non_existent_directory() -> None:
    assert 'cli: error: directory not found: "non-existent directory"' in str(
        cli.main(
            [
                "rflx",
                "generate",
                "-d",
                "non-existent directory",
                MESSAGE_SPEC_FILE,
                SESSION_SPEC_FILE,
            ]
        )
    )


@pytest.mark.parametrize(
    "args, expected",
    [
        ([], generator.Debug.NONE),
        (["--debug", "built-in"], generator.Debug.BUILTIN),
        (["--debug", "external"], generator.Debug.EXTERNAL),
    ],
)
def test_main_generate_debug(
    args: list[str], expected: generator.Debug, monkeypatch: MonkeyPatch, tmp_path: Path
) -> None:
    result = []

    def generator_mock(  # pylint: disable = unused-argument
        self: object,
        prefix: str,
        workers: int,
        reproducible: bool,
        debug: generator.Debug,
        ignore_unsupported_checksum: bool,
    ) -> None:
        result.append(debug)

    monkeypatch.setattr(generator.Generator, "__init__", generator_mock)
    monkeypatch.setattr(
        generator.Generator,
        "generate",
        lambda self, model, integration, directory, library_files, top_level_package: None,
    )
    assert (
        cli.main(
            ["rflx", "generate", "-d", str(tmp_path), *args, MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]
        )
        == 0
    )
    assert result == [expected]


def test_main_graph(tmp_path: Path) -> None:
    assert (
        cli.main(["rflx", "graph", "-d", str(tmp_path), MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]) == 0
    )


def test_main_graph_non_existent_file(tmp_path: Path) -> None:
    assert 'cli: error: file not found: "non-existent file"' in str(
        cli.main(["rflx", "graph", "-d", str(tmp_path), "non-existent file"])
    )


def test_main_graph_non_existent_files(tmp_path: Path) -> None:
    assert (
        'cli: error: file not found: "non-existent file 1"\n'
        'cli: error: file not found: "non-existent file 2"'
        in str(
            cli.main(
                ["rflx", "graph", "-d", str(tmp_path), "non-existent file 1", "non-existent file 2"]
            )
        )
    )


def test_main_graph_non_existent_directory() -> None:
    assert 'cli: error: directory not found: "non-existent directory"' in str(
        cli.main(
            ["rflx", "graph", "-d", "non-existent directory", MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]
        )
    )


def test_main_graph_no_output_files(tmp_path: Path) -> None:
    assert (
        cli.main(["rflx", "graph", "-d", str(tmp_path), str(SPEC_DIR / "empty_package.rflx")]) == 0
    )


def test_main_validate_required_arg_not_provided(tmp_path: Path) -> None:
    with pytest.raises(SystemExit, match="2"):
        cli.main(
            [
                "rflx",
                "validate",
                "Test::Message",
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
            ]
        )

    with pytest.raises(SystemExit, match="2"):
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
            ]
        )


def test_main_validate_no_test_data_provided(tmp_path: Path) -> None:
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
            ]
        )
        == "cli: error: must provide directory with valid and/or invalid messages"
    )


def test_main_validate_output_file_exists(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.json"
    tmp_file.write_text("")
    assert tmp_file.is_file()
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
                "-o",
                str(tmp_file),
                "-c",
                "checksum",
            ]
        )
    ) == f"cli: error: output file already exists: {tmp_file}"


def test_main_validate_path_does_not_exist(tmp_path: Path) -> None:
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "-i",
                str(tmp_path / "non_existent_dir"),
                "-c",
                "checksum",
            ]
        )
    ) == f"cli: error: {tmp_path}/non_existent_dir does not exist or is not a directory"


def test_main_validate_path_is_not_directory(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.txt"
    tmp_file.write_text("")
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_file),
                "Test::Message",
                "-i",
                str(tmp_file),
                "-c",
                "checksum",
            ]
        )
    ) == f"cli: error: {tmp_file} does not exist or is not a directory"


def test_main_validate_invalid_identifier(tmp_path: Path) -> None:
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Ethernet Frame",
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
            ]
        )
        == 'cli: error: invalid identifier: id: error: " " in identifier parts of "Ethernet Frame"'
    )


def test_main_validate_non_fatal_error(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda a, b, c, d, e: None)
    monkeypatch.setattr(
        validator.Validator, "validate", lambda a, b, c, d, e, f, g, h: raise_parser_error()
    )
    assert (
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
            ]
        )
        == "<stdin>:8:22: parser: error: TEST"
    )


def test_main_validate_validation_error(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda a, b, c, d, e: None)
    monkeypatch.setattr(
        validator.Validator, "validate", lambda a, b, c, d, e, f, g, h: raise_validation_error()
    )
    assert "validator: error: TEST" in str(
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "-v",
                str(tmp_path),
            ]
        )
    )


@pytest.mark.parametrize("raise_error", [raise_unexpected_exception, raise_pyrflx_error])
def test_main_validate_fatal_error(
    monkeypatch: MonkeyPatch, tmp_path: Path, raise_error: Callable[[], None]
) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda a, b, c, d, e: None)
    monkeypatch.setattr(
        validator.Validator, "validate", lambda a, b, c, d, e, f, g, h: raise_error()
    )
    assert "RecordFlux Bug" in str(
        cli.main(
            [
                "rflx",
                "validate",
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "-v",
                str(tmp_path),
            ]
        )
    )


def test_main_unexpected_exception(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(cli, "generate", lambda x: raise_fatal_error())
    assert re.fullmatch(
        r"\n-* RecordFlux Bug -*.*Traceback.*-*.*RecordFlux/issues.*",
        str(
            cli.main(
                ["rflx", "generate", "-d", str(tmp_path), MESSAGE_SPEC_FILE, SESSION_SPEC_FILE]
            )
        ),
        re.DOTALL,
    )


def test_fail_fast() -> None:
    assert (
        len(
            str(
                cli.main(
                    [
                        "rflx",
                        "--max-errors",
                        "5",
                        "check",
                        str(SPEC_DIR / "multiple_errors.rflx"),
                    ]
                )
            ).split("\n")
        )
        == 10
    )
