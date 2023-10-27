from __future__ import annotations

import argparse
import re
from argparse import ArgumentParser
from collections.abc import Callable
from io import TextIOWrapper
from pathlib import Path
from typing import ClassVar, Optional

import pytest

import rflx.specification
from rflx import cli, generator, validator
from rflx.converter import iana
from rflx.error import FatalError, Location, Severity, Subsystem, fail, fatal_fail
from rflx.ls.server import server
from rflx.pyrflx import PyRFLXError
from tests.const import DATA_DIR, SPEC_DIR

MESSAGE_SPEC_FILE = str(SPEC_DIR / "tlv.rflx")
SESSION_SPEC_FILE = str(SPEC_DIR / "session.rflx")
IANA_XML_FILE = str(DATA_DIR / "bootp-dhcp-parameters.xml")


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


def test_main_check_parser_error(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(cli, "check", lambda _: raise_parser_error())
    assert "<stdin>:8:22: parser: error: TEST" in str(
        cli.main(["rflx", "check", MESSAGE_SPEC_FILE]),
    )


def test_main_check_model_error_parse(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(cli, "check", lambda _: raise_model_error())
    assert "<stdin>:8:22: model: error: TEST" in str(cli.main(["rflx", "check", MESSAGE_SPEC_FILE]))


def test_main_check_model_error_create_model(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(rflx.specification.Parser, "parse", lambda _x, _y: raise_parser_error())
    monkeypatch.setattr(rflx.specification.Parser, "create_model", lambda _: raise_model_error())
    assert "<stdin>:8:22: parser: error: TEST\n<stdin>:8:22: model: error: TEST" in str(
        cli.main(["rflx", "check", MESSAGE_SPEC_FILE]),
    )


def test_main_check_non_existent_file() -> None:
    assert 'error: file not found: "non-existent file"' in str(
        cli.main(["rflx", "check", "non-existent file"]),
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
            ],
        )
        == 0
    )
    assert len(list(tmp_path.glob("*"))) == 0


def test_main_generate_prefix(tmp_path: Path) -> None:
    for index, prefix in enumerate(["", " ", "A", "A.B", "A.B.C"]):
        path = tmp_path / str(index)
        path.mkdir()
        assert (
            cli.main(
                [
                    "rflx",
                    "generate",
                    "-d",
                    str(path),
                    "-p",
                    prefix,
                    MESSAGE_SPEC_FILE,
                    SESSION_SPEC_FILE,
                ],
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
                ],
            ),
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
            ],
        ),
    )


@pytest.mark.parametrize(
    ("args", "expected"),
    [
        ([], generator.Debug.NONE),
        (["--debug", "built-in"], generator.Debug.BUILTIN),
        (["--debug", "external"], generator.Debug.EXTERNAL),
    ],
)
def test_main_generate_debug(
    args: list[str],
    expected: generator.Debug,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    result = []

    def generator_mock(
        self: object,  # noqa: ARG001
        prefix: str,  # noqa: ARG001
        workers: int,  # noqa: ARG001
        reproducible: bool,  # noqa: ARG001
        debug: generator.Debug,
        ignore_unsupported_checksum: bool,  # noqa: ARG001
    ) -> None:
        result.append(debug)

    monkeypatch.setattr(generator.Generator, "__init__", generator_mock)
    monkeypatch.setattr(
        generator.Generator,
        "generate",
        lambda self, model, integration, directory, library_files, top_level_package: None,  # noqa: ARG005, E501
    )
    assert (
        cli.main(
            ["rflx", "generate", "-d", str(tmp_path), *args, MESSAGE_SPEC_FILE, SESSION_SPEC_FILE],
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
        cli.main(["rflx", "graph", "-d", str(tmp_path), "non-existent file"]),
    )


def test_main_graph_non_existent_files(tmp_path: Path) -> None:
    assert (
        'cli: error: file not found: "non-existent file 1"\n'
        'cli: error: file not found: "non-existent file 2"'
        in str(
            cli.main(
                [
                    "rflx",
                    "graph",
                    "-d",
                    str(tmp_path),
                    "non-existent file 1",
                    "non-existent file 2",
                ],
            ),
        )
    )


def test_main_graph_non_existent_directory() -> None:
    assert 'cli: error: directory not found: "non-existent directory"' in str(
        cli.main(
            ["rflx", "graph", "-d", "non-existent directory", MESSAGE_SPEC_FILE, SESSION_SPEC_FILE],
        ),
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
            ],
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
            ],
        )


def test_main_validate(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda _a, _b, _c, _d, _e: None)
    monkeypatch.setattr(
        validator.Validator,
        "validate",
        lambda _a, _b, _c, _d, _e, _f, _g, _h: None,
    )
    assert (
        cli.main(
            # The positional and optional arguments are intentionally mixed
            # below so that a positional argument appears after the "-i" and its
            # value. '-i' and '-v' occur intentionally multiple times.
            [
                "rflx",
                "validate",
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
                "-v",
                str(tmp_path),
                "-i",
                str(tmp_path),
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "--split-disjunctions",
                "--abort-on-error",
                "--coverage",
                "-c",
                "CHECKSUM_MODULE",
                "-o",
                str(tmp_path),
                "--target-coverage",
                "99",
            ],
        )
        == 0
    )
    assert (
        cli.main(
            # The positional and optional arguments are intentionally mixed
            # below so that a positional argument appears after the "-v" and its
            # value.
            [
                "rflx",
                "validate",
                "-v",
                str(tmp_path),
                str(tmp_path / "test.rflx"),
                "Test::Message",
                "--split-disjunctions",
                "-i",
                str(tmp_path),
            ],
        )
        == 0
    )


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
            ],
        )
        == 'cli: error: invalid identifier: id: error: " " in identifier parts of "Ethernet Frame"'
    )


def test_main_validate_non_fatal_error(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda _a, _b, _c, _d, _e: None)
    monkeypatch.setattr(
        validator.Validator,
        "validate",
        lambda _a, _b, _c, _d, _e, _f, _g, _h: raise_parser_error(),
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
            ],
        )
        == "<stdin>:8:22: parser: error: TEST"
    )


def test_main_validate_validation_error(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda _a, _b, _c, _d, _e: None)
    monkeypatch.setattr(
        validator.Validator,
        "validate",
        lambda _a, _b, _c, _d, _e, _f, _g, _h: raise_validation_error(),
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
            ],
        ),
    )


@pytest.mark.parametrize("raise_error", [raise_unexpected_exception, raise_pyrflx_error])
def test_main_validate_fatal_error(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    raise_error: Callable[[], None],
) -> None:
    monkeypatch.setattr(validator.Validator, "__init__", lambda _a, _b, _c, _d, _e: None)
    monkeypatch.setattr(
        validator.Validator,
        "validate",
        lambda _a, _b, _c, _d, _e, _f, _g, _h: raise_error(),
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
            ],
        ),
    )


def test_main_unexpected_exception(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(cli, "generate", lambda _: raise_fatal_error())
    assert re.fullmatch(
        r"\n-* RecordFlux Bug -*.*Traceback.*-*.*RecordFlux/issues.*",
        str(
            cli.main(
                ["rflx", "generate", "-d", str(tmp_path), MESSAGE_SPEC_FILE, SESSION_SPEC_FILE],
            ),
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
                    ],
                ),
            ).split("\n"),
        )
        == 10
    )


def test_install_gnatstudio_plugin(tmp_path: Path) -> None:
    gnat_studio_dir = tmp_path / ".gnatstudio"
    gnat_studio_dir.mkdir(parents=True, exist_ok=True)
    args = ["rflx", "install", "gnatstudio", "--gnat-studio-dir", str(gnat_studio_dir)]

    # Install plugin into empty dir
    cli.main(args)
    plugin = gnat_studio_dir / "plug-ins/recordflux.py"
    assert plugin.exists()
    assert plugin.is_file()

    # Install with plugin dir already present
    plugin.unlink()
    cli.main(args)
    assert plugin.exists()
    assert plugin.is_file()


def test_install_vscode_extension(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    run_called = []
    vscode_extension = tmp_path / "recordflux.vsix"

    monkeypatch.setattr(cli, "run", lambda x: run_called.append(x))
    monkeypatch.setattr(cli, "vscode_extension", lambda: vscode_extension)

    with pytest.raises(SystemExit, match=r"^2$"):
        cli.main(["rflx", "install", "vscode"])

    vscode_extension.touch()

    cli.main(["rflx", "install", "vscode"])

    assert run_called == [["code", "--install-extension", vscode_extension, "--force"]]


def test_install_invalid() -> None:
    args = ["rflx", "install", "invalid"]

    with pytest.raises(SystemExit, match=r"^2$"):
        cli.main(args)


def test_missing_unsafe_option() -> None:
    assert (
        cli.main(
            [
                "rflx",
                "--no-verification",
                "check",
                MESSAGE_SPEC_FILE,
            ],
        )
        == 'cli: error: unsafe option "--no-verification" given without "--unsafe"'
    )


def test_exception_in_unsafe_mode(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(cli, "generate", lambda _: raise_fatal_error())
    assert re.fullmatch(
        r"\n-*\nEXCEPTION IN UNSAFE MODE, PLEASE RERUN WITHOUT UNSAFE OPTIONS\n-*\n"
        r"Traceback.*\n-*$",
        str(
            cli.main(
                [
                    "rflx",
                    "--unsafe",
                    "generate",
                    "-d",
                    str(tmp_path),
                    MESSAGE_SPEC_FILE,
                    SESSION_SPEC_FILE,
                ],
            ),
        ),
        re.DOTALL,
    )


def test_main_convert_iana(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    result = []

    def convert_mock(
        data: str,
        source: TextIOWrapper,
        always_valid: bool,
        output_dir: Path,
        _reproducible: bool = False,
    ) -> None:
        result.append((data, source.name, always_valid, output_dir))

    monkeypatch.setattr(iana, "convert", convert_mock)
    assert cli.main(["rflx", "convert", "iana", "-d", str(tmp_path / "1"), IANA_XML_FILE]) == 0
    assert (
        cli.main(
            ["rflx", "convert", "iana", "-d", str(tmp_path / "2"), "--always-valid", IANA_XML_FILE],
        )
        == 0
    )

    data = Path(IANA_XML_FILE).read_text(encoding="utf-8")
    assert result == [
        (data, IANA_XML_FILE, False, tmp_path / "1"),
        (data, IANA_XML_FILE, True, tmp_path / "2"),
    ]


def test_main_run_ls(monkeypatch: pytest.MonkeyPatch) -> None:
    called = []
    monkeypatch.setattr(server, "start_io", lambda: called.append(True))
    assert cli.main(["rflx", "run_ls"]) == 0
    assert any(called)


@pytest.mark.parametrize(
    ("requirement", "name", "extra"),
    [
        ("pydantic (<2,>=1)", "pydantic", None),
        ("pydantic<2,>=1", "pydantic", None),
        ("ruamel.yaml (<0.18,>=0.17)", "ruamel.yaml", None),
        ("ruamel.yaml<0.18,>=0.17", "ruamel.yaml", None),
        ("setuptools-scm (<8,>=6.2) ; extra == 'devel'", "setuptools-scm", "devel"),
        ('setuptools_scm<8,>=6.2; extra == "devel"', "setuptools_scm", "devel"),
    ],
)
def test_requirement(requirement: str, name: str, extra: Optional[str]) -> None:
    r = cli.Requirement(requirement)
    assert r.name == name
    assert r.extra == extra


def test_requirement_error() -> None:
    with pytest.raises(
        FatalError,
        match=r'^cli: error: failed parsing requirement "\(<2,>=1\) pydantic"$',
    ):
        cli.Requirement("(<2,>=1) pydantic")


class TestDuplicateArgs:
    """
    Test the rejection of duplicate options in the CLI.

    Concretely, such options that have action 'store' (the default action) are tested here. The test
    data consists of a full list of call arguments (optionally split to prefix and suffix) and the
    duplicate options. The actual values for the arguments do not matter in these tests, but must
    have the expected data type.
    """

    call_main_prefix: ClassVar[list[str]] = [
        "rflx",
        "--max-errors",
        "0",
        "--workers",
        "3",
    ]
    # Note: 'check' is used below as a concrete rflx subcommand. However, the focus in this set of
    # test data is on the rflx common arguments.
    call_main_suffix: ClassVar[list[str]] = [
        "check",
        "SPECIFICATION_FILE",
    ]
    call_generate: ClassVar[list[str]] = [
        "rflx",
        "generate",
        "SPECIFICATION_FILE",
        "-p",
        "PREFIX",
        "-d",
        "OUTPUT_DIRECTORY",
        "--debug",
        "built-in",
        "--integration-files-dir",
        "INTEGRATION_FILES_DIR",
    ]
    call_graph: ClassVar[list[str]] = [
        "rflx",
        "graph",
        "SPECIFICATION_FILE",
        "-f",
        "jpg",
        "-d",
        "OUTPUT_DIRECTORY",
    ]
    call_validate: ClassVar[list[str]] = [
        "rflx",
        "validate",
        "SPECIFICATION_FILE",
        "MESSAGE_IDENTIFIER",
        "-v",
        "VALID_SAMPLES_DIRECTORIES",
        "-i",
        "INVALID_SAMPLES_DIRECTORIES",
        "-c",
        "CHECKSUM_MODULE",
        "-o",
        "OUTPUT_FILE",
        "--target-coverage",
        "50",
    ]
    call_install: ClassVar[list[str]] = [
        "rflx",
        "install",
        "gnatstudio",
        "--gnat-studio-dir",
        "GNAT_STUDIO_DIR",
    ]
    call_convert_iana: ClassVar[list[str]] = [
        "rflx",
        "convert",
        "iana",
        "-d",
        "SOME_PATH",
    ]

    class ArgParseError(Exception):
        """Used to mock the command line parsing error in argparse."""

    def raise_argparse_error(self, message: str) -> None:
        raise self.ArgParseError(message)

    @pytest.mark.parametrize(
        ("call_prefix", "duplicate_option", "duplicate_option_value", "call_suffix"),
        [
            (call_main_prefix, "--max-errors", "5", call_main_suffix),
            (call_main_prefix, "--workers", "10", call_main_suffix),
            (call_generate, "-p", "OTHER_STR", []),
            (call_generate, "-d", "OTHER_STR", []),
            (call_generate, "--debug", "external", []),
            (call_generate, "--integration-files-dir", "OTHER_STR", []),
            (call_graph, "-f", "png", []),
            (call_graph, "-d", "OTHER_STR", []),
            (call_validate, "-c", "OTHER_STR", []),
            (call_validate, "-o", "OTHER_STR", []),
            (call_validate, "--target-coverage", "75", []),
            (call_install, "--gnat-studio-dir", "OTHER_STR", []),
            (call_convert_iana, "-d", "OTHER_STR", []),
        ],
    )
    def test_duplicate_args(
        self,
        monkeypatch: pytest.MonkeyPatch,
        call_prefix: list[str],
        duplicate_option: str,
        duplicate_option_value: str,
        call_suffix: list[str],
    ) -> None:
        monkeypatch.setattr(ArgumentParser, "error", lambda _s, m: self.raise_argparse_error(m))
        args = [*call_prefix, duplicate_option, duplicate_option_value, *call_suffix]
        with pytest.raises(self.ArgParseError, match=f"^{duplicate_option} appears several times$"):
            cli.main(args)

    def test_uniquestore_nargs_is_0(self) -> None:
        parser = argparse.ArgumentParser()
        message = (
            "nargs for store actions must be != 0; if you "
            "have nothing to store, actions such as store "
            "true or store const may be more appropriate"
        )
        with pytest.raises(ValueError, match=f"^{message}$"):
            parser.add_argument("-x", action=cli.UniqueStore, nargs=0)

    def test_uniquestore_bad_const(self) -> None:
        parser = argparse.ArgumentParser()
        message = "nargs must be '\\?' to supply const"
        with pytest.raises(ValueError, match=f"^{message}$"):
            parser.add_argument("-x", action=cli.UniqueStore, const=0)
