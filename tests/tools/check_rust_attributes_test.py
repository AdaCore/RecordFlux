from __future__ import annotations

import sys
import textwrap
from pathlib import Path

import pytest

from rflx.rapidflux import RecordFluxError
from tools.check_rust_attributes import check_file, get_line_col, main


@pytest.mark.parametrize(
    ("source_code", "expected_output"),
    [
        (
            textwrap.dedent(
                """\
                #[foo]
                #[serial]
                #[bar]
                """,
            ),
            textwrap.dedent(
                """\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            3 | | #[bar]
              | |__^
              |
              """,
            ),
        ),
        (
            textwrap.dedent(
                """\
            #[serial]
            #[foo]
            """,
            ),
            textwrap.dedent(
                """\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            2 | | #[foo]
              | |__^
              |
              """,
            ),
        ),
        (
            textwrap.dedent(
                """\
            #[foo]
            #[serial]
            #[bar]

            #[serial]
            #[bar]
            #[foo]
            """,
            ),
            textwrap.dedent(
                """\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            3 | | #[bar]
              | |__^
              |
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            6 | | #[bar]
              | |__^
              |
              """,
            ),
        ),
        (
            textwrap.dedent(
                """
            #[foo]
            #[bar]
            #[serial]
            """,
            ),
            "",
        ),
        (
            textwrap.dedent(
                """
            #[foo]
            #[bar]
            #[parallel]
            """,
            ),
            "",
        ),
        (
            textwrap.dedent(
                """\
            #[parallel]
            #[foo]
            """,
            ),
            textwrap.dedent(
                """\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            2 | | #[foo]
              | |__^
              |
              """,
            ),
        ),
        (
            textwrap.dedent(
                """\
                #[foo]
                #[parallel]
                #[bar]
                """,
            ),
            textwrap.dedent(
                """\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {}/temp.rs:1:1
              |
            3 | | #[bar]
              | |__^
              |
              """,
            ),
        ),
    ],
)
def test_check_file(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    source_code: str,
    expected_output: str,
) -> None:
    temp_file = tmp_path / "temp.rs"
    temp_file.write_text(source_code)

    errors = RecordFluxError()
    check_file(temp_file, errors)
    errors.print_messages()
    assert capfd.readouterr().err == expected_output.replace("{}", str(tmp_path))


def test_cli_error(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    temp_file = tmp_path / "temp.rs"
    temp_file.write_text(
        """\
            #[foo]
            #[serial]
            #[bar]
        """,
    )

    monkeypatch.setattr(sys, "argv", ["dummy", str(temp_file)])
    assert main() == 1
    assert capfd.readouterr().err == textwrap.dedent(
        f"""\
            error: invalid order of attributes; "parallel" and "serial" must always be the last \
attribute
             --> {tmp_path}/temp.rs:2:13
              |
            2 |               #[serial]
              |  _____________^
            3 | |             #[bar]
              | |______________^
              |
          """,
    )


def test_cli_good(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    temp_file = tmp_path / "temp.rs"
    temp_file.write_text(
        """
            #[foo]
            #[bar]
            #[serial]
        """,
    )

    monkeypatch.setattr(sys, "argv", ["dummy", str(temp_file)])
    assert main() == 0
    read = capfd.readouterr()
    assert read.err == ""
    assert read.out == ""


def test_file_not_found() -> None:
    with pytest.raises(
        FileNotFoundError,
        match=r"^\[Errno 2\] No such file or directory: 'foo.rs'$",
    ):
        check_file(Path("foo.rs"), RecordFluxError())


@pytest.mark.parametrize(
    ("content", "offset", "expected_location"),
    [
        (
            textwrap.dedent(
                """\
                1
                """,
            ),
            0,
            (1, 1),
        ),
        (
            textwrap.dedent(
                """\
                foo
                bar
                """,
            ),
            4,
            (2, 1),
        ),
        (
            "",
            0,
            (1, 1),
        ),
    ],
)
def test_get_line_col(content: str, offset: int, expected_location: tuple[int, int]) -> None:
    assert get_line_col(content, offset) == expected_location
