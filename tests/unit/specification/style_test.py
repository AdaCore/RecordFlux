from __future__ import annotations

import re
import textwrap
from pathlib import Path

import pytest

from rflx import const
from rflx.rapidflux import RecordFluxError, source_code
from rflx.specification import style
from tests.utils import assert_stderr_regex


def write_spec_and_check(spec: str, spec_file: Path) -> RecordFluxError:
    error = RecordFluxError()
    with spec_file.open("w", newline="") as f:
        f.write(spec)
    basic_style_checks, _ = style.determine_enabled_checks(error, spec, spec_file)
    error.extend(style.check(spec_file, basic_style_checks).entries)
    return error


@pytest.mark.parametrize(
    "spec",
    [
        "",
        """-- Full line comment
package Test is

   ---------
   -- Box --
   ---------

   -- Multiline comment
   -- * 🍪
   --    * 🍵
   --       * 🌞
   type I is range 0 .. 2 ** 16 + (-1); -- Inline comment

   type S is sequence of Test::T;
""",
        """--
   type E is
      (A => 1,
       B => 2,
       C => 3)
   with Size => 8;
""",
        """--
   type E is
      (-- Alfa (A)
       A => 1,

       -- Bravo (B)
       B => 2,

       -- Charlie (C)
       C => 3)
   with Size => 8;
""",
        """--
   type E is
      (A   =>   1,
       AB  =>  12,
       ABC => 123)
   with Size => 8;
""",
        """--
   type M is
      message
         Tag : E;
         Length : I;
         Value : Opaque
            with Size => Length * 8;
      end message;
""",
        """--
      with function Get_Tag return E;
      with function Create_Message
         (Tag : E;
          Value : Opaque)
      return M;
      with function X return Boolean;
""",
        """--
         Tag := Get_Tag;
         Response := Create_Message (Tag,
                                     Message.Data);
         Channel'Write (Response);
""",
        """--
         with Desc => "rfc1149.txt+51:4-52:9"
""",
    ],
)
def test_check_no_error(spec: str, tmp_path: Path) -> None:
    spec_file = tmp_path / "test.rflx"
    spec_file.write_text(spec)
    style.check(spec_file, const.BASIC_STYLE_CHECKS).propagate()


@pytest.mark.parametrize(
    ("spec", "error"),
    [
        (
            "\npackage Test is end Test;",
            r"1:1: error: leading blank line \[style:blank-lines\]",
        ),
        (
            "package Test is end Test;\n\n",
            r"2:1: error: trailing blank line \[style:blank-lines\]",
        ),
        (
            "package Test is\n\n\nend Test;",
            r"3:1: error: multiple blank lines \[style:blank-lines\]",
        ),
        (
            """package Test is\tend Test;""",
            r'1:16: error: illegal whitespace character "\\t" \[style:characters\]',
        ),
        (
            "package Test is\r\nend Test;",
            r'1:16: error: incorrect line terminator "\\r" \[style:characters\]',
        ),
        (
            "package Test is end Test; ",
            r"1:26: error: trailing whitespace \[style:trailing-spaces\]",
        ),
        (
            "package Test is\n         type T is range 0 .. 2 ** 16 with Size => 16;\nend Test;",
            r"2:10: error: unexpected keyword indentation \(expected 3 or 6\)"
            r" \[style:indentation\]",
        ),
        (
            "package Test is\n   type T is mod 2* 128;\nend Test;",
            r'2:19: error: missing space before "\*" \[style:token-spacing\]',
        ),
        (
            "package Test is end Test; --A test package",
            r'1:29: error: missing space after "--" \[style:token-spacing\]',
        ),
        (
            f"package Test is end Test; -- {'X' * 100}",
            r"1:121: error: line too long \(129/120\) \[style:line-length\]",
        ),
        (
            "package Test is\n"
            "   type E is unsigned 16;\n"
            "   type S is sequence of Test ::E;\n"
            "end Test;",
            r'3:31: error: space before "::" \[style:token-spacing\]',
        ),
        (
            "package Test is\n"
            "   type E is unsigned 16;\n"
            "   type S is sequence of Test:: E;\n"
            "end Test;",
            r'3:33: error: space after "::" \[style:token-spacing\]',
        ),
    ],
)
def test_check_error(tmp_path: Path, spec: str, error: str) -> None:
    spec_file = tmp_path / "test.rflx"
    with pytest.raises(RecordFluxError, match=rf"^{spec_file}:{error}$"):
        write_spec_and_check(spec, spec_file).propagate()


@pytest.mark.parametrize(
    ("base_spec", "disabled_checks"),
    [
        (
            "package Test is end Test;\n\n",
            "blank-lines",
        ),
        (
            "package Test is\n\n\nend Test;",
            "blank-lines",
        ),
        (
            """package Test is\tend Test;""",
            "characters",
        ),
        (
            "package Test is\r\nend Test;",
            "characters",
        ),
        (
            "package Test is end Test; ",
            "trailing-spaces",
        ),
        (
            "package Test is\n         type T is range 0 .. 2 ** 16 with Size => 16;\nend Test;",
            "indentation",
        ),
        (
            "package Test is\n   type T is mod 2* 128;\nend Test;",
            "token-spacing",
        ),
        (
            "package Test is end Test; --A test package",
            "token-spacing",
        ),
        (
            f"package Test is end Test; -- {'X' * 100}",
            "line-length",
        ),
        (
            "package Test is\n"
            "   type E is unsigned 16;\n"
            "   type S is sequence of Test ::E;\n"
            "end Test;",
            "token-spacing",
        ),
        (
            "package Test is\n"
            "   type E is unsigned 16;\n"
            "   type S is sequence of Test:: E;\n"
            "end Test;",
            "token-spacing",
        ),
        (
            "package Test is \n"
            "   type E is unsigned 16;\r\n"
            "    type S is sequence of Test:: E;\n"
            "end Test;\n\n",
            "all",
        ),
        (
            "package Test is \n"
            "   type E is unsigned 16;\r\n"
            "    type S is sequence of Test:: E;\n"
            "end Test;\n\n",
            "blank-lines, characters, indentation, token-spacing, trailing-spaces",
        ),
    ],
)
def test_deactivation_of_checks_on_file_level(
    base_spec: str,
    disabled_checks: str,
    tmp_path: Path,
) -> None:
    spec = f"-- style: disable = {disabled_checks}\n\n{base_spec}"
    spec_file = tmp_path / "test.rflx"
    write_spec_and_check(spec, spec_file).propagate()


@pytest.mark.parametrize(
    ("header", "expected_basic_style_checks", "expected_model_style_checks"),
    [
        (
            "",
            const.BASIC_STYLE_CHECKS,
            const.MODEL_STYLE_CHECKS,
        ),
        (
            "-- style: disable = all\n",
            set(),
            set(),
        ),
        (
            "-- style: disable = indentation\n",
            const.BASIC_STYLE_CHECKS - {const.StyleCheck("indentation")},
            {const.StyleCheck("integer-syntax")},
        ),
        (
            "-- style: disable = integer-syntax\n",
            const.BASIC_STYLE_CHECKS,
            const.MODEL_STYLE_CHECKS - {const.StyleCheck("integer-syntax")},
        ),
        (
            "-- style: disable = indentation,integer-syntax\n",
            const.BASIC_STYLE_CHECKS - {const.StyleCheck("indentation")},
            const.MODEL_STYLE_CHECKS - {const.StyleCheck("integer-syntax")},
        ),
        (
            "-- style: disable = integer-syntax,indentation\n",
            const.BASIC_STYLE_CHECKS - {const.StyleCheck("indentation")},
            const.MODEL_STYLE_CHECKS - {const.StyleCheck("integer-syntax")},
        ),
    ],
)
def test_header_no_error(
    header: str,
    expected_basic_style_checks: set[const.StyleCheck],
    expected_model_style_checks: set[const.StyleCheck],
    tmp_path: Path,
) -> None:
    spec_file = tmp_path / "test.rflx"
    spec = f"{header}\npackage Test is end Test;"
    spec_file.write_text(spec)
    error = RecordFluxError()
    basic_style_checks, model_style_checks = style.determine_enabled_checks(
        error,
        spec.split("\n", 1)[0],
        spec_file,
    )
    assert basic_style_checks == expected_basic_style_checks
    assert model_style_checks == expected_model_style_checks


@pytest.mark.parametrize(
    ("header", "error_msg"),
    [
        (
            "-- style: disable = foo",
            r'1:1: error: invalid check "foo"',
        ),
    ],
)
def test_header_error(
    header: str,
    error_msg: str,
    tmp_path: Path,
) -> None:
    spec_file = tmp_path / "test.rflx"
    spec = f"{header}\npackage Test is end Test;"
    spec_file.write_text(spec)
    error = RecordFluxError()
    _, _ = style.determine_enabled_checks(error, spec, spec_file)
    with pytest.raises(RecordFluxError, match=rf"^{spec_file}:{error_msg}$"):
        error.propagate()


@pytest.mark.parametrize(
    ("spec_code", "expected_message"),
    [
        (
            textwrap.dedent(
                """\
                package Test is
                type M is
                      message
                         A : Opaque with Size => 8;
                      end message;
                end Test;
            """,
            ),
            textwrap.dedent(
                """\
                error: unexpected keyword indentation (expected 3 or 6) [style:indentation]
                 --> {}:2:1
                  |
                2 | type M is
                  | ^
                  |
                """,
            ),
        ),
        (
            textwrap.dedent(
                """\
                package Test is
                    type M is
                      message
                         A : Opaque with Size => 8;
                      end message;
                end Test;
            """,
            ),
            textwrap.dedent(
                """\
                error: unexpected keyword indentation (expected 3 or 6) [style:indentation]
                 --> {}:2:5
                  |
                2 |     type M is
                  |     ^
                  |
                """,
            ),
        ),
    ],
)
def test_incorrect_indentation_message_display(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    spec_code: str,
    expected_message: str,
) -> None:
    spec_file = tmp_path / "test.rflx"
    spec_file.write_text(spec_code)
    source_code.register(spec_file, spec_file.read_text())

    style.check(spec_file, const.BASIC_STYLE_CHECKS).print_messages()
    assert_stderr_regex(f"^{re.escape(expected_message.format(spec_file))}$", capfd)
