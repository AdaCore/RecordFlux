from pathlib import Path

import pytest

from rflx.error import RecordFluxError
from rflx.specification import style


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
def test_no_error(spec: str, tmp_path: Path) -> None:
    spec_file = tmp_path / "test.rflx"
    spec_file.write_text(spec)
    style.check(spec_file).propagate()


@pytest.mark.parametrize(
    "spec,error",
    [
        (
            "\npackage Test is end Test;",
            r"1:1: style: error: leading blank line",
        ),
        (
            "package Test is end Test;\n\n",
            r"2:1: style: error: trailing blank line",
        ),
        (
            "package Test is\n\n\nend Test;",
            r"3:1: style: error: multiple blank lines",
        ),
        (
            """package Test is\tend Test;""",
            r'1:16: style: error: illegal whitespace character "\\t"',
        ),
        (
            "package Test is\r\nend Test;",
            r'1:16: style: error: incorrect line terminator "\\r"',
        ),
        (
            "package Test is end Test; ",
            r"1:26: style: error: trailing whitespace",
        ),
        (
            "package Test is\n         type T is mod 2 ** 16;\nend Test;",
            r"2:9: style: error: unexpected keyword indentation \(expected 3 or 6\)",
        ),
        (
            "package Test is\n   type T is mod 2* 128;\nend Test;",
            r'2:19: style: error: missing space before "\*"',
        ),
        (
            "package Test is end Test; --A test package",
            r'1:29: style: error: missing space after "--"',
        ),
        (
            f"package Test is end Test; -- {'X' * 100}",
            r"1:121: style: error: line too long \(129/120\)",
        ),
        (
            "package Test is\n"
            "   type E is mod 2 ** 16;\n"
            "   type S is sequence of Test ::E;\n"
            "end Test;",
            r'3:31: style: error: space before "::"',
        ),
        (
            "package Test is\n"
            "   type E is mod 2 ** 16;\n"
            "   type S is sequence of Test:: E;\n"
            "end Test;",
            r'3:33: style: error: space after "::"',
        ),
    ],
)
def test_error(tmp_path: Path, spec: str, error: str) -> None:
    spec_file = tmp_path / "test.rflx"
    spec_file.write_text(spec)
    with pytest.raises(RecordFluxError, match=rf"^{spec_file}:{error}$"):
        style.check(spec_file).propagate()
