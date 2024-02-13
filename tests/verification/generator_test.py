import textwrap
from pathlib import Path

from rflx.generator.optimizer import optimize


def test_optimize(tmp_path: Path) -> None:
    f = tmp_path / "test.adb"
    f.write_text(
        textwrap.dedent(
            """\
            procedure Test with
               SPARK_Mode
            is
            begin
               if False then
                  goto Error;
               end if;
               <<Error>>
            end Test;
            """,
        ),
    )

    optimize(tmp_path)

    assert f.read_text() == textwrap.dedent(
        """\
        procedure Test with
           SPARK_Mode
        is
        begin
           <<Error>>
        end Test;
        """,
    )
