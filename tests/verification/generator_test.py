import textwrap
from pathlib import Path

import pytest

from rflx.generator.optimizer import gnatprove_supports_limit_lines, optimize
from tests import utils


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


@pytest.mark.skipif(utils.spark_version() != "24", reason="depends on SPARK 24")
def test_gnatprove_supports_limit_lines_24() -> None:
    assert not gnatprove_supports_limit_lines()


@pytest.mark.skipif(utils.spark_version() != "25", reason="depends on SPARK 25")
def test_gnatprove_supports_limit_lines_25() -> None:
    assert gnatprove_supports_limit_lines()
