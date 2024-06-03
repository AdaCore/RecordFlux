import textwrap
from pathlib import Path

import pytest

from rflx import cli
from rflx.rapidflux import RecordFluxError


def test_parse_no_subsequent_errors_caused_by_style_errors(tmp_path: Path) -> None:
    a = tmp_path / "a.rflx"
    a.write_text(
        textwrap.dedent(
            """\
        with B;

        package A is

           type M is
              message
                 L : B::T;
                 P : Opaque
                    with Size => L * 8;
              end message;

        for M use (P => B::M);

        end A;
        """,
        ),
    )

    b = tmp_path / "b.rflx"
    b.write_text(
        textwrap.dedent(
            """\
        package B is

           type T is range 0 .. 255 with Size => 8;

           type M is
              message
                 L : B::T;
                 P : Opaque
                    with Size => L * 8;
              end message;

        end B;
        """,
        ),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            rf"{a}:12:1: error: unexpected keyword indentation \(expected 3, 12 or 15\)"
            r" \[indentation\]"
            r"$"
        ),
    ):
        cli.parse([a], no_caching=True, no_verification=False)
