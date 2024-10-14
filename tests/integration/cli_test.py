import textwrap
from pathlib import Path

import pytest

from rflx import cli
from rflx.rapidflux import RecordFluxError


def test_parse_no_subsequent_errors_caused_by_basic_style_errors(tmp_path: Path) -> None:
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

           type T is unsigned 8;

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
            r" \[style:indentation\]"
            r"$"
        ),
    ):
        cli.parse([a], no_caching=True, no_verification=False)


def test_parse_no_subsequent_errors_caused_by_model_style_errors(tmp_path: Path) -> None:
    p = tmp_path / "p.rflx"
    p.write_text(
        textwrap.dedent(
            """\
        package P is
           type I is range 0 .. 2 ** 4 - 1 with Size => 4;
           type M is
              message
                 F : I;
                 G : I;
              end message;
        end P;
        """,
        ),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            rf'{p}:2:9: error: "I" covers the entire range of an unsigned integer type'
            r" \[style:integer-syntax\]\n"
            rf'{p}:2:9: help: use "type I is unsigned 4" instead'
            r"$"
        ),
    ):
        cli.parse([p], no_caching=True, no_verification=False)


@pytest.mark.parametrize(
    ("spec"),
    [
        """\
        -- no style header
        package P is
           type I is range 0 .. 2 ** 4 - 1 with Size => 4;
        end P;
        """,
        """\
        -- style: disable = line-length, blank-lines
        package P is
           type I is range 0 .. 15 with Size => 4;
        end P;
        """,
    ],
)
def test_integer_style_error(spec: str, tmp_path: Path) -> None:
    p = tmp_path / "p.rflx"
    p.write_text(textwrap.dedent(spec))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            rf'{p}:3:9: error: "I" covers the entire range of an unsigned integer type'
            r" \[style:integer-syntax\]\n"
            rf'{p}:3:9: help: use "type I is unsigned 4" instead'
            r"$"
        ),
    ):
        cli.parse([p], no_caching=True, no_verification=False)


@pytest.mark.parametrize(
    ("spec"),
    [
        """\
        package P is
           type I is range 1 .. 15 with Size => 4;
        end P;
        """,
        """\
        -- style: disable = line-length, integer-syntax, blank-lines
        package P is
           type I is range 0 .. 15 with Size => 4;
        end P;
        """,
        """\
        -- style: disable = all
        package P is
           type I is range 0 .. 15 with Size => 4;
        end P;
        """,
        """\
        package P is
           type U is unsigned 4;
        end P;
        """,
    ],
)
def test_integer_style_no_error(spec: str, tmp_path: Path) -> None:
    p = tmp_path / "p.rflx"
    p.write_text(textwrap.dedent(spec))

    cli.parse([p], no_caching=True, no_verification=False)
