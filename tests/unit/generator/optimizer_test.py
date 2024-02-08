from __future__ import annotations

import textwrap
from pathlib import Path
from subprocess import CompletedProcess

import pytest

import rflx.generator
from rflx.error import RecordFluxError
from rflx.generator.optimizer import (
    Check,
    analyze,
    gnatprove_found,
    instrument,
    optimize,
    prove,
    remove,
)


@pytest.mark.parametrize(
    ("content", "prove_results", "expected_content"),
    [
        (
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   if not False then
                      goto Error;
                   end if;
                   <<Error>>
                end Test;""",
            ),
            [True],
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   <<Error>>
                end Test;""",
            ),
        ),
        (
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   if not False then
                      goto Error;
                   end if;
                   <<Error>>
                end Test;""",
            ),
            [False],
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   if not False then
                      goto Error;
                   end if;
                   <<Error>>
                end Test;""",
            ),
        ),
    ],
)
def test_optimize(
    content: str,
    prove_results: list[bool],
    expected_content: str,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setattr(rflx.generator.optimizer, "gnatprove_found", lambda: True)

    def prove_mock(f: Path, line: int, workers: int = 0) -> bool:  # noqa: ARG001
        return prove_results.pop()

    monkeypatch.setattr(rflx.generator.optimizer, "prove", prove_mock)

    f = tmp_path / "test.adb"
    f.write_text(content)

    optimize(tmp_path)

    assert f.read_text() == expected_content


def test_optimize_error(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(rflx.generator.optimizer, "gnatprove_found", lambda: False)
    with pytest.raises(
        RecordFluxError,
        match=r"^generator: error: GNATprove is required for code optimization$",
    ):
        optimize(tmp_path)


def test_gnatprove_found() -> None:
    gnatprove_found()


@pytest.mark.parametrize(
    ("content", "expected_content", "expected_checks"),
    [
        ("", "", {}),
        (
            textwrap.dedent(
                """\
                if not X then
                    goto Error;
                end if;""",
            ),
            textwrap.dedent(
                """\
                if not X then
                    pragma Assert (False);
                    goto Error;
                end if;""",
            ),
            {2: Check(1, 2, 4)},
        ),
        (
            textwrap.dedent(
                """\
                -- ...
                if not X then
                    Ctx.P.Next_State := S_Final;
                    -- ...
                    -- ...
                    -- ...
                    -- ...
                    pragma Assert (Invariant);
                    goto Error;
                end if;""",
            ),
            textwrap.dedent(
                """\
                -- ...
                if not X then
                    Ctx.P.Next_State := S_Final;
                    -- ...
                    -- ...
                    -- ...
                    -- ...
                    pragma Assert (Invariant);
                    pragma Assert (False);
                    goto Error;
                end if;""",
            ),
            {9: Check(2, 9, 11)},
        ),
        (
            textwrap.dedent(
                """\
                if not X then
                    Ctx.P.Next_State := S_Final;
                    -- ...
                    -- ...
                    -- ...
                    pragma Assert (Invariant);
                    goto Error;
                end if;
                if not X then
                    goto Error;
                end if;
                if not X then
                    -- ...
                    -- ...
                else
                    pragma Assert (Invariant);
                    goto Error;
                end if;""",
            ),
            textwrap.dedent(
                """\
                if not X then
                    Ctx.P.Next_State := S_Final;
                    -- ...
                    -- ...
                    -- ...
                    pragma Assert (Invariant);
                    pragma Assert (False);
                    goto Error;
                end if;
                if not X then
                    pragma Assert (False);
                    goto Error;
                end if;
                if not X then
                    -- ...
                    -- ...
                else
                    pragma Assert (Invariant);
                    goto Error;
                end if;""",
            ),
            {7: Check(1, 7, 9), 11: Check(10, 11, 13)},
        ),
    ],
)
def test_instrument(
    content: str,
    expected_content: str,
    expected_checks: dict[int, Check],
    tmp_path: Path,
) -> None:
    f = tmp_path / "test.adb"
    f.write_text(content)

    assert instrument(f) == expected_checks
    assert f.read_text() == expected_content


@pytest.mark.parametrize(
    ("checks", "prove_results", "expected"),
    [
        (
            {},
            [],
            {},
        ),
        (
            {2: Check(1, 2, 4)},
            [False],
            {},
        ),
        (
            {2: Check(1, 2, 4)},
            [True],
            {2: Check(1, 2, 4)},
        ),
        (
            {2: Check(1, 2, 4), 7: Check(1, 7, 9), 11: Check(10, 11, 13)},
            [False, True, False],
            {7: Check(1, 7, 9)},
        ),
        (
            {2: Check(1, 2, 4), 7: Check(1, 7, 9), 11: Check(10, 11, 13)},
            [True, False, True],
            {2: Check(1, 2, 4), 11: Check(10, 11, 13)},
        ),
    ],
)
def test_analyze(
    checks: dict[int, Check],
    prove_results: list[bool],
    expected: dict[int, Check],
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    def prove_mock(f: Path, line: int, workers: int = 0) -> bool:  # noqa: ARG001
        return prove_results.pop()

    monkeypatch.setattr(rflx.generator.optimizer, "prove", prove_mock)

    f = tmp_path / "test.adb"
    f.write_text("")

    assert analyze(f, checks) == expected


def test_prove(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    f = tmp_path / "test.adb"
    f.write_text("")

    monkeypatch.setattr(
        rflx.generator.optimizer,
        "run",
        lambda cmd, cwd, stdout, stderr: CompletedProcess("", 0),  # noqa: ARG005
    )

    assert prove(f, 0)

    monkeypatch.setattr(
        rflx.generator.optimizer,
        "run",
        lambda cmd, cwd, stdout, stderr: CompletedProcess("", 1),  # noqa: ARG005
    )

    assert not prove(f, 0)


@pytest.mark.parametrize(
    ("content", "checks", "assertions", "expected"),
    [
        (
            "",
            {},
            [],
            "",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9",
            {4: Check(2, 4, 6)},
            [],
            "1\n7\n8\n9",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9",
            {},
            [1, 3, 5, 7, 9],
            "2\n4\n6\n8",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9",
            {3: Check(1, 2, 3), 8: Check(7, 8, 9)},
            [5],
            "4\n6",
        ),
    ],
)
def test_remove(
    content: str,
    checks: dict[int, Check],
    assertions: list[int],
    expected: str,
    tmp_path: Path,
) -> None:
    f = tmp_path / "test.adb"
    f.write_text(content)

    remove(f, checks, assertions)

    assert f.read_text() == expected
