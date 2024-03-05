from __future__ import annotations

import textwrap
from functools import partial
from pathlib import Path
from subprocess import CompletedProcess

import pytest

import rflx.generator
from rflx.error import RecordFluxError
from rflx.generator.optimizer import (
    Check,
    analyze,
    get_proof_results_for_asserts,
    gnatprove_found,
    instrument,
    optimize,
    prove,
    remove,
)
from tests.const import DATA_DIR


def prove_mock(
    prove_results: dict[int, bool],
    f: Path,  # noqa: ARG001
    lines: list[int],  # noqa: ARG001
    workers: int = 0,  # noqa: ARG001
    timeout: int = 0,  # noqa: ARG001
) -> dict[int, bool]:
    return prove_results


def run_mock(
    return_code: int,
    cmd: object,  # noqa: ARG001
    cwd: object = None,  # noqa: ARG001
    stdout: object = None,  # noqa: ARG001
    stderr: object = None,  # noqa: ARG001
) -> CompletedProcess[object]:
    return CompletedProcess("", return_code)


@pytest.mark.parametrize(
    ("content", "prove_results", "expected_content"),
    [
        (
            textwrap.dedent(
                """\
                procedure Test is
                begin
                end Test;
                """,
            ),
            {},
            textwrap.dedent(
                """\
                procedure Test is
                begin
                end Test;
                """,
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
                end Test;
                """,
            ),
            {4: True},
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   <<Error>>
                end Test;
                """,
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
                end Test;
                """,
            ),
            {4: False},
            textwrap.dedent(
                """\
                procedure Test is
                begin
                   if not False then
                      goto Error;
                   end if;
                   <<Error>>
                end Test;
                """,
            ),
        ),
    ],
)
def test_optimize(
    content: str,
    prove_results: dict[int, bool],
    expected_content: str,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setattr(rflx.generator.optimizer, "gnatprove_found", lambda: True)
    monkeypatch.setattr(rflx.generator.optimizer, "prove", partial(prove_mock, prove_results))

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
            {2: False},
            {},
        ),
        (
            {2: Check(1, 2, 4)},
            {2: True},
            {2: Check(1, 2, 4)},
        ),
        (
            {2: Check(1, 2, 4), 7: Check(1, 7, 9), 11: Check(10, 11, 13)},
            {2: False, 7: True, 11: False},
            {7: Check(1, 7, 9)},
        ),
        (
            {2: Check(1, 2, 4), 7: Check(1, 7, 9), 11: Check(10, 11, 13)},
            {2: True, 7: False, 11: True},
            {2: Check(1, 2, 4), 11: Check(10, 11, 13)},
        ),
    ],
)
def test_analyze(
    checks: dict[int, Check],
    prove_results: dict[int, bool],
    expected: dict[int, Check],
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setattr(rflx.generator.optimizer, "prove", partial(prove_mock, prove_results))

    f = tmp_path / "test.adb"
    f.write_text("")

    assert analyze(f, checks) == expected


def test_prove(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    f = tmp_path / "test.adb"
    f.write_text("")

    expected_result = {1: True, 2: False}

    monkeypatch.setattr(rflx.generator.optimizer, "run", partial(run_mock, 0))
    monkeypatch.setattr(
        rflx.generator.optimizer,
        "get_proof_results_for_asserts",
        lambda _: expected_result,
    )

    assert prove(f, []) == expected_result


def test_prove_with_limit_lines(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    f = tmp_path / "test.adb"
    f.write_text("")

    expected_result = {1: True, 2: False}

    monkeypatch.setattr(rflx.generator.optimizer, "run", partial(run_mock, 0))
    monkeypatch.setattr(rflx.generator.optimizer, "gnatprove_supports_limit_lines", lambda: True)
    monkeypatch.setattr(
        rflx.generator.optimizer,
        "get_proof_results_for_asserts",
        lambda _: expected_result,
    )

    assert prove(f, [1, 2]) == expected_result


def test_prove_error(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(rflx.generator.optimizer, "run", partial(run_mock, 1))

    with pytest.raises(
        RecordFluxError,
        match=r"^generator: error: gnatprove terminated with exit code 1$",
    ):
        prove(tmp_path, [])


def test_get_proof_results_for_asserts() -> None:
    assert get_proof_results_for_asserts(DATA_DIR / "rflx-test-session.spark") == {
        51: True,
        57: False,
        59: False,
        66: True,
        67: False,
        70: False,
        81: False,
        83: False,
        90: True,
        91: True,
        94: False,
        105: False,
        113: False,
        117: True,
        118: False,
        121: False,
        124: True,
        129: True,
        130: True,
        132: True,
        133: True,
        150: True,
        153: True,
        174: True,
        175: True,
        177: True,
    }


@pytest.mark.parametrize(
    ("content", "checks", "assertions", "expected"),
    [
        (
            "\n",
            {},
            [],
            "\n",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9\n",
            {4: Check(2, 4, 6)},
            [],
            "1\n7\n8\n9\n",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9\n",
            {},
            [1, 3, 5, 7, 9],
            "2\n4\n6\n8\n",
        ),
        (
            "1\n2\n3\n4\n5\n6\n7\n8\n9\n",
            {3: Check(1, 2, 3), 8: Check(7, 8, 9)},
            [5],
            "4\n6\n",
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
