from __future__ import annotations

import sys
from pathlib import Path

import pytest

from tools import check_unit_test_file_coverage


def test_missing_test_dir(tmp_path: Path) -> None:
    source_dir = tmp_path / "source"
    test_dir = tmp_path / "test"
    source_dir.mkdir()

    with pytest.raises(
        check_unit_test_file_coverage.CheckUnitTestFileError,
        match=f"^missing test directory: {test_dir}$",
    ):
        check_unit_test_file_coverage.check_file_coverage(source_dir=source_dir, test_dir=test_dir)


def test_missing_source_dir(tmp_path: Path) -> None:
    source_dir = tmp_path / "source"
    test_dir = tmp_path / "test"
    test_dir.mkdir()

    with pytest.raises(
        check_unit_test_file_coverage.CheckUnitTestFileError,
        match=f"^missing source directory: {source_dir}$",
    ):
        check_unit_test_file_coverage.check_file_coverage(source_dir=source_dir, test_dir=test_dir)


@pytest.mark.parametrize(
    ("source_files", "test_files", "expected", "ignore"),
    [
        ([], [], None, None),
        (["unrelated.txt"], [], None, None),
        (["subdir/unrelated.txt"], [], None, None),
        (["__init__.py"], [], None, None),
        (["subdir/__init__.py"], [], None, None),
        (["unrelated.txt", "__init__.py"], [], None, None),
        (["subdir/unrelated.txt", "__init__.py"], [], None, None),
        (["unrelated.txt", "subdir/__init__.py"], [], None, None),
        ([], ["unrelated.txt"], None, None),
        ([], ["subdir/unrelated.txt"], None, None),
        ([], ["__init__.py"], None, None),
        ([], ["subdir/__init__.py"], None, None),
        ([], ["unrelated.txt", "__init__.py"], None, None),
        ([], ["unrelated.txt", "subdir/__init__.py"], None, None),
        ([], ["subdir/unrelated.txt", "subdir/__init__.py"], None, None),
        (["file1.py"], ["file1_test.py"], None, None),
        (["file1.pyi"], ["file1_test.py"], None, None),
        (["file1.py", "file1.pyi"], ["file1_test.py"], None, None),
        (["subdir/file1.py"], ["subdir/file1_test.py"], None, None),
        (["subdir/file1.pyi"], ["subdir/file1_test.py"], None, None),
        (["subdir/file1.py", "subdir/file1.pyi"], ["subdir/file1_test.py"], None, None),
        (["file1.py", "__init__.py"], ["file1_test.py"], None, None),
        (["file1.py"], ["file1_test.py", "__init__.py"], None, None),
        (["file1.py"], [], "no tests for: file1.py", None),
        (["subdir/file1.py"], [], "no tests for: subdir/file1.py", None),
        (["file1.py", "__init__.py"], [], "no tests for: file1.py", None),
        (["subdir/file1.py", "__init__.py"], [], "no tests for: subdir/file1.py", None),
        (["file1.py"], ["__init__.py"], "no tests for: file1.py", None),
        (["file1.py", "file2.py"], [], "no tests for: file1.py, file2.py", None),
        (["file1.py", "subdir/file2.py"], [], "no tests for: file1.py, subdir/file2.py", None),
        ([], ["file1_test.py"], "no corresponding source for: file1_test.py", None),
        ([], ["subdir/file1_test.py"], "no corresponding source for: subdir/file1_test.py", None),
        (["__init__.py"], ["file1_test.py"], "no corresponding source for: file1_test.py", None),
        (
            ["__init__.py"],
            ["subdir/file1_test.py"],
            "no corresponding source for: subdir/file1_test.py",
            None,
        ),
        (
            ["subdir/__init__.py"],
            ["file1_test.py"],
            "no corresponding source for: file1_test.py",
            None,
        ),
        ([], ["file1_test.py", "__init__.py"], "no corresponding source for: file1_test.py", None),
        ([], ["unrelated.py"], "no corresponding source for: unrelated.py", None),
        ([], ["subdir/unrelated.py"], "no corresponding source for: subdir/unrelated.py", None),
        (
            [],
            ["file1_test.py", "file2_test.py"],
            "no corresponding source for: file1_test.py, file2_test.py",
            None,
        ),
        (
            [],
            ["subdir/file1_test.py", "file2_test.py"],
            "no corresponding source for: file2_test.py, subdir/file1_test.py",
            None,
        ),
        (
            ["file1.py"],
            ["file1_test.py", "file2_test.py"],
            "no corresponding source for: file2_test.py",
            None,
        ),
        (
            ["subdir/file1.py"],
            ["subdir/file1_test.py", "file2_test.py"],
            "no corresponding source for: file2_test.py",
            None,
        ),
        (
            ["subdir/file1.py"],
            [],
            None,
            ["subdir"],
        ),
        (
            [],
            ["subdir/file1_test.py"],
            None,
            ["subdir"],
        ),
        (
            ["subdir/file1.py", "subdir2/file2.py"],
            ["subdir/file1_test.py", "file2_test.py"],
            "no corresponding source for: file2_test.py",
            ["subdir2"],
        ),
        (
            ["subdir/file1.py", "subdir2/file2.py"],
            ["subdir/file1_test.py", "subdir3/file2_test.py"],
            "no tests for: subdir2/file2.py",
            ["subdir3"],
        ),
        (
            ["file1.py"],
            ["file1_test.py", "subdir/file2_test.py"],
            "no corresponding source for: subdir/file2_test.py",
            None,
        ),
        (
            ["file1.py"],
            ["file2_test.py"],
            "no tests for: file1.py\nno corresponding source for: file2_test.py",
            None,
        ),
        (
            ["subdir/file1.py"],
            ["file2_test.py"],
            "no tests for: subdir/file1.py\nno corresponding source for: file2_test.py",
            None,
        ),
        (
            ["file1.py"],
            ["subdir/file2_test.py"],
            "no tests for: file1.py\nno corresponding source for: subdir/file2_test.py",
            None,
        ),
        (
            ["file1.py", "file2.py"],
            ["file2_test.py", "file3_test.py"],
            "no tests for: file1.py\nno corresponding source for: file3_test.py",
            None,
        ),
        (
            ["file1.py", "subdir/file2.py"],
            ["subdir/file2_test.py", "file3_test.py"],
            "no tests for: file1.py\nno corresponding source for: file3_test.py",
            None,
        ),
        (
            ["subdir/file1.py", "file2.py"],
            ["file2_test.py", "subdir/file3_test.py"],
            "no tests for: subdir/file1.py\nno corresponding source for: subdir/file3_test.py",
            None,
        ),
    ],
)
def test_no_files(
    tmp_path: Path,
    source_files: list[str],
    test_files: list[str],
    expected: str | None,
    ignore: list[str] | None,
) -> None:
    source_dir = tmp_path / "source"
    test_dir = tmp_path / "test"

    source_dir.mkdir()
    test_dir.mkdir()

    for source_file in source_files:
        (source_dir / source_file).parent.mkdir(parents=True, exist_ok=True)
        (source_dir / source_file).touch()

    for test_file in test_files:
        (test_dir / test_file).parent.mkdir(parents=True, exist_ok=True)
        (test_dir / test_file).touch()

    if expected:
        with pytest.raises(
            check_unit_test_file_coverage.CheckUnitTestFileError,
            match=f"^{expected}$",
        ):
            check_unit_test_file_coverage.check_file_coverage(
                source_dir=source_dir,
                test_dir=test_dir,
                ignore=None if ignore is None else [Path(i) for i in ignore],
            )
    else:
        check_unit_test_file_coverage.check_file_coverage(
            source_dir=source_dir,
            test_dir=test_dir,
            ignore=None if ignore is None else [Path(i) for i in ignore],
        )


@pytest.mark.parametrize(
    ("file", "ignore_list", "expected"),
    [
        ("a", ["b", "c", "d"], False),
        ("b", ["b", "c", "d"], True),
        ("a", ["a/b", "a/c", "a/d"], False),
        ("a/b/c", ["a"], True),
        ("a/b/c", ["a/b"], True),
        ("a/b/c", ["a", "b", "c"], True),
        ("a/b/c", ["a/b", "b", "c"], True),
        ("a/b/c", ["a/b/c", "b", "c"], True),
        ("a", [], False),
        ("a", None, False),
    ],
)
def test_ignored(file: str, ignore_list: list[str] | None, expected: bool) -> None:
    assert (
        check_unit_test_file_coverage._ignored(  # noqa: SLF001
            Path(file),
            [Path(i) for i in ignore_list] if ignore_list else None,
        )
        == expected
    )


def test_cli(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    stored_source_dir: Path | None = None
    stored_test_dir: Path | None = None

    def dummy_check_file_coverage(
        source_dir: Path,
        test_dir: Path,
        ignore: list[Path] | None = None,  # noqa: ARG001
    ) -> None:
        nonlocal stored_source_dir, stored_test_dir
        stored_source_dir = source_dir
        stored_test_dir = test_dir
        raise check_unit_test_file_coverage.CheckUnitTestFileError("Dummy Error")

    with monkeypatch.context() as m:
        m.setattr(check_unit_test_file_coverage, "check_file_coverage", dummy_check_file_coverage)
        m.setattr(
            sys,
            "argv",
            [
                "prog",
                "--source-dir",
                str(tmp_path / "stored_source_dir"),
                "--test-dir",
                str(tmp_path / "stored_test_dir"),
            ],
        )
        with pytest.raises(
            SystemExit,
            match="^Dummy Error$",
        ):
            check_unit_test_file_coverage.main()
        assert stored_source_dir == tmp_path / "stored_source_dir"
        assert stored_test_dir == tmp_path / "stored_test_dir"
