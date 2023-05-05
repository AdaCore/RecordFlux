import sys
from pathlib import Path

import pytest

from tools.generate_gpr_file import GPRGenError, main


def test_main(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    input_gpr_file = tmp_path / "libtest.gpr.orig"
    input_gpr_file.write_text("   for Interfaces use (Foo, Bar);")
    monkeypatch.setattr(
        sys,
        "argv",
        ["generate_gpr_file", "test", tmp_path, input_gpr_file],
    )
    main()
    gpr_file = (tmp_path / "libtest.gpr").read_text()
    assert "library project Libtest" in gpr_file
    assert 'for Interfaces use ("Foo", " Bar");' in gpr_file


def test_main_invalid_arguments(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    input_gpr_file = tmp_path / "libtest.gpr.orig"
    input_gpr_file.write_text("")
    monkeypatch.setattr(
        sys,
        "argv",
        ["generate_gpr_file", "test"],
    )
    with pytest.raises(
        SystemExit,
        match=r"^Insufficient arguments: generate_gpr_file libname output-path input-gpr-file$",
    ):
        main()


def test_main_invalid_input_gpr_file(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    input_gpr_file = tmp_path / "libtest.gpr.orig"
    input_gpr_file.write_text("")
    monkeypatch.setattr(
        sys,
        "argv",
        ["generate_gpr_file", "test", tmp_path, input_gpr_file],
    )
    with pytest.raises(
        GPRGenError, match=rf'^Project file "{input_gpr_file}" contains no interfaces$'
    ):
        main()
