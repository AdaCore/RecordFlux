from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

SYNTAX_FILE = Path("rflx/ide/vim/recordflux.vim")


@pytest.mark.parametrize(
    "args",
    [
        (["vim", "--not-a-term"]),
        (["nvim", "--headless"]),
    ],
)
def test_vim_syntax_source(args: list[str]) -> None:
    ret = subprocess.run(
        [*args, "-c", f":source {SYNTAX_FILE}", "-c", ":quit"],
        check=False,
        stderr=subprocess.PIPE,
    )
    assert ret.stderr.decode() == ""
