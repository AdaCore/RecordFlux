#!/usr/bin/env python3

import site
import sys
from pathlib import Path

from langkit.compile_context import CompileCtx
from langkit.libmanage import ManageScript

site.addsitedir(str(Path(__file__).absolute().parent.parent))

from language.lexer import rflx_lexer as lexer  # noqa: E402
from language.parser import grammar  # noqa: E402

BUILD_DIR = sys.argv[1]
VERSION = sys.argv[2]


class Manage(ManageScript):  # type: ignore[misc]
    def create_context(self, args: object) -> CompileCtx:
        return CompileCtx(lang_name="Rflx", lexer=lexer, grammar=grammar)


manage = Manage()

manage.run(
    [
        "generate",
        "--version",
        VERSION,
        "--build-dir",
        f"{BUILD_DIR}",
        "--relative-project",
        "--no-pretty-print",
        "--relative-project",
        "--disable-warning",
        "undocumented-nodes",
    ]
)
