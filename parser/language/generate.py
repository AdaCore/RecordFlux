#!/usr/bin/env python3

import os
import sys
from pathlib import Path

from langkit.compile_context import CompileCtx
from langkit.libmanage import ManageScript

from language.lexer import rflx_lexer as lexer
from language.parser import grammar

base_dir = Path(os.path.dirname(os.path.abspath(__file__))) / ".."

build_dir = sys.argv[1]
version = sys.argv[2]


class Manage(ManageScript):  # type: ignore[misc]
    def create_context(self, args: object) -> CompileCtx:
        return CompileCtx(lang_name="RFLX", lexer=lexer, grammar=grammar)


manage = Manage()

manage.run(
    [
        "generate",
        "--version",
        version,
        "--build-dir",
        f"{build_dir}",
        "--relative-project",
        "--no-pretty-print",
        "--relative-project",
        "--disable-warning",
        "undocumented-nodes",
    ]
)
