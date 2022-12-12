#!/usr/bin/env python3

"""Check the correctness of code examples in the documentation."""

from __future__ import annotations

import enum
import os
import re
import subprocess
import sys
import tempfile
import textwrap
from pathlib import Path
from typing import Optional

from librflxlang import AnalysisContext, GrammarRule
from ruamel.yaml import YAML
from ruamel.yaml.parser import ParserError

import rflx.error
import rflx.specification
from rflx.common import STDIN
from rflx.error import RecordFluxError
from rflx.specification import style
from rflx.specification.parser import diagnostics_to_error
from tests.const import GENERATED_DIR


class CheckDocError(Exception):
    pass


class CodeBlockType(enum.Enum):
    UNKNOWN = enum.auto()
    RFLX = enum.auto()
    ADA = enum.auto()
    PYTHON = enum.auto()
    YAML = enum.auto()
    IGNORE = enum.auto()

    def __str__(self) -> str:
        return self.name.title()


class State(enum.Enum):
    OUTSIDE = enum.auto()
    HEADER = enum.auto()
    INSIDE = enum.auto()
    EMPTY = enum.auto()


def parse_code_block_type(type_str: str) -> CodeBlockType:
    normalized = type_str.lower()
    types = {
        "rflx": CodeBlockType.RFLX,
        "ada": CodeBlockType.ADA,
        "python": CodeBlockType.PYTHON,
        "yaml": CodeBlockType.YAML,
        "ignore": CodeBlockType.IGNORE,
    }
    if normalized not in types:
        return CodeBlockType.UNKNOWN
    return types[normalized]


def check_file(filename: Path, content: str) -> bool:
    # pylint: disable=too-many-branches, too-many-statements

    found = False
    state = State.OUTSIDE
    block = ""
    block_start: Optional[int] = None
    doc_check_type: Optional[CodeBlockType] = None
    indent: int = 0
    subtype: Optional[str] = None

    for lineno, line in enumerate(content.splitlines()):
        if state == State.INSIDE:
            match = re.match(r"^\S", line)
            if match:
                check_code(filename, block_start, block, doc_check_type, indent, subtype)
                state = State.OUTSIDE
                doc_check_type = None
                indent = 0
                subtype = None
                block_start = None
                block = ""
                found = True
            else:
                block += f"{line}\n"

            # fall-through: continue matching this line as it may already be the start of
            # the next block

        match = re.match(r"^\s*\.\. code-block::", line)
        if match:
            raise CheckDocError(
                (f"{filename}:{lineno}: code-block directive forbidden (use 'code::' instead)")
            )

        match = re.match(r"^\s*\.\. doc-check: (?P<type>\S+)\s*$", line)
        if match:
            state = State.HEADER
            check = match.group("type").split(",")
            doc_check_type = parse_code_block_type(check[0])
            if doc_check_type == CodeBlockType.UNKNOWN:
                raise CheckDocError((f'{filename}:{lineno}: invalid doc-check type "{check[0]}"'))
            if len(check) > 1:
                subtype = check[1]

                # Indent by 3 by default if a subtype is give but no indentation.
                # Most often we test type declaration for which 3 is a valid indentation.
                indent = int(check[2]) if len(check) > 2 else 3

            continue

        match = re.match(r"^\s*\.\. code:: (?P<tag>\S+)\s*$", line)
        if match:
            code_type = parse_code_block_type(match.group("tag"))
            if doc_check_type:
                if (
                    (doc_check_type != CodeBlockType.RFLX or code_type != CodeBlockType.ADA)
                    and doc_check_type != CodeBlockType.IGNORE
                    and doc_check_type != code_type
                ):
                    raise CheckDocError(
                        f"{filename}:{lineno}: "
                        "inconsistent code block type "
                        f"(block: {code_type}, doc: {doc_check_type})"
                    )
            else:
                doc_check_type = code_type

            state = State.HEADER
            continue

        if state == State.HEADER:
            if re.match(r"^ +:[^:]+:$", line):
                continue

            match = re.match("^$", line)
            if not match:
                raise CheckDocError(f"{filename}:{lineno}: missing empty line in code block")

            block_start = lineno
            state = State.INSIDE
            continue

    if state == State.INSIDE:
        check_code(filename, block_start, block, doc_check_type, indent, subtype)
        found = True

    return found


def check_files(files: list[Path]) -> None:

    found = False

    for filename in files:
        with open(filename, "r", encoding="utf-8") as f:
            found = check_file(filename, f.read()) or found

    if not found:
        files_str = ", ".join(str(f) for f in files)
        raise CheckDocError(f"No code blocks found (checked {files_str})")


def check_code(
    filename: Path,
    lineno: Optional[int],
    block: str,
    code_type: Optional[CodeBlockType],
    indent: int,
    subtype: Optional[str] = None,
) -> None:
    assert lineno
    # Remove trailing empty line as this is an error for RecordFlux style checks. It could be
    # filtered  out in the code block parser, but that would complicate things significantly.
    block = textwrap.indent(textwrap.dedent(block).rstrip("\n"), indent * " ")
    try:
        if code_type == CodeBlockType.IGNORE:
            pass
        elif code_type == CodeBlockType.RFLX:
            check_rflx_code(block, subtype)
        elif code_type == CodeBlockType.ADA:
            check_ada_code(block, subtype)
        elif code_type == CodeBlockType.PYTHON:
            check_python_code(block)
        elif code_type == CodeBlockType.YAML:
            check_yaml_code(block)
    except CheckDocError as error:
        raise CheckDocError(f"{filename}:{lineno}: error in code block\n{error}") from error


def parse(data: str, rule: str) -> None:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error, STDIN):
        error.propagate()
    error.extend(style.check_string(data))
    error.propagate()


def check_rflx_code(block: str, subtype: Optional[str] = None) -> None:
    try:
        if subtype is None:
            parser = rflx.specification.Parser()
            parser.parse_string(block)
            parser.create_model()
        else:
            if not hasattr(GrammarRule, f"{subtype}_rule"):
                raise CheckDocError(f'invalid code block subtype "{subtype}"')
            parse(data=block, rule=getattr(GrammarRule, f"{subtype}_rule"))
    except RecordFluxError as rflx_error:
        raise CheckDocError(str(rflx_error)) from rflx_error


def check_ada_code(block: str, subtype: Optional[str] = None) -> None:
    args = []
    unit = "main"

    if subtype is None:
        data = block
    elif subtype == "declaration":
        data = f"procedure {unit.title()} is {block} begin null; end {unit.title()};"
    elif subtype == "api":
        args = ["-gnats", "-gnaty", "-gnatwe"]
        formated_block = textwrap.indent(textwrap.dedent(block), "   ")
        data = f"package {unit.title()} is\n{formated_block}\nend {unit.title()};"
    else:
        raise CheckDocError(f"invalid Ada subtype '{subtype}'")

    with tempfile.TemporaryDirectory() as tmpdirname:

        tmpdir = Path(tmpdirname).resolve()

        (tmpdir / f"{unit}.adb").write_text(data, encoding="utf-8")
        os.symlink(GENERATED_DIR.resolve(), tmpdir / "generated", target_is_directory=True)

        result = subprocess.run(
            ["gprbuild", "-j0", "--no-project", "-q", "-u", "--src-subdirs=generated", unit, *args],
            check=False,
            capture_output=True,
            encoding="utf-8",
            cwd=tmpdir,
        )
        try:
            result.check_returncode()
        except subprocess.CalledProcessError as gprbuild_error:
            raise CheckDocError(result.stderr) from gprbuild_error


def check_python_code(block: str) -> None:

    with tempfile.TemporaryDirectory() as tmpdirname:

        tmpdir = Path(tmpdirname)
        filename = tmpdir / "test.py"
        filename.write_text(block, encoding="utf-8")

        result = subprocess.run(
            ["python3", filename], check=False, capture_output=True, encoding="utf-8"
        )
        try:
            result.check_returncode()
        except subprocess.CalledProcessError as python_error:
            raise CheckDocError(result.stderr) from python_error


def check_yaml_code(block: str) -> None:
    yaml = YAML(typ="safe")
    try:
        yaml.load(block)
    except ParserError as yaml_error:
        raise CheckDocError(f"{yaml_error}") from yaml_error


if __name__ == "__main__":
    try:
        check_files(list(Path("doc").glob("**/*.rst")))
    except CheckDocError as e:
        sys.exit(f"Error checking code blocks: {e}")
