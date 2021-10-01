#!/usr/bin/env python3

"""
This tool checks the correctness of code examples in the documentation.
"""

import enum
import os
import subprocess
import sys
from pathlib import Path

from librflxlang import AnalysisContext, GrammarRule

import rflx.error
import rflx.specification
from rflx.error import RecordFluxError
from rflx.specification.parser import STDIN, diagnostics_to_error
from tests.const import GENERATED_DIR, SPEC_DIR


class CodeBlockType(enum.Enum):
    UNKNOWN = enum.auto()
    RFLX = enum.auto()
    ADA = enum.auto()
    PYTHON = enum.auto()


class State(enum.Enum):
    OUTSIDE = enum.auto()
    INSIDE = enum.auto()
    HEADER = enum.auto()


def check_code_blocks() -> bool:  # pylint: disable=too-many-branches
    valid = True
    code_blocks = []
    state = State.OUTSIDE

    for p in [Path("README.md")] + list(Path("doc").rglob("*.adoc")):
        is_markdown = p.suffix == ".md"
        if is_markdown:
            prefix = "```"
            separator = "```"
        else:
            prefix = "[source,"
            separator = "----"
        for l in p.read_text():
            if state == State.OUTSIDE and l.startswith(prefix):
                if is_markdown:
                    state = State.INSIDE
                else:
                    state = State.HEADER
                block = ""
                if l.startswith(f"{prefix}ada,rflx"):
                    block_type = CodeBlockType.RFLX
                    subtype = l[len(prefix) + 9 : -2]
                elif l.startswith(f"{prefix}ada"):
                    block_type = CodeBlockType.ADA
                elif l.startswith(f"{prefix}python"):
                    block_type = CodeBlockType.PYTHON
                else:
                    block_type = CodeBlockType.UNKNOWN
                continue

            if state == State.HEADER and l == f"{separator}\n":
                state = State.INSIDE
                continue

            if state == State.INSIDE and l == f"{separator}\n":
                state = State.OUTSIDE
                code_blocks.append((block_type, subtype, block))
                continue

            if state == State.INSIDE:
                block += l

    ("build" / SPEC_DIR.parent).mkdir(parents=True, exist_ok=True)
    os.symlink(os.getcwd() / SPEC_DIR, "build" / SPEC_DIR)
    os.chdir("build")

    for block_type, subtype, block in code_blocks:
        valid = check_code(block, block_type, subtype) and valid

    os.unlink(SPEC_DIR)

    return valid


def check_code(block: str, block_type: CodeBlockType, subtype: str = None) -> bool:
    if block_type == CodeBlockType.RFLX:
        assert subtype is not None
        return check_rflx_code(block, subtype)
    if block_type is CodeBlockType.ADA:
        return check_ada_code(block)
    if block_type is CodeBlockType.PYTHON:
        return check_python_code(block)
    return True


def parse(data: str, rule: GrammarRule) -> None:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error, STDIN):
        error.propagate()


def check_rflx_code(block: str, subtype: str) -> bool:
    try:
        if subtype == "":
            parser = rflx.specification.Parser()
            parser.parse_string(block)
            parser.create_model()
        else:
            if not hasattr(GrammarRule, f"{subtype}_rule"):
                print_error(f'invalid code block subtype "{subtype}"', block)
                return False
            parse(data=block, rule=getattr(GrammarRule, f"{subtype}_rule"))
    except RecordFluxError as e:
        print_error(str(e), block)
        return False
    return True


def check_ada_code(block: str) -> bool:
    valid = True
    unit = "main"

    Path(f"{unit}.adb").write_text(block, encoding="utf-8")

    try:
        subprocess.run(
            ["gprbuild", "-q", "-u", unit, f"--src-subdirs=../{GENERATED_DIR}"], check=True
        )
        os.unlink(f"{unit}.ali")
        os.unlink(f"{unit}.o")
    except subprocess.CalledProcessError:
        valid = False
        print_error("", block)

    os.unlink(f"{unit}.adb")

    return valid


def check_python_code(block: str) -> bool:
    valid = True
    filename = Path("test.py")

    filename.write_text(block, encoding="utf-8")

    try:
        subprocess.run(["python3", filename], check=True)
    except subprocess.CalledProcessError:
        valid = False
        print_error("", block)

    os.unlink(filename)

    return valid


def print_error(message: str, code_block: str) -> None:
    print(f"{message}\n\n```\n{code_block}\n```\n" + "-" * 68 + "\n")


if __name__ == "__main__":
    if not check_code_blocks():
        sys.exit("incorrect code block")
