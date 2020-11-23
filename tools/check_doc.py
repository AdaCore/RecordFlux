#!/usr/bin/env python3

"""
This tool checks the correctness of code examples in the documentation.
"""

import enum
import os
import pathlib
import subprocess
import sys

from librecordfluxdsllang import AnalysisContext

import rflx.error
import rflx.specification
from rflx.error import RecordFluxError
from rflx.specification.parser import GrammarRule, diagnostics_to_error
from tests.const import GENERATED_DIR, SPEC_DIR


class CodeBlockType(enum.Enum):
    UNKNOWN = enum.auto()
    RFLX = enum.auto()
    RFLX_PARTIAL = enum.auto()
    RFLX_CONTEXT = enum.auto()
    RFLX_DECLARATION = enum.auto()
    ADA = enum.auto()
    PYTHON = enum.auto()


def check_code_blocks() -> bool:
    # pylint: disable=too-many-branches
    valid = True
    code_blocks = []
    inside = False

    for p in [pathlib.Path("README.md")] + list(pathlib.Path("doc").rglob("*.md")):
        with open(p) as f:
            for l in f:
                if not inside and l.startswith("```"):
                    inside = True
                    block = ""
                    if l.endswith("Ada RFLX\n"):
                        block_type = CodeBlockType.RFLX
                    elif l.endswith("Ada RFLX partial\n"):
                        block_type = CodeBlockType.RFLX_PARTIAL
                    elif l.endswith("Ada RFLX context\n"):
                        block_type = CodeBlockType.RFLX_CONTEXT
                    elif l.endswith("Ada RFLX declaration\n"):
                        block_type = CodeBlockType.RFLX_DECLARATION
                    elif l.endswith("Ada\n"):
                        block_type = CodeBlockType.ADA
                    elif l.endswith("Python\n"):
                        block_type = CodeBlockType.PYTHON
                    else:
                        block_type = CodeBlockType.UNKNOWN
                    continue

                if inside and l.startswith("```"):
                    inside = False
                    code_blocks.append((block_type, block))
                    continue

                if inside:
                    block += l

    ("build" / SPEC_DIR.parent).mkdir(parents=True, exist_ok=True)
    os.symlink(os.getcwd() / SPEC_DIR, "build" / SPEC_DIR)
    os.chdir("build")

    for block_type, block in code_blocks:
        valid = check_code(block, block_type) and valid

    os.unlink(SPEC_DIR)

    return valid


def check_code(block: str, block_type: CodeBlockType) -> bool:
    if block_type in [
        CodeBlockType.RFLX,
        CodeBlockType.RFLX_PARTIAL,
        CodeBlockType.RFLX_CONTEXT,
        CodeBlockType.RFLX_DECLARATION,
    ]:
        return check_rflx_code(block, block_type)
    if block_type is CodeBlockType.ADA:
        return check_ada_code(block)
    if block_type is CodeBlockType.PYTHON:
        return check_python_code(block)
    return True


def parse(data: str, rule: GrammarRule) -> None:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error):
        error.propagate()


def check_rflx_code(block: str, block_type: CodeBlockType) -> bool:
    try:
        if block_type == CodeBlockType.RFLX:
            parser = rflx.specification.Parser()
            parser.parse_string(block)
            parser.create_model()
        elif block_type == CodeBlockType.RFLX_PARTIAL:
            parse(data=block, rule=GrammarRule.specification_rule)
        elif block_type == CodeBlockType.RFLX_CONTEXT:
            parse(data=block, rule=GrammarRule.context_clause_rule)
        elif block_type == CodeBlockType.RFLX_DECLARATION:
            parse(data=block, rule=GrammarRule.basic_declarations_rule)
    except RecordFluxError as e:
        print(f"{e}\n\naffected code block:\n\n{block}")
        return False
    return True


def check_ada_code(block: str) -> bool:
    valid = True
    unit = "main"

    with open(f"{unit}.adb", "w") as f:
        f.write(block)

    try:
        subprocess.run(
            ["gprbuild", "-q", "-u", unit, f"--src-subdirs=../{GENERATED_DIR}"], check=True
        )
        os.unlink(f"{unit}.ali")
        os.unlink(f"{unit}.o")
    except subprocess.CalledProcessError:
        valid = False
        print(f"\naffected code block:\n\n{block}")

    os.unlink(f"{unit}.adb")

    return valid


def check_python_code(block: str) -> bool:
    valid = True
    filename = "test.py"

    with open(filename, "w") as f:
        f.write(block)

    try:
        subprocess.run(["python3", filename], check=True)
    except subprocess.CalledProcessError:
        valid = False
        print(f"\naffected code block:\n\n{block}")

    os.unlink(filename)

    return valid


if __name__ == "__main__":
    if not check_code_blocks():
        sys.exit("incorrect code block")
