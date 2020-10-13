#!/usr/bin/env python3

import enum
import os
import pathlib
import subprocess
import sys

import pyparsing

import rflx.specification
from tests.const import GENERATED_DIR, SPEC_DIR


class CodeBlockType(enum.Enum):
    UNKNOWN = 0
    RFLX = 1
    ADA = 2
    PYTHON = 3


def check_code_blocks() -> bool:
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
    if block_type is CodeBlockType.RFLX:
        return check_rflx_code(block)
    if block_type is CodeBlockType.ADA:
        return check_ada_code(block)
    if block_type is CodeBlockType.PYTHON:
        return check_python_code(block)
    return True


def check_rflx_code(block: str) -> bool:
    valid = True

    try:
        rflx.specification.Parser().parse_string(block)
    except (pyparsing.ParseException, pyparsing.ParseFatalException) as e:
        valid = False
        print(pyparsing.ParseException.explain(e, 0))
        print(f"\naffected code block:\n\n{block}")

    return valid


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
