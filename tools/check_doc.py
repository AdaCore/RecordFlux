#!/usr/bin/env python3

import enum
import os
import pathlib
import subprocess
import sys

import pyparsing

import rflx.parser


class CodeBlockType(enum.Enum):
    RFLX = 1
    ADA = 2
    UNKNOWN = 3


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
                    else:
                        block_type = CodeBlockType.UNKNOWN
                    continue

                if inside and l.startswith("```"):
                    inside = False
                    code_blocks.append((block_type, block))
                    continue

                if inside:
                    block += l

    pathlib.Path("build").mkdir(exist_ok=True)
    os.chdir("build")

    for block_type, block in code_blocks:
        if block_type is CodeBlockType.RFLX:
            valid = check_rflx_code(block) and valid
        if block_type is CodeBlockType.ADA:
            valid = check_ada_code(block) and valid

    return valid


def check_rflx_code(block: str) -> bool:
    valid = True

    try:
        rflx.parser.Parser().parse_string(block)
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
        subprocess.run(["gprbuild", "-q", "-u", unit, "--src-subdirs=../generated"], check=True)
        os.unlink(f"{unit}.ali")
        os.unlink(f"{unit}.o")
    except subprocess.CalledProcessError:
        valid = False
        print(f"\naffected code block:\n\n{block}")

    os.unlink(f"{unit}.adb")

    return valid


if __name__ == "__main__":
    if not check_code_blocks():
        sys.exit("incorrect code block")
