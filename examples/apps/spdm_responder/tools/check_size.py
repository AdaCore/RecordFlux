#!/usr/bin/env python3

import argparse
import subprocess
import sys
from pathlib import Path

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("arch", help="target architecture")
    parser.add_argument("file", help="elf file to check", type=Path)
    parser.add_argument("section", help="linker section to check")
    parser.add_argument("size", help="maximum section size", type=int)
    args = parser.parse_args()

    if args.arch in ["riscv32", "riscv64"]:
        prefix = f"{args.arch}-elf-"
    elif args.arch in ["arm"]:
        prefix = f"{args.arch}-eabi-"
    else:
        sys.exit(f"unknown architecture: {args.arch}")

    sections = (
        subprocess.check_output([f"{prefix}size", "-A", args.file]).decode("utf-8").split("\n")
    )
    for section in sections:
        sec = section.split()
        if sec[0] == args.section:
            size = int(sec[1])
            print(size)  # noqa: T201
            sys.exit(size > args.size)
    sys.exit(f"section {args.section} not found")
