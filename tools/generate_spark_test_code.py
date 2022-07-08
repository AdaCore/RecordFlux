#!/usr/bin/env -S python3 -O

"""Generate the SPARK code for the test project and all feature tests."""

import filecmp
import logging
import sys
from pathlib import Path

from rflx.generator import Generator
from rflx.integration import Integration
from rflx.specification import Parser
from tests.const import SPEC_DIR
from tests.unit.generator_test import MODELS

logging.basicConfig(level=logging.INFO, format="%(message)s")
logging.disable(logging.NOTSET)

OUTPUT_DIRECTORY = Path("tests/spark/generated")

SPECIFICATION_FILES = [
    SPEC_DIR / "ethernet.rflx",
    SPEC_DIR / "in_ethernet.rflx",
    SPEC_DIR / "in_ipv4.rflx",
    SPEC_DIR / "ipv4.rflx",
    SPEC_DIR / "udp.rflx",
]

SHARED_DIRECTORY = Path("tests/integration/shared")

FEATURE_TESTS = [
    p
    for p in Path("tests/integration").iterdir()
    if p.is_dir() and not p.match("__pycache__") and p != SHARED_DIRECTORY
]


def main() -> int:
    parser = Parser()
    parser.parse(*SPECIFICATION_FILES)

    for model in [parser.create_model(), *MODELS]:
        Generator(
            "RFLX",
            reproducible=True,
            ignore_unsupported_checksum=True,
        ).generate(model, Integration(), OUTPUT_DIRECTORY)

    generate(SHARED_DIRECTORY)
    shared_files = {f.name: f for f in (SHARED_DIRECTORY / "generated").iterdir()}

    for feature_test in FEATURE_TESTS:
        generate(feature_test)
        for f in (feature_test / "generated").iterdir():
            if f.name in shared_files and filecmp.cmp(f, shared_files[f.name]):
                f.unlink()
                f.symlink_to(f"../../shared/generated/{f.name}")

    return 0


def generate(feature_test: Path) -> None:
    output_directory = feature_test / "generated"
    output_directory.mkdir(exist_ok=True)
    parser = Parser()
    parser.parse(feature_test / "test.rflx")
    Generator(
        "RFLX",
        reproducible=True,
        ignore_unsupported_checksum=True,
    ).generate(parser.create_model(), parser.get_integration(), output_directory)


if __name__ == "__main__":
    sys.exit(main())
