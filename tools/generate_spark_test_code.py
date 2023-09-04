#!/usr/bin/env -S python3 -O

"""Generate the SPARK code for the test project and all feature tests."""

from __future__ import annotations

import filecmp
import logging
from pathlib import Path

from rflx.common import unique
from rflx.generator import Generator
from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.const import FEATURE_DIR, SPEC_DIR
from tests.data import models

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

SHARED_DIRECTORY = FEATURE_DIR / "shared"

FEATURE_TESTS = [
    p
    for p in FEATURE_DIR.iterdir()
    if p.is_dir() and not p.match("__pycache__") and p != SHARED_DIRECTORY
]


def main() -> None:
    generate_spark_tests()
    deduplicate_feature_specs()
    generate_feature_tests()


def generate_spark_tests() -> None:
    remove_ada_files(OUTPUT_DIRECTORY)

    parser = Parser()
    parser.parse(*SPECIFICATION_FILES)
    model = merge_models([parser.create_model(), *models.spark_test_models()])
    Generator(
        "RFLX",
        reproducible=True,
        ignore_unsupported_checksum=True,
    ).generate(model, Integration(), OUTPUT_DIRECTORY)


def generate_feature_tests() -> None:
    generate(SHARED_DIRECTORY)
    shared_files = {f.name: f for f in (SHARED_DIRECTORY / "generated").iterdir()}

    for feature_test in FEATURE_TESTS:
        generate(feature_test)
        for f in (feature_test / "generated").iterdir():
            if f.name in shared_files and filecmp.cmp(f, shared_files[f.name]):
                f.unlink()
                f.symlink_to(f"../../shared/generated/{f.name}")


def deduplicate_feature_specs() -> None:
    shared_files = {f.name: f for f in SHARED_DIRECTORY.glob("*.rflx")}

    for feature_test in FEATURE_TESTS:
        for f in feature_test.glob("*.rflx"):
            if f.name in shared_files and filecmp.cmp(f, shared_files[f.name]):
                f.unlink()
                f.symlink_to(f"../shared/{f.name}")


def generate(feature_test: Path) -> None:
    output_directory = feature_test / "generated"
    output_directory.mkdir(exist_ok=True)

    remove_ada_files(output_directory)

    parser = Parser()
    parser.parse(feature_test / "test.rflx")
    Generator(
        "RFLX",
        reproducible=True,
        ignore_unsupported_checksum=True,
    ).generate(parser.create_model(), parser.get_integration(), output_directory)


def remove_ada_files(directory: Path) -> None:
    for f in directory.glob("*.ad?"):
        print(f"Removing {f}")  # noqa: T201
        f.unlink()


def merge_models(models: list[Model]) -> Model:
    return Model(list(unique(d for m in models for d in m.declarations)))


if __name__ == "__main__":
    main()
