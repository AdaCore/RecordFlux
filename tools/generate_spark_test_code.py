#!/usr/bin/env -S python3 -O

"""Generate the SPARK code for the test project and all feature tests."""

import logging
import sys
from pathlib import Path

import tests.data.models
from rflx.generator import Generator
from rflx.model import Model
from rflx.specification import Parser
from tests.const import SPEC_DIR

logging.basicConfig(level=logging.INFO, format="%(message)s")
logging.disable(logging.NOTSET)

OUTPUT_DIRECTORY = Path("tests/spark/generated")

MODELS = [
    tests.data.models.EXPRESSION_MODEL,
    tests.data.models.ENUMERATION_MODEL,
    tests.data.models.SEQUENCE_MODEL,
    tests.data.models.DERIVATION_MODEL,
    tests.data.models.NULL_MODEL,
    tests.data.models.TLV_MODEL,
    tests.data.models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
    Model(tests.data.models.FIXED_SIZE_SIMPLE_MESSAGE.dependencies),
]

SPECIFICATION_FILES = [
    SPEC_DIR / "ethernet.rflx",
    SPEC_DIR / "in_ethernet.rflx",
    SPEC_DIR / "in_ipv4.rflx",
    SPEC_DIR / "ipv4.rflx",
    SPEC_DIR / "udp.rflx",
]

FEATURE_TESTS = [p for p in Path("tests/integration").iterdir() if p.is_dir()]


def main() -> int:
    parser = Parser()
    parser.parse(*SPECIFICATION_FILES)

    for model in [parser.create_model(), *MODELS]:
        generator = Generator(model, "RFLX", reproducible=True, ignore_unsupported_checksum=True)
        generator.write_units(OUTPUT_DIRECTORY)
        generator.write_library_files(OUTPUT_DIRECTORY)
        generator.write_top_level_package(OUTPUT_DIRECTORY)

    for feature_test in FEATURE_TESTS:
        output_directory = feature_test / "generated"
        output_directory.mkdir(exist_ok=True)
        parser = Parser()
        parser.parse(feature_test / "test.rflx")
        generator = Generator(
            parser.create_model(), "RFLX", reproducible=True, ignore_unsupported_checksum=True
        )
        generator.write_units(output_directory)
        generator.write_library_files(output_directory)
        generator.write_top_level_package(output_directory)

    return 0


if __name__ == "__main__":
    sys.exit(main())
