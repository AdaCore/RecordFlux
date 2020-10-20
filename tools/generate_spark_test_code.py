#!/usr/bin/env -S python3 -O

import argparse
import logging
import pathlib
import sys
from typing import Sequence

import tests.data.models
from rflx.generator import Generator
from rflx.specification import Parser
from tests.const import EX_SPEC_DIR

logging.basicConfig(level=logging.INFO, format="%(message)s")
logging.disable(logging.NOTSET)


MODELS = [
    tests.data.models.EXPRESSION_MODEL,
    tests.data.models.ENUMERATION_MODEL,
    tests.data.models.ARRAYS_MODEL,
    tests.data.models.DERIVATION_MODEL,
    tests.data.models.NULL_MODEL,
    tests.data.models.TLV_MODEL,
    tests.data.models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
]

SPECIFICATION_FILES = [
    EX_SPEC_DIR / "ethernet.rflx",
    EX_SPEC_DIR / "in_ethernet.rflx",
    EX_SPEC_DIR / "in_ipv4.rflx",
    EX_SPEC_DIR / "ipv4.rflx",
    EX_SPEC_DIR / "udp.rflx",
]


def main(argv: Sequence[str]) -> int:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument(
        "directory", metavar="DIRECTORY", help="output directory", type=pathlib.Path
    )
    args = arg_parser.parse_args(argv[1:])

    parser = Parser()
    parser.parse(*SPECIFICATION_FILES)

    for model in [parser.create_model(), *MODELS]:
        generator = Generator(model, "RFLX", reproducible=True)
        generator.write_units(args.directory)
        generator.write_library_files(args.directory)
        generator.write_top_level_package(args.directory)

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
