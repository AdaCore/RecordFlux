#!/usr/bin/env -S python3 -O

import argparse
import logging
import pathlib
import sys
from typing import Sequence

import tests.models
from rflx.generator import Generator
from rflx.parser import Parser

logging.basicConfig(level=logging.INFO, format="%(message)s")
logging.disable(logging.NOTSET)


MODELS = [
    tests.models.EXPRESSION_MODEL,
    tests.models.ENUMERATION_MODEL,
    tests.models.ARRAYS_MODEL,
    tests.models.DERIVATION_MODEL,
    tests.models.NULL_MODEL,
    tests.models.TLV_MODEL,
    tests.models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
]

SPECIFICATION_FILES = [
    "specs/ethernet.rflx",
    "specs/in_ethernet.rflx",
    "specs/in_ipv4.rflx",
    "specs/ipv4.rflx",
    "specs/udp.rflx",
]


def main(argv: Sequence[str]) -> int:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument(
        "directory", metavar="DIRECTORY", help="output directory", type=pathlib.Path
    )
    args = arg_parser.parse_args(argv[1:])

    parser = Parser()
    for f in SPECIFICATION_FILES:
        parser.parse(pathlib.Path(f))

    generator = Generator("RFLX", reproducible=True)
    generator.generate(parser.create_model())
    for model in MODELS:
        generator.generate(model)
    generator.write_units(args.directory)
    generator.write_library_files(args.directory)
    generator.write_top_level_package(args.directory)

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
