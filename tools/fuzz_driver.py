#!/usr/bin/env python3

import argparse
import random
import sys
from pathlib import Path

from cobrafuzz.fuzzer import Fuzzer

from rflx import error
from rflx.model import Cache, Digest
from rflx.specification import parser


class SilentlyNeverVerify(Cache):
    def __init__(self) -> None:
        pass

    def is_verified(self, _: Digest) -> bool:
        return True

    def add_verified(self, digest: Digest) -> None:
        pass


def fuzz(buf: bytes) -> None:
    try:
        string = buf.decode("utf-8")
        p = parser.Parser(cache=SilentlyNeverVerify())
        p.parse_string(string)
        p.create_model()
    except (UnicodeDecodeError, error.RecordFluxError):
        pass
    except KeyboardInterrupt:  # pragma: no cover
        sys.exit()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--state-dir",
        type=Path,
        required=True,
        help="Directory to hold fuzzer state",
    )
    parser.add_argument(
        "--corpus-dir",
        type=Path,
        required=True,
        help="Directory to extract corpus from",
    )
    parser.add_argument(
        "--artifact-file",
        type=Path,
        help="Store single artifact in given file",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=-1,
        help="Maxium number of runs",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=30,
        help="Time after which an execution is considered a hang",
    )
    arguments = parser.parse_args()

    corpus = [Path(p) for p in arguments.corpus_dir.glob("**/*.rflx")]
    random.shuffle(corpus)

    fuzzer = Fuzzer(
        fuzz,
        dirs=[Path(arguments.state_dir), *corpus],
        timeout=arguments.timeout,
        runs=arguments.runs,
        exact_artifact_path=arguments.artifact_file,
    )
    fuzzer.start()


if __name__ == "__main__":
    main()  # pragma: no cover
