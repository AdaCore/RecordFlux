#!/usr/bin/env python3

"""Check that the minimum required version of pip is installed."""

import sys

import pip
from pkg_resources import parse_version

MINIMUM_REQUIRED_VERSION = "22.2"

if __name__ == "__main__":
    if parse_version(pip.__version__) < parse_version(MINIMUM_REQUIRED_VERSION):
        sys.exit(
            f"minimum required version of pip is {MINIMUM_REQUIRED_VERSION},"
            f" currently installed version is {pip.__version__}"
        )
