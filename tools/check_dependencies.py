#!/usr/bin/env python3

"""Check that all development dependencies of RecordFlux are installed."""

import sys
from importlib.metadata import PackageNotFoundError, requires, version

from packaging.markers import Marker
from packaging.requirements import Requirement


def check_dependencies() -> bool:
    result = True

    for requirement in requires("RecordFlux") or []:
        r = Requirement(requirement)
        if r.marker != Marker('extra == "devel"'):
            continue
        try:
            installed_version = version(r.name)
            if installed_version not in r.specifier:
                print(  # noqa: T201
                    f"{r.name} has version {installed_version}, should be {r.specifier}",
                )
                result = False
        except PackageNotFoundError:
            print(f"{r.name} not found")  # noqa: T201
            result = False

    return result


if __name__ == "__main__":
    if not check_dependencies():
        sys.exit(1)
