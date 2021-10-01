#!/usr/bin/env python3

"""Check that all development dependencies of RecordFlux are installed."""

import sys

from pkg_resources import DistributionNotFound, get_distribution


def check_dependencies() -> bool:
    result = True

    requirements = get_distribution("RecordFlux").requires(extras=("devel",))
    for r in requirements:
        try:
            pkg = get_distribution(r.project_name)
            if pkg not in r:
                print(f"{r.project_name} has version {pkg.version}, should be {r}")
                result = False
        except DistributionNotFound:
            print(f"{r.project_name} not found")
            result = False

    return result


if __name__ == "__main__":
    if not check_dependencies():
        sys.exit(1)
