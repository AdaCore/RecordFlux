#!/usr/bin/env python3

import sys

from pkg_resources import DistributionNotFound, get_distribution


def check_dependencies() -> bool:
    result = True

    requirements = get_distribution("RecordFlux").requires(extras=("devel",))
    for r in requirements:
        try:
            pkg = get_distribution(r.name)  # type: ignore
            if r.specifier and not r.specifier.contains(pkg.version):  # type: ignore
                print(
                    f"{r.name} has version {pkg.version}, should be {r.specifier}"  # type: ignore
                )
                result = False
        except DistributionNotFound:
            print(f"{r.name} not found")  # type: ignore
            result = False

    return result


if __name__ == "__main__":
    if not check_dependencies():
        sys.exit(1)
