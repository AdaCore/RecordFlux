#!/usr/bin/env python3

"""Check that each requirement is referenced at least once in the given directories."""

from __future__ import annotations

import argparse
import itertools
import re
import sys
from collections.abc import Sequence
from pathlib import Path

ID_SEPARATOR = "-"
ID_REGEX = r"§(?:[A-Z0-9]+" + ID_SEPARATOR + r")*[A-Z0-9]+"


class ID:
    def __init__(self, identifier: str):
        if not re.match(ID_REGEX, identifier):
            raise ValueError(f'invalid identifier "{identifier}"')

        self._parts = tuple(p for p in identifier[1:].split(ID_SEPARATOR))

    def __str__(self) -> str:
        return "§" + ID_SEPARATOR.join(str(p) for p in self._parts)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return NotImplemented
        return self.parts < other.parts[: len(self.parts)]

    @property
    def parts(self) -> tuple[str, ...]:
        return self._parts


class Requirement:
    def __init__(
        self,
        identifier: str,
        description: str,
        referenced: bool = False,
        requirements: list[Requirement] | None = None,
    ):
        self._identifier = identifier
        self._description = description.capitalize()
        self._referenced = referenced
        self.requirements = requirements or []

    def __contains__(self, value: str) -> bool:
        return value == self.identifier or any(value in req for req in self.requirements)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return NotImplemented
        return ID(self.identifier) < ID(other.identifier)

    @property
    def identifier(self) -> str:
        return str(self._identifier)

    @property
    def description(self) -> str:
        return self._description

    @property
    def referenced(self) -> bool:
        if self.requirements:
            return all(req.referenced for req in self.requirements)
        return self._referenced

    @referenced.setter
    def referenced(self, value: bool) -> None:
        if self.requirements:
            raise ValueError('"referenced" attribute of meta requirement must not be set')
        self._referenced = value


def main(argv: Sequence[str]) -> bool | str:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("requirements", metavar="REQUIREMENTS_FILE", type=Path)
    arg_parser.add_argument("directories", metavar="DIRECTORY", type=Path, nargs="+")
    arg_parser.add_argument("--checkbox-list", help="print checkbox list", action="store_true")
    args = arg_parser.parse_args(argv[1:])

    requirements, error = parse_requirements(args.requirements)
    if not requirements:
        return "no requirements found"

    references = search_references(args.directories)

    error |= check_references(requirements, references)

    if args.checkbox_list:
        print_checkbox_list(requirements)

    print_statistics(requirements)

    return error


def parse_requirements(requirements_file: Path) -> tuple[list[Requirement], bool]:
    requirements: dict[str, Requirement] = {}
    error = False

    for l in requirements_file.read_text().split("\n"):
        if "§" not in l:
            continue

        match = re.search(r"(?:^|[-#.!?§/*] )([^-#.!?§/*]*)? \[(§[^\]]+)\]", l)
        if match:
            for identifier in re.findall(ID_REGEX, match.group(2)):
                req = Requirement(identifier, match.group(1))
                if req.identifier in requirements or any(
                    identifier == req.identifier for identifier in requirements
                ):
                    print(f'duplicate requirement "{req.identifier}"')  # noqa: T201
                    error = True
                requirements[req.identifier] = req

                if ID_SEPARATOR in req.identifier:
                    parent = req.identifier.rsplit(ID_SEPARATOR, 1)[0]
                    if parent in requirements:
                        requirements[parent].requirements.append(req)
                    else:
                        print(  # noqa: T201
                            f'missing requirement "{parent}" for "{req.identifier}"',
                        )
                        error = True
        else:
            print(f'ignored "{l}"')  # noqa: T201

    return (list(requirements.values()), error)


def search_references(directories: Sequence[Path]) -> list[str]:
    references: list[str] = []

    for d in directories:
        for f in itertools.chain(d.rglob("*.py"), d.rglob("*.rflx")):
            if f.is_file():
                content = f.read_text()
                references.extend(r for r in re.findall(ID_REGEX, content))

    return references


def check_references(requirements: Sequence[Requirement], references: Sequence[str]) -> bool:
    error = False

    for req in requirements:
        if req.identifier in references:
            if not req.requirements:
                req.referenced = True
            else:
                print(f'meta requirement "{req.identifier}" must not be referenced')  # noqa: T201
                error = True

    undefined = [
        ref
        for ref in references
        if all(ref != req.identifier and ref not in req for req in requirements)
    ]
    if undefined:
        for ref in undefined:
            print(f'reference to undefined requirement "{ref}"')  # noqa: T201
        error = True

    for r in [r for r in requirements if not r.referenced and not r.requirements]:
        print(f'unreferenced requirement "{r.identifier}"')  # noqa: T201
        error = True

    return error


def print_checkbox_list(requirements: Sequence[Requirement]) -> None:
    print()  # noqa: T201
    for r in sorted(requirements):
        indentation = "    " * r.identifier.count(ID_SEPARATOR)
        status = "x" if r.referenced else " "
        print(f"{indentation}- [{status}] {r.description} ({r.identifier})")  # noqa: T201
    print()  # noqa: T201


def print_statistics(requirements: Sequence[Requirement]) -> None:
    total = len(requirements)
    referenced = len({r for r in requirements if r.referenced})
    print(f"{referenced} / {total} ({referenced / total * 100:.2f} %)")  # noqa: T201


if __name__ == "__main__":
    sys.exit(main(sys.argv))
