from __future__ import annotations

import re
from importlib import metadata

import rflx
from rflx import __version__
from rflx.error import fatal_fail


def version() -> str:
    return "\n".join(
        [
            f"RecordFlux {__version__}",
            *dependencies(),
        ],
    )


def dependencies() -> list[str]:
    return [
        f"{r.name} {metadata.version(r.name)}"
        for r in (Requirement(r) for r in metadata.requires("RecordFlux") or [])
        if r.extra != "devel"
    ]


def is_gnat_tracker_release() -> bool:
    """
    Return True iff the current instance of the tool is part of the GNATtracker release track.

    RecordFlux has two release tracks: PyPI and GNATtracker. This function
    determines the active track indirectly from the version number. The PyPI
    track uses semantic versioning while the GNATtracker track does not.
    """
    # Note: The regex below is from https://semver.org/ with one slight
    # difference: the prerelease section is allowed to start also with a '.'
    # instead of '-'. This allows for the intermediate version numbers, such as
    # "0.22.1.dev13+1ef4d7163" that are generated by Poetry Dynamic Versioning
    # also to be categorized as being on the PyPI release track.
    semver_regex = (
        r"^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)"
        r"(?:[-\.](?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)"
        r"(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?"
        r"(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"
    )
    match = re.match(semver_regex, rflx.__version__)
    return not bool(match)


class Requirement:
    def __init__(self, string: str) -> None:
        self.name: str
        self.extra: str | None

        match = re.match(r'([^<=> (]{1,})[^;]*(?: *; extra == [\'"](.*)[\'"])?', string)

        if match:
            groups = match.groups()
            assert len(groups) == 2
            assert isinstance(groups[0], str)
            self.name = groups[0]
            self.extra = groups[1]
        else:
            fatal_fail(f'failed parsing requirement "{string}"')
