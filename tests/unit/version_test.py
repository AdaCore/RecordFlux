from __future__ import annotations

from typing import Optional

import pytest

from rflx import __version__, version
from rflx.error import FatalError


def test_version() -> None:
    assert version.version().startswith(f"RecordFlux {__version__}")


@pytest.mark.parametrize(
    ("requirement", "name", "extra"),
    [
        ("pydantic (<2,>=1)", "pydantic", None),
        ("pydantic<2,>=1", "pydantic", None),
        ("ruamel.yaml (<0.18,>=0.17)", "ruamel.yaml", None),
        ("ruamel.yaml<0.18,>=0.17", "ruamel.yaml", None),
        ("setuptools-scm (<8,>=6.2) ; extra == 'devel'", "setuptools-scm", "devel"),
        ('setuptools_scm<8,>=6.2; extra == "devel"', "setuptools_scm", "devel"),
    ],
)
def test_requirement(requirement: str, name: str, extra: Optional[str]) -> None:
    r = version.Requirement(requirement)
    assert r.name == name
    assert r.extra == extra


def test_requirement_error() -> None:
    with pytest.raises(
        FatalError,
        match=r'^error: failed parsing requirement "\(<2,>=1\) pydantic"$',
    ):
        version.Requirement("(<2,>=1) pydantic")
