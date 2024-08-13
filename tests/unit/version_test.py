from __future__ import annotations

import pytest

import rflx
from rflx import __version__, version
from rflx.error import FatalError
from rflx.version import is_gnat_tracker_release


def test_version() -> None:
    assert version.version().startswith(f"RecordFlux {__version__}")


@pytest.mark.parametrize(
    ("rflx_version", "expected"),
    [
        ("0.22.1", False),
        ("0.22.1.dev13+1ef4d7163", False),
        ("0.22.1-dev13+1ef4d7163", False),
        ("25.1.0", False),
        ("25.0", True),
        ("25.dev20240804", True),
        ("27.1", True),
        ("27.2", True),
    ],
)
def test_is_gnat_tracker_release(
    rflx_version: str,
    expected: bool,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(rflx, "__version__", rflx_version)
    assert is_gnat_tracker_release() == expected


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
def test_requirement(requirement: str, name: str, extra: str | None) -> None:
    r = version.Requirement(requirement)
    assert r.name == name
    assert r.extra == extra


def test_requirement_error() -> None:
    with pytest.raises(
        FatalError,
        match=r'^error: failed parsing requirement "\(<2,>=1\) pydantic"$',
    ):
        version.Requirement("(<2,>=1) pydantic")
