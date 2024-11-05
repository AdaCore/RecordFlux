from __future__ import annotations

from collections.abc import Callable, Mapping
from pathlib import Path
from typing import Final

import pytest

from rflx import ada, expr
from rflx.common import file_name
from rflx.generator import Generator, const
from rflx.generator.common import Debug
from rflx.generator.message import create_structure
from rflx.identifier import ID
from rflx.integration import Integration, IntegrationFile
from rflx.model import (
    Model,
    type_decl,
)
from rflx.model.message import FINAL, INITIAL, Field, Link, Message
from rflx.rapidflux import Location, RecordFluxError
from tests.const import GENERATED_DIR
from tests.data import models
from tests.utils import assert_equal_code


def create_integration(integration_files: Mapping[str, IntegrationFile]) -> Integration:
    integration = Integration()
    for package, integration_file in integration_files.items():
        integration.add_integration_file(package, integration_file)
    return integration


def test_invalid_prefix() -> None:
    with pytest.raises(RecordFluxError, match=r'^error: invalid prefix "A..B"$'):
        Generator("A..B")


def test_unsupported_checksum(tmp_path: Path) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:10:10: error: unsupported checksum"
            r" \(consider --ignore-unsupported-checksum option\)"
            r"$"
        ),
    ):
        Generator().generate(models.tlv_with_checksum_model(), Integration(), tmp_path)


def test_ignore_unsupported_checksum(capfd: pytest.CaptureFixture[str], tmp_path: Path) -> None:
    Generator(ignore_unsupported_checksum=True).generate(
        models.tlv_with_checksum_model(),
        Integration(),
        tmp_path,
    )
    captured = capfd.readouterr()
    assert "warning: unsupported checksum ignored" in captured.err


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_declaration(tmp_path: Path) -> None:
    class TestType(type_decl.TypeDecl):
        pass

    with pytest.raises(AssertionError, match='unexpected declaration "TestType"'):
        Generator().generate(Model([TestType("P::T")]), Integration(), tmp_path)


LIBRARY_FILES: Final[set[str]] = {file_name(str(f)) + ".ads" for f in const.LIBRARY_SPECS} | {
    file_name(str(f)) + ".adb" for f in const.LIBRARY_BODIES
}


@pytest.mark.parametrize(
    ("debug", "debug_expected"),
    [
        (Debug.NONE, set()),
        (Debug.BUILTIN, set()),
        (Debug.EXTERNAL, {"rflx_debug.ads"}),
    ],
)
@pytest.mark.parametrize(
    ("prefix", "library_files", "top_level_package", "expected"),
    [
        ("RFLX", True, True, {"rflx.ads"} | {f"rflx-{f}" for f in LIBRARY_FILES}),
        ("RFLX", True, False, {f"rflx-{f}" for f in LIBRARY_FILES}),
        ("RFLX", False, True, {"rflx.ads"}),
        ("RFLX", False, False, set()),
        ("", True, True, LIBRARY_FILES),
        ("", True, False, LIBRARY_FILES),
        ("", False, True, set()),
        ("", False, False, set()),
    ],
)
def test_generate(  # noqa: PLR0913
    debug: Debug,
    debug_expected: set[str],
    prefix: str,
    library_files: bool,
    top_level_package: bool,
    expected: set[str],
    tmp_path: Path,
) -> None:
    Generator(prefix, reproducible=True, debug=debug).generate(
        Model(),
        Integration(),
        tmp_path,
        library_files,
        top_level_package,
    )
    present = {f.name for f in tmp_path.glob("*.ad?")}
    assert present == expected | (
        {(f"{file_name(prefix)}-{f}" if prefix else f) for f in debug_expected}
        if library_files
        else set()
    )

    if prefix and debug == Debug.NONE:
        for f in present:
            assert (tmp_path / f).read_text() == (GENERATED_DIR / f).read_text(), f"Error in {f}"


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_generate_missing_template_directory(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setattr(const, "TEMPLATE_DIR", tmp_path / "non-existent directory")
    with pytest.raises(AssertionError, match="^template directory not found"):
        Generator().generate(Model(), Integration(), tmp_path)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_generate_missing_template_files(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(const, "TEMPLATE_DIR", tmp_path)
    with pytest.raises(AssertionError, match="^template file not found"):
        Generator().generate(Model(), Integration(), tmp_path)


def test_generate_partial_update(tmp_path: Path) -> None:
    Generator().generate(models.tlv_model(), Integration(), tmp_path)
    Generator().generate(models.tlv_model(), Integration(), tmp_path)
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "error: partial update of generated files\n"
            "note: files not generated in the current run could lead to unexpected"
            " behavior: tlv-message.adb, tlv-message.ads, tlv.ads\n"
            "note: remove the affected files or choose another directory and retry"
            "$"
        ),
    ):
        Generator().generate(models.ethernet_model(), Integration(), tmp_path)


@pytest.mark.parametrize("model", models.spark_test_models())
def test_equality_spark_tests(model: Callable[[], Model], tmp_path: Path) -> None:
    assert_equal_code(model(), Integration(), GENERATED_DIR, tmp_path, accept_extra_files=True)


def test_generate_field_size_optimization() -> None:
    message = Message(
        ID("P::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Length"), location=Location((1, 1))),
            Link(
                Field("Length"),
                Field("Data"),
                size=expr.Add(
                    expr.Size(expr.Variable("Length")),
                    expr.Number(8),
                    location=Location((2, 2)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field("Data"),
                FINAL,
                location=Location((3, 3)),
            ),
        ],
        {
            Field(ID("Length", location=Location((1, 1)))): models.universal_length(),
            Field(ID("Data", location=Location((2, 2)))): type_decl.OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    structure = create_structure(message)
    assert (
        ada.ExpressionFunctionDeclaration(
            specification=ada.FunctionSpecification(
                identifier=ID("Field_Size_Length"),
                parameters=[
                    ada.Parameter(identifiers=[ID("Struct")], type_identifier=ID("Structure")),
                ],
                return_type=ID("RFLX_Types::Bit_Length"),
            ),
            expression=ada.Number(value=16),
        )
        in structure.private
    )


def test_generate_multiple_initial_conditions(tmp_path: Path) -> None:
    message = Message(
        ID("P::Message", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Tag", location=Location((2, 2)))),
                condition=expr.Equal(
                    expr.Variable(ID("P", location=Location((3, 3)))),
                    expr.TRUE,
                    location=Location((3, 3)),
                ),
                location=Location((3, 3)),
            ),
            Link(Field(ID("Tag", location=Location((4, 4)))), FINAL, location=Location((4, 4))),
            Link(
                INITIAL,
                Field(ID("Length", location=Location((5, 5)))),
                condition=expr.Equal(
                    expr.Variable(ID("P", location=Location((6, 6)))),
                    expr.FALSE,
                    location=Location((6, 6)),
                ),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("Length", location=Location((7, 7)))),
                Field(ID("Data", location=Location((8, 8)))),
                size=expr.Add(
                    expr.Size(expr.Variable(ID("Length", location=Location((9, 9))))),
                    expr.Number(8),
                    location=Location((8, 8)),
                ),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("Data", location=Location((10, 10)))),
                FINAL,
                location=Location((10, 10)),
            ),
        ],
        {
            Field(ID("Tag", location=Location((1, 1)))): models.enumeration(),
            Field(ID("Length", location=Location((1, 1)))): models.universal_length(),
            Field(ID("Data", location=Location((1, 1)))): type_decl.OPAQUE,
            Field(ID("P", location=Location((1, 1)))): type_decl.BOOLEAN,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    Generator().generate(Model([message]), Integration(), tmp_path)
