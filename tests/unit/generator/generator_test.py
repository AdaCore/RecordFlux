from __future__ import annotations

import textwrap
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

import pytest

from rflx import ada, const as constants, expr
from rflx.common import file_name
from rflx.generator import Generator, const
from rflx.generator.common import Debug
from rflx.generator.message import create_structure
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import Model, type_decl
from rflx.model.message import FINAL, INITIAL, Field, Link, Message
from rflx.rapidflux import Location, RecordFluxError
from tests.const import DATA_DIR, GENERATED_DIR
from tests.data import models
from tests.utils import assert_equal_code

GENERATOR_TEST_DATA_DIR = DATA_DIR / "generator/generated"


@dataclass
class TC:
    name: str
    model: Callable[[], Model]
    integration: Callable[[], Integration]


GENERATOR_TEST_CASES = [
    TC(
        "boolean_variable",
        lambda: Model(
            [
                Message(
                    ID("P::Message", Location((1, 1))),
                    [
                        Link(INITIAL, Field("A"), location=Location((1, 1))),
                        Link(Field("A"), Field("B"), location=Location((1, 1))),
                        Link(
                            Field("B"),
                            FINAL,
                            condition=expr.Variable("A"),
                            location=Location((2, 2)),
                        ),
                    ],
                    {
                        Field(ID("A", location=Location((1, 1)))): type_decl.BOOLEAN,
                        Field(ID("B", location=Location((2, 2)))): type_decl.Integer(
                            "P::T",
                            first=expr.Number(0),
                            last=expr.Number(127),
                            size=expr.Number(7),
                        ),
                    },
                ),
            ],
        ),
        lambda: Integration(),
    ),
]


@pytest.mark.parametrize(("tc"), GENERATOR_TEST_CASES)
def test_equality(tc: TC, tmp_path: Path) -> None:
    assert_equal_code(
        tc.model(),
        tc.integration(),
        GENERATOR_TEST_DATA_DIR / tc.name,
        tmp_path,
    )


def test_invalid_prefix() -> None:
    with pytest.raises(RecordFluxError, match=r'^error: invalid prefix "A..B"$'):
        Generator("A..B")


def test_unsupported_checksum(tmp_path: Path) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(r"^error: unsupported checksum \(consider --ignore-unsupported-checksum option\)$"),
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
        ("RFLX", True, True, {"rflx.ads"} | {f"rflx-{f}" for f in const.LIBRARY_FILES}),
        ("RFLX", True, False, {f"rflx-{f}" for f in const.LIBRARY_FILES}),
        ("RFLX", False, True, {"rflx.ads"}),
        ("RFLX", False, False, set()),
        ("", True, True, set(const.LIBRARY_FILES)),
        ("", True, False, set(const.LIBRARY_FILES)),
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


def test_generate_unused_valid_function_parameter(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(Generator, "_license_header", "")
    types = [
        type_decl.Integer(
            "P::T",
            first=expr.Number(0),
            last=expr.Sub(
                expr.Pow(expr.Number(2), expr.Number(constants.MAX_SCALAR_SIZE)),
                expr.Number(1),
            ),
            size=expr.Number(constants.MAX_SCALAR_SIZE),
        ),
    ]
    Generator().generate(
        Model(types),
        Integration(),
        tmp_path,
        library_files=False,
        top_level_package=False,
    )
    assert (tmp_path / "p.ads").exists()
    assert (tmp_path / "p.ads").read_text() == textwrap.dedent(
        '''\
        pragma Ada_2012;
        pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
        pragma Warnings (Off, "redundant conversion");
        with RFLX_Types;

        package P with
          SPARK_Mode
        is

           type T is range 0 .. 2**63 - 1 with
             Size =>
               63;

           pragma Warnings (Off, "unused variable ""Val""");

           pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

           function Valid_T (Val : RFLX_Types.Base_Integer) return Boolean is
             (True);

           pragma Warnings (On, "formal parameter ""Val"" is not referenced");

           pragma Warnings (On, "unused variable ""Val""");

           function To_Base_Integer (Val : P.T) return RFLX_Types.Base_Integer is
             (RFLX_Types.Base_Integer (Val));

           function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
             (P.T (Val))
            with
             Pre =>
               Valid_T (Val);

        end P;
        ''',
    )


@pytest.mark.parametrize(
    ("always_valid", "expected"),
    [
        (
            False,
            """\
            pragma Ada_2012;
            pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
            pragma Warnings (Off, "redundant conversion");
            with RFLX_Types;

            package P with
              SPARK_Mode
            is

               type T is (E1) with
                 Size =>
                   63;
               for T use (E1 => 1);

               use type RFLX_Types.Base_Integer;

               function Valid_T (Val : RFLX_Types.Base_Integer) return Boolean is
                 (Val in 1);

               function To_Base_Integer (Enum : P.T) return RFLX_Types.Base_Integer is
                 ((case Enum is
                      when E1 =>
                         1));

               pragma Warnings (Off, "unreachable branch");

               function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
                 ((case Val is
                      when 1 =>
                         E1,
                      when others =>
                         P.T'Last))
                with
                 Pre =>
                   Valid_T (Val);

               pragma Warnings (On, "unreachable branch");

            end P;
            """,
        ),
        (
            True,
            """\
            pragma Ada_2012;
            pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
            pragma Warnings (Off, "redundant conversion");
            with RFLX_Types;

            package P with
              SPARK_Mode
            is

               type T_Enum is (E1) with
                 Size =>
                   63;
               for T_Enum use (E1 => 1);

               type T (Known : Boolean := False) is
                  record
                     case Known is
                        when True =>
                           Enum : T_Enum;
                        when False =>
                           Raw : RFLX_Types.Base_Integer;
                     end case;
                  end record;

               function Valid_T (Unused_Val : RFLX_Types.Base_Integer) return Boolean is
                 (True);

               function Valid_T (Val : T) return Boolean is
                 ((if Val.Known then True else Valid_T (Val.Raw) and Val.Raw not in 1));

               function To_Base_Integer (Enum : P.T_Enum) return RFLX_Types.Base_Integer is
                 ((case Enum is
                      when E1 =>
                         1));

               function To_Actual (Enum : T_Enum) return P.T is
                 ((True, Enum));

               function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
                 ((case Val is
                      when 1 =>
                         (True, E1),
                      when others =>
                         (False, Val)))
                with
                 Pre =>
                   Valid_T (Val);

               function To_Base_Integer (Val : P.T) return RFLX_Types.Base_Integer is
                 ((if Val.Known then To_Base_Integer (Val.Enum) else Val.Raw));

            end P;
            """,
        ),
    ],
)
def test_generate_enumeration_base_type_use(
    always_valid: bool,
    expected: str,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(Generator, "_license_header", "")
    types = [
        type_decl.Enumeration(
            "P::T",
            literals=[("E1", expr.Number(1))],
            size=expr.Number(constants.MAX_SCALAR_SIZE),
            always_valid=always_valid,
        ),
    ]
    Generator().generate(
        Model(types),
        Integration(),
        tmp_path,
        library_files=False,
        top_level_package=False,
    )
    assert (tmp_path / "p.ads").exists()
    assert (tmp_path / "p.ads").read_text() == textwrap.dedent(expected)


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
    )
    structure = create_structure("", message)
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
    )
    Generator().generate(Model([message]), Integration(), tmp_path)
