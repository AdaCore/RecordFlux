import pytest

from rflx import ada, ada_parser


@pytest.mark.parametrize(
    ("unit"),
    [
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P"),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("A.B.C"),
            body_context=[],
            body=ada.PackageBody("A.B.C"),
        ),
    ],
)
def test_roundtrip(unit: ada.Unit):
    result = ada_parser.parse(unit.ads + unit.adb)
    assert result.ads == unit.ads
    assert result.adb == unit.adb
