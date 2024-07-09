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
        ada.PackageUnit(
            declaration_context=[
                ada.Pragma("Style_Checks", [ada.String("N3aAbCdefhiIklnOprStux")]),
                ada.Pragma(
                    "Warnings",
                    [
                        ada.Variable("Off"),
                        ada.String('""Always_Terminates"" is not a valid aspect identifier'),
                    ],
                ),
            ],
            declaration=ada.PackageDeclaration("P"),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                aspects=[ada.SparkMode(), ada.AlwaysTerminates(ada.TRUE)],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[ada.ModularType("T", ada.Pow(ada.Number(2), ada.Number(8)))],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.ModularType(
                        "T",
                        ada.Number(8),
                        aspects=[ada.Annotate("GNATprove", "No_Wrap_Around")],
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[ada.RangeType("T", first=ada.Number(0), last=ada.Number(255))],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.ExpressionFunctionDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="Expr_Function",
                            return_type="T",
                            parameters=[
                                ada.Parameter(["A"], "T1"),
                                ada.Parameter(["B"], "T2"),
                            ],
                        ),
                        expression=ada.IfExpr(
                            condition_expressions=[
                                (
                                    ada.Less(ada.Variable("A"), ada.Number(1000)),
                                    ada.Number(42),
                                ),
                            ],
                        ),
                        aspects=[ada.Postcondition(ada.TRUE)],
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
    ],
)
def test_roundtrip(unit: ada.Unit) -> None:
    result = ada_parser.parse(unit.ads + unit.adb)
    assert result.ads == unit.ads
    assert result.adb == unit.adb
