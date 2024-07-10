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
                declarations=[
                    ada.ModularType("T1", ada.Number(8)),
                    ada.ModularType("T2", ada.Pow(ada.Number(2), ada.Number(8))),
                    ada.ModularType(
                        "T3",
                        ada.Sub(ada.Pow(ada.Number(2), ada.Number(8)), ada.Number(1)),
                    ),
                    ada.ModularType(
                        "T4",
                        ada.Add(ada.Number(1), ada.Number(2), ada.Number(3), ada.Number(4)),
                    ),
                    ada.ModularType(
                        "T5",
                        ada.Sub(
                            ada.Sub(
                                ada.Add(ada.Number(1), ada.Number(2), ada.Number(3), ada.Number(4)),
                                ada.Number(1),
                            ),
                            ada.Number(1),
                        ),
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
                                    ada.AndThen(
                                        ada.Less(
                                            ada.Variable("Bits"),
                                            ada.Size(ada.Variable("U64")),
                                        ),
                                        ada.Greater(ada.Variable("Bits"), ada.Number(1)),
                                    ),
                                    ada.Less(
                                        ada.Variable("V"),
                                        ada.Pow(ada.Number(2), ada.Variable("Bits")),
                                    ),
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
