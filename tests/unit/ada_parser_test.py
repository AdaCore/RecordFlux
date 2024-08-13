import textwrap

import pytest

from rflx import ada, ada_parser
from rflx.identifier import ID
from rflx.rapidflux import RecordFluxError


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
                        ada.String.escaped('"Always_Terminates" is not a valid aspect identifier'),
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
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            return_type="T",
                        ),
                        aspects=[ada.Precondition(ada.TRUE)],
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
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            return_type="T",
                            parameters=[
                                ada.Parameter(["P1"], "Boolean"),
                                ada.Parameter(["P2"], "Natural"),
                            ],
                        ),
                        aspects=[
                            ada.Precondition(
                                ada.AndThen(
                                    ada.Call(identifier="Is_Valid", arguments=[ada.Variable("P")]),
                                    ada.Greater(ada.Variable("P2"), ada.Number(42)),
                                ),
                            ),
                        ],
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
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            return_type="T",
                            parameters=[
                                ada.Parameter(["P1"], "Boolean"),
                                ada.Parameter(["P2"], "Natural"),
                            ],
                        ),
                        aspects=[
                            ada.Precondition(
                                ada.AndThen(
                                    ada.Less(
                                        ada.Variable("Bits"),
                                        ada.Size(ada.Variable("U64")),
                                    ),
                                    ada.Less(
                                        ada.Variable("Amount"),
                                        ada.Size(ada.Variable("U64")),
                                    ),
                                    ada.Call(
                                        "Fits_Into",
                                        [ada.Variable("V"), ada.Variable("Bits")],
                                    ),
                                ),
                            ),
                        ],
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
                    ada.SubprogramDeclaration(
                        specification=ada.ProcedureSpecification(
                            identifier="P",
                            parameters=[
                                ada.Parameter(["A_Param"], "T1"),
                                ada.InOutParameter(["B_Param"], "T2"),
                            ],
                        ),
                        aspects=[
                            ada.Postcondition(ada.Equal(ada.Variable("B_Param"), ada.Number(42))),
                        ],
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
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            parameters=[
                                ada.Parameter(["P"], "T"),
                            ],
                            return_type="T",
                        ),
                        aspects=[
                            ada.Precondition(
                                ada.In(
                                    ada.Variable("T"),
                                    ada.ValueRange(ada.Number(0), ada.Number(42)),
                                ),
                            ),
                        ],
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
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            parameters=[
                                ada.Parameter(["P"], "T"),
                            ],
                            return_type="T",
                        ),
                        aspects=[
                            ada.Precondition(
                                ada.In(
                                    ada.Variable("T"),
                                    ada.ValueRange(ada.Number(0), ada.Number(42)),
                                ),
                            ),
                        ],
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[ada.RangeType("T", first=ada.Number(0), last=ada.Number(255))],
            ),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            parameters=[
                                ada.Parameter(["P"], "T"),
                            ],
                            return_type="T",
                        ),
                        aspects=[
                            ada.Precondition(
                                ada.In(
                                    ada.Variable("T"),
                                    ada.ValueRange(ada.Number(0), ada.Number(42)),
                                ),
                            ),
                        ],
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[
                    ada.SubprogramBody(
                        specification=ada.FunctionSpecification("F", "T"),
                        declarations=[ada.Pragma("Unreferenced", [ada.Variable("X")])],
                        statements=[ada.ReturnStatement(ada.Variable("Y"))],
                    ),
                    ada.ObjectDeclaration(
                        identifiers=["X"],
                        type_identifier="T",
                        expression=ada.Call("F", [ada.Number(32)]),
                        constant=True,
                    ),
                ],
            ),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P", declarations=[]),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[
                    ada.SubprogramBody(
                        specification=ada.ProcedureSpecification("F", [ada.Parameter(["P"], "T")]),
                        declarations=[],
                        statements=[ada.PragmaStatement("Unreferenced", [ada.Variable("P")])],
                    ),
                ],
            ),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P", declarations=[]),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[
                    ada.SubprogramBody(
                        specification=ada.FunctionSpecification(
                            "F",
                            "Boolean",
                            [ada.Parameter(["P"], "T")],
                        ),
                        declarations=[],
                        statements=[
                            ada.IfStatement(
                                condition_statements=[
                                    (
                                        ada.Equal(ada.Variable("P"), ada.Number(42)),
                                        [ada.ReturnStatement(ada.TRUE)],
                                    ),
                                ],
                                else_statements=[ada.ReturnStatement(ada.FALSE)],
                            ),
                        ],
                    ),
                ],
            ),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            return_type="T",
                        ),
                        aspects=[
                            ada.Ghost(),
                            ada.Global(),
                            ada.Convention(ada.ConventionKind.Intrinsic),
                            ada.Import(),
                        ],
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
                    ada.ExpressionFunctionDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="Fits_Into_Upper",
                            return_type="Boolean",
                            parameters=[
                                ada.Parameter(["V"], "U64"),
                                ada.Parameter(["Bits", "Lower"], "Natural"),
                            ],
                        ),
                        expression=ada.IfExpr(
                            condition_expressions=[
                                (
                                    ada.Less(ada.Variable("Bits"), ada.Size("U64")),
                                    ada.LessEqual(
                                        ada.Variable("V"),
                                        ada.Sub(
                                            ada.Pow(ada.Number(2), ada.Variable("Bits")),
                                            ada.Pow(ada.Number(2), ada.Variable("Lower")),
                                        ),
                                    ),
                                ),
                                (
                                    ada.AndThen(
                                        ada.Greater(ada.Variable("Lower"), ada.Number(0)),
                                        ada.Less(ada.Variable("Lower"), ada.Size("U64")),
                                    ),
                                    ada.LessEqual(
                                        ada.Variable("V"),
                                        ada.Sub(
                                            ada.Last("U64"),
                                            ada.Add(
                                                ada.Pow(ada.Number(2), ada.Variable("Lower")),
                                                ada.Number(1),
                                            ),
                                        ),
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
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.GenericPackageInstantiation("P", "G", ["A", "B", "F.C"]),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.PlainDerivedType(
                        "T",
                        "Natural",
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
                    ada.DerivedRangeType(
                        identifier="T",
                        type_identifier="Natural",
                        first=ada.Number(2),
                        last=ada.Number(42),
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
                    ada.AccessType(
                        identifier="P",
                        object_identifier="T",
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
                declarations=[],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[ada.PrivateType("PT")],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[
                ada.SubprogramDeclaration(
                    ada.ProcedureSpecification("P", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[
                ada.SubprogramDeclaration(
                    ada.FunctionSpecification("F", "U", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[
                ada.PrivateType("PT"),
                ada.SubprogramDeclaration(
                    ada.ProcedureSpecification("P", [ada.Parameter(["P1"], "T")]),
                ),
                ada.SubprogramDeclaration(
                    ada.FunctionSpecification("F", "U", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.SubprogramDeclaration(
                        ada.ProcedureSpecification(
                            "P",
                            [ada.Parameter(["P1"], "T", ada.Number(42))],
                        ),
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P"),
            body_context=[],
            body=ada.PackageBody("P"),
            formal_parameters=[ada.PrivateType("PT", discriminants=[ada.Discriminant(["D"], "T")])],
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration(
                "P",
                declarations=[
                    ada.UseTypeClause("T"),
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
                    ada.PrivateType("PT"),
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
                    ada.PrivateType("PT", discriminants=[ada.Discriminant(["D"], "T")]),
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
                    ada.PrivateType(
                        "PT",
                        discriminants=[ada.Discriminant(["D"], "T", ada.Number(42))],
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
                    ada.SubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            identifier="F",
                            return_type="T",
                            parameters=[ada.Parameter("C", "T")],
                        ),
                        aspects=[ada.Precondition(ada.Not(ada.Constrained("C")))],
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
                    ada.SubprogramDeclaration(
                        specification=ada.ProcedureSpecification(
                            identifier="F",
                            parameters=[ada.OutParameter("C", "T"), ada.InOutParameter("B", "T")],
                        ),
                        aspects=[ada.Depends({"C": ["B"], "B": "null"})],
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
                    ada.PrivateType("PT"),
                ],
                private_declarations=[
                    ada.RangeType("PT", ada.Number(1), ada.Number(42)),
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
                    ada.EnumerationType(
                        "Enum",
                        literals={ID("E1"): None, ID("E2"): None, ID("E3"): None},
                        size=ada.Number(8),
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
                    ada.RecordType(
                        "R",
                        components=[
                            ada.Component("C1", "T", ada.Number(42)),
                            ada.Component("C2", "U"),
                        ],
                        discriminants=[
                            ada.Discriminant(["D1"], "T", ada.Number(0)),
                            ada.Discriminant(["D2"], "T"),
                        ],
                    ),
                ],
            ),
            body_context=[],
            body=ada.PackageBody("P"),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P"),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[
                    ada.SubprogramBody(
                        specification=ada.ProcedureSpecification("F", [ada.Parameter(["P"], "T")]),
                        declarations=[],
                        statements=[ada.CallStatement("G", [ada.Variable("P")])],
                    ),
                ],
            ),
        ),
        ada.PackageUnit(
            declaration_context=[],
            declaration=ada.PackageDeclaration("P"),
            body_context=[],
            body=ada.PackageBody(
                "P",
                declarations=[
                    ada.SubprogramBody(
                        specification=ada.ProcedureSpecification(
                            "F",
                            [ada.OutParameter(["P"], "T")],
                        ),
                        declarations=[],
                        statements=[ada.Assignment("P", ada.Number(42))],
                    ),
                ],
            ),
        ),
    ],
)
def test_roundtrip_model(unit: ada.Unit) -> None:
    result = ada_parser.parse(unit.ads + unit.adb)
    assert result.ads == unit.ads
    assert result.adb == unit.adb


@pytest.mark.parametrize(
    ("data"),
    [
        """\
        package P
        is

        end P;
        """,
        """\
        package P
        is

           function F (P : Boolean) return Natural is
             (if P then 0 else 42);

        end P;
        """,
        """\
        package P
        is

           pragma Assert (if X > 5 then True else False);

        end P;
        """,
        """\
        package P
        is

           function Fits_Into_Upper (V : U64; Bits, Lower : Natural) return Boolean is
             (if
                  Bits < U64'Size
               then
                  V <= 2**Bits - 2**Lower
               elsif
                  Lower > 0
                  and then Lower < U64'Size
               then
                  V <= U64'Last - 2**Lower + 1);

        end P;
        """,
        """\
        package P
        is

        end P;
        package body P
        is

           function Mask_Upper (V : U64; Mask : Natural) return U64 is
           begin
              return V
                     and 2**Mask - 1;
           end Mask_Upper;

        end P;
        """,
        """\
        with F;
        with G;

        package P is new G (F.A, F.B, F.C);
        """,
        """\
        package P
        is

           function F (Val : Integer) return Boolean is
             (case Val is
                  when 0 =>
                     False,
                  when 1 =>
                     True,
                  when others =>
                     False);

        end P;
        """,
        """\
        package P
        is

           type T is new Natural;

        end P;
        """,
        """\
        package P
        is

           type T is new Natural range 1 .. 42;

        end P;
        """,
        """\
        package P
        is

           type A is array (I range <>) of B;

        end P;
        """,
        """\
        package P
        is

           type P is access T;

        end P;
        """,
        """\
        package P
        is

           type T is range 1 .. U'Last * 8;

        end P;
        """,
        """\
        generic
        package P
        is

        end P;
        """,
        """\
        generic
           type PT is private;
        package P
        is

        end P;
        """,
        """\
        generic
           with procedure P (P1 : T);
        package P
        is

        end P;
        """,
        """\
        generic
           with function F (P1 : T) return U;
        package P
        is

        end P;
        """,
        """\
        generic
           type PT is private;
           with procedure P (P1 : T);
           with function F (P1 : T) return U;
        package P
        is

        end P;
        """,
        """\
        generic
           with function F (P1 : T := 42) return U;
        package P
        is

        end P;
        """,
        """\
        generic
           type PT (D : T) is private;
        package P
        is

        end P;
        """,
        """\
        package P
        is

           use type T;

        end P;
        """,
        """\
        package P
        is

           type PT is private;

        end P;
        """,
        """\
        package P
        is

           type PT (D1 : T; D2 : T) is private;

        end P;
        """,
        """\
        package P
        is

           type PT (D1 : T; D2 : T := 42) is private;

        end P;
        """,
        """\
        package P
        is

           procedure F (C : T) with
             Pre =>
               not C'Constrained;

        end P;
        """,
        """\
        package P
        is

           procedure F (C : out T; B : in out T) with
             Depends =>
               (C => B, B => null);

        end P;
        """,
        """\
        package P
        is

           procedure F (C : T) with
             Post =>
               P (C) = P (C)'Old;

        end P;
        """,
        """\
        package P
        is

           procedure S (C : T) with
             Pre =>
               P (C) = L'(if V (C) then E (C) else F (C))'Old;

        end P;
        """,
        """\
        package P
        is

           procedure S (C : T) with
             Contract_Cases =>
               (P (C) =>
                   True,
                others =>
                   False);

        end P;
        """,
        """\
        package P
        is

           procedure P (C : T) with
             Global =>
               null;

        end P;
        """,
        """\
        package P
        is

           G1, G2 : Integer;

           procedure P (C : T) with
             Global =>
               (Input => G1, In_Out => G2);

        end P;
        """,
        """\
        package P
        is

           procedure P (C : T; D : in out T; E : out T) with
             Depends =>
               (D => (D, C), E => C);

        end P;
        """,
        """\
        package P with
          Always_Terminates
        is

        end P;
        """,
        """\
        package P
        is

           type T (D : U) is private with
             Default_Initial_Condition =>
               P (D)
               and Q (D);

        end P;
        """,
        """\
        package P
        is

           type T is private;

        private

           type T is range 0 .. 42;

        end P;
        """,
        """\
        package P
        is

           type Color is (Red, Green, Blue);

        end P;
        """,
        """\
        package P
        is

           type Color is (Red, Green, Blue) with
             Size =>
               8;

        end P;
        """,
        """\
        package P
        is

           type R is
              record
                 F : T;
              end record;

        end P;
        """,
        """\
        package P
        is

           type N is null record;

        end P;
        """,
        """\
        package P
        is

           type T (D : U) is private with
             Dynamic_Predicate =>
               P (D)
               and Q (D);

        end P;
        """,
        """\
        package P
        is

        end P;
        package body P
        is

           procedure Proc (V : T) is
           begin
              C (V);
           end Proc;

        end P;
        """,
        """\
        package P
        is

        end P;
        package body P
        is

           procedure Proc (R : out T) is
           begin
              R := 42;
           end Proc;

        end P;
        """,
    ],
)
def test_roundtrip_text(data: str) -> None:
    data = textwrap.dedent(data)
    result = ada_parser.parse(data)
    assert result.ads + result.adb == data


@pytest.mark.parametrize(
    ("data", "message"),
    [
        (
            """\
        package P
        is

           type R is record
             F1, F2 : T;
           end record;

        end P;
        """,
            "<stdin>:5:6: error: identifier lists unsupported for record component declaration",
        ),
    ],
)
def test_parse_error(data: str, message: str) -> None:
    data = textwrap.dedent(data)
    with pytest.raises(RecordFluxError, match=rf"^{message}$"):
        ada_parser.parse(data)
