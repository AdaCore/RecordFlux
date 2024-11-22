from __future__ import annotations

import textwrap

import lark
import pytest

from rflx import ada, ada_parser
from rflx.identifier import ID
from rflx.rapidflux import RecordFluxError


def package(  # noqa: PLR0913
    name: str = "P",
    declaration: ada.PackageDeclaration | None = None,
    declaration_context: list[ada.ContextItem] | None = None,
    declaration_aspects: list[ada.Aspect] | None = None,
    declaration_declarations: list[ada.Declaration] | None = None,
    declaration_private_declarations: list[ada.Declaration] | None = None,
    body_declarations: list[ada.Declaration] | None = None,
    formal_parameters: (
        list[
            ada.FormalSubprogramDeclaration
            | ada.TypeDeclaration
            | ada.FormalTypeDeclaration
            | ada.FormalPackageDeclaration
        ]
        | None
    ) = None,
) -> ada.PackageUnit:
    return ada.PackageUnit(
        declaration_context=declaration_context or [],
        declaration=declaration
        or ada.PackageDeclaration(
            name,
            declarations=declaration_declarations,
            private_declarations=declaration_private_declarations,
            aspects=declaration_aspects,
        ),
        body_context=[],
        body=ada.PackageBody(
            name,
            declarations=body_declarations,
        ),
        formal_parameters=formal_parameters,
    )


def function_declaration(
    parameters: list[ada.Parameter] | None = None,
    aspects: list[ada.Aspect] | None = None,
) -> ada.PackageUnit:
    return package(
        declaration_declarations=[
            ada.SubprogramDeclaration(
                specification=ada.FunctionSpecification(
                    identifier="S",
                    return_type="T",
                    parameters=parameters,
                ),
                aspects=aspects,
            ),
        ],
    )


def procedure_body(
    statements: list[ada.Statement],
    parameters: list[ada.Parameter] | None = None,
) -> ada.PackageUnit:
    return package(
        body_declarations=[
            ada.SubprogramBody(
                specification=ada.ProcedureSpecification("S", parameters=parameters),
                declarations=[],
                statements=statements,
            ),
        ],
    )


@pytest.mark.parametrize(
    ("unit"),
    [
        package(),
        package(name="A.B.C"),
        package(
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
        ),
        package(declaration_aspects=[ada.SparkMode(), ada.AlwaysTerminates(ada.TRUE)]),
        package(declaration_aspects=[ada.SparkMode(off=True)]),
        package(declaration_aspects=[ada.SparkMode(off=False)]),
        package(
            declaration_declarations=[
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
                ada.ModularType("T6", ada.Rem(ada.Number(15), ada.Number(8))),
            ],
        ),
        package(
            declaration_declarations=[
                ada.ModularType(
                    "T",
                    ada.Number(8),
                    aspects=[ada.Annotate("GNATprove", "No_Wrap_Around")],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(255)),
            ],
        ),
        package(
            declaration_declarations=[
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
        function_declaration(aspects=[ada.Precondition(ada.TRUE)]),
        function_declaration(
            parameters=[
                ada.Parameter(["P1"], "Boolean"),
                ada.Parameter(["P2"], "Natural"),
            ],
            aspects=[
                ada.Precondition(
                    ada.AndThen(
                        ada.Call(identifier="Is_Valid", arguments=[ada.Variable("P")]),
                        ada.Greater(ada.Variable("P2"), ada.Number(42)),
                    ),
                ),
            ],
        ),
        function_declaration(
            parameters=[
                ada.Parameter(["P1"], "Boolean"),
                ada.Parameter(["P2"], "Natural"),
            ],
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
        function_declaration(
            parameters=[
                ada.Parameter(["A_Param"], "T1"),
                ada.InOutParameter(["B_Param"], "T2"),
            ],
            aspects=[
                ada.Postcondition(ada.Equal(ada.Variable("B_Param"), ada.Number(42))),
            ],
        ),
        function_declaration(
            parameters=[
                ada.Parameter(["P"], "T"),
            ],
            aspects=[
                ada.Precondition(
                    ada.In(
                        ada.Variable("T"),
                        ada.ValueRange(ada.Number(0), ada.Number(42)),
                    ),
                ),
            ],
        ),
        package(
            declaration_declarations=[
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
            body_declarations=[
                ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(255)),
            ],
        ),
        package(
            declaration_declarations=[
                ada.AbstractSubprogramDeclaration(
                    specification=ada.FunctionSpecification(
                        identifier='"+"',
                        parameters=[
                            ada.Parameter(["L", "R"], "T"),
                        ],
                        return_type="T",
                    ),
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.SubprogramRenamingDeclaration(
                    specification=ada.FunctionSpecification(
                        identifier="S",
                        parameters=[
                            ada.Parameter(["P"], "T"),
                        ],
                        return_type="T",
                    ),
                    subprogram_identifier="U",
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.GenericFunctionInstantiation(
                    identifier="I",
                    generic_name="S",
                    associations=[("P1", ada.Variable("U")), ("P2", ada.Number(42))],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.GenericFunctionInstantiation(
                    identifier="I",
                    generic_name="S",
                    associations=[(None, ada.Variable("U")), (None, ada.Number(42))],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.GenericProcedureInstantiation(
                    identifier="I",
                    generic_name="S",
                    associations=[("P1", ada.Variable("U")), ("P2", ada.Number(42))],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.GenericProcedureInstantiation(
                    identifier="I",
                    generic_name="S",
                    associations=[(None, ada.Variable("U")), (None, ada.Number(42))],
                ),
            ],
        ),
        package(
            declaration_declarations=[
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
            body_declarations=[
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
        package(
            body_declarations=[
                ada.SubprogramBody(
                    specification=ada.ProcedureSpecification("F", [ada.Parameter(["P"], "T")]),
                    declarations=[],
                    statements=[ada.PragmaStatement("Unreferenced", [ada.Variable("P")])],
                ),
            ],
        ),
        package(
            body_declarations=[
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
                                (
                                    ada.Equal(ada.Variable("P"), ada.Number(43)),
                                    [ada.ReturnStatement(ada.FALSE)],
                                ),
                            ],
                            else_statements=[ada.ReturnStatement(ada.FALSE)],
                        ),
                    ],
                ),
            ],
        ),
        package(
            declaration_declarations=[
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
        package(
            declaration_declarations=[
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
        package(
            declaration=ada.GenericPackageInstantiation(
                "P",
                "G",
                [(None, "A"), (None, "B"), (None, "F.C")],
            ),
        ),
        package(
            declaration_declarations=[
                ada.PlainDerivedType(
                    "T",
                    "Natural",
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.DerivedRangeType(
                    identifier="T",
                    type_identifier="Natural",
                    first=ada.Number(2),
                    last=ada.Number(42),
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.DerivedRecordType(
                    identifier="R",
                    type_identifier="S",
                    record_extension=[
                        ada.Component("C1", "T"),
                        ada.Component("C2", "T", default=ada.Number(1)),
                    ],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.AccessType(
                    identifier="P",
                    object_identifier="T",
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.Subtype(
                    identifier="S",
                    base_identifier="T",
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.RangeSubtype(
                    identifier="S",
                    base_identifier="T",
                    first=ada.Number(1),
                    last=ada.Number(5),
                ),
            ],
        ),
        package(
            formal_parameters=[],
        ),
        package(
            formal_parameters=[ada.PrivateType("PT")],
        ),
        package(
            formal_parameters=[ada.DiscreteType("DT")],
        ),
        package(
            formal_parameters=[ada.FormalSignedIntegerType("SIT")],
        ),
        package(
            formal_parameters=[ada.ArrayType("AT", "I", "T")],
        ),
        package(
            formal_parameters=[ada.AccessType("T_Ptr", "T")],
        ),
        package(
            formal_parameters=[
                ada.FormalSubprogramDeclaration(
                    ada.ProcedureSpecification("P", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        package(
            formal_parameters=[
                ada.FormalSubprogramDeclaration(
                    ada.FunctionSpecification("F", "U", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        package(
            formal_parameters=[
                ada.PrivateType("PT"),
                ada.FormalSubprogramDeclaration(
                    ada.ProcedureSpecification("P", [ada.Parameter(["P1"], "T")]),
                ),
                ada.FormalSubprogramDeclaration(
                    ada.FunctionSpecification("F", "U", [ada.Parameter(["P1"], "T")]),
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.SubprogramDeclaration(
                    ada.ProcedureSpecification(
                        "P",
                        [ada.Parameter(["P1"], "T", ada.Number(42))],
                    ),
                ),
            ],
        ),
        package(
            formal_parameters=[ada.PrivateType("PT", discriminants=[ada.Discriminant(["D"], "T")])],
        ),
        package(
            declaration_declarations=[
                ada.UseTypeClause("T"),
            ],
        ),
        package(
            declaration_declarations=[
                ada.UseTypeClause("T", "U", "V"),
            ],
        ),
        package(
            declaration_declarations=[
                ada.PrivateType("PT"),
            ],
        ),
        package(
            declaration_declarations=[
                ada.PrivateType("PT", discriminants=[ada.Discriminant(["D"], "T")]),
            ],
        ),
        package(
            declaration_declarations=[
                ada.PrivateType(
                    "PT",
                    discriminants=[ada.Discriminant(["D"], "T", ada.Number(42))],
                ),
            ],
        ),
        package(
            declaration_declarations=[
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
        package(
            declaration_declarations=[
                ada.SubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[ada.OutParameter("C", "T"), ada.InOutParameter("B", "T")],
                    ),
                    aspects=[ada.Depends({ID("C"): [ID("B")], ID("B"): ["null"]})],
                ),
            ],
        ),
        package(
            declaration_declarations=[
                ada.PrivateType("PT"),
            ],
            declaration_private_declarations=[
                ada.SignedIntegerType("PT", ada.Number(1), ada.Number(42)),
            ],
        ),
        package(
            declaration_declarations=[
                ada.EnumerationType(
                    "Enum",
                    literals={ID("E1"): None, ID("E2"): None, ID("E3"): None},
                    size=ada.Number(8),
                ),
            ],
        ),
        package(
            declaration_declarations=[
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
        package(
            declaration=ada.PackageDeclaration(identifier="P"),
            formal_parameters=[
                ada.FormalPackageDeclaration(
                    identifier="Q",
                    generic_identifier="R",
                    associations=[(ID("P1"), ID("A")), (ID("P2"), None), (ID("others"), None)],
                ),
            ],
        ),
        package(
            declaration=ada.PackageDeclaration(identifier="P"),
            formal_parameters=[
                ada.FormalPackageDeclaration(
                    identifier="Q",
                    generic_identifier="R",
                    associations=[(None, ID("A")), (None, ID("B")), (ID("others"), None)],
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T")],
            statements=[ada.CallStatement("G", [ada.Variable("P")])],
        ),
        procedure_body(
            parameters=[ada.OutParameter(["P"], "T")],
            statements=[ada.Assignment("P", ada.Number(42))],
        ),
        procedure_body(
            parameters=[ada.OutParameter(["P"], "T")],
            statements=[
                ada.Assignment(
                    "P",
                    ada.NamedAggregate(
                        ("E1", ada.Number(1)),
                        ("E2", ada.Number(2)),
                        ("E3", ada.Number(3)),
                    ),
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.OutParameter(["P"], "T")],
            statements=[
                ada.Assignment(
                    "P",
                    ada.Slice(ada.Variable("B"), ada.Number(1), ada.Size("B")),
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.OutParameter(["P"], "T")],
            statements=[
                ada.Declare(
                    declarations=[ada.ObjectDeclaration(["B"], "T", ada.Number(5))],
                    statements=[
                        ada.Assignment(
                            "P",
                            ada.Slice(ada.Variable("B"), ada.Number(1), ada.Size("B")),
                        ),
                    ],
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T")],
            statements=[
                ada.ForIn(
                    identifier="I",
                    iterator=ada.ValueRange(ada.Variable("P"), ada.Number(10)),
                    statements=[
                        ada.Assignment(
                            "X",
                            ada.Add(ada.Variable("X"), ada.Number(1)),
                        ),
                    ],
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T")],
            statements=[
                ada.ForIn(
                    identifier="I",
                    iterator=ada.ValueRange(ada.Variable("P"), ada.Number(10)),
                    statements=[
                        ada.Assignment(
                            "X",
                            ada.Add(ada.Variable("X"), ada.Number(1)),
                        ),
                    ],
                    reverse=True,
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T")],
            statements=[
                ada.Assignment(ada.Call("X", [ada.Number(5)]), ada.Number(42)),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T"), ada.OutParameter(["R"], "T")],
            statements=[
                ada.CaseStatement(
                    control_expression=ada.Variable("P"),
                    case_statements=[
                        (ada.Number(1), [ada.Assignment("R", ada.Number(1))]),
                        (ada.Number(2), [ada.Assignment("R", ada.Number(2))]),
                        (ada.Variable("others"), [ada.Assignment("R", ada.Number(0))]),
                    ],
                ),
            ],
        ),
        procedure_body(
            parameters=[ada.Parameter(["P"], "T")],
            statements=[ada.ReturnStatement()],
        ),
        procedure_body(
            parameters=[ada.InOutParameter(["P"], "T")],
            statements=[ada.Assignment(ada.Variable("P"), ada.Call("F", [ada.Variable("P")]))],
        ),
        procedure_body(
            parameters=[ada.InOutParameter(["P"], "String")],
            statements=[
                ada.Assignment(
                    ada.Variable("P"),
                    ada.Concatenation(ada.String("Left"), ada.String("Right")),
                ),
            ],
        ),
    ],
)
def test_roundtrip_model(unit: ada.PackageUnit) -> None:
    result = ada_parser.parse(unit.ads + unit.adb)
    assert repr(result) == repr(unit)


@pytest.mark.parametrize(
    ("spec", "body"),
    [
        (
            """\
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               function F (P : Boolean) return Natural is
                 (if P then 0 else 42);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               pragma Assert (if X > 5 then True else False);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

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
            None,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               function Mask_Upper (V : U64; Mask : Natural) return U64 is
               begin
                  return V
                         and 2**Mask - 1;
               end Mask_Upper;

            end P;
            """,
        ),
        (
            """\
            with F;
            with G;

            package P is new G (F.A, F.B, F.C);
            """,
            None,
        ),
        (
            """\
            package P is

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
            None,
        ),
        (
            """\
            package P is

               type T is new Natural;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type T is new Natural range 1 .. 42;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type A is array (I range <>) of B;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type P is access T;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type T is range 1 .. U'Last * 8;

            end P;
            """,
            None,
        ),
        (
            """\
            generic
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type PT is private;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               with procedure P (P1 : T);
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               with function F (P1 : T) return U;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               E : T;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type PT is private;
               with procedure P (P1 : T);
               with function F (P1 : T) return U;
               E : T;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               with function F (P1 : T := 42) return U;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type PT (D : T) is private;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               use type T;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               use type T, U, V;

               use type W;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type PT is private;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type PT (D1 : T; D2 : T) is private;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type PT (D1 : T; D2 : T := 42) is private;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure F (C : T)
               with
                 Pre =>
                   not C'Constrained;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure F (C : out T; B : in out T; D : in T)
               with
                 Depends =>
                   (C => B, B => null);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure F (C : T)
               with
                 Post =>
                   P (C) = P (C)'Old;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure S (C : T)
               with
                 Pre =>
                   P (C) = L'(if V (C) then E (C) else F (C))'Old;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure S (C : T)
               with
                 Contract_Cases =>
                   (P (C) =>
                       True,
                    others =>
                       False);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure P (C : T)
               with
                 Global =>
                   null;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               G1, G2, G3 : Integer;

               procedure P (C : T)
               with
                 Global =>
                   (Input => G1, Output => G2, In_Out => G3);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               procedure P (C : T; D : in out T; E : out T)
               with
                 Depends =>
                   (D => (D, C), E => C);

            end P;
            """,
            None,
        ),
        (
            """\
            package P
            with
              Always_Terminates
            is

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type T (D : U) is private
               with
                 Default_Initial_Condition =>
                   P (D)
                   and Q (D);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type T is private;

            private

               type T is range 0 .. 42;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type Color is (Red, Green, Blue);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type Color is (Red, Green, Blue)
               with
                 Size =>
                   8;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type R is
                  record
                     F : T;
                  end record;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type N is null record;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               type T (D : U) is private
               with
                 Dynamic_Predicate =>
                   P (D)
                   and Q (D);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (V : T) is
               begin
                  C (V);
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
               begin
                  R := 42;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
               begin
                  R := (F1 => 1, F2 => 2, F3 => 3);
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
               begin
                  R := B (1 .. B'Last - 1);
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
               begin
                  declare
                     X : T := 1;
                  begin
                     R := B (X .. B'Last - 1);
                  end;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
                  X : T;
               begin
                  for I in 1 .. 10 loop
                     X := X + 1;
                  end loop;
                  R := X;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T) is
                  X : T;
               begin
                  for I in reverse 1 .. 10 loop
                     X := X + 1;
                  end loop;
                  R := X;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T; I : Natural) is
                  X : T;
               begin
                  X (I) := T'Val (R);
                  Y;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               procedure Proc (R : out T; I : Natural) is
               begin
                  case I is
                     when 1 =>
                        R := 1;
                     when 2 =>
                        R := 2;
                     when others =>
                        R := 0;
                  end case;
               end Proc;

            end P;
            """,
        ),
        (
            """\
            package P is

            end P;
            """,
            """\
            package body P is

               function F (I : Natural) return Natural is
               begin
                  return I + 1;
               end F;

            end P;
            """,
        ),
        (
            """\
            package P is

               procedure S (C : T)
               with
                 Pre =>
                   V (C)
                   and then (S (C) >= C.First + I (E)) - 1;

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type T is range <>;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type T is (<>);
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type T is array (I) of E;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type T is array (I range <>) of E;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               type T is access U;
            package P is

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               function "+" (L : Natural; R : Natural) return Natural is abstract;

               function "-" (L, R : Natural) return Natural is abstract;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               function F (L : Natural; R : Natural) return Natural renames U;

               procedure P (P : Natural) renames U;

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               function F is new G (X, 42);

               function F is new G (P1 => X, P2 => 42);

               procedure P is new G (X, 42);

               procedure P is new G (P1 => X, P2 => 42);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               pragma Assert (L'Pos (L'Last) /= M'Pos (M'Last));

               pragma Assert (L'Succ (A) = B);

               pragma Assert (L'Val (A) = B);

            end P;
            """,
            None,
        ),
        (
            """\
            package P is

               subtype I is L range 1 .. L'Last;

            end P;
            """,
            None,
        ),
        (
            """\
            generic
               with package Q is new R (<>);
               with package S is new T (others => <>);
            package P is

            end P;
            """,
            None,
        ),
    ],
    ids=range(63),
)
def test_roundtrip_text(spec: str, body: str | None) -> None:
    spec = textwrap.dedent(spec)
    body = textwrap.dedent(body or "")
    result = ada_parser.parse(spec + body)
    assert result.ads == spec
    assert result.adb == body


@pytest.mark.parametrize(
    ("data", "message"),
    [
        (
            """\
        package P is

           type R is record
             F1, F2 : T;
           end record;

        end P;
        """,
            "<stdin>:4:6: error: identifier lists unsupported for record component declaration",
        ),
        (
            """\
        package P is

           subtype S is T range <>;

        end P;
        """,
            "<stdin>:3:4: error: signed integer constraint invalid for subtypes",
        ),
        (
            """\
        package P is

           type S is new T range <>;

        end P;
        """,
            "<stdin>:3:14: error: signed integer constraint invalid for derived types",
        ),
        (
            """\
        package P is

           X : T := (1, 2, P => 3);

        end P;
        """,
            "<stdin>:3:13: error: invalid mixed positional and named aggregate",
        ),
        (
            """\
        package P is

           type E is (E1, E2)
           with
             Size => 8, Size => 16;

        end P;
        """,
            "<stdin>:3:4: error: multiple size aspects",
        ),
        (
            """\
        package P is

        end Q;
        """,
            "<stdin>:1:1: error: inconsistent package identifiers",
        ),
        (
            """\
        package P is

        end P;

        package body P is

        end Q;
        """,
            "<stdin>:5:1: error: inconsistent package identifiers",
        ),
        (
            """\
        package P is

            procedure P
            with
              Global => (In_Out => X, In_Out =>X);

        end P;
        """,
            "<stdin>:5:17: error: duplicate In_Out",
        ),
        (
            """\
        package P is

            procedure P
            with
              Global => (Input => X, Input =>X);

        end P;
        """,
            "<stdin>:5:17: error: duplicate Input",
        ),
        (
            """\
        package P is

            procedure P
            with
              Global => (Output => X, Output =>X);

        end P;
        """,
            "<stdin>:5:17: error: duplicate Output",
        ),
        (
            """\
        package P is

            type T is array (T range 1 .. 5) of U;

        end P;
        """,
            "<stdin>:3:15: error: discrete array subtypes not implemented",
        ),
        (
            """\
        package P is

        end P;

        package P is

        end P;

        package P is

        end P;
        """,
            "<stdin>:1:1: error: expected exactly two units, got 3",
        ),
        (
            """\
        package body P is

        end P;
        """,
            "<stdin>:1:1: error: expected package declaration",
        ),
        (
            """\
        package P is

        end P;

        package Q is

        end Q;
        """,
            "<stdin>:1:1: error: expected package body as second unit",
        ),
        (
            """\
        package P is

        end P;

        package body P is

           procedure P is
           begin
              null;
           end Q;

        end P;
        """,
            "<stdin>:7:4: error: inconsistent identifier",
        ),
        (
            """\
        package P is

           X : T range 0 .. 1;

        end P;
        """,
            "<stdin>:3:4: error: invalid constraint in object declaration",
        ),
        (
            """\
        package P is

           type R is record
              E : T range <>;
           end record;

        end P;
        """,
            "<stdin>:4:11: error: invalid constraint in component definition",
        ),
        (
            """\
        package P is

           type P is access T range <>;

        end P;
        """,
            "<stdin>:3:14: error: invalid constraint in access to object definition",
        ),
        (
            """\
        use Q, R;

        package P is

        end P;
        """,
            "<stdin>:1:1: error: multiple packages in one use-clause not implemented",
        ),
    ],
)
def test_parse_error(data: str, message: str) -> None:
    data = textwrap.dedent(data)
    with pytest.raises(RecordFluxError, match=rf"^{message}$"):
        ada_parser.parse(data)


@pytest.mark.parametrize(
    "aspect_specification",
    [
        "",
        "\n           with\n             Ghost",
        "\n           with\n             Convention =>\n               Intrinsic",
        "\n           with\n             Pre =>\n               P > 0",
    ],
    ids=range(4),
)
@pytest.mark.parametrize("declarative_part", ["", "\n              X : Natural;"], ids=range(2))
@pytest.mark.parametrize("null_exclusion", ["", "not null "], ids=range(2))
@pytest.mark.parametrize(
    "overriding_indicator",
    ["", "overriding ", "not overriding "],
    ids=range(3),
)
@pytest.mark.parametrize(
    "data",
    [
        "{overriding_indicator}function S is new G{aspect_specification};",
        "{overriding_indicator}function S is new G (A, B){aspect_specification};",
        "{overriding_indicator}function S return {null_exclusion}T{aspect_specification};",
        '{overriding_indicator}function "+" (L, R : T) return {null_exclusion}T'
        "{aspect_specification};",
        "{overriding_indicator}function S return {null_exclusion}T{aspect_specification}"
        "{is_keyword}{declarative_part}"
        "\n           begin"
        "\n              return 5;"
        "\n           end S;",
        '{overriding_indicator}function "+" (L, R : T) return {null_exclusion}T'
        "{aspect_specification}{is_keyword}{declarative_part}"
        "\n           begin"
        "\n              return 5;"
        '\n           end "+";',
        "{overriding_indicator}function S return {null_exclusion}T is"
        "\n             (42)"
        "{aspect_specification};",
        "{overriding_indicator}function S return {null_exclusion}T is abstract"
        "{aspect_specification};",
        "{overriding_indicator}function S return {null_exclusion}T is separate"
        "{aspect_specification};",
        "{overriding_indicator}function S return {null_exclusion}T renames U"
        "{aspect_specification};",
        "{overriding_indicator}procedure S renames G{aspect_specification};",
        "{overriding_indicator}procedure S (P : T) renames G{aspect_specification};",
        "{overriding_indicator}procedure S{aspect_specification};",
        "{overriding_indicator}procedure S (P : T){aspect_specification};",
        "{overriding_indicator}procedure S is new G (A, B){aspect_specification};",
        "{overriding_indicator}procedure S is new G (A, 42){aspect_specification};",
        "{overriding_indicator}procedure S is new G (P1 => A, P2 => 42){aspect_specification};",
        "{overriding_indicator}procedure S{aspect_specification}{is_keyword}"
        "\n           begin"
        "\n              return;"
        "\n           end S;",
        "{overriding_indicator}procedure S (P : T){aspect_specification}{is_keyword}"
        "\n           begin"
        "\n              return;"
        "\n           end S;",
        "{overriding_indicator}procedure S is abstract{aspect_specification};",
        "{overriding_indicator}procedure S is separate{aspect_specification};",
    ],
    ids=range(21),
)
def test_roundtrip_declarations(
    aspect_specification: str,
    declarative_part: str,
    null_exclusion: str,
    overriding_indicator: str,
    data: str,
) -> None:
    is_keyword = "\n           is" if aspect_specification else " is"
    text = textwrap.dedent(
        f"""\
        package P is

           {data}

        end P;
        """.format(
            is_keyword=is_keyword,
            aspect_specification=aspect_specification,
            null_exclusion=null_exclusion,
            overriding_indicator=overriding_indicator,
            declarative_part=declarative_part,
        ),
    )
    result = ada_parser.parse(text)
    assert result.ads + result.adb == text


def test_missing_handler() -> None:
    dummy_grammar = lark.Lark('start: dummy\ndummy: "dummy"')
    with pytest.raises(ada_parser.ParseError, match='missing handler for rule "dummy"'):
        ada_parser.TreeToAda().transform(dummy_grammar.parse("dummy"))
