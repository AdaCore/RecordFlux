from __future__ import annotations

import textwrap
from collections.abc import Callable

import pytest

from rflx import ada, expression as expr, typing_ as rty
from rflx.identifier import ID
from tests.utils import assert_equal


def test_id_str() -> None:
    assert ID("A.B.C").ada_str == "A.B.C"


def test_id_add() -> None:
    assert ID("A") + ID("B.C") == ID("AB.C")
    assert ID("B.C") + ID("D") == ID("B.CD")


def test_id_add_str() -> None:
    assert "A" + ID("B.C") == ID("AB.C")
    assert ID("B.C") + "D" == ID("B.CD")
    assert ID("B.C") + "" == ID("B.C")
    assert "" + ID("B.C") == ID("B.C")


def test_id_mul_id() -> None:
    assert ID("A") * ID("B.C") == ID("A.B.C")
    assert ID("B.C") * ID("D") == ID("B.C.D")


def test_id_mul_str() -> None:
    assert "A" * ID("B.C") == ID("A.B.C")
    assert ID("B.C") * "D" == ID("B.C.D")
    assert "" * ID("B.C") == ID("B.C")
    assert ID("B.C") * "" == ID("B.C")


def test_not_rflx_expr() -> None:
    assert ada.Not(ada.Variable("X")).rflx_expr() == expr.Not(expr.Variable("X"))


def test_neg_str() -> None:
    assert str(ada.Neg(ada.Variable("X"))) == "-X"
    assert str(ada.Neg(ada.Neg(ada.Variable("X")))) == "-(-X)"


def test_neg_rflx_expr() -> None:
    assert ada.Neg(ada.Variable("X")).rflx_expr() == expr.Neg(expr.Variable("X"))


def test_bool_expr_str() -> None:
    assert_equal(
        str(
            ada.And(
                ada.Variable("A"),
                ada.Or(ada.Variable("B"), ada.Variable("C")),
                ada.Variable("D"),
            ),
        ),
        textwrap.dedent(
            """\
            A
            and (B
                 or C)
            and D""",
        ),
    )
    assert_equal(
        str(
            ada.AndThen(
                ada.Variable("A"),
                ada.OrElse(ada.Variable("B"), ada.Variable("C")),
                ada.Variable("D"),
            ),
        ),
        textwrap.dedent(
            """\
            A
            and then (B
                      or else C)
            and then D""",
        ),
    )


@pytest.mark.parametrize("expression", [ada.And, ada.AndThen, ada.Or, ada.OrElse])
def test_bool_expr_rflx_expr(expression: Callable[[ada.Expr, ada.Expr], ada.Expr]) -> None:
    result = expression(ada.Variable("X"), ada.Variable("Y")).rflx_expr()
    expected = getattr(expr, expression.__name__)(expr.Variable("X"), expr.Variable("Y"))
    assert result == expected


def test_and_str() -> None:
    assert str(ada.And(ada.Variable("X"), ada.Variable("Y"))) == "X\nand Y"


def test_and_then_str() -> None:
    assert str(ada.AndThen(ada.Variable("X"), ada.Variable("Y"))) == "X\nand then Y"


def test_or_str() -> None:
    assert str(ada.Or(ada.Variable("X"), ada.Variable("Y"))) == "X\nor Y"


def test_or_else_str() -> None:
    assert str(ada.OrElse(ada.Variable("X"), ada.Variable("Y"))) == "X\nor else Y"


@pytest.mark.parametrize(
    "expression",
    [ada.Add, ada.Mul, ada.Sub, ada.Div, ada.Pow, ada.Mod, ada.Rem],
)
def test_math_expr_ada_expr(expression: Callable[[ada.Expr, ada.Expr], ada.Expr]) -> None:
    result = expression(ada.Variable("X"), ada.Variable("Y")).rflx_expr()
    expected = getattr(expr, expression.__name__)(expr.Variable("X"), expr.Variable("Y"))
    assert result == expected


def test_add_str() -> None:
    assert str(ada.Add(ada.Number(1), ada.Call("Test", []))) == "1 + Test"
    assert str(ada.Add(ada.Number(1), -ada.Call("Test", []))) == "1 - Test"


def test_attribute() -> None:
    assert isinstance(ada.Range("X"), ada.Attribute)
    assert isinstance(ada.Old("X"), ada.Attribute)
    assert isinstance(ada.Result("X"), ada.Attribute)
    assert isinstance(ada.Constrained("X"), ada.Attribute)


@pytest.mark.parametrize(
    "relation",
    [ada.Less, ada.LessEqual, ada.Equal, ada.GreaterEqual, ada.Greater, ada.NotEqual],
)
def test_math_relation_rflx_expr(relation: Callable[[ada.Expr, ada.Expr], ada.Expr]) -> None:
    result = relation(ada.Variable("X"), ada.Variable("Y")).rflx_expr()
    expected = getattr(expr, relation.__name__)(expr.Variable("X"), expr.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("relation", [ada.In, ada.NotIn])
def test_composite_relation_rflx_expr(relation: Callable[[ada.Expr, ada.Expr], ada.Expr]) -> None:
    result = relation(ada.Variable("X"), ada.Variable("Y")).rflx_expr()
    expected = getattr(expr, relation.__name__)(expr.Variable("X"), expr.Variable("Y"))
    assert result == expected


def test_attribute_str() -> None:
    assert str(ada.First("X")) == "X'First"
    assert str(-ada.First("X")) == "-X'First"


def test_literal_rflx_expr() -> None:
    assert ada.Literal("X").rflx_expr() == expr.Literal("X")


def test_variable_rflx_expr() -> None:
    assert ada.Variable("X").rflx_expr() == expr.Variable("X")


@pytest.mark.parametrize("attribute", [ada.Size, ada.Length, ada.First, ada.Last])
def test_attribute_rflx_expr(attribute: Callable[[ada.Expr], ada.Expr]) -> None:
    result = attribute(ada.Variable("X")).rflx_expr()
    expected = getattr(expr, attribute.__name__)(expr.Variable("X"))
    assert result == expected


def test_attribute_expression_str() -> None:
    assert str(ada.Val("X", ada.Number(1))) == "X'Val (1)"


def test_selected_str() -> None:
    assert str(ada.Selected(ada.Variable("X"), "Y")) == "X.Y"
    assert str(-ada.Selected(ada.Variable("X"), "Y")) == "-X.Y"


def test_selected_rflx_expr() -> None:
    assert ada.Selected(ada.Variable("X"), "Y").rflx_expr() == expr.Selected(
        expr.Variable("X"),
        "Y",
    )


def test_indexed_str() -> None:
    assert str(ada.Indexed(ada.Variable("X"), ada.Variable("Y"))) == "X (Y)"
    assert str(-ada.Indexed(ada.Variable("X"), ada.Variable("Y"))) == "-X (Y)"


def test_indexed_rflx_expr() -> None:
    assert ada.Indexed(ada.Variable("X"), ada.Variable("Y")).rflx_expr() == expr.Indexed(
        expr.Variable("X"),
        expr.Variable("Y"),
    )


def test_call_rflx_expr() -> None:
    assert ada.Call("X", [ada.Variable("Y"), ada.Variable("Z")]).rflx_expr() == expr.Call(
        "X",
        rty.UNDEFINED,
        [expr.Variable("Y"), expr.Variable("Z")],
    )


def test_slice_rflx_expr() -> None:
    assert ada.Slice(
        ada.Variable("X"),
        ada.Variable("Y"),
        ada.Variable("Z"),
    ).rflx_expr() == expr.Slice(expr.Variable("X"), expr.Variable("Y"), expr.Variable("Z"))


def test_aggregate_str() -> None:
    assert str(ada.Aggregate(ada.Number(1), ada.Number(2))) == "(1, 2)"


def test_aggregate_rflx_expr() -> None:
    assert ada.Aggregate(ada.Number(1), ada.Number(2)).rflx_expr() == expr.Aggregate(
        expr.Number(1),
        expr.Number(2),
    )


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_aggregate_invalid() -> None:
    with pytest.raises(AssertionError):
        str(ada.Aggregate())
    with pytest.raises(AssertionError):
        str(ada.Aggregate(ada.Number(1)))


def test_in_str() -> None:
    assert str(ada.In(ada.Variable("X"), ada.Variable("Y"))) == "X in Y"


def test_not_in_str() -> None:
    assert str(ada.NotIn(ada.Variable("X"), ada.Variable("Y"))) == "X not in Y"


def test_if_str() -> None:
    assert_equal(
        str(
            ada.If(
                [
                    (ada.Variable("X"), ada.Number(1)),
                    (ada.Variable("Y"), ada.Number(2)),
                ],
                ada.Number(3),
            ),
        ),
        "(if X then 1 elsif Y then 2 else 3)",
    )
    assert_equal(
        str(
            ada.If(
                [
                    (
                        ada.Variable("Some_Complex_Condition"),
                        ada.Variable("Some_Complex_Expression"),
                    ),
                    (
                        ada.Variable("Another_Complex_Condition"),
                        ada.Variable("Another_Complex_Expression"),
                    ),
                ],
                ada.Variable("Some_Complex_Expression"),
            ),
        ),
        textwrap.dedent(
            """\
            (if
                Some_Complex_Condition
             then
                Some_Complex_Expression
             elsif
                Another_Complex_Condition
             then
                Another_Complex_Expression
             else
                Some_Complex_Expression)""",
        ),
    )


def test_if_expr_rflx_expr() -> None:
    assert ada.IfExpr(
        [(ada.Variable("X"), ada.Variable("Y"))],
        ada.Variable("Z"),
    ).rflx_expr() == expr.IfExpr([(expr.Variable("X"), expr.Variable("Y"))], expr.Variable("Z"))


def test_case_str() -> None:
    assert_equal(
        str(
            ada.Case(
                ada.Variable("X"),
                [
                    (ada.Variable("Y"), ada.Number(1)),
                    (ada.Variable("Z"), ada.Number(1)),
                    (ada.Variable("others"), ada.Number(2)),
                ],
            ),
        ),
        textwrap.dedent(
            """\
            (case X is
                when Y | Z =>
                   1,
                when others =>
                   2)""",
        ),
    )


def test_value_range_rflx_expr() -> None:
    assert ada.ValueRange(ada.Variable("X"), ada.Variable("Y")).rflx_expr() == expr.ValueRange(
        expr.Variable("X"),
        expr.Variable("Y"),
    )


def test_quantified_expression_str() -> None:
    assert (
        str(ada.ForAllOf("X", ada.Variable("Y"), ada.Variable("Z"))) == "(for all X of Y =>\n    Z)"
    )
    assert (
        str(ada.ForAllIn("X", ada.Variable("Y"), ada.Variable("Z"))) == "(for all X in Y =>\n    Z)"
    )
    assert (
        str(ada.ForSomeIn("X", ada.Variable("Y"), ada.Variable("Z")))
        == "(for some X in Y =>\n    Z)"
    )


@pytest.mark.parametrize("expression", [ada.ForAllOf, ada.ForAllIn, ada.ForSomeIn])
def test_quantified_expression_rflx_expr(
    expression: Callable[[str, ada.Expr, ada.Expr], ada.Expr],
) -> None:
    result = expression("X", ada.Variable("Y"), ada.Variable("Z")).rflx_expr()
    expected = getattr(expr, expression.__name__)("X", expr.Variable("Y"), expr.Variable("Z"))
    assert result == expected


def test_number_str() -> None:
    assert str(ada.Number(15)) == "15"


def test_number_str_long() -> None:
    assert str(ada.Number(539535)) == "539535"


def test_number_str_neg_long() -> None:
    assert str(ada.Number(-539535)) == "(-539535)"


def test_number_str_hex() -> None:
    assert str(ada.Number(4096, 16)) == "16#1000#"


def test_number_str_neg_hex() -> None:
    assert str(ada.Number(-4096, 16)) == "(-16#1000#)"


def test_number_str_dec() -> None:
    assert str(ada.Number(4096, 10)) == "10#4096#"


def test_number_str_oct() -> None:
    assert str(ada.Number(45432, 8)) == "8#130570#"


def test_number_str_neg_oct() -> None:
    assert str(ada.Number(-45432, 8)) == "(-8#130570#)"


def test_number_str_bin() -> None:
    assert str(ada.Number(454, 2)) == "2#111000110#"


def test_number_rflx_expr() -> None:
    assert ada.Number(42).rflx_expr() == expr.Number(42)


def test_string_str() -> None:
    assert str(ada.String("X Y")) == '"X Y"'
    assert str(ada.String('X "Y"')) == '"X ""Y"""'


def test_string_rflx_expr() -> None:
    assert ada.String("X Y").rflx_expr() == expr.String("X Y")


def test_named_aggregate_str() -> None:
    assert (
        str(
            ada.NamedAggregate(
                ("X", ada.Number(1)),
                (ada.ValueRange(ada.Number(2), ada.Number(3)), ada.Variable("Y")),
            ),
        )
        == "(X => 1, 2 .. 3 => Y)"
    )


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_named_aggregate_invalid() -> None:
    with pytest.raises(AssertionError):
        str(ada.NamedAggregate())


def test_named_aggregate_rflx_expr() -> None:
    assert ada.NamedAggregate(
        ("X", ada.Number(1)),
        (ada.ValueRange(ada.Number(2), ada.Number(3)), ada.Variable("Y")),
    ).rflx_expr() == expr.NamedAggregate(
        ("X", expr.Number(1)),
        (expr.ValueRange(expr.Number(2), expr.Number(3)), expr.Variable("Y")),
    )


def test_raise_str() -> None:
    assert str(ada.Raise("X")) == "raise X"
    assert str(ada.Raise("X", ada.String("Y"))) == 'raise X with "Y"'


def test_expr_str() -> None:
    assert_equal(
        str(
            ada.If(
                [
                    (
                        ada.Or(
                            ada.And(ada.Variable("X"), ada.Variable("Y")),
                            ada.Variable("Z"),
                        ),
                        ada.Number(1),
                    ),
                    (ada.Variable("Y"), ada.Number(2)),
                ],
                ada.Div(ada.Mul(ada.Variable("A"), ada.Number(3)), ada.Number(8)),
            ),
        ),
        "(if (X and Y) or Z then 1 elsif Y then 2 else (A * 3) / 8)",
    )
    assert_equal(
        str(
            ada.If(
                [
                    (
                        ada.Or(
                            ada.And(ada.Variable("Variable_X"), ada.Variable("Variable_Y")),
                            ada.Variable("Variable_Z"),
                        ),
                        ada.Number(1),
                    ),
                    (ada.Variable("Variable_Y"), ada.Number(2)),
                ],
                ada.Div(ada.Mul(ada.Variable("Variable_A"), ada.Number(3)), ada.Number(8)),
            ),
        ),
        textwrap.dedent(
            """\
            (if
                (Variable_X
                 and Variable_Y)
                or Variable_Z
             then
                1
             elsif
                Variable_Y
             then
                2
             else
                (Variable_A * 3) / 8)""",
        ),
    )
    assert_equal(
        str(
            ada.And(
                ada.If(
                    [
                        (
                            ada.Or(
                                ada.And(
                                    ada.Variable("Variable_X"),
                                    ada.Variable("Variable_Y"),
                                ),
                                ada.Variable("Variable_Z"),
                            ),
                            ada.Number(1),
                        ),
                        (ada.Variable("Variable_Y"), ada.Number(2)),
                    ],
                    ada.Div(
                        ada.Mul(ada.Variable("Variable_A"), ada.Number(3)),
                        ada.Number(8),
                    ),
                ),
                ada.Variable("A"),
                ada.Or(ada.Variable("B"), ada.Variable("C")),
                ada.Variable("D"),
            ),
        ),
        textwrap.dedent(
            """\
            (if
                (Variable_X
                 and Variable_Y)
                or Variable_Z
             then
                1
             elsif
                Variable_Y
             then
                2
             else
                (Variable_A * 3) / 8)
            and A
            and (B
                 or C)
            and D""",
        ),
    )
    assert_equal(
        str(
            ada.ForAllOf(
                "X",
                ada.Variable("Z"),
                ada.If(
                    [
                        (
                            ada.Or(
                                ada.And(
                                    ada.Variable("Variable_X"),
                                    ada.Variable("Variable_Y"),
                                ),
                                ada.Variable("Variable_Z"),
                            ),
                            ada.Number(1),
                        ),
                        (ada.Variable("Variable_Y"), ada.Number(2)),
                    ],
                    ada.Div(
                        ada.Mul(ada.Variable("Variable_A"), ada.Number(3)),
                        ada.Number(8),
                    ),
                ),
            ),
        ),
        textwrap.dedent(
            """\
            (for all X of Z =>
                (if
                    (Variable_X
                     and Variable_Y)
                    or Variable_Z
                 then
                    1
                 elsif
                    Variable_Y
                 then
                    2
                 else
                    (Variable_A * 3) / 8))""",
        ),
    )
    assert str(ada.Equal(ada.String("S"), ada.Variable("X"))) == '"S" = X'


def test_call_str() -> None:
    assert str(ada.Call("A", [])) == "A"
    assert str(ada.Call("A", [ada.Variable("B"), ada.Variable("C")])) == "A (B, C)"
    assert (
        str(ada.Call("A", [], {ID("B"): ada.Number(1), ID("C"): ada.Number(2)}))
        == "A (B => 1, C => 2)"
    )
    assert (
        str(
            ada.Call(
                "A",
                [ada.Variable("B"), ada.Variable("C")],
                {ID("D"): ada.Number(1), ID("E"): ada.Number(2)},
            ),
        )
        == "A (B, C, D => 1, E => 2)"
    )
    assert str(-ada.Call("A", [])) == "-A"


def test_conversion_str() -> None:
    assert str(ada.Conversion("A", ada.Variable("B"))) == "A (B)"
    assert str(ada.Not(ada.Conversion("A", ada.Variable("B")))) == "not A (B)"


def test_conversion_rflx_expr() -> None:
    assert ada.Conversion("X", ada.Variable("Y")).rflx_expr() == expr.Conversion(
        "X",
        expr.Variable("Y"),
    )


@pytest.mark.parametrize(
    ("aspect", "expected"),
    [
        (ada.Precondition(ada.Variable("X")), "Pre =>\n  X"),
        (ada.Postcondition(ada.Variable("X")), "Post =>\n  X"),
        (ada.ClassPrecondition(ada.Variable("X")), "Pre'Class =>\n  X"),
        (ada.ClassPostcondition(ada.Variable("X")), "Post'Class =>\n  X"),
        (
            ada.ContractCases((ada.Variable("X"), ada.Variable("Y"))),
            "Contract_Cases =>\n  (X =>\n      Y)",
        ),
        (ada.Depends({"X": ["Y", "Z"]}), "Depends =>\n  (X => (Y, Z))"),
        (ada.DynamicPredicate(ada.Variable("X")), "Dynamic_Predicate =>\n  X"),
        (ada.SizeAspect(ada.Variable("X")), "Size =>\n  X"),
        (ada.InitialCondition(ada.Variable("X")), "Initial_Condition =>\n  X"),
        (ada.DefaultInitialCondition(ada.Variable("X")), "Default_Initial_Condition =>\n  X"),
        (ada.SparkMode(), "SPARK_Mode"),
        (ada.SparkMode(off=True), "SPARK_Mode =>\n  Off"),
        (ada.Ghost(), "Ghost"),
        (ada.Import(), "Import"),
        (ada.Annotate("X"), "Annotate =>\n  (X)"),
        (ada.ElaborateBody(), "Elaborate_Body"),
    ],
)
def test_aspects(aspect: ada.Aspect, expected: str) -> None:
    assert str(aspect) == expected


def test_formal_package_declaration() -> None:
    assert (
        str(ada.FormalPackageDeclaration("A", "B", ["C", "D"])) == "with package A is new B (C, D);"
    )


def test_generic_package_instantiation() -> None:
    assert (
        str(ada.GenericPackageInstantiation("A", "B", ["C", "D"])) == "package A is new B (C, D);"
    )


def test_generic_package_instantiation_hash() -> None:
    assert hash(ada.GenericPackageInstantiation("A", "B", ["C", "D"])) is not None


def test_package_renaming_declaration() -> None:
    assert str(ada.PackageRenamingDeclaration("A", "B")) == "package A renames B;"


def test_modular_type() -> None:
    assert str(ada.ModularType("A", ada.Number(256))) == "type A is mod 256;"


def test_range_type() -> None:
    assert (
        str(ada.RangeType("A", ada.Number(1), ada.Number(100), [ada.SizeAspect(ada.Number(8))]))
        == "type A is range 1 .. 100 with\n  Size =>\n    8;"
    )


def test_enumeration_type() -> None:
    assert (
        str(ada.EnumerationType("A", {ID("B"): None, ID("C"): None}, ada.Number(8)))
        == "type A is (B, C) with\n  Size =>\n    8;"
    )
    assert (
        str(
            ada.EnumerationType(
                "A",
                {ID("B"): ada.Number(1), ID("C"): ada.Number(2)},
                ada.Number(8),
            ),
        )
        == "type A is (B, C) with\n  Size =>\n    8;\nfor A use (B => 1, C => 2);"
    )


def test_subtype() -> None:
    assert str(ada.Subtype("A", "B")) == "subtype A is B;"


def test_range_subtype() -> None:
    assert (
        str(ada.RangeSubtype("A", "B", ada.Number(1), ada.Number(2)))
        == "subtype A is B range 1 .. 2;"
    )


def test_derived_type() -> None:
    assert str(ada.DerivedType("A", "B")) == "type A is new B;"
    assert str(ada.DerivedType("A", "B", [])) == "type A is new B with null record;"
    assert (
        str(ada.DerivedType("A", "B", [ada.Component("C", "D")]))
        == "type A is new B with\n   record\n      C : D;\n   end record;"
    )


def test_private_type() -> None:
    assert str(ada.PrivateType("A")) == "type A is private;"


def test_discrete_type() -> None:
    assert str(ada.DiscreteType("A")) == "type A is (<>);"


def test_array_type() -> None:
    assert str(ada.ArrayType("A", "B", "C")) == "type A is array (B) of C;"


def test_unconstrained_array_type() -> None:
    assert str(ada.UnconstrainedArrayType("A", "B", "C")) == "type A is array (B range <>) of C;"


def test_record_type() -> None:
    assert str(ada.RecordType("A", [])) == "type A is null record;"
    assert (
        str(ada.RecordType("A", [ada.Component("B", "C")]))
        == "type A is\n   record\n      B : C;\n   end record;"
    )


def test_null_component() -> None:
    assert str(ada.NullComponent()) == "null;"


def test_access_type() -> None:
    assert str(ada.AccessType("A", "B")) == "type A is access B;"


def test_access_parameter() -> None:
    assert str(ada.AccessParameter("A", "B")) == "A : access B"
    assert str(ada.AccessParameter("A", "B", ada.Variable("C"))) == "A : access B := C"
    assert str(ada.AccessParameter("A", "B", constant=True)) == "A : access constant B"


def test_generic_procedure_instantiation() -> None:
    assert (
        str(ada.GenericProcedureInstantiation("A", ada.ProcedureSpecification("B"), ["C", "D"]))
        == "procedure A is new B (C, D);"
    )


def test_generic_function_instantiation() -> None:
    assert (
        str(ada.GenericFunctionInstantiation("A", ada.FunctionSpecification("B", "T"), ["C", "D"]))
        == "function A is new B (C, D);"
    )


def test_subprogram_renaming_declaration() -> None:
    assert (
        str(ada.SubprogramRenamingDeclaration(ada.ProcedureSpecification("A"), "B"))
        == "procedure A renames B;"
    )


def test_call_statement_str() -> None:
    assert str(ada.CallStatement("A", [])) == "A;"
    assert str(ada.CallStatement("A", [ada.Variable("B"), ada.Variable("C")])) == "A (B, C);"
    assert (
        str(ada.CallStatement("A", [], {ID("B"): ada.Number(1), ID("C"): ada.Number(2)}))
        == "A (B => 1, C => 2);"
    )
    assert (
        str(
            ada.CallStatement(
                "A",
                [ada.Variable("B"), ada.Variable("C")],
                {ID("D"): ada.Number(1), ID("E"): ada.Number(2)},
            ),
        )
        == "A (B, C, D => 1, E => 2);"
    )


def test_exit_statement_str() -> None:
    assert str(ada.ExitStatement(ada.Variable("A"))) == "exit when A;"


def test_while_str() -> None:
    assert_equal(
        str(
            ada.While(
                ada.Variable("X"),
                [ada.NullStatement()],
            ),
        ),
        textwrap.dedent(
            """\
            while X loop
               null;
            end loop;""",
        ),
    )
    assert_equal(
        str(
            ada.While(
                ada.And(
                    ada.Variable("X"),
                    ada.Variable("Y"),
                ),
                [ada.NullStatement()],
            ),
        ),
        textwrap.dedent(
            """\
            while
               X
               and Y
            loop
               null;
            end loop;""",
        ),
    )


def test_qualified_expr() -> None:
    assert str(ada.QualifiedExpr("T", ada.Variable("A"))) == "T'(A)"


def test_qualified_expr_rflx_expr() -> None:
    assert ada.QualifiedExpr("X", ada.Variable("Y")).rflx_expr() == expr.QualifiedExpr(
        "X",
        expr.Variable("Y"),
    )


def test_parameter() -> None:
    assert str(ada.Parameter(["P1"], "T")) == "P1 : T"
    assert str(ada.Parameter(["P1"], ID("Boolean"))) == "P1 : Boolean"


def test_raise_statement() -> None:
    assert str(ada.RaiseStatement("X")) == "raise X;"
    assert str(ada.RaiseStatement("X", ada.String("Y"))) == 'raise X with "Y";'


def test_concatenation() -> None:
    assert str(ada.Concatenation(ada.String("X"), ada.String("Y"))) == '"X" & "Y"'


def test_for_loop() -> None:
    assert str(ada.ForOf("X", ada.Variable("Y"), [ada.NullStatement()])) == textwrap.dedent(
        """\
        for X of Y loop
           null;
        end loop;""",
    )


def test_always_terminates() -> None:
    assert str(ada.AlwaysTerminates()) == "Always_Terminates"
    assert (
        str(ada.AlwaysTerminates(ada.LessEqual(ada.Variable("X"), ada.Number(1))))
        == "Always_Terminates =>\n  X <= 1"
    )


def test_subprogram_variant() -> None:
    assert (
        str(ada.SubprogramVariant(ada.Decreases(ada.Variable("X"))))
        == "Subprogram_Variant =>\n  (Decreases =>\n    X)"
    )
    assert (
        str(ada.SubprogramVariant(ada.Increases(ada.Number(1))))
        == "Subprogram_Variant =>\n  (Increases =>\n    1)"
    )


def test_declarative_items() -> None:
    assert str(
        ada.PackageDeclaration("P", declarations=[ada.ObjectDeclaration("X", "Boolean")]),
    ) == textwrap.dedent(
        """\
        package P
        is

           X : Boolean;

        end P;
        """,
    )
