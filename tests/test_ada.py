import rflx.ada as ada
from tests.utils import assert_equal, multilinestr


def test_bool_expr_str() -> None:
    assert_equal(
        str(
            ada.And(
                ada.Variable("A"), ada.Or(ada.Variable("B"), ada.Variable("C")), ada.Variable("D")
            )
        ),
        multilinestr(
            """A
               and (B
                    or C)
               and D"""
        ),
    )
    assert_equal(
        str(
            ada.AndThen(
                ada.Variable("A"),
                ada.OrElse(ada.Variable("B"), ada.Variable("C")),
                ada.Variable("D"),
            )
        ),
        multilinestr(
            """A
               and then (B
                         or else C)
               and then D"""
        ),
    )


def test_and_str() -> None:
    assert str(ada.And(ada.Variable("X"), ada.Variable("Y"))) == "X\nand Y"


def test_and_then_str() -> None:
    assert str(ada.AndThen(ada.Variable("X"), ada.Variable("Y"))) == "X\nand then Y"


def test_or_str() -> None:
    assert str(ada.Or(ada.Variable("X"), ada.Variable("Y"))) == "X\nor Y"


def test_or_else_str() -> None:
    assert str(ada.OrElse(ada.Variable("X"), ada.Variable("Y"))) == "X\nor else Y"


def test_add_str() -> None:
    assert str(ada.Add(ada.Number(1), ada.Call("Test", []))) == "1 + Test"
    assert str(ada.Add(ada.Number(1), -ada.Call("Test", []))) == "1 - Test"


def test_attribute() -> None:
    assert isinstance(ada.Range("X"), ada.Attribute)
    assert isinstance(ada.Old("X"), ada.Attribute)
    assert isinstance(ada.Result("X"), ada.Attribute)
    assert isinstance(ada.Constrained("X"), ada.Attribute)


def test_attribute_str() -> None:
    assert str(ada.First("X")) == "X'First"
    assert str(-ada.First("X")) == "(-X'First)"


def test_attribute_expression_str() -> None:
    assert str(ada.Val("X", ada.Number(1))) == "X'Val (1)"


def test_indexed_str() -> None:
    assert str(ada.Indexed(ada.Variable("X"), ada.Variable("Y"))) == "X (Y)"
    assert str(-ada.Indexed(ada.Variable("X"), ada.Variable("Y"))) == "(-X (Y))"


def test_aggregate_str() -> None:
    assert str(ada.Aggregate(ada.Number(1))) == "(1)"
    assert str(ada.Aggregate(ada.Number(1), ada.Number(2))) == "(1, 2)"


def test_in_str() -> None:
    assert str(ada.In(ada.Variable("X"), ada.Variable("Y"))) == "X in Y"


def test_not_in_str() -> None:
    assert str(ada.NotIn(ada.Variable("X"), ada.Variable("Y"))) == "X not in Y"


def test_if_str() -> None:
    assert_equal(
        str(
            ada.If(
                [(ada.Variable("X"), ada.Number(1)), (ada.Variable("Y"), ada.Number(2))],
                ada.Number(3),
            )
        ),
        multilinestr(
            """(if
                   X
                then
                   1
                elsif
                   Y
                then
                   2
                else
                   3)"""
        ),
    )


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
            )
        ),
        multilinestr(
            """(case X is
                   when Y | Z =>
                      1,
                   when others =>
                      2)"""
        ),
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


def test_string_str() -> None:
    assert str(ada.String("X Y")) == '"X Y"'


def test_expr_str() -> None:
    assert_equal(
        str(
            ada.And(
                ada.If(
                    [(ada.Variable("X"), ada.Number(1)), (ada.Variable("Y"), ada.Number(2))],
                    ada.Number(3),
                ),
                ada.Variable("A"),
                ada.Or(ada.Variable("B"), ada.Variable("C")),
                ada.Variable("D"),
            )
        ),
        multilinestr(
            """(if
                   X
                then
                   1
                elsif
                   Y
                then
                   2
                else
                   3)
               and A
               and (B
                    or C)
               and D"""
        ),
    )
    assert_equal(
        str(
            ada.ForAllOf(
                "X",
                ada.Variable("Z"),
                ada.If(
                    [(ada.Variable("X"), ada.Number(1)), (ada.Variable("Y"), ada.Number(2))],
                    ada.Number(3),
                ),
            )
        ),
        multilinestr(
            """(for all X of Z =>
                   (if
                       X
                    then
                       1
                    elsif
                       Y
                    then
                       2
                    else
                       3))"""
        ),
    )
    assert str(ada.Equal(ada.String("S"), ada.Variable("X"))) == '"S" = X'


def test_call_str() -> None:
    assert str(ada.Call("A", [])) == "A"
    assert str(ada.Call("A", [ada.Variable("B"), ada.Variable("C")])) == "A (B, C)"
    assert str(-ada.Call("A", [])) == "(-A)"


def test_conversion_str() -> None:
    assert str(ada.Conversion("A", ada.Variable("B"))) == "A (B)"
    assert str(ada.Not(ada.Conversion("A", ada.Variable("B")))) == "not A (B)"


def test_import() -> None:
    assert str(ada.Import()) == "Import"


def test_subtype() -> None:
    assert str(ada.Subtype("A", "B")) == "subtype A is B;"


def test_range_subtype() -> None:
    assert (
        str(ada.RangeSubtype("A", "B", ada.Number(1), ada.Number(2)))
        == "subtype A is B range 1 .. 2;"
    )


def test_derived_type() -> None:
    assert str(ada.DerivedType("A", "B")) == "type A is new B;"


def test_private_type() -> None:
    assert str(ada.PrivateType("A")) == "type A is private;"


def test_discrete_type() -> None:
    assert str(ada.DiscreteType("A")) == "type A is (<>);"


def test_array_type() -> None:
    assert str(ada.ArrayType("A", "B", "C")) == "type A is array (B) of C;"


def test_unconstrained_array_type() -> None:
    assert str(ada.UnconstrainedArrayType("A", "B", "C")) == "type A is array (B range <>) of C;"


def test_access_type() -> None:
    assert str(ada.AccessType("A", "B")) == "type A is access B;"


def test_subprogram_renaming_declaration() -> None:
    assert (
        str(ada.SubprogramRenamingDeclaration(ada.ProcedureSpecification("A"), "B"))
        == "procedure A renames B;"
    )
