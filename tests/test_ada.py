import rflx.ada as ada
import rflx.expression as expr


def test_import() -> None:
    assert str(ada.Import()) == "Import"


def test_subtype() -> None:
    assert str(ada.Subtype("A", "B")) == "subtype A is B;"


def test_range_subtype() -> None:
    assert (
        str(ada.RangeSubtype("A", "B", expr.Number(1), expr.Number(2)))
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
