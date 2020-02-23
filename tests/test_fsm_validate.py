from rflx.expression import Equal, Variable
from rflx.fsm_expression import (
    Binding,
    Comprehension,
    Contains,
    Field,
    ForAll,
    ForSome,
    MessageAggregate,
    NotContains,
    String,
    SubprogramCall,
)


def test_binding_aggregate() -> None:
    binding = Binding(
        MessageAggregate("M1", {"Data": Variable("B1")}),
        {"B1": MessageAggregate("M2", {"Data": Variable("B2")})},
    )
    expected = MessageAggregate("M1", {"Data": MessageAggregate("M2", {"Data": Variable("B2")})})
    result = binding.simplified()
    assert result == expected


def test_binding_forall_predicate() -> None:
    binding = Binding(
        ForAll("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))), {"Bar": Variable("Baz")},
    )
    expected = ForAll("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_forall_iterable() -> None:
    binding = Binding(
        ForAll("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))), {"Y": Variable("Baz")},
    )
    expected = ForAll("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_forsome_predicate() -> None:
    binding = Binding(
        ForSome("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Bar": Variable("Baz")},
    )
    expected = ForSome("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_forsome_iterable() -> None:
    binding = Binding(
        ForSome("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))), {"Y": Variable("Baz")},
    )
    expected = ForSome("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_contains_left() -> None:
    binding = Binding(Contains(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = Contains(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_contains_right() -> None:
    binding = Binding(Contains(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = Contains(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_left() -> None:
    binding = Binding(NotContains(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = NotContains(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_right() -> None:
    binding = Binding(NotContains(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = NotContains(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_subprogram() -> None:
    binding = Binding(
        SubprogramCall("Sub", [Variable("A"), Variable("B"), Variable("C")]),
        {"B": Variable("Baz")},
    )
    expected = SubprogramCall("Sub", [Variable("A"), Variable("Baz"), Variable("C")])
    result = binding.simplified()
    assert result == expected


def test_binding_field() -> None:
    binding = Binding(Field(Variable("A"), "fld"), {"A": Variable("Baz")})
    expected = Field(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_list_comprehension() -> None:
    binding = Binding(
        Comprehension(
            "E", Variable("List"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
        ),
        {"List": Variable("Foo")},
    )
    expected = Comprehension(
        "E", Variable("Foo"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
    )
    result = binding.simplified()
    assert result == expected


def test_simplify_string() -> None:
    value = String("Test")
    assert value == value.simplified()


def test_binding_multiple_bindings() -> None:
    binding = Binding(
        Field(Variable("A"), "fld"), {"A": Binding(Variable("B"), {"B": Variable("Baz")})}
    )
    expected = Field(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_multiple_variables() -> None:
    binding = Binding(SubprogramCall("Sub", [Variable("A"), Variable("A")]), {"A": Variable("Baz")})
    expected = SubprogramCall("Sub", [Variable("Baz"), Variable("Baz")])
    result = binding.simplified()
    assert result == expected
