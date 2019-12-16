from rflx.expression import And, Equal, NotEqual, Or, Variable
from rflx.fsm_expression import Contains, NotContains, Valid
from rflx.fsm_parser import FSMParser


def test_simple_equation() -> None:
    result = FSMParser.condition().parseString("Foo.Bar = abc")[0]
    assert result == Equal(Variable("Foo.Bar"), Variable("abc"))


def test_simple_inequation() -> None:
    result = FSMParser.condition().parseString("Foo.Bar /= abc")[0]
    assert result == NotEqual(Variable("Foo.Bar"), Variable("abc"))


def test_valid() -> None:
    result = FSMParser.condition().parseString("Something'Valid")[0]
    assert result == Valid(Variable("Something"))


def test_conjunction_valid() -> None:
    result = FSMParser.condition().parseString("Foo'Valid and Bar'Valid")[0]
    assert result == And(Valid(Variable("Foo")), Valid(Variable("Bar")))


def test_conjunction() -> None:
    result = FSMParser.condition().parseString("Foo = Bar and Bar /= Baz")[0]
    assert result == And(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_disjunction() -> None:
    result = FSMParser.condition().parseString("Foo = Bar or Bar /= Baz")[0]
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_in_operator() -> None:
    result = FSMParser.condition().parseString("Foo in Bar")[0]
    assert result == Contains(Variable("Foo"), Variable("Bar"))


def test_not_in_operator() -> None:
    result = FSMParser.condition().parseString("Foo not in Bar")[0]
    assert result == NotContains(Variable("Foo"), Variable("Bar"))
