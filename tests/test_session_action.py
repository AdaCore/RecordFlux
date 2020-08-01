from rflx.expression import Call, String, Variable
from rflx.parser.session import action
from rflx.statement import Assignment, Erase, Reset


def test_simple_assignment() -> None:
    result = action("Foo := Bar")
    assert result == Assignment("Foo", Variable("Bar"))


def test_simple_subprogram_call() -> None:
    result = action("Sub (Arg)")
    expected = Call("Sub", [Variable("Arg")])
    assert result == expected


def test_list_append() -> None:
    result = action("Extensions_List'Append (Foo)")
    expected = Assignment(
        "Extensions_List", Call("Append", [Variable("Extensions_List"), Variable("Foo")]),
    )
    assert result == expected


def test_subprogram_string_argument() -> None:
    result = action('Sub (Arg1, "String arg", Arg2)')
    expected = Call("Sub", [Variable("Arg1"), String("String arg"), Variable("Arg2")])
    assert result == expected


def test_variable_erasure() -> None:
    result = action("Variable := null")
    expected = Erase("Variable")
    assert result == expected


def test_list_reset() -> None:
    result = action("SomeList'Reset")
    expected = Reset("SomeList")
    assert result == expected
