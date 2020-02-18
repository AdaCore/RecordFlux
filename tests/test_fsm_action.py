from rflx.expression import Variable
from rflx.fsm_expression import SubprogramCall
from rflx.fsm_parser import FSMParser
from rflx.statement import Assignment


def test_simple_assignment() -> None:
    result = FSMParser.action().parseString("Foo := Bar")[0]
    assert result == Assignment("Foo", Variable("Bar"))


def test_simple_subprogram_call() -> None:
    result = FSMParser.action().parseString("Sub (Arg)")[0]
    expected = SubprogramCall("Sub", [Variable("Arg")])
    assert result == expected


def test_list_append() -> None:
    result = FSMParser.action().parseString("Extensions_List'Append (Foo)")[0]
    expected = Assignment(
        "Extensions_List", SubprogramCall("Append", [Variable("Extensions_List"), Variable("Foo")]),
    )
    assert result == expected
