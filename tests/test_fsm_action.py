from rflx.expression import Variable
from rflx.fsm_parser import FSMParser
from rflx.statement import Assignment


def test_simple_assignment() -> None:
    result = FSMParser.action().parseString("Foo := Bar")[0]
    assert result == Assignment("Foo", Variable("Bar"))
