from rflx.fsm_parser import FSMParser


def test_simple_equation() -> None:
    FSMParser.condition().parseString("Foo.Bar = abc")
