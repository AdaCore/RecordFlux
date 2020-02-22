import pytest
from pyparsing import ParseException

from rflx.expression import FALSE
from rflx.fsm_declaration import Argument, Subprogram, VariableDeclaration
from rflx.fsm_parser import FSMParser
from rflx.identifier import ID


def test_simple_function_declaration() -> None:
    result = FSMParser.declaration().parseString(
        "Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type"
    )[0]
    expected = (
        ID("Foo"),
        Subprogram([Argument("Arg1", "Arg1_Type"), Argument("Arg2", "Arg2_Type")], "Foo_Type",),
    )
    assert result == expected


def test_invalid_function_name() -> None:
    with pytest.raises(ParseException):
        # pylint: disable=expression-not-assigned
        FSMParser.declaration().parseString(
            "Foo.Bar (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type"
        )[0]


def test_invalid_parameter_name() -> None:
    with pytest.raises(ParseException):
        # pylint: disable=expression-not-assigned
        FSMParser.declaration().parseString(
            "Foo (Arg1 : Arg1_Type; Arg2.Invalid : Arg2_Type) return Foo_Type"
        )[0]


def test_parameterless_function_declaration() -> None:
    result = FSMParser.declaration().parseString("Foo return Foo_Type")[0]
    expected = (ID("Foo"), Subprogram([], "Foo_Type"))
    assert result == expected


def test_simple_variable_declaration() -> None:
    result = FSMParser.declaration().parseString(
        "Certificate_Authorities : TLS_Handshake.Certificate_Authorities"
    )[0]
    expected = (
        ID("Certificate_Authorities"),
        VariableDeclaration("TLS_Handshake.Certificate_Authorities"),
    )
    assert result == expected


def test_variable_declaration_with_initialization() -> None:
    result = FSMParser.declaration().parseString(
        "Certificate_Authorities_Received : Boolean := False"
    )[0]
    expected = (
        ID("Certificate_Authorities_Received"),
        VariableDeclaration("Boolean", FALSE),
    )
    assert result == expected
