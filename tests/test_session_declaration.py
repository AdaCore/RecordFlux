import pytest
from pyparsing import ParseException

from rflx.expression import (
    FALSE,
    Argument,
    PrivateDeclaration,
    Renames,
    Selected,
    Subprogram,
    Variable,
    VariableDeclaration,
)
from rflx.identifier import ID
from rflx.parser.session import declaration


def test_simple_function_declaration() -> None:
    result = declaration().parseString("Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type")[
        0
    ]
    expected = (
        ID("Foo"),
        Subprogram([Argument("Arg1", "Arg1_Type"), Argument("Arg2", "Arg2_Type")], "Foo_Type",),
    )
    assert result == expected


def test_invalid_function_name() -> None:
    with pytest.raises(ParseException):
        # pylint: disable=expression-not-assigned
        declaration().parseString("Foo.Bar (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type")[0]


def test_invalid_parameter_name() -> None:
    with pytest.raises(ParseException):
        # pylint: disable=expression-not-assigned
        declaration().parseString(
            "Foo (Arg1 : Arg1_Type; Arg2.Invalid : Arg2_Type) return Foo_Type"
        )[0]


def test_private_variable_declaration() -> None:
    result = declaration().parseString("Hash_Context is private")[0]
    expected = (ID("Hash_Context"), PrivateDeclaration())
    assert result == expected


def test_parameterless_function_declaration() -> None:
    result = declaration().parseString("Foo return Foo_Type")[0]
    expected = (ID("Foo"), Subprogram([], "Foo_Type"))
    assert result == expected


def test_simple_variable_declaration() -> None:
    result = declaration().parseString(
        "Certificate_Authorities : TLS_Handshake.Certificate_Authorities"
    )[0]
    expected = (
        ID("Certificate_Authorities"),
        VariableDeclaration("TLS_Handshake.Certificate_Authorities"),
    )
    assert result == expected


def test_variable_declaration_with_initialization() -> None:
    result = declaration().parseString("Certificate_Authorities_Received : Boolean := False")[0]
    expected = (
        ID("Certificate_Authorities_Received"),
        VariableDeclaration("Boolean", FALSE),
    )
    assert result == expected


def test_renames() -> None:
    result = declaration().parseString(
        "Certificate_Message : TLS_Handshake.Certificate renames CCR_Handshake_Message.Payload"
    )[0]
    expected = (
        ID("Certificate_Message"),
        Renames(
            "TLS_Handshake.Certificate", Selected(Variable("CCR_Handshake_Message"), "Payload")
        ),
    )
    assert result == expected
