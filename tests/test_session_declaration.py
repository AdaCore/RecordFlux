import pytest

from rflx.declaration import Argument, PrivateDeclaration, Renames, Subprogram, VariableDeclaration
from rflx.error import RecordFluxError
from rflx.expression import FALSE, Selected, Variable
from rflx.identifier import ID
from rflx.parser.session import declaration


def test_simple_function_declaration() -> None:
    result = declaration("Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type")
    expected = (
        ID("Foo"),
        Subprogram([Argument("Arg1", "Arg1_Type"), Argument("Arg2", "Arg2_Type")], "Foo_Type",),
    )
    assert result == expected


def test_invalid_function_name() -> None:
    with pytest.raises(RecordFluxError):
        declaration("Foo.Bar (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Foo_Type")


def test_invalid_parameter_name() -> None:
    with pytest.raises(RecordFluxError):
        declaration("Foo (Arg1 : Arg1_Type; Arg2.Invalid : Arg2_Type) return Foo_Type")


def test_private_variable_declaration() -> None:
    result = declaration("Hash_Context is private")
    expected = (ID("Hash_Context"), PrivateDeclaration())
    assert result == expected


def test_parameterless_function_declaration() -> None:
    result = declaration("Foo return Foo_Type")
    expected = (ID("Foo"), Subprogram([], "Foo_Type"))
    assert result == expected


def test_simple_variable_declaration() -> None:
    result = declaration("Certificate_Authorities : TLS_Handshake.Certificate_Authorities")
    expected = (
        ID("Certificate_Authorities"),
        VariableDeclaration("TLS_Handshake.Certificate_Authorities"),
    )
    assert result == expected


def test_variable_declaration_with_initialization() -> None:
    result = declaration("Certificate_Authorities_Received : Boolean := False")
    expected = (
        ID("Certificate_Authorities_Received"),
        VariableDeclaration("Boolean", FALSE),
    )
    assert result == expected


def test_renames() -> None:
    result = declaration(
        "Certificate_Message : TLS_Handshake.Certificate renames CCR_Handshake_Message.Payload"
    )
    expected = (
        ID("Certificate_Message"),
        Renames(
            "TLS_Handshake.Certificate", Selected(Variable("CCR_Handshake_Message"), "Payload")
        ),
    )
    assert result == expected
