from pathlib import Path, PosixPath

import pytest

from rflx import expression as expr
from rflx.error import Location
from rflx.identifier import ID
from rflx.ls.model import LSModel, Symbol, SymbolCategory
from rflx.model import UncheckedEnumeration, UncheckedInteger, UncheckedSequence
from rflx.specification.parser import Parser


@pytest.fixture()
def model() -> LSModel:
    parser = Parser()
    for path in Path("tests/unit/ls/data").glob("*.rflx"):
        if path.is_file():
            parser.parse_string(path.read_text())
    return LSModel(parser.create_unchecked_model())


def test_model_get_types(model: LSModel) -> None:
    assert model.get_symbols("notatype") == []
    assert set(model.get_symbols("Message")) == {
        Symbol(
            ID("Universal::Message"),
            SymbolCategory.MESSAGE,
            Location((37, 9), PosixPath("<stdin>"), (77, 18)),
            None,
        ),
        Symbol(
            ID("Test::Session::Message"),
            SymbolCategory.SESSION_MEMBER,
            Location((33, 7), PosixPath("<stdin>"), (33, 35)),
            ID("Test::Session"),
        ),
    }
    assert set(model.get_symbols("Message_Type")) == {
        Symbol(
            ID("Universal::Message_Type"),
            SymbolCategory.ENUMERATION,
            Location((3, 9), PosixPath("<stdin>"), (10, 71)),
            None,
        ),
        Symbol(
            ID("Universal::Message::Message_Type"),
            SymbolCategory.MESSAGE_FIELD,
            Location((39, 10), PosixPath("<stdin>"), (39, 22)),
            ID("Universal::Message"),
        ),
        Symbol(
            ID("Test::Definite_Message::Message_Type"),
            SymbolCategory.MESSAGE_FIELD,
            Location((11, 10), PosixPath("<stdin>"), (11, 22)),
            ID("Test::Definite_Message"),
        ),
        Symbol(
            ID("Test::Session::Process::Message_Type"),
            SymbolCategory.SESSION_STATE_VARIABLE,
            Location((49, 10), PosixPath("<stdin>"), (49, 22)),
            ID("Test::Session::Process"),
        ),
        Symbol(
            ID("Test::Session::Create_Message::Message_Type"),
            SymbolCategory.SESSION_FUNCTION_PARAMETER,
            Location((23, 11), PosixPath("<stdin>"), (23, 23)),
            ID("Test::Session::Create_Message"),
        ),
        Symbol(
            ID("Test::Session::Valid_Message::Message_Type"),
            SymbolCategory.SESSION_FUNCTION_PARAMETER,
            Location((29, 11), PosixPath("<stdin>"), (29, 23)),
            ID("Test::Session::Valid_Message"),
        ),
    }


def test_integer_to_symbols() -> None:
    integer = UncheckedInteger(
        ID("Package::Integer_Identifier"),
        expr.Number(0),
        expr.Number(100),
        expr.Number(8),
        None,
    )
    symbols = LSModel._to_symbols(integer)  # noqa: SLF001
    assert len(symbols) == 1
    assert symbols[0] == Symbol(
        ID("Package::Integer_Identifier"),
        SymbolCategory.NUMERIC,
        None,
        None,
    )


def test_enumeration_to_symbols() -> None:
    symbols = LSModel._to_symbols(  # noqa: SLF001
        UncheckedEnumeration(
            ID("Package::Enumeration_Identifier"),
            [(ID("Literal"), expr.Number(0))],
            expr.Number(1),
            always_valid=True,
            location=None,
        ),
    )
    assert len(symbols) == 2
    assert symbols[0] == Symbol(
        ID("Package::Literal"),
        SymbolCategory.ENUMERATION_LITERAL,
        None,
        None,
    )
    assert symbols[1] == Symbol(
        ID("Package::Enumeration_Identifier"),
        SymbolCategory.ENUMERATION,
        None,
        None,
    )


def test_sequence_to_symbols() -> None:
    symbols = LSModel._to_symbols(  # noqa: SLF001
        UncheckedSequence(
            ID("Package::Sequence_Identifier"),
            ID("Package::Integer_Identifier"),
            None,
        ),
    )
    assert len(symbols) == 1
    assert symbols[0] == Symbol(
        ID("Package::Sequence_Identifier"),
        SymbolCategory.SEQUENCE,
        None,
        None,
    )


def test_to_symbol() -> None:
    symbol = LSModel._to_symbol(ID("Package_Identifier"))  # noqa: SLF001
    assert symbol == Symbol(ID("Package_Identifier"), SymbolCategory.PACKAGE, None, None)


def test_model_complete(model: LSModel) -> None:
    assert "Integer" in model
    assert "Enum_T" in model
    assert "Msg" in model
    assert "Msg_LE_Nested" in model
    assert "Msg_LE" in model

    assert "Message_Type" in model
    assert "MT_Null" in model
    assert "MT_Data" in model
    assert "MT_Value" in model
    assert "MT_Values" in model
    assert "MT_Option_Types" in model
    assert "MT_Options" in model
    assert "MT_Unconstrained_Data" in model
    assert "MT_Unconstrained_Options" in model
    assert "Length" in model
    assert "Value" in model
    assert "Values" in model
    assert "Option_Type" in model
    assert "Option_Types" in model
    assert "Option" in model
    assert "Options" in model
    assert "Message" in model

    assert "Result" in model
    assert "Length" in model
    assert "Definite_Message" in model
    assert "Session" in model
