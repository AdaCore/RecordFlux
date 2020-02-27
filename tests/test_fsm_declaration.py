import pytest
from pyparsing import ParseException

from rflx.error import RecordFluxError
from rflx.expression import (
    FALSE,
    Argument,
    Channel,
    PrivateDeclaration,
    Renames,
    Subprogram,
    Variable,
    VariableDeclaration,
)
from rflx.fsm import FSM, State, StateMachine, StateName, Transition
from rflx.fsm_expression import Field, SubprogramCall
from rflx.fsm_parser import FSMParser
from rflx.identifier import ID
from rflx.statement import Assignment


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


def test_private_variable_declaration() -> None:
    result = FSMParser.declaration().parseString("Hash_Context is private")[0]
    expected = (ID("Hash_Context"), PrivateDeclaration())
    assert result == expected


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


def test_renames() -> None:
    result = FSMParser.declaration().parseString(
        "Certificate_Message : TLS_Handshake.Certificate renames CCR_Handshake_Message.Payload"
    )[0]
    expected = (
        ID("Certificate_Message"),
        Renames("TLS_Handshake.Certificate", Field(Variable("CCR_Handshake_Message"), "Payload")),
    )
    assert result == expected


def test_channels() -> None:
    f = FSM()
    f.parse_string(
        "fsm",
        """
            channels:
                - name: Channel1_Read_Write
                  mode: Read_Write
                - name: Channel2_Read
                  mode: Read
                - name: Channel3_Write
                  mode: Write
            initial: START
            final: END
            states:
              - name: START
                transitions:
                  - target: END
                variables:
                  - "Local : Boolean"
                actions:
                  - Local := Write(Channel1_Read_Write, Read(Channel2_Read))
                  - Local := Write(Channel3_Write, Local)
              - name: END
        """,
    )
    expected = StateMachine(
        name="fsm",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[Transition(target=StateName("END"))],
                declarations={ID("Local"): VariableDeclaration("Boolean")},
                actions=[
                    Assignment(
                        "Local",
                        SubprogramCall(
                            "Write",
                            [
                                Variable("Channel1_Read_Write"),
                                SubprogramCall("Read", [Variable("Channel2_Read")]),
                            ],
                        ),
                    ),
                    Assignment(
                        "Local",
                        SubprogramCall("Write", [Variable("Channel3_Write"), Variable("Local")],),
                    ),
                ],
            ),
            State(name=StateName("END")),
        ],
        declarations={
            "Channel1_Read_Write": Channel(read=True, write=True),
            "Channel2_Read": Channel(read=True, write=False),
            "Channel3_Write": Channel(read=False, write=True),
        },
    )
    assert f.fsms[0] == expected


def test_channel_with_invalid_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match="^session: error: channel Channel1_Read_Write has invalid mode Invalid",
    ):
        FSM().parse_string(
            "fsm",
            """
                channels:
                    - name: Channel1_Read_Write
                      mode: Invalid
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                    variables:
                      - "Local : Boolean"
                    actions:
                      - Local := Read(Channel1_Read_Write)
                  - name: END
            """,
        )


def test_channel_without_name() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: channel 0 has no name"):
        FSM().parse_string(
            "fsm",
            """
                channels:
                    - mode: Read_Write
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_channel_without_mode() -> None:
    with pytest.raises(
        RecordFluxError, match="^session: error: channel Channel_Without_Mode has no mode"
    ):
        FSM().parse_string(
            "fsm",
            """
                channels:
                    - name: Channel_Without_Mode
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )
