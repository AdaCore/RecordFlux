"""
Collection of models for testing.

All types are represented by functions instead of variables to avoid time-consuming operations (such
as message verifications) during test collection. This also prevents new processes from starting
before the current process has finished its bootstrapping phase, which would result in a runtime
error.
"""

from __future__ import annotations

from collections.abc import Callable
from functools import lru_cache
from typing import Final

from rflx.error import Location
from rflx.expression import (
    Aggregate,
    And,
    Div,
    Equal,
    First,
    GreaterEqual,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Or,
    Pow,
    Size,
    Sub,
    ValidChecksum,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    OPAQUE,
    DerivedMessage,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Model,
    Refinement,
    Sequence,
    Session,
    State,
    Transition,
)


@lru_cache
def null_message() -> Message:
    return Message("Null_Msg::Message", [], {})


@lru_cache
def null_model() -> Model:
    return Model([null_message()])


@lru_cache
def tlv_tag() -> Enumeration:
    return Enumeration(
        "TLV::Tag",
        [("Msg_Data", Number(1)), ("Msg_Error", Number(3))],
        Number(8),
        always_valid=False,
    )


@lru_cache
def tlv_length() -> Integer:
    return Integer(
        "TLV::Length",
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def tlv_message() -> Message:
    return Message(
        "TLV::Message",
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
            Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), FINAL),
        ],
        {Field("Tag"): tlv_tag(), Field("Length"): tlv_length(), Field("Value"): OPAQUE},
    )


@lru_cache
def tlv_model() -> Model:
    return Model([tlv_tag(), tlv_length(), tlv_message()])


@lru_cache
def tlv_messages() -> Sequence:
    return Sequence("TLV::Messages", tlv_message())


@lru_cache
def tlv_tags() -> Sequence:
    return Sequence("TLV::Tags", tlv_tag())


@lru_cache
def tlv_with_checksum_tag() -> Enumeration:
    return Enumeration(
        "TLV_With_Checksum::Tag",
        [("Msg_Data", Number(1)), ("Msg_Error", Number(3))],
        Number(8),
        always_valid=False,
    )


@lru_cache
def tlv_with_checksum_length() -> Integer:
    return Integer(
        "TLV_With_Checksum::Length",
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def tlv_with_checksum_checksum() -> Integer:
    return Integer(
        "TLV_With_Checksum::Checksum",
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def tlv_with_checksum_message() -> Message:
    return Message(
        "TLV_With_Checksum::Message",
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
            Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), Field("Checksum")),
            Link(Field("Checksum"), FINAL, ValidChecksum("Checksum")),
        ],
        {
            Field("Tag"): tlv_with_checksum_tag(),
            Field("Length"): tlv_with_checksum_length(),
            Field("Value"): OPAQUE,
            Field("Checksum"): tlv_with_checksum_checksum(),
        },
        checksums={ID("Checksum"): [Variable("Tag"), Size("Value"), Variable("Value")]},
    )


@lru_cache
def tlv_with_checksum_model() -> Model:
    return Model(
        [tlv_with_checksum_tag(), tlv_with_checksum_length(), tlv_with_checksum_message()],
    )


@lru_cache
def null_message_in_tlv_message() -> Refinement:
    return Refinement("In_TLV", tlv_message(), Field("Value"), null_message())


@lru_cache
def null_message_in_tlv_message_model() -> Model:
    return Model(
        [tlv_tag(), tlv_length(), tlv_message(), null_message(), null_message_in_tlv_message()],
    )


@lru_cache
def ethernet_address() -> Integer:
    return Integer(
        "Ethernet::Address",
        Number(0),
        Sub(Pow(Number(2), Number(48)), Number(1)),
        Number(48),
    )


@lru_cache
def ethernet_type_length() -> Integer:
    return Integer(
        "Ethernet::Type_Length",
        Number(46),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def ethernet_tpid() -> Integer:
    return Integer("Ethernet::TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16))


@lru_cache
def ethernet_tci() -> Integer:
    return Integer(
        "Ethernet::TCI",
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def ethernet_frame() -> Message:
    return Message(
        "Ethernet::Frame",
        [
            Link(INITIAL, Field("Destination")),
            Link(Field("Destination"), Field("Source")),
            Link(Field("Source"), Field("Type_Length_TPID")),
            Link(
                Field("Type_Length_TPID"),
                Field("TPID"),
                Equal(Variable("Type_Length_TPID"), Number(0x8100, 16)),
                first=First("Type_Length_TPID"),
            ),
            Link(
                Field("Type_Length_TPID"),
                Field("Type_Length"),
                NotEqual(Variable("Type_Length_TPID"), Number(0x8100, 16)),
                first=First("Type_Length_TPID"),
            ),
            Link(Field("TPID"), Field("TCI")),
            Link(Field("TCI"), Field("Type_Length")),
            Link(
                Field("Type_Length"),
                Field("Payload"),
                LessEqual(Variable("Type_Length"), Number(1500)),
                Mul(Variable("Type_Length"), Number(8)),
            ),
            Link(
                Field("Type_Length"),
                Field("Payload"),
                GreaterEqual(Variable("Type_Length"), Number(1536)),
            ),
            Link(
                Field("Payload"),
                FINAL,
                And(
                    GreaterEqual(Div(Size("Payload"), Number(8)), Number(46)),
                    LessEqual(Div(Size("Payload"), Number(8)), Number(1500)),
                ),
            ),
        ],
        {
            Field("Destination"): ethernet_address(),
            Field("Source"): ethernet_address(),
            Field("Type_Length_TPID"): ethernet_type_length(),
            Field("TPID"): ethernet_tpid(),
            Field("TCI"): ethernet_tci(),
            Field("Type_Length"): ethernet_type_length(),
            Field("Payload"): OPAQUE,
        },
    )


@lru_cache
def ethernet_model() -> Model:
    return Model(
        [
            ethernet_address(),
            ethernet_type_length(),
            ethernet_tpid(),
            ethernet_tci(),
            ethernet_frame(),
        ],
    )


@lru_cache
def enumeration_priority() -> Enumeration:
    return Enumeration(
        "Enumeration::Priority",
        [("Low", Number(1)), ("Medium", Number(4)), ("High", Number(7))],
        Number(8),
        always_valid=True,
    )


@lru_cache
def enumeration_message() -> Message:
    return Message(
        "Enumeration::Message",
        [Link(INITIAL, Field("Priority")), Link(Field("Priority"), FINAL)],
        {Field("Priority"): enumeration_priority()},
    )


@lru_cache
def enumeration_model() -> Model:
    return Model([enumeration_priority(), enumeration_message()])


@lru_cache
def sequence_length() -> Integer:
    return Integer(
        "Sequence::Length",
        Number(0),
        Sub(Pow(Number(2), Number(8)), Number(1)),
        Number(8),
    )


@lru_cache
def sequence_integer() -> Integer:
    return Integer("Sequence::Integer", Number(1), Number(100), Number(16))


@lru_cache
def sequence_integer_vector() -> Sequence:
    return Sequence("Sequence::Integer_Vector", sequence_integer())


@lru_cache
def sequence_enumeration() -> Enumeration:
    return Enumeration(
        "Sequence::Enumeration",
        [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
        Number(8),
        always_valid=False,
    )


@lru_cache
def sequence_enumeration_vector() -> Sequence:
    return Sequence("Sequence::Enumeration_Vector", sequence_enumeration())


@lru_cache
def sequence_av_enumeration() -> Enumeration:
    return Enumeration(
        "Sequence::AV_Enumeration",
        [("AV_Zero", Number(0)), ("AV_One", Number(1)), ("AV_Two", Number(2))],
        Number(8),
        always_valid=True,
    )


@lru_cache
def sequence_av_enumeration_vector() -> Sequence:
    return Sequence(
        "Sequence::AV_Enumeration_Vector",
        sequence_av_enumeration(),
    )


@lru_cache
def sequence_message() -> Message:
    return Message(
        "Sequence::Message",
        [
            Link(INITIAL, Field("Length")),
            Link(Field("Length"), Field("Integer_Vector"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Integer_Vector"), Field("Enumeration_Vector"), size=Number(16)),
            Link(Field("Enumeration_Vector"), Field("AV_Enumeration_Vector"), size=Number(16)),
            Link(Field("AV_Enumeration_Vector"), FINAL),
        ],
        {
            Field("Length"): sequence_length(),
            Field("Integer_Vector"): sequence_integer_vector(),
            Field("Enumeration_Vector"): sequence_enumeration_vector(),
            Field("AV_Enumeration_Vector"): sequence_av_enumeration_vector(),
        },
    )


@lru_cache
def sequence_inner_message() -> Message:
    return Message(
        "Sequence::Inner_Message",
        [
            Link(INITIAL, Field("Length")),
            Link(Field("Length"), Field("Payload"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Payload"), FINAL),
        ],
        {Field("Length"): sequence_length(), Field("Payload"): OPAQUE},
    )


@lru_cache
def sequence_inner_messages() -> Sequence:
    return Sequence("Sequence::Inner_Messages", sequence_inner_message())


@lru_cache
def sequence_messages_message() -> Message:
    return Message(
        "Sequence::Messages_Message",
        [
            Link(INITIAL, Field("Length")),
            Link(Field("Length"), Field("Messages"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Messages"), FINAL),
        ],
        {Field("Length"): sequence_length(), Field("Messages"): sequence_inner_messages()},
    )


@lru_cache
def sequence_sequence_size_defined_by_message_size() -> Message:
    return Message(
        "Sequence::Sequence_Size_Defined_By_Message_Size",
        [
            Link(INITIAL, Field("Header")),
            Link(Field("Header"), Field("Vector")),
            Link(Field("Vector"), FINAL),
        ],
        {
            Field("Header"): sequence_enumeration(),
            Field("Vector"): sequence_integer_vector(),
        },
    )


@lru_cache
def sequence_model() -> Model:
    return Model(
        [
            sequence_length(),
            sequence_integer(),
            sequence_integer_vector(),
            sequence_enumeration(),
            sequence_enumeration_vector(),
            sequence_av_enumeration(),
            sequence_av_enumeration_vector(),
            sequence_message(),
            sequence_inner_message(),
            sequence_inner_messages(),
            sequence_messages_message(),
            sequence_sequence_size_defined_by_message_size(),
        ],
    )


@lru_cache
def expression_message() -> Message:
    return Message(
        "Expression::Message",
        [
            Link(INITIAL, Field("Payload"), size=Number(16)),
            Link(
                Field("Payload"),
                FINAL,
                Equal(Variable("Payload"), Aggregate(Number(1), Number(2))),
            ),
        ],
        {Field("Payload"): OPAQUE},
    )


@lru_cache
def expression_model() -> Model:
    return Model([expression_message()])


@lru_cache
def derivation_message() -> DerivedMessage:
    return DerivedMessage("Derivation::Message", tlv_message())


@lru_cache
def derivation_model() -> Model:
    return Model([derivation_message()])


@lru_cache
def valid_message() -> Message:
    return Message(
        "P::M",
        [
            Link(INITIAL, Field("F"), size=Number(16)),
            Link(Field("F"), FINAL),
        ],
        {Field("F"): OPAQUE},
    )


@lru_cache
def integer() -> Integer:
    return Integer("P::Integer", Number(1), Number(220), Number(8))


@lru_cache
def enumeration() -> Enumeration:
    return Enumeration(
        "P::Enumeration",
        [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
        Number(8),
        always_valid=False,
        location=Location((10, 2)),
    )


@lru_cache
def message() -> Message:
    return Message(
        "P::M",
        [
            Link(INITIAL, Field("F"), size=Number(16)),
            Link(Field("F"), FINAL),
        ],
        {Field("F"): OPAQUE},
    )


@lru_cache
def refinement() -> Refinement:
    return Refinement("In_Message", message(), Field("F"), message())


@lru_cache
def universal_message_type() -> Enumeration:
    return Enumeration(
        "Universal::Message_Type",
        [
            ("MT_Null", Number(0)),
            ("MT_Data", Number(1)),
            ("MT_Value", Number(2)),
            ("MT_Values", Number(3)),
            ("MT_Option_Types", Number(4)),
            ("MT_Options", Number(5)),
            ("MT_Unconstrained_Data", Number(6)),
            ("MT_Unconstrained_Options", Number(7)),
        ],
        size=Number(8),
        always_valid=False,
    )


@lru_cache
def universal_length() -> Integer:
    return Integer(
        "Universal::Length",
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
    )


@lru_cache
def universal_value() -> Integer:
    return Integer(
        "Universal::Value",
        Number(0),
        Sub(Pow(Number(2), Number(8)), Number(1)),
        Number(8),
    )


@lru_cache
def universal_values() -> Sequence:
    return Sequence("Universal::Values", universal_value())


@lru_cache
def universal_option_type() -> Enumeration:
    return Enumeration(
        "Universal::Option_Type",
        [
            ("OT_Null", Number(0)),
            ("OT_Data", Number(1)),
        ],
        size=Number(8),
        always_valid=True,
    )


@lru_cache
def universal_option_types() -> Sequence:
    return Sequence("Universal::Option_Types", universal_option_type())


@lru_cache
def universal_option() -> Message:
    return Message(
        "Universal::Option",
        [
            Link(INITIAL, Field("Option_Type")),
            Link(
                Field("Option_Type"),
                FINAL,
                condition=Equal(Variable("Option_Type"), Variable("OT_Null")),
            ),
            Link(
                Field("Option_Type"),
                Field("Length"),
                condition=Equal(Variable("Option_Type"), Variable("OT_Data")),
            ),
            Link(
                Field("Length"),
                Field("Data"),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(Field("Data"), FINAL),
        ],
        {
            Field("Option_Type"): universal_option_type(),
            Field("Length"): universal_length(),
            Field("Data"): OPAQUE,
        },
    )


@lru_cache
def universal_options() -> Sequence:
    return Sequence("Universal::Options", universal_option())


UNIVERSAL_MESSAGE_ID: Final = ID("Universal::Message")


@lru_cache
def universal_message() -> Message:
    return Message(
        UNIVERSAL_MESSAGE_ID,
        [
            Link(INITIAL, Field("Message_Type")),
            Link(
                Field("Message_Type"),
                FINAL,
                condition=Equal(Variable("Message_Type"), Variable("MT_Null")),
            ),
            Link(
                Field("Message_Type"),
                Field("Data"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Unconstrained_Data")),
            ),
            Link(
                Field("Message_Type"),
                Field("Options"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Unconstrained_Options")),
            ),
            Link(
                Field("Message_Type"),
                Field("Length"),
                condition=And(
                    NotEqual(Variable("Message_Type"), Variable("MT_Null")),
                    NotEqual(Variable("Message_Type"), Variable("MT_Unconstrained_Data")),
                    NotEqual(Variable("Message_Type"), Variable("MT_Unconstrained_Options")),
                ),
            ),
            Link(
                Field("Length"),
                Field("Data"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Data")),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(Field("Data"), FINAL),
            Link(
                Field("Length"),
                Field("Value"),
                condition=And(
                    Equal(Variable("Message_Type"), Variable("MT_Value")),
                    Equal(Variable("Length"), Div(Size("Universal::Value"), Number(8))),
                ),
            ),
            Link(Field("Value"), FINAL),
            Link(
                Field("Length"),
                Field("Values"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Values")),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(Field("Values"), FINAL),
            Link(
                Field("Length"),
                Field("Option_Types"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Option_Types")),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(Field("Option_Types"), FINAL),
            Link(
                Field("Length"),
                Field("Options"),
                condition=Equal(Variable("Message_Type"), Variable("MT_Options")),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(Field("Options"), FINAL),
        ],
        {
            Field("Message_Type"): universal_message_type(),
            Field("Length"): universal_length(),
            Field("Data"): OPAQUE,
            Field("Value"): universal_value(),
            Field("Values"): universal_values(),
            Field("Option_Types"): universal_option_types(),
            Field("Options"): universal_options(),
        },
    )


@lru_cache
def universal_refinement() -> Refinement:
    return Refinement("Universal", universal_message(), Field("Data"), universal_option())


@lru_cache
def universal_model() -> Model:
    return Model(
        [
            universal_message_type(),
            universal_length(),
            OPAQUE,
            universal_option_type(),
            universal_option_types(),
            universal_option(),
            universal_options(),
            universal_value(),
            universal_values(),
            universal_message(),
            universal_refinement(),
        ],
    )


@lru_cache
def fixed_size_message() -> Message:
    return Message(
        "Fixed_Size::Message",
        [
            Link(INITIAL, Field("Message_Type")),
            Link(
                Field("Message_Type"),
                Field("Data"),
                condition=Or(
                    Equal(Variable("Message_Type"), Variable("Universal::MT_Null")),
                    Equal(Variable("Message_Type"), Variable("Universal::MT_Data")),
                    Equal(Variable("Message_Type"), Variable("Universal::MT_Values")),
                    Equal(Variable("Message_Type"), Variable("Universal::MT_Options")),
                ),
                size=Number(64),
            ),
            Link(
                Field("Data"),
                Field("Values"),
                size=Mul(Number(8), Size("Universal::Value")),
            ),
            Link(
                Field("Values"),
                Field("Options"),
                size=Number(64),
            ),
            Link(
                Field("Options"),
                FINAL,
            ),
        ],
        {
            Field("Message_Type"): universal_message_type(),
            Field("Data"): OPAQUE,
            Field("Values"): universal_values(),
            Field("Options"): universal_options(),
        },
    )


@lru_cache
def fixed_size_simple_message() -> Message:
    return Message(
        "Fixed_Size::Simple_Message",
        [
            Link(INITIAL, Field("Message_Type")),
            Link(
                Field("Message_Type"),
                Field("Data"),
                condition=Equal(Variable("Message_Type"), Variable("Universal::OT_Data")),
                size=Number(24),
            ),
            Link(
                Field("Data"),
                FINAL,
            ),
        ],
        {
            Field("Message_Type"): universal_option_type(),
            Field("Data"): OPAQUE,
        },
    )


@lru_cache
def definite_message() -> Message:
    return Message(
        "Definite::Message",
        [
            Link(INITIAL, Field("Length")),
            Link(
                Field("Length"),
                Field("Data"),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(
                Field("Data"),
                FINAL,
            ),
        ],
        {
            Field("Length"): universal_length(),
            Field("Data"): OPAQUE,
        },
    )


@lru_cache
def session() -> Session:
    return Session(
        identifier="P::S",
        states=[
            State("A", transitions=[Transition(target="null")]),
        ],
        declarations=[],
        parameters=[],
        types=[],
    )


def spark_test_models() -> list[Callable[[], Model]]:
    """
    Return callables that create models corresponding to generated code in tests/spark/generated.

    Using callable functions instead of the models directly enables the caller to postpone the
    time-consuming creation of the models to a later time. For instance, when using this function to
    parameterize a test function, no model creation is necessary during collection time.
    """
    return [
        derivation_model,
        enumeration_model,
        ethernet_model,
        expression_model,
        null_message_in_tlv_message_model,
        null_model,
        sequence_model,
        tlv_model,
        lambda: Model(fixed_size_simple_message().dependencies),
    ]
