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
from rflx.expr import (
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
        ID("TLV::Tag", location=Location((1, 1))),
        [
            (ID("Msg_Data", location=Location((1, 1))), Number(1)),
            (ID("Msg_Error", location=Location((1, 1))), Number(3)),
        ],
        Number(8),
        always_valid=False,
        location=Location((1, 1)),
    )


@lru_cache
def tlv_length() -> Integer:
    return Integer(
        ID("TLV::Length", location=Location((1, 1))),
        Number(0),
        Sub(Pow(Number(2), Number(16)), Number(1)),
        Number(16),
        location=Location((1, 1)),
    )


@lru_cache
def tlv_message() -> Message:
    return Message(
        ID("TLV::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("Tag", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("Tag", location=Location((2, 2)))),
                Field(ID("Length", location=Location((2, 2)))),
                Equal(
                    Variable(ID("Tag", location=Location((3, 3)))),
                    Variable(ID("Msg_Data", location=Location((3, 3)))),
                    location=Location((3, 1)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Tag", location=Location((4, 4)))),
                FINAL,
                Equal(
                    Variable(ID("Tag", location=Location((5, 5)))),
                    Variable(ID("Msg_Error", location=Location((5, 5)))),
                    location=Location((5, 1)),
                ),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("Length", location=Location((6, 6)))),
                Field(ID("Value", location=Location((6, 6)))),
                size=Mul(
                    Variable(ID("Length", location=Location((7, 7))), location=Location((7, 7))),
                    Number(8),
                    location=Location((7, 7)),
                ),
                location=Location((4, 4)),
            ),
            Link(Field(ID("Value", location=Location((8, 8)))), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("Tag", location=Location((1, 1)))): tlv_tag(),
            Field(ID("Length", location=Location((2, 2)))): tlv_length(),
            Field(ID("Value", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((1, 1)),
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
        ID("TLV_With_Checksum::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("Tag", location=Location((2, 2)))), location=Location((2, 2))),
            Link(
                Field(ID("Tag", location=Location((3, 3)))),
                Field(ID("Length", location=Location((3, 3)))),
                Equal(
                    Variable(ID("Tag", location=Location((4, 4)))),
                    Variable(ID("Msg_Data", location=Location((4, 4)))),
                    location=Location((4, 4)),
                ),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("Tag", location=Location((5, 5)))),
                FINAL,
                Equal(
                    Variable(ID("Tag", location=Location((5, 5)))),
                    Variable(ID("Msg_Error", location=Location((5, 5)))),
                    location=Location((5, 5)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("Length", location=Location((6, 6)))),
                Field(ID("Value", location=Location((6, 6)))),
                size=Mul(
                    Variable(ID("Length", location=Location((7, 7)))),
                    Number(8),
                    location=Location((7, 7)),
                ),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("Value", location=Location((8, 8)))),
                Field(ID("Checksum", location=Location((8, 8)))),
                location=Location((8, 8)),
            ),
            Link(
                Field(ID("Checksum", location=Location((9, 9)))),
                FINAL,
                ValidChecksum(ID("Checksum", location=Location((9, 9)))),
                location=Location((9, 9)),
            ),
        ],
        {
            Field(ID("Tag", location=Location((1, 1)))): tlv_with_checksum_tag(),
            Field(ID("Length", location=Location((2, 2)))): tlv_with_checksum_length(),
            Field(ID("Value", location=Location((3, 3)))): OPAQUE,
            Field(ID("Checksum", location=Location((4, 4)))): tlv_with_checksum_checksum(),
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
        ID("Ethernet::Type_Length"),
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
        ID("Ethernet::Frame", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Destination", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Destination", location=Location((3, 3)))),
                Field(ID("Source", location=Location((3, 3)))),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("Source", location=Location((4, 4)))),
                Field(ID("Type_Length_TPID", location=Location((4, 4)))),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("Type_Length_TPID", location=Location((5, 5)))),
                Field(ID("TPID", location=Location((5, 5)))),
                Equal(
                    Variable(ID("Type_Length_TPID", location=Location((6, 6)))),
                    Number(0x8100, 16),
                    Location((6, 1)),
                ),
                first=First(ID("Type_Length_TPID", location=Location((6, 6)))),
                location=Location((8, 8)),
            ),
            Link(
                Field(ID("Type_Length_TPID", location=Location((7, 7)))),
                Field(ID("Type_Length", location=Location((7, 7)))),
                NotEqual(
                    Variable(ID("Type_Length_TPID", location=Location((8, 8)))),
                    Number(0x8100, 16),
                    Location((8, 1)),
                ),
                first=First(ID("Type_Length_TPID", location=Location((8, 8)))),
                location=Location((9, 9)),
            ),
            Link(
                Field(ID("TPID", location=Location((9, 9)))),
                Field(ID("TCI", location=Location((9, 9)))),
                location=Location((10, 10)),
            ),
            Link(
                Field(ID("TCI", location=Location((10, 10)))),
                Field(ID("Type_Length", location=Location((10, 10)))),
                location=Location((11, 11)),
            ),
            Link(
                Field(ID("Type_Length", location=Location((11, 11)))),
                Field(ID("Payload", location=Location((11, 11)))),
                LessEqual(
                    Variable(ID("Type_Length", location=Location((12, 12)))),
                    Number(1500),
                    Location((12, 1)),
                ),
                Mul(
                    Variable(ID("Type_Length", location=Location((12, 12)))),
                    Number(8),
                    location=Location((12, 12)),
                ),
                location=Location((12, 12)),
            ),
            Link(
                Field(ID("Type_Length", location=Location((13, 13)))),
                Field(ID("Payload", location=Location((13, 13)))),
                GreaterEqual(
                    Variable(ID("Type_Length", location=Location((12, 12)))),
                    Number(1536),
                    Location((14, 1)),
                ),
                location=Location((13, 13)),
            ),
            Link(
                Field(ID("Payload", location=Location((15, 15)))),
                FINAL,
                And(
                    GreaterEqual(
                        Div(Size(ID("Payload", location=Location((16, 16)))), Number(8)),
                        Number(46),
                        Location((14, 1)),
                    ),
                    LessEqual(
                        Div(Size(ID("Payload", location=Location((16, 16)))), Number(8)),
                        Number(1500),
                        Location((15, 1)),
                    ),
                ),
                location=Location((14, 14)),
            ),
        ],
        {
            Field(ID("Destination", location=Location((1, 1)))): ethernet_address(),
            Field(ID("Source", location=Location((2, 2)))): ethernet_address(),
            Field(ID("Type_Length_TPID", location=Location((3, 3)))): ethernet_type_length(),
            Field(ID("TPID", location=Location((3, 3)))): ethernet_tpid(),
            Field(ID("TCI", location=Location((4, 4)))): ethernet_tci(),
            Field(ID("Type_Length", location=Location((5, 5)))): ethernet_type_length(),
            Field(ID("Payload", location=Location((6, 6)))): OPAQUE,
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
        ID("Enumeration::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Priority"), location=Location((2, 2))),
            Link(Field("Priority"), FINAL, location=Location((3, 3))),
        ],
        {Field(ID("Priority", location=Location((1, 1)))): enumeration_priority()},
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
        ID("Sequence::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Length"), location=Location((1, 1))),
            Link(
                Field("Length"),
                Field("Integer_Vector"),
                size=Mul(Variable("Length"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field("Integer_Vector"),
                Field("Enumeration_Vector"),
                size=Number(16, location=Location((3, 3))),
                location=Location((3, 3)),
            ),
            Link(
                Field("Enumeration_Vector"),
                Field("AV_Enumeration_Vector"),
                size=Number(16, location=Location((4, 4))),
                location=Location((4, 4)),
            ),
            Link(Field("AV_Enumeration_Vector"), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("Length", location=Location((1, 1)))): sequence_length(),
            Field(ID("Integer_Vector", location=Location((2, 2)))): sequence_integer_vector(),
            Field(
                ID("Enumeration_Vector", location=Location((3, 3))),
            ): sequence_enumeration_vector(),
            Field(
                ID("AV_Enumeration_Vector", location=Location((4, 4))),
            ): sequence_av_enumeration_vector(),
        },
    )


@lru_cache
def sequence_inner_message() -> Message:
    return Message(
        ID("Sequence::Inner_Message", location=Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Length", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Length", location=Location((3, 3)))),
                Field(ID("Payload", location=Location((3, 3)))),
                size=Mul(
                    Variable(ID("Length", location=Location((3, 3)))),
                    Number(8),
                    location=Location((3, 3)),
                ),
                location=Location((3, 3)),
            ),
            Link(Field(ID("Payload", location=Location((4, 4)))), FINAL, location=Location((4, 4))),
        ],
        {
            Field(ID("Length", location=Location((1, 1)))): sequence_length(),
            Field(ID("Payload", location=Location((2, 2)))): OPAQUE,
        },
    )


@lru_cache
def sequence_inner_messages() -> Sequence:
    return Sequence(ID("Sequence::Inner_Messages", Location((1, 1))), sequence_inner_message())


@lru_cache
def sequence_messages_message() -> Message:
    return Message(
        ID("Sequence::Messages_Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Length"), location=Location((1, 1))),
            Link(
                Field("Length"),
                Field("Messages"),
                size=Mul(Variable("Length"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(Field("Messages"), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("Length", location=Location((1, 1)))): sequence_length(),
            Field(ID("Messages", location=Location((2, 2)))): sequence_inner_messages(),
        },
    )


@lru_cache
def sequence_sequence_size_defined_by_message_size() -> Message:
    return Message(
        ID("Sequence::Sequence_Size_Defined_By_Message_Size", Location((1, 1))),
        [
            Link(INITIAL, Field("Header"), location=Location((1, 1))),
            Link(Field("Header"), Field("Vector"), location=Location((2, 2))),
            Link(Field("Vector"), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("Header", location=Location((1, 1)))): sequence_enumeration(),
            Field(ID("Vector", location=Location((2, 2)))): sequence_integer_vector(),
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
        ID("Expression::Message", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field("Payload"),
                size=Number(16, location=Location((1, 1))),
                location=Location((1, 1)),
            ),
            Link(
                Field("Payload"),
                FINAL,
                Equal(Variable("Payload"), Aggregate(Number(1), Number(2))),
                location=Location((2, 2)),
            ),
        ],
        {Field(ID("Payload", location=Location((1, 1)))): OPAQUE},
    )


@lru_cache
def expression_model() -> Model:
    return Model([expression_message()])


@lru_cache
def derivation_message() -> DerivedMessage:
    return DerivedMessage(ID("Derivation::Message", Location((1, 1))), tlv_message())


@lru_cache
def derivation_model() -> Model:
    return Model([derivation_message()])


@lru_cache
def valid_message() -> Message:
    return Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("F"), size=Number(16)),
            Link(Field("F"), FINAL),
        ],
        {Field("F"): OPAQUE},
    )


@lru_cache
def integer() -> Integer:
    return Integer(
        ID("P::Integer", location=Location((1, 1))),
        Number(1),
        Number(220),
        Number(8),
        location=Location((1, 1)),
    )


@lru_cache
def enumeration() -> Enumeration:
    return Enumeration(
        ID("P::Enumeration", location=Location((1, 1))),
        [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
        Number(8),
        always_valid=False,
        location=Location((10, 2)),
    )


@lru_cache
def message() -> Message:
    return Message(
        ID("P::M", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field("F"),
                size=Number(16, location=Location((1, 1))),
                location=Location((1, 1)),
            ),
            Link(Field("F"), FINAL, location=Location((2, 2))),
        ],
        {Field(ID("F", location=Location((1, 1)))): OPAQUE},
    )


@lru_cache
def refinement() -> Refinement:
    return Refinement("In_Message", message(), Field("F"), message())


@lru_cache
def universal_message_type() -> Enumeration:
    return Enumeration(
        ID("Universal::Message_Type", location=Location((1, 1))),
        [
            (ID("MT_Null", location=Location((2, 2))), Number(0)),
            (ID("MT_Data", location=Location((3, 3))), Number(1)),
            (ID("MT_Value", location=Location((4, 4))), Number(2)),
            (ID("MT_Values", location=Location((5, 5))), Number(3)),
            (ID("MT_Option_Types", location=Location((6, 6))), Number(4)),
            (ID("MT_Options", location=Location((7, 7))), Number(5)),
            (ID("MT_Unconstrained_Data", location=Location((8, 8))), Number(6)),
            (ID("MT_Unconstrained_Options", location=Location((9, 9))), Number(7)),
        ],
        size=Number(8),
        always_valid=False,
        location=Location((1, 1)),
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
        ID("Universal::Option", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Option_Type", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Option_Type", location=Location((2, 2)))),
                FINAL,
                condition=Equal(
                    Variable(ID("Option_Type", location=Location((3, 3)))),
                    Variable(ID("OT_Null", location=Location((3, 3)))),
                    location=Location((3, 3)),
                ),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("Option_Type", location=Location((4, 4)))),
                Field(ID("Length", location=Location((4, 4)))),
                condition=Equal(
                    Variable(ID("Option_Type", location=Location((5, 5)))),
                    Variable(ID("OT_Data", location=Location((5, 5)))),
                    location=Location((5, 5)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("Length", location=Location((6, 6)))),
                Field(ID("Data", location=Location((6, 6)))),
                size=Mul(
                    Variable(ID("Length", location=Location((6, 6)))),
                    Number(8),
                    location=Location((6, 6)),
                ),
                location=Location((5, 5)),
            ),
            Link(Field(ID("Data", location=Location((7, 7)))), FINAL, location=Location((7, 7))),
        ],
        {
            Field(ID("Option_Type", location=Location((1, 1)))): universal_option_type(),
            Field(ID("Length", location=Location((2, 2)))): universal_length(),
            Field(ID("Data", location=Location((3, 3)))): OPAQUE,
        },
    )


@lru_cache
def universal_options() -> Sequence:
    return Sequence(ID("Universal::Options", Location((1, 1))), universal_option())


UNIVERSAL_MESSAGE_ID: Final = ID("Universal::Message", Location((1, 1)))


@lru_cache
def universal_message() -> Message:
    return Message(
        UNIVERSAL_MESSAGE_ID,
        [
            Link(
                INITIAL,
                Field(ID("Message_Type", location=Location((2, 2)))),
                location=Location((1, 1)),
            ),
            Link(
                Field(ID("Message_Type", location=Location((3, 3)))),
                FINAL,
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((4, 4)))),
                    Variable(ID("MT_Null", location=Location((4, 4)))),
                    location=Location((4, 4)),
                ),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("Message_Type", location=Location((5, 5)))),
                Field(ID("Data", location=Location((5, 5)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((6, 6)))),
                    Variable(ID("MT_Unconstrained_Data", location=Location((1, 1)))),
                    location=Location((1, 1)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("Message_Type", location=Location((7, 7)))),
                Field(ID("Options", location=Location((7, 7)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((8, 8)))),
                    Variable(ID("MT_Unconstrained_Options", location=Location((8, 8)))),
                    location=Location((8, 8)),
                ),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("Message_Type", location=Location((9, 9)))),
                Field(ID("Length", location=Location((9, 9)))),
                condition=And(
                    NotEqual(
                        Variable(ID("Message_Type", location=Location((10, 10)))),
                        Variable(ID("MT_Null", location=Location((10, 10)))),
                        location=Location((10, 10)),
                    ),
                    NotEqual(
                        Variable(ID("Message_Type", location=Location((11, 11)))),
                        Variable(ID("MT_Unconstrained_Data", location=Location((11, 11)))),
                        location=Location((11, 11)),
                    ),
                    NotEqual(
                        Variable(ID("Message_Type", location=Location((12, 12)))),
                        Variable(ID("MT_Unconstrained_Options", location=Location((12, 12)))),
                        location=Location((12, 12)),
                    ),
                    location=Location((12, 12)),
                ),
                location=Location((9, 9)),
            ),
            Link(
                Field(ID("Length", location=Location((13, 13)))),
                Field(ID("Data", location=Location((13, 13)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((14, 14)))),
                    Variable(ID("MT_Data", location=Location((14, 14)))),
                    location=Location((14, 14)),
                ),
                size=Mul(
                    Variable(ID("Length", location=Location((15, 15)))),
                    Number(8),
                    location=Location((15, 15)),
                ),
                location=Location((13, 13)),
            ),
            Link(
                Field(ID("Data", location=Location((16, 16)))),
                FINAL,
                location=Location((16, 16)),
            ),
            Link(
                Field(ID("Length", location=Location((17, 17)))),
                Field(ID("Value", location=Location((18, 18)))),
                condition=And(
                    Equal(
                        Variable(ID("Message_Type", location=Location((19, 19)))),
                        Variable(ID("MT_Value", location=Location((19, 19)))),
                        location=Location((19, 19)),
                    ),
                    Equal(
                        Variable(ID("Length", location=Location((20, 20)))),
                        Div(Size(ID("Universal::Value", location=Location((20, 20)))), Number(8)),
                        location=Location((20, 20)),
                    ),
                    location=Location((19, 19)),
                ),
            ),
            Link(
                Field(ID("Value", location=Location((21, 21)))),
                FINAL,
                location=Location((21, 21)),
            ),
            Link(
                Field(ID("Length", location=Location((22, 22)))),
                Field(ID("Values", location=Location((23, 23)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((24, 24)))),
                    Variable(ID("MT_Values", location=Location((24, 24)))),
                    location=Location((24, 24)),
                ),
                size=Mul(
                    Variable(ID("Length", location=Location((25, 25)))),
                    Number(8),
                    location=Location((25, 25)),
                ),
                location=Location((22, 22)),
            ),
            Link(
                Field(ID("Values", location=Location((26, 26)))),
                FINAL,
                location=Location((26, 26)),
            ),
            Link(
                Field(ID("Length", location=Location((27, 27)))),
                Field(ID("Option_Types", location=Location((28, 28)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((29, 29)))),
                    Variable(ID("MT_Option_Types", location=Location((29, 29)))),
                    location=Location((29, 29)),
                ),
                size=Mul(
                    Variable(ID("Length", location=Location((30, 30)))),
                    Number(8),
                    location=Location((30, 30)),
                ),
            ),
            Link(
                Field(ID("Option_Types", location=Location((31, 31)))),
                FINAL,
                location=Location((31, 31)),
            ),
            Link(
                Field(ID("Length", location=Location((32, 32)))),
                Field(ID("Options", location=Location((32, 32)))),
                condition=Equal(
                    Variable(ID("Message_Type", location=Location((33, 33)))),
                    Variable(ID("MT_Options", location=Location((33, 33)))),
                    location=Location((33, 33)),
                ),
                size=Mul(
                    Variable(ID("Length", location=Location((34, 34)))),
                    Number(8),
                    location=Location((34, 34)),
                ),
                location=Location((34, 34)),
            ),
            Link(
                Field(ID("Options", location=Location((35, 35)))),
                FINAL,
                location=Location((35, 35)),
            ),
        ],
        {
            Field(ID("Message_Type", location=Location((1, 1)))): universal_message_type(),
            Field(ID("Length", location=Location((2, 2)))): universal_length(),
            Field(ID("Data", location=Location((3, 3)))): OPAQUE,
            Field(ID("Value", location=Location((4, 4)))): universal_value(),
            Field(ID("Values", location=Location((5, 5)))): universal_values(),
            Field(ID("Option_Types", location=Location((6, 6)))): universal_option_types(),
            Field(ID("Options", location=Location((7, 7)))): universal_options(),
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
        ID("Fixed_Size::Message", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Message_Type", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Message_Type", location=Location((3, 3)))),
                Field(ID("Data", location=Location((3, 3)))),
                condition=Or(
                    Equal(
                        Variable(ID("Message_Type", location=Location((1, 1)))),
                        Variable(ID("Universal::MT_Null", location=Location((1, 1)))),
                        location=Location((1, 1)),
                    ),
                    Equal(
                        Variable(ID("Message_Type", location=Location((2, 2)))),
                        Variable(ID("Universal::MT_Data", location=Location((2, 2)))),
                        location=Location((2, 2)),
                    ),
                    Equal(
                        Variable(ID("Message_Type", location=Location((3, 3)))),
                        Variable(ID("Universal::MT_Values", location=Location((3, 3)))),
                        location=Location((3, 3)),
                    ),
                    Equal(
                        Variable(ID("Message_Type", location=Location((4, 4)))),
                        Variable(ID("Universal::MT_Options", location=Location((4, 4)))),
                        location=Location((4, 4)),
                    ),
                    location=Location((5, 5)),
                ),
                size=Number(64, location=Location((5, 5))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("Data", location=Location((5, 5)))),
                Field(ID("Values", location=Location((5, 5)))),
                size=Mul(
                    Number(8),
                    Size(ID("Universal::Value", location=Location((5, 5)))),
                    location=Location((5, 5)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("Values", location=Location((6, 6)))),
                Field(ID("Options", location=Location((6, 6)))),
                size=Number(64, location=Location((6, 6))),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("Options", location=Location((7, 7)))),
                FINAL,
                location=Location((7, 7)),
            ),
        ],
        {
            Field(ID("Message_Type", location=Location((1, 1)))): universal_message_type(),
            Field(ID("Data", location=Location((2, 2)))): OPAQUE,
            Field(ID("Values", location=Location((3, 3)))): universal_values(),
            Field(ID("Options", location=Location((4, 4)))): universal_options(),
        },
    )


@lru_cache
def fixed_size_simple_message() -> Message:
    return Message(
        ID("Fixed_Size::Simple_Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Message_Type"), location=Location((1, 1))),
            Link(
                Field("Message_Type"),
                Field("Data"),
                condition=Equal(
                    Variable("Message_Type"),
                    Variable("Universal::OT_Data"),
                    location=Location((4, 4)),
                ),
                size=Number(24, location=Location((4, 4))),
                location=Location((2, 2)),
            ),
            Link(
                Field("Data"),
                FINAL,
                location=Location((5, 5)),
            ),
        ],
        {
            Field(ID("Message_Type", location=Location((1, 1)))): universal_option_type(),
            Field(ID("Data", location=Location((2, 2)))): OPAQUE,
        },
    )


@lru_cache
def definite_message() -> Message:
    return Message(
        ID("Definite::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Length"), location=Location((1, 1))),
            Link(
                Field("Length"),
                Field("Data"),
                size=Mul(Variable("Length"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field("Data"),
                FINAL,
                location=Location((3, 3)),
            ),
        ],
        {
            Field(ID("Length", location=Location((1, 1)))): universal_length(),
            Field(ID("Data", location=Location((2, 2)))): OPAQUE,
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
