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
    UnprovenMessage,
)

NULL_MESSAGE = Message("Null::Message", [], {}, skip_proof=True)
NULL_MODEL = Model([NULL_MESSAGE])

TLV_TAG = Enumeration(
    "TLV::Tag", [("Msg_Data", Number(1)), ("Msg_Error", Number(3))], Number(8), always_valid=False
)
TLV_LENGTH = Integer(
    "TLV::Length", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
TLV_MESSAGE = Message(
    "TLV::Message",
    [
        Link(INITIAL, Field("Tag")),
        Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
        Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
        Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Value"), FINAL),
    ],
    {Field("Tag"): TLV_TAG, Field("Length"): TLV_LENGTH, Field("Value"): OPAQUE},
    skip_proof=True,
)
TLV_MODEL = Model([TLV_TAG, TLV_LENGTH, TLV_MESSAGE])

TLV_MESSAGES = Sequence("TLV::Messages", TLV_MESSAGE)
TLV_TAGS = Sequence("TLV::Tags", TLV_TAG)

TLV_WITH_CHECKSUM_TAG = Enumeration(
    "TLV_With_Checksum::Tag",
    [("Msg_Data", Number(1)), ("Msg_Error", Number(3))],
    Number(8),
    always_valid=False,
)
TLV_WITH_CHECKSUM_LENGTH = Integer(
    "TLV_With_Checksum::Length", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
TLV_WITH_CHECKSUM_CHECKSUM = Integer(
    "TLV_With_Checksum::Checksum", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
TLV_WITH_CHECKSUM_MESSAGE = Message(
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
        Field("Tag"): TLV_WITH_CHECKSUM_TAG,
        Field("Length"): TLV_WITH_CHECKSUM_LENGTH,
        Field("Value"): OPAQUE,
        Field("Checksum"): TLV_WITH_CHECKSUM_CHECKSUM,
    },
    checksums={ID("Checksum"): [Variable("Tag"), Size("Value"), Variable("Value")]},
    skip_proof=True,
)
TLV_WITH_CHECKSUM_MODEL = Model(
    [TLV_WITH_CHECKSUM_TAG, TLV_WITH_CHECKSUM_LENGTH, TLV_WITH_CHECKSUM_MESSAGE]
)

NULL_MESSAGE_IN_TLV_MESSAGE = Refinement("In_TLV", TLV_MESSAGE, Field("Value"), NULL_MESSAGE)
NULL_MESSAGE_IN_TLV_MESSAGE_MODEL = Model(
    [TLV_TAG, TLV_LENGTH, TLV_MESSAGE, NULL_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE]
)

ETHERNET_ADDRESS = Integer(
    "Ethernet::Address", Number(0), Sub(Pow(Number(2), Number(48)), Number(1)), Number(48)
)
ETHERNET_TYPE_LENGTH = Integer(
    "Ethernet::Type_Length", Number(46), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
ETHERNET_TPID = Integer("Ethernet::TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16))
ETHERNET_TCI = Integer(
    "Ethernet::TCI", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
ETHERNET_FRAME = Message(
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
        Field("Destination"): ETHERNET_ADDRESS,
        Field("Source"): ETHERNET_ADDRESS,
        Field("Type_Length_TPID"): ETHERNET_TYPE_LENGTH,
        Field("TPID"): ETHERNET_TPID,
        Field("TCI"): ETHERNET_TCI,
        Field("Type_Length"): ETHERNET_TYPE_LENGTH,
        Field("Payload"): OPAQUE,
    },
    skip_proof=True,
)
ETHERNET_MODEL = Model(
    [ETHERNET_ADDRESS, ETHERNET_TYPE_LENGTH, ETHERNET_TPID, ETHERNET_TCI, ETHERNET_FRAME]
)

ENUMERATION_PRIORITY = Enumeration(
    "Enumeration::Priority",
    [("Low", Number(1)), ("Medium", Number(4)), ("High", Number(7))],
    Number(8),
    always_valid=True,
)
ENUMERATION_MESSAGE = Message(
    "Enumeration::Message",
    [Link(INITIAL, Field("Priority")), Link(Field("Priority"), FINAL)],
    {Field("Priority"): ENUMERATION_PRIORITY},
    skip_proof=True,
)
ENUMERATION_MODEL = Model([ENUMERATION_PRIORITY, ENUMERATION_MESSAGE])

SEQUENCE_LENGTH = Integer(
    "Sequence::Length", Number(0), Sub(Pow(Number(2), Number(8)), Number(1)), Number(8)
)
SEQUENCE_INTEGER = Integer("Sequence::Integer", Number(1), Number(100), Number(16))
SEQUENCE_INTEGER_VECTOR = Sequence("Sequence::Integer_Vector", SEQUENCE_INTEGER)
SEQUENCE_ENUMERATION = Enumeration(
    "Sequence::Enumeration",
    [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
    Number(8),
    always_valid=False,
)
SEQUENCE_ENUMERATION_VECTOR = Sequence("Sequence::Enumeration_Vector", SEQUENCE_ENUMERATION)
SEQUENCE_AV_ENUMERATION = Enumeration(
    "Sequence::AV_Enumeration",
    [("AV_Zero", Number(0)), ("AV_One", Number(1)), ("AV_Two", Number(2))],
    Number(8),
    always_valid=True,
)
SEQUENCE_AV_ENUMERATION_VECTOR = Sequence(
    "Sequence::AV_Enumeration_Vector", SEQUENCE_AV_ENUMERATION
)
SEQUENCE_MESSAGE = Message(
    "Sequence::Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Integer_Vector"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Integer_Vector"), Field("Enumeration_Vector"), size=Number(16)),
        Link(Field("Enumeration_Vector"), Field("AV_Enumeration_Vector"), size=Number(16)),
        Link(Field("AV_Enumeration_Vector"), FINAL),
    ],
    {
        Field("Length"): SEQUENCE_LENGTH,
        Field("Integer_Vector"): SEQUENCE_INTEGER_VECTOR,
        Field("Enumeration_Vector"): SEQUENCE_ENUMERATION_VECTOR,
        Field("AV_Enumeration_Vector"): SEQUENCE_AV_ENUMERATION_VECTOR,
    },
    skip_proof=True,
)
SEQUENCE_INNER_MESSAGE = Message(
    "Sequence::Inner_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Payload"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Payload"), FINAL),
    ],
    {Field("Length"): SEQUENCE_LENGTH, Field("Payload"): OPAQUE},
    skip_proof=True,
)
SEQUENCE_INNER_MESSAGES = Sequence("Sequence::Inner_Messages", SEQUENCE_INNER_MESSAGE)
SEQUENCE_MESSAGES_MESSAGE = Message(
    "Sequence::Messages_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Messages"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Messages"), FINAL),
    ],
    {Field("Length"): SEQUENCE_LENGTH, Field("Messages"): SEQUENCE_INNER_MESSAGES},
    skip_proof=True,
)
SEQUENCE_SEQUENCE_SIZE_DEFINED_BY_MESSAGE_SIZE = Message(
    "Sequence::Sequence_Size_Defined_By_Message_Size",
    [
        Link(INITIAL, Field("Header")),
        Link(Field("Header"), Field("Vector")),
        Link(Field("Vector"), FINAL),
    ],
    {
        Field("Header"): SEQUENCE_ENUMERATION,
        Field("Vector"): SEQUENCE_INTEGER_VECTOR,
    },
    skip_proof=True,
)
SEQUENCE_MODEL = Model(
    [
        SEQUENCE_LENGTH,
        SEQUENCE_INTEGER,
        SEQUENCE_INTEGER_VECTOR,
        SEQUENCE_ENUMERATION,
        SEQUENCE_ENUMERATION_VECTOR,
        SEQUENCE_AV_ENUMERATION,
        SEQUENCE_AV_ENUMERATION_VECTOR,
        SEQUENCE_MESSAGE,
        SEQUENCE_INNER_MESSAGE,
        SEQUENCE_INNER_MESSAGES,
        SEQUENCE_MESSAGES_MESSAGE,
        SEQUENCE_SEQUENCE_SIZE_DEFINED_BY_MESSAGE_SIZE,
    ]
)

EXPRESSION_MESSAGE = Message(
    "Expression::Message",
    [
        Link(INITIAL, Field("Payload"), size=Number(16)),
        Link(Field("Payload"), FINAL, Equal(Variable("Payload"), Aggregate(Number(1), Number(2)))),
    ],
    {Field("Payload"): OPAQUE},
    skip_proof=True,
)
EXPRESSION_MODEL = Model([EXPRESSION_MESSAGE])

DERIVATION_MESSAGE = DerivedMessage("Derivation::Message", TLV_MESSAGE)
DERIVATION_MODEL = Model([DERIVATION_MESSAGE])

VALID_MESSAGE = UnprovenMessage(
    "P::M",
    [
        Link(INITIAL, Field("F"), size=Number(16)),
        Link(Field("F"), FINAL),
    ],
    {Field("F"): OPAQUE},
)

INTEGER = Integer("P::Integer", Number(1), Number(220), Number(8))
ENUMERATION = Enumeration(
    "P::Enumeration",
    [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
    Number(8),
    always_valid=False,
    location=Location((10, 2)),
)

MESSAGE = Message(
    "P::M",
    [
        Link(INITIAL, Field("F"), size=Number(16)),
        Link(Field("F"), FINAL),
    ],
    {Field("F"): OPAQUE},
)
REFINEMENT = Refinement("In_Message", MESSAGE, Field("F"), MESSAGE)

UNIVERSAL_MESSAGE_TYPE = Enumeration(
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
UNIVERSAL_LENGTH = Integer(
    "Universal::Length", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
UNIVERSAL_VALUE = Integer(
    "Universal::Value", Number(0), Sub(Pow(Number(2), Number(8)), Number(1)), Number(8)
)
UNIVERSAL_VALUES = Sequence("Universal::Values", UNIVERSAL_VALUE)
UNIVERSAL_OPTION_TYPE = Enumeration(
    "Universal::Option_Type",
    [
        ("OT_Null", Number(0)),
        ("OT_Data", Number(1)),
    ],
    size=Number(8),
    always_valid=True,
)
UNIVERSAL_OPTION_TYPES = Sequence("Universal::Option_Types", UNIVERSAL_OPTION_TYPE)
UNIVERSAL_OPTION = Message(
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
        Field("Option_Type"): UNIVERSAL_OPTION_TYPE,
        Field("Length"): UNIVERSAL_LENGTH,
        Field("Data"): OPAQUE,
    },
)
UNIVERSAL_OPTIONS = Sequence("Universal::Options", UNIVERSAL_OPTION)
UNIVERSAL_MESSAGE = Message(
    "Universal::Message",
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
        Field("Message_Type"): UNIVERSAL_MESSAGE_TYPE,
        Field("Length"): UNIVERSAL_LENGTH,
        Field("Data"): OPAQUE,
        Field("Value"): UNIVERSAL_VALUE,
        Field("Values"): UNIVERSAL_VALUES,
        Field("Option_Types"): UNIVERSAL_OPTION_TYPES,
        Field("Options"): UNIVERSAL_OPTIONS,
    },
)
UNIVERSAL_REFINEMENT = Refinement("Universal", UNIVERSAL_MESSAGE, Field("Data"), UNIVERSAL_OPTION)
UNIVERSAL_MODEL = Model(
    [
        UNIVERSAL_MESSAGE_TYPE,
        UNIVERSAL_LENGTH,
        OPAQUE,
        UNIVERSAL_OPTION_TYPE,
        UNIVERSAL_OPTION_TYPES,
        UNIVERSAL_OPTION,
        UNIVERSAL_OPTIONS,
        UNIVERSAL_VALUE,
        UNIVERSAL_VALUES,
        UNIVERSAL_MESSAGE,
        UNIVERSAL_REFINEMENT,
    ]
)

FIXED_SIZE_MESSAGE = Message(
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
        Field("Message_Type"): UNIVERSAL_MESSAGE_TYPE,
        Field("Data"): OPAQUE,
        Field("Values"): UNIVERSAL_VALUES,
        Field("Options"): UNIVERSAL_OPTIONS,
    },
)
FIXED_SIZE_SIMPLE_MESSAGE = Message(
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
        Field("Message_Type"): UNIVERSAL_OPTION_TYPE,
        Field("Data"): OPAQUE,
    },
)

DEFINITE_MESSAGE = Message(
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
        Field("Length"): UNIVERSAL_LENGTH,
        Field("Data"): OPAQUE,
    },
)

SESSION = Session(
    identifier="P::S",
    states=[
        State("A", transitions=[Transition(target="null")]),
    ],
    declarations=[],
    parameters=[],
    types=[],
)
