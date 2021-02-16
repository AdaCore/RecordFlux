from rflx.expression import (
    Aggregate,
    And,
    Div,
    Equal,
    First,
    GreaterEqual,
    Last,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Pow,
    Size,
    Sub,
    Variable,
)
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    DerivedMessage,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
    State,
    Transition,
    UnprovenMessage,
    UnprovenSession,
)

NULL_MESSAGE = Message("Null::Message", [], {}, skip_proof=True)
NULL_MODEL = Model([NULL_MESSAGE])

TLV_TAG = Enumeration(
    "TLV::Tag", [("Msg_Data", Number(1)), ("Msg_Error", Number(3))], Number(2), False
)
TLV_LENGTH = ModularInteger("TLV::Length", Pow(Number(2), Number(14)))
TLV_MESSAGE = Message(
    "TLV::Message",
    [
        Link(INITIAL, Field("Tag")),
        Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
        Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
        Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Value"), FINAL),
    ],
    {Field("Tag"): TLV_TAG, Field("Length"): TLV_LENGTH, Field("Value"): Opaque()},
    skip_proof=True,
)
TLV_MODEL = Model([TLV_TAG, TLV_LENGTH, TLV_MESSAGE])

NULL_MESSAGE_IN_TLV_MESSAGE = Refinement("In_TLV", TLV_MESSAGE, Field("Value"), NULL_MESSAGE)
NULL_MESSAGE_IN_TLV_MESSAGE_MODEL = Model(
    [TLV_TAG, TLV_LENGTH, TLV_MESSAGE, NULL_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE]
)

ETHERNET_ADDRESS = ModularInteger("Ethernet::Address", Pow(Number(2), Number(48)))
ETHERNET_TYPE_LENGTH = RangeInteger(
    "Ethernet::Type_Length", Number(46), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
ETHERNET_TPID = RangeInteger("Ethernet::TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16))
ETHERNET_TCI = ModularInteger("Ethernet::TCI", Pow(Number(2), Number(16)))
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
            Sub(Last("Message"), Last("Type_Length")),
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
        Field("Payload"): Opaque(),
    },
    skip_proof=True,
)
ETHERNET_MODEL = Model(
    [ETHERNET_ADDRESS, ETHERNET_TYPE_LENGTH, ETHERNET_TPID, ETHERNET_TCI, ETHERNET_FRAME]
)

ENUMERATION_PRIORITY = Enumeration(
    "Enumeration::Priority",
    [("Low", Number(1)), ("Medium", Number(4)), ("High", Number(7))],
    Number(3),
    True,
)
ENUMERATION_MESSAGE = Message(
    "Enumeration::Message",
    [Link(INITIAL, Field("Priority")), Link(Field("Priority"), FINAL)],
    {Field("Priority"): ENUMERATION_PRIORITY},
    skip_proof=True,
)
ENUMERATION_MODEL = Model([ENUMERATION_PRIORITY, ENUMERATION_MESSAGE])

ARRAYS_LENGTH = ModularInteger("Arrays::Length", Pow(Number(2), Number(8)))
ARRAYS_MODULAR_INTEGER = ModularInteger("Arrays::Modular_Integer", Pow(Number(2), Number(16)))
ARRAYS_MODULAR_VECTOR = Array("Arrays::Modular_Vector", ARRAYS_MODULAR_INTEGER)
ARRAYS_RANGE_INTEGER = RangeInteger("Arrays::Range_Integer", Number(1), Number(100), Number(8))
ARRAYS_RANGE_VECTOR = Array("Arrays::Range_Vector", ARRAYS_RANGE_INTEGER)
ARRAYS_ENUMERATION = Enumeration(
    "Arrays::Enumeration",
    [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
    Number(8),
    False,
)
ARRAYS_ENUMERATION_VECTOR = Array("Arrays::Enumeration_Vector", ARRAYS_ENUMERATION)
ARRAYS_AV_ENUMERATION = Enumeration(
    "Arrays::AV_Enumeration",
    [("AV_Zero", Number(0)), ("AV_One", Number(1)), ("AV_Two", Number(2))],
    Number(8),
    True,
)
ARRAYS_AV_ENUMERATION_VECTOR = Array("Arrays::AV_Enumeration_Vector", ARRAYS_AV_ENUMERATION)
ARRAYS_MESSAGE = Message(
    "Arrays::Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Modular_Vector"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Modular_Vector"), Field("Range_Vector"), size=Number(16)),
        Link(Field("Range_Vector"), Field("Enumeration_Vector"), size=Number(16)),
        Link(Field("Enumeration_Vector"), Field("AV_Enumeration_Vector"), size=Number(16)),
        Link(Field("AV_Enumeration_Vector"), FINAL),
    ],
    {
        Field("Length"): ARRAYS_LENGTH,
        Field("Modular_Vector"): ARRAYS_MODULAR_VECTOR,
        Field("Range_Vector"): ARRAYS_RANGE_VECTOR,
        Field("Enumeration_Vector"): ARRAYS_ENUMERATION_VECTOR,
        Field("AV_Enumeration_Vector"): ARRAYS_AV_ENUMERATION_VECTOR,
    },
    skip_proof=True,
)
ARRAYS_INNER_MESSAGE = Message(
    "Arrays::Inner_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Payload"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Payload"), FINAL),
    ],
    {Field("Length"): ARRAYS_LENGTH, Field("Payload"): Opaque()},
    skip_proof=True,
)
ARRAYS_INNER_MESSAGES = Array("Arrays::Inner_Messages", ARRAYS_INNER_MESSAGE)
ARRAYS_MESSAGES_MESSAGE = Message(
    "Arrays::Messages_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Messages"), size=Mul(Variable("Length"), Number(8))),
        Link(Field("Messages"), FINAL),
    ],
    {Field("Length"): ARRAYS_LENGTH, Field("Messages"): ARRAYS_INNER_MESSAGES},
    skip_proof=True,
)
ARRAYS_ARRAY_SIZE_DEFINED_BY_MESSAGE_SIZE = Message(
    "Arrays::Array_Size_Defined_By_Message_Size",
    [
        Link(INITIAL, Field("Header")),
        Link(Field("Header"), Field("Vector"), size=Sub(Size("Message"), Size("Header"))),
        Link(Field("Vector"), FINAL),
    ],
    {
        Field("Header"): ARRAYS_ENUMERATION,
        Field("Vector"): ARRAYS_MODULAR_VECTOR,
    },
    skip_proof=True,
)
ARRAYS_MODEL = Model(
    [
        ARRAYS_LENGTH,
        ARRAYS_MODULAR_INTEGER,
        ARRAYS_MODULAR_VECTOR,
        ARRAYS_RANGE_INTEGER,
        ARRAYS_RANGE_VECTOR,
        ARRAYS_ENUMERATION,
        ARRAYS_ENUMERATION_VECTOR,
        ARRAYS_AV_ENUMERATION,
        ARRAYS_AV_ENUMERATION_VECTOR,
        ARRAYS_MESSAGE,
        ARRAYS_INNER_MESSAGE,
        ARRAYS_INNER_MESSAGES,
        ARRAYS_MESSAGES_MESSAGE,
        ARRAYS_ARRAY_SIZE_DEFINED_BY_MESSAGE_SIZE,
    ]
)

EXPRESSION_MESSAGE = Message(
    "Expression::Message",
    [
        Link(INITIAL, Field("Payload"), size=Number(16)),
        Link(Field("Payload"), FINAL, Equal(Variable("Payload"), Aggregate(Number(1), Number(2)))),
    ],
    {Field("Payload"): Opaque()},
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
    {Field("F"): Opaque()},
)

INVALID_MESSAGE = UnprovenMessage(
    "P::M",
    [
        Link(INITIAL, Field("F")),
        Link(Field("F"), FINAL),
    ],
    {Field("F"): Opaque()},
)

MODULAR_INTEGER = ModularInteger("P::Modular", Number(256))
RANGE_INTEGER = RangeInteger("P::Range", Number(1), Number(100), Number(8))
ENUMERATION = Enumeration(
    "P::Enumeration",
    [("Zero", Number(0)), ("One", Number(1)), ("Two", Number(2))],
    Number(8),
    False,
)

MESSAGE = Message(
    "P::M",
    [
        Link(INITIAL, Field("F"), size=Number(16)),
        Link(Field("F"), FINAL),
    ],
    {Field("F"): Opaque()},
)
REFINEMENT = Refinement("In_Message", MESSAGE, Field("F"), MESSAGE)

SESSION = UnprovenSession(
    identifier="P::S",
    initial="A",
    final="B",
    states=[
        State("A", transitions=[Transition(target="B")]),
        State("B"),
    ],
    declarations=[],
    parameters=[],
    types=[],
).proven()
