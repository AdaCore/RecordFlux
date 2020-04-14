from rflx.expression import (
    Aggregate,
    And,
    Div,
    Equal,
    First,
    GreaterEqual,
    Last,
    Length,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Pow,
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
)

NULL_MESSAGE = Message("Null.Message", [], {})
NULL_MODEL = Model([NULL_MESSAGE])

TLV_TAG = Enumeration("TLV.Tag", {"Msg_Data": Number(1), "Msg_Error": Number(3)}, Number(2), False)
TLV_LENGTH = ModularInteger("TLV.Length", Pow(Number(2), Number(14)))
TLV_MESSAGE = Message(
    "TLV.Message",
    [
        Link(INITIAL, Field("Tag")),
        Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
        Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
        Link(Field("Length"), Field("Value"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Value"), FINAL),
    ],
    {Field("Tag"): TLV_TAG, Field("Length"): TLV_LENGTH, Field("Value"): Opaque()},
)
TLV_MODEL = Model([TLV_TAG, TLV_LENGTH, TLV_MESSAGE])

NULL_MESSAGE_IN_TLV_MESSAGE = Refinement("In_TLV", TLV_MESSAGE, Field("Value"), NULL_MESSAGE)
NULL_MESSAGE_IN_TLV_MESSAGE_MODEL = Model(
    [TLV_TAG, TLV_LENGTH, TLV_MESSAGE, NULL_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE]
)

ETHERNET_ADDRESS = ModularInteger("Ethernet.Address", Pow(Number(2), Number(48)))
ETHERNET_TYPE_LENGTH = RangeInteger(
    "Ethernet.Type_Length", Number(46), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
)
ETHERNET_TPID = RangeInteger("Ethernet.TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16))
ETHERNET_TCI = ModularInteger("Ethernet.TCI", Pow(Number(2), Number(16)))
ETHERNET_FRAME = Message(
    "Ethernet.Frame",
    [
        Link(INITIAL, Field("Destination")),
        Link(Field("Destination"), Field("Source")),
        Link(Field("Source"), Field("Type_Length_TPID")),
        Link(
            Field("Type_Length_TPID"),
            Field("TPID"),
            Equal(Variable("Type_Length_TPID"), Number(0x8100, 16)),
            first=First(Variable("Type_Length_TPID")),
        ),
        Link(
            Field("Type_Length_TPID"),
            Field("Type_Length"),
            NotEqual(Variable("Type_Length_TPID"), Number(0x8100, 16)),
            first=First(Variable("Type_Length_TPID")),
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
            Sub(Last(Variable("Message")), Last(Variable("Type_Length"))),
        ),
        Link(
            Field("Payload"),
            FINAL,
            And(
                GreaterEqual(Div(Length(Variable("Payload")), Number(8)), Number(46)),
                LessEqual(Div(Length(Variable("Payload")), Number(8)), Number(1500)),
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
)
ETHERNET_MODEL = Model(
    [ETHERNET_ADDRESS, ETHERNET_TYPE_LENGTH, ETHERNET_TPID, ETHERNET_TCI, ETHERNET_FRAME]
)

ENUMERATION_PRIORITY = Enumeration(
    "Enumeration.Priority",
    {"LOW": Number(1), "MEDIUM": Number(4), "HIGH": Number(7)},
    Number(3),
    True,
)
ENUMERATION_MESSAGE = Message(
    "Enumeration.Message",
    [Link(INITIAL, Field("Priority")), Link(Field("Priority"), FINAL)],
    {Field("Priority"): ENUMERATION_PRIORITY},
)
ENUMERATION_MODEL = Model([ENUMERATION_PRIORITY, ENUMERATION_MESSAGE])

ARRAYS_LENGTH = ModularInteger("Arrays.Length", Pow(Number(2), Number(8)))
ARRAYS_MODULAR_INTEGER = ModularInteger("Arrays.Modular_Integer", Pow(Number(2), Number(16)))
ARRAYS_MODULAR_VECTOR = Array("Arrays.Modular_Vector", ARRAYS_MODULAR_INTEGER)
ARRAYS_RANGE_INTEGER = RangeInteger("Arrays.Range_Integer", Number(1), Number(100), Number(8))
ARRAYS_RANGE_VECTOR = Array("Arrays.Range_Vector", ARRAYS_RANGE_INTEGER)
ARRAYS_ENUMERATION = Enumeration(
    "Arrays.Enumeration", {"ZERO": Number(0), "ONE": Number(1), "TWO": Number(2)}, Number(8), False,
)
ARRAYS_ENUMERATION_VECTOR = Array("Arrays.Enumeration_Vector", ARRAYS_ENUMERATION)
ARRAYS_AV_ENUMERATION = Enumeration(
    "Arrays.AV_Enumeration",
    {"AV_ZERO": Number(0), "AV_ONE": Number(1), "AV_TWO": Number(2)},
    Number(8),
    True,
)
ARRAYS_AV_ENUMERATION_VECTOR = Array("Arrays.AV_Enumeration_Vector", ARRAYS_AV_ENUMERATION)
ARRAYS_MESSAGE = Message(
    "Arrays.Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Modular_Vector"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Modular_Vector"), Field("Range_Vector"), length=Number(16)),
        Link(Field("Range_Vector"), Field("Enumeration_Vector"), length=Number(16)),
        Link(Field("Enumeration_Vector"), Field("AV_Enumeration_Vector"), length=Number(16)),
        Link(Field("AV_Enumeration_Vector"), FINAL),
    ],
    {
        Field("Length"): ARRAYS_LENGTH,
        Field("Modular_Vector"): ARRAYS_MODULAR_VECTOR,
        Field("Range_Vector"): ARRAYS_RANGE_VECTOR,
        Field("Enumeration_Vector"): ARRAYS_ENUMERATION_VECTOR,
        Field("AV_Enumeration_Vector"): ARRAYS_AV_ENUMERATION_VECTOR,
    },
)
ARRAYS_INNER_MESSAGE = Message(
    "Arrays.Inner_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Payload"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Payload"), FINAL),
    ],
    {Field("Length"): ARRAYS_LENGTH, Field("Payload"): Opaque()},
)
ARRAYS_INNER_MESSAGES = Array("Arrays.Inner_Messages", ARRAYS_INNER_MESSAGE)
ARRAYS_MESSAGES_MESSAGE = Message(
    "Arrays.Messages_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Messages"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Messages"), FINAL),
    ],
    {Field("Length"): ARRAYS_LENGTH, Field("Messages"): ARRAYS_INNER_MESSAGES},
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
    ]
)

EXPRESSION_MESSAGE = Message(
    "Expression.Message",
    [
        Link(INITIAL, Field("Payload"), length=Number(16)),
        Link(Field("Payload"), FINAL, Equal(Variable("Payload"), Aggregate(Number(1), Number(2)))),
    ],
    {Field("Payload"): Opaque()},
)
EXPRESSION_MODEL = Model([EXPRESSION_MESSAGE])

DERIVATION_MESSAGE = DerivedMessage("Derivation.Message", ARRAYS_MESSAGE)
DERIVATION_MODEL = Model([*ARRAYS_MODEL.types, DERIVATION_MESSAGE])

MODULAR_INTEGER = ModularInteger("P.Modular", Number(256))
RANGE_INTEGER = RangeInteger("P.Range", Number(1), Number(100), Number(8))
