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
    ModularInteger,
    Payload,
    RangeInteger,
    Reference,
    Refinement,
)


def create_null_message() -> Message:
    return Message("Null.Message", [], {})


def create_tlv_message() -> Message:
    tag_type = Enumeration(
        "TLV.Tag", {"Msg_Data": Number(1), "Msg_Error": Number(3)}, Number(2), False
    )
    length_type = ModularInteger("TLV.Length", Pow(Number(2), Number(14)))

    structure = [
        Link(INITIAL, Field("Tag")),
        Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("Msg_Data"))),
        Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("Msg_Error"))),
        Link(Field("Length"), Field("Value"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Value"), FINAL),
    ]

    types = {Field("Tag"): tag_type, Field("Length"): length_type, Field("Value"): Payload()}

    return Message("TLV.Message", structure, types)


def create_null_message_in_tlv_message() -> Refinement:
    return Refinement("In_TLV", "TLV.Message", Field("Value"), "Null.Message")


def create_ethernet_frame() -> Message:
    address_type = ModularInteger("Ethernet.Address", Pow(Number(2), Number(48)))
    type_length_type = RangeInteger(
        "Ethernet.Type_Length", Number(46), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16)
    )
    tpid_type = RangeInteger("Ethernet.TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16))
    tci_type = ModularInteger("Ethernet.TCI", Pow(Number(2), Number(16)))

    structure = [
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
                GreaterEqual(Div(Length("Payload"), Number(8)), Number(46)),
                LessEqual(Div(Length("Payload"), Number(8)), Number(1500)),
            ),
        ),
    ]

    types = {
        Field("Destination"): address_type,
        Field("Source"): address_type,
        Field("Type_Length_TPID"): type_length_type,
        Field("TPID"): tpid_type,
        Field("TCI"): tci_type,
        Field("Type_Length"): type_length_type,
        Field("Payload"): Payload(),
    }

    return Message("Ethernet.Frame", structure, types)


def create_enumeration_message() -> Message:
    priority_type = Enumeration(
        "Enumeration.Priority",
        {"LOW": Number(1), "MEDIUM": Number(4), "HIGH": Number(7)},
        Number(3),
        True,
    )

    structure = [Link(INITIAL, Field("Priority")), Link(Field("Priority"), FINAL)]

    types = {Field("Priority"): priority_type}

    return Message("Enumeration.Message", structure, types)


def create_array_message() -> Message:
    length_type = ModularInteger("Arrays.Length", Pow(Number(2), Number(8)))

    modular_type = ModularInteger("Arrays.Modular_Integer", Pow(Number(2), Number(16)))
    modular_vector_type = Array("Arrays.Modular_Vector", modular_type)

    range_type = RangeInteger("Arrays.Range_Integer", Number(1), Number(100), Number(8))
    range_vector_type = Array("Arrays.Range_Vector", range_type)

    enum_type = Enumeration(
        "Arrays.Enumeration",
        {"ZERO": Number(0), "ONE": Number(1), "TWO": Number(2)},
        Number(8),
        False,
    )
    enum_vector_type = Array("Arrays.Enumeration_Vector", enum_type)

    av_enum_type = Enumeration(
        "Arrays.AV_Enumeration",
        {"AV_ZERO": Number(0), "AV_ONE": Number(1), "AV_TWO": Number(2)},
        Number(8),
        True,
    )
    av_enum_vector_type = Array("Arrays.AV_Enumeration_Vector", av_enum_type)

    structure = [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Modular_Vector"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Modular_Vector"), Field("Range_Vector"), length=Number(16)),
        Link(Field("Range_Vector"), Field("Enumeration_Vector"), length=Number(16)),
        Link(Field("Enumeration_Vector"), Field("AV_Enumeration_Vector"), length=Number(16)),
        Link(Field("AV_Enumeration_Vector"), FINAL),
    ]

    types = {
        Field("Length"): length_type,
        Field("Modular_Vector"): modular_vector_type,
        Field("Range_Vector"): range_vector_type,
        Field("Enumeration_Vector"): enum_vector_type,
        Field("AV_Enumeration_Vector"): av_enum_vector_type,
    }

    return Message("Arrays.Message", structure, types)


def create_array_inner_message() -> Message:
    length_type = ModularInteger("Arrays.Length", Pow(Number(2), Number(8)))

    structure = [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Payload"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Payload"), FINAL),
    ]

    types = {Field("Length"): length_type, Field("Payload"): Payload()}

    return Message("Arrays.Inner_Message", structure, types)


def create_array_messages_message() -> Message:
    structure = [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Messages"), length=Mul(Variable("Length"), Number(8))),
        Link(Field("Messages"), FINAL),
    ]

    types = {
        Field("Length"): ModularInteger("Arrays.Length", Pow(Number(2), Number(8))),
        Field("Messages"): Array("Arrays.Inner_Messages", Reference("Arrays.Inner_Message")),
    }

    return Message("Arrays.Messages_Message", structure, types)


def create_expression_message() -> Message:
    structure = [
        Link(INITIAL, Field("Payload"), length=Number(16)),
        Link(Field("Payload"), FINAL, Equal(Variable("Payload"), Aggregate(Number(1), Number(2)))),
    ]

    types = {Field("Payload"): Payload()}

    return Message("Expression.Message", structure, types)


def create_derivation_message() -> Message:
    return DerivedMessage(
        "Derivation.Message", ARRAY_MESSAGE.full_name, ARRAY_MESSAGE.structure, ARRAY_MESSAGE.types
    )


NULL_MESSAGE = create_null_message()
TLV_MESSAGE = create_tlv_message()
NULL_MESSAGE_IN_TLV_MESSAGE = create_null_message_in_tlv_message()
ETHERNET_FRAME = create_ethernet_frame()
ENUMERATION_MESSAGE = create_enumeration_message()
ARRAY_MESSAGE = create_array_message()
ARRAY_INNER_MESSAGE = create_array_inner_message()
ARRAY_MESSAGES_MESSAGE = create_array_messages_message()
EXPRESSION_MESSAGE = create_expression_message()
DERIVATION_MESSAGE = create_derivation_message()
