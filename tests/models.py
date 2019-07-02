from rflx.expression import (Aggregate, And, Div, Equal, First, GreaterEqual, Last, Length,
                             LengthValue, LessEqual, Mul, NotEqual, Number, Pow, Sub, Value)
from rflx.model import (FINAL, Array, DerivedMessage, Edge, Enumeration, InitialNode, Message,
                        ModularInteger, Node, RangeInteger)


def create_ethernet_frame() -> Message:
    address_type = ModularInteger('Address_Type', Pow(Number(2), Number(48)))
    type_length_type = RangeInteger('Type_Length_Type',
                                    Number(46),
                                    Sub(Pow(Number(2), Number(16)), Number(1)),
                                    Number(16))
    tpid_type = RangeInteger('TPID_Type',
                             Number(0x8100),
                             Number(0x8100),
                             Number(16))
    tci_type = ModularInteger('TCI_Type',
                              Pow(Number(2), Number(16)))
    payload_type = Array('Payload_Type')

    initial = InitialNode()
    destination = Node('Destination', address_type)
    source = Node('Source', address_type)
    type_length_tpid = Node('Type_Length_TPID', type_length_type)
    tpid = Node('TPID', tpid_type)
    tci = Node('TCI', tci_type)
    type_length = Node('Type_Length', type_length_type)
    payload = Node('Payload', payload_type)

    initial.edges = [Edge(destination)]
    destination.edges = [Edge(source)]
    source.edges = [Edge(type_length_tpid)]
    type_length_tpid.edges = [Edge(tpid,
                                   Equal(Value('Type_Length_TPID'), Number(0x8100)),
                                   first=First('Type_Length_TPID')),
                              Edge(type_length,
                                   NotEqual(Value('Type_Length_TPID'), Number(0x8100)),
                                   first=First('Type_Length_TPID'))]
    tpid.edges = [Edge(tci)]
    tci.edges = [Edge(type_length)]
    type_length.edges = [Edge(payload,
                              LessEqual(Value('Type_Length'), Number(1500)),
                              Mul(LengthValue('Type_Length'), Number(8))),
                         Edge(payload,
                              GreaterEqual(Value('Type_Length'), Number(1536)),
                              Sub(Last('Message'), Last('Type_Length')))]
    payload.edges = [Edge(FINAL,
                          And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                              LessEqual(Div(Length('Payload'), Number(8)), Number(1500))))]

    return Message('Ethernet.Frame', initial)


def create_enumeration_message() -> Message:
    priority_type = Enumeration('Priority',
                                {'LOW': Number(1), 'MEDIUM': Number(4), 'HIGH': Number(7)},
                                Number(3),
                                True)

    initial = InitialNode()
    priority = Node('Priority', priority_type)

    initial.edges = [Edge(priority)]
    priority.edges = [Edge(FINAL)]

    return Message('Enumeration.Message', initial)


def create_array_message() -> Message:
    length_type = ModularInteger('Length_Type', Pow(Number(2), Number(8)))

    modular_type = ModularInteger('Modular_Integer', Pow(Number(2), Number(16)))
    modular_vector_type = Array('Modular_Vector', modular_type)

    range_type = RangeInteger('Range_Integer', Number(1), Number(100), Number(8))
    range_vector_type = Array('Range_Vector', range_type)

    enum_type = Enumeration('Enumeration',
                            {'ZERO': Number(0), 'ONE': Number(1), 'TWO': Number(2)},
                            Number(8),
                            False)
    enum_vector_type = Array('Enumeration_Vector', enum_type)

    av_enum_type = Enumeration('AV_Enumeration',
                               {'AV_ZERO': Number(0), 'AV_ONE': Number(1), 'AV_TWO': Number(2)},
                               Number(8),
                               True)
    av_enum_vector_type = Array('AV_Enumeration_Vector', av_enum_type)

    initial = InitialNode()
    length = Node('Length', length_type)
    modular_vector = Node('Modular_Vector', modular_vector_type)
    range_vector = Node('Range_Vector', range_vector_type)
    enum_vector = Node('Enumeration_Vector', enum_vector_type)
    av_enum_vector = Node('AV_Enumeration_Vector', av_enum_vector_type)

    initial.edges = [Edge(length)]
    length.edges = [Edge(modular_vector, length=Mul(LengthValue('Length'), Number(8)))]
    modular_vector.edges = [Edge(range_vector, length=Number(16))]
    range_vector.edges = [Edge(enum_vector, length=Number(16))]
    enum_vector.edges = [Edge(av_enum_vector, length=Number(16))]
    av_enum_vector.edges = [Edge(FINAL)]

    return Message('Arrays.Message', initial)


def create_expression_message() -> Message:
    payload_array = Array('Payload_Array')

    initial = InitialNode()
    payload = Node('Payload', payload_array)

    initial.edges = [Edge(payload,
                          length=Number(16))]
    payload.edges = [Edge(FINAL,
                          Equal(Value('Payload'), Aggregate(Number(1), Number(2))))]

    return Message('Expression.Message', initial)


def create_derivation_message() -> Message:
    return DerivedMessage('Derivation.Message', ARRAY_MESSAGE.full_name, ARRAY_MESSAGE.initial_node)


ETHERNET_FRAME = create_ethernet_frame()
ENUMERATION_MESSAGE = create_enumeration_message()
ARRAY_MESSAGE = create_array_message()
EXPRESSION_MESSAGE = create_expression_message()
DERIVATION_MESSAGE = create_derivation_message()
