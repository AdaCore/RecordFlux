from rflx.expression import (And, Div, Equal, First, GreaterEqual, Last, Length, LengthValue,
                             LessEqual, Mul, NotEqual, Number, NumberArray, Pow, Sub, Value)
from rflx.model import (FINAL, PDU, Array, Edge, Enumeration, InitialNode, ModularInteger, Node,
                        RangeInteger)


def create_ethernet_pdu() -> PDU:
    uint48 = ModularInteger('UINT48', Pow(Number(2), Number(48)))
    uint16 = RangeInteger('UINT16',
                          Number(0),
                          Sub(Pow(Number(2), Number(16)), Number(1)),
                          Number(16))
    payload_array = Array('Payload_Array')

    initial = InitialNode()
    destination = Node('Destination', uint48)
    source = Node('Source', uint48)
    tpid = Node('TPID', uint16)
    tci = Node('TCI', uint16)
    ether_type = Node('EtherType', uint16)
    payload = Node('Payload', payload_array)

    initial.edges = [Edge(destination)]
    destination.edges = [Edge(source)]
    source.edges = [Edge(tpid)]
    tpid.edges = [Edge(tci,
                       Equal(Value('TPID'), Number(0x8100))),
                  Edge(ether_type,
                       NotEqual(Value('TPID'), Number(0x8100)),
                       first=First('TPID'))]
    tci.edges = [Edge(ether_type)]
    ether_type.edges = [Edge(payload,
                             LessEqual(Value('EtherType'), Number(1500)),
                             Mul(LengthValue('EtherType'), Number(8))),
                        Edge(payload,
                             GreaterEqual(Value('EtherType'), Number(1536)),
                             Sub(Last('Message'), Last('EtherType')))]
    payload.edges = [Edge(FINAL,
                          And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                              LessEqual(Div(Length('Payload'), Number(8)), Number(1500))))]

    return PDU('Ethernet.Frame', initial)


def create_enumeration_pdu() -> PDU:
    priority_type = Enumeration('Priority',
                                {'LOW': Number(1), 'MEDIUM': Number(4), 'HIGH': Number(7)},
                                Number(3),
                                True)

    initial = InitialNode()
    priority = Node('Priority', priority_type)

    initial.edges = [Edge(priority)]
    priority.edges = [Edge(FINAL)]

    return PDU('Enumeration.Message', initial)


def create_array_pdu() -> PDU:
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

    return PDU('Arrays.Message', initial)


def create_expression_pdu() -> PDU:
    payload_array = Array('Payload_Array')

    initial = InitialNode()
    payload = Node('Payload', payload_array)

    initial.edges = [Edge(payload,
                          length=Number(16))]
    payload.edges = [Edge(FINAL,
                          Equal(Value('Payload'), NumberArray(Number(1), Number(2))))]

    return PDU('Expression.Message', initial)


ETHERNET_PDU = create_ethernet_pdu()
ENUMERATION_PDU = create_enumeration_pdu()
ARRAY_PDU = create_array_pdu()
EXPRESSION_PDU = create_expression_pdu()
