from rflx.expression import (And, Div, Equal, First, GreaterEqual, Last, Length, LengthValue,
                             LessEqual, Mul, NotEqual, Number, Pow, Sub, Value)
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


ETHERNET_PDU = create_ethernet_pdu()
ENUMERATION_PDU = create_enumeration_pdu()
