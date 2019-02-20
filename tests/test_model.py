import unittest
from collections import OrderedDict

from rflx.expression import (Add, And, Div, Equal, ExpressionError, First, GreaterEqual, Last,
                             Length, LengthValue, LessEqual, Mul, NotEqual, Number, Or, Pow, Sub,
                             TRUE, UNDEFINED, Value)
from rflx.model import (Array, Edge, FINAL, Field, InitialNode, ModelError, ModularInteger, Node,
                        Null, PDU, RangeInteger, Variant)

from tests.models import ETHERNET_PDU


SOME_LOG_EXPR = Equal(UNDEFINED, UNDEFINED)


# pylint: disable=too-many-public-methods
class TestModel(unittest.TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_modular_size(self) -> None:
        self.assertEqual(ModularInteger('UINT64', Pow(Number(2), Number(64))).size,
                         Number(64))

    def test_modular_invalid_modulus_power_of_two(self) -> None:
        with self.assertRaises(ModelError):
            ModularInteger('X', Number(255))

    def test_modular_invalid_modulus_variable(self) -> None:
        with self.assertRaises(ModelError):
            ModularInteger('X', Pow(Number(2), Value('X')))

    def test_modular_invalid_modulus_limit(self) -> None:
        with self.assertRaises(ModelError):
            ModularInteger('X', Pow(Number(2), Number(128)))

    def test_range_size(self) -> None:
        self.assertEqual(RangeInteger('UINT32',
                                      Number(0),
                                      Sub(Pow(Number(2), Number(32)), Number(1)),
                                      Number(32)
                                      ).size,
                         Number(32))

    def test_range_invalid_first_variable(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Add(Number(1), Value('X')), Number(15), Number(4))

    def test_range_invalid_last_variable(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(1), Add(Number(1), Value('X')), Number(4))

    def test_range_invalid_first_negative(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(-1), Number(0), Number(1))

    def test_range_invalid_range(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(1), Number(0), Number(1))

    def test_range_invalid_size_variable(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(0), Number(256), Add(Number(8), Value('X')))

    def test_range_invalid_size_too_small(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(0), Number(256), Number(8))

    def test_array_invalid_call(self) -> None:
        with self.assertRaises(ModelError):
            Array('X').size  # pylint: disable=expression-not-assigned

    def test_pdu_fields_invalid_cyclic(self) -> None:
        t = ModularInteger('T', Number(2))

        initial = InitialNode()
        n1 = Node('X', t)
        n2 = Node('Y', t)

        initial.edges = [Edge(n1, TRUE)]
        n1.edges = [Edge(n2, TRUE, Number(1))]
        n2.edges = [Edge(n1, TRUE, Number(1))]

        with self.assertRaises(ModelError):
            PDU('Z', initial).fields()

    def test_pdu_fields_invalid_dupe(self) -> None:
        t1 = ModularInteger('T1', Number(2))
        t2 = ModularInteger('T2', Number(4))

        initial = InitialNode()
        n1 = Node('X', t1)
        n2 = Node('X', t2)

        initial.edges = [Edge(n1, TRUE)]
        n1.edges = [Edge(n2, TRUE)]
        n2.edges = [Edge(FINAL, TRUE)]

        with self.assertRaises(ModelError):
            PDU('Z', initial).fields()

    def test_pdu_fields_invalid_self_reference(self) -> None:
        t = ModularInteger('T', Number(2))

        initial = InitialNode()
        n1 = Node('X', t)
        n2 = Node('Y', t)

        initial.edges = [Edge(n1, TRUE)]
        n1.edges = [Edge(n2, first=First('Y'))]
        n2.edges = [Edge(FINAL)]

        with self.assertRaisesRegex(ExpressionError, 'self-reference to "Y\'First"'):
            PDU('Z', initial).fields()

    def test_pdu_fields_length_after_payload(self) -> None:
        int_type = ModularInteger('T', Number(256))
        payload_type = Array('Payload_Type')

        initial = InitialNode()
        version = Node('Version', int_type)
        payload = Node('Payload', payload_type)
        length = Node('Length', int_type)

        initial.edges = [Edge(version, TRUE)]
        version.edges = [Edge(payload, length=Value('Length'))]
        payload.edges = [Edge(length, first=Add(Last('Buffer'),
                                                -Length('Length'),
                                                Number(1)))]
        length.edges = [Edge(FINAL)]

        pdu = PDU('Foo', initial)

        expected = OrderedDict([
            ('Version',
             Field('Version',
                   int_type,
                   TRUE,
                   {
                       '0':
                       Variant(
                           [],
                           TRUE,
                           {
                               Length('Version'): Number(8),
                               First('Version'): Number(0),
                               Last('Version'): Number(7)
                           })
                   })),
            ('Payload',
             Field('Payload',
                   payload_type,
                   TRUE,
                   {
                       '00':
                       Variant(
                           [('Version', '0')],
                           TRUE,
                           {
                               Length('Version'): Number(8),
                               First('Version'): Number(0),
                               Last('Version'): Number(7),
                               Length('Payload'): Value('Length'),
                               First('Payload'): Number(8),
                               Last('Payload'): Add(Value('Length'), Number(7))
                           })
                   })),
            ('Length',
             Field('Length',
                   int_type,
                   TRUE,
                   {
                       '000':
                       Variant(
                           [('Version', '0'),
                            ('Payload', '00')],
                           TRUE,
                           {
                               Length('Version'): Number(8),
                               First('Version'): Number(0),
                               Last('Version'): Number(7),
                               Length('Payload'): Value('Length'),
                               First('Payload'): Number(8),
                               Last('Payload'): Add(Value('Length'), Number(7)),
                               Length('Length'): Number(8),
                               First('Length'): Add(Last('Buffer'), Number(-7)),
                               Last('Length'): Last('Buffer')
                           })
                   })),
            ('FINAL',
             Field('FINAL',
                   Null(),
                   TRUE,
                   {
                       '0000':
                       Variant(
                           [('Version', '0'),
                            ('Payload', '00'),
                            ('Length', '000')],
                           TRUE,
                           {
                               Length('Version'): Number(8),
                               First('Version'): Number(0),
                               Last('Version'): Number(7),
                               Length('Payload'): Value('Length'),
                               First('Payload'): Number(8),
                               Last('Payload'): Add(Value('Length'), Number(7)),
                               Length('Length'): Number(8),
                               First('Length'): Add(Last('Buffer'), Number(-7)),
                               Last('Length'): Last('Buffer')
                           })
                   }))
        ])

        self.assertEqual(pdu.fields(), expected)

    def test_pdu_fields_ethernet(self) -> None:
        expected = OrderedDict([
            ('Destination',
             Field('Destination',
                   ModularInteger('UINT48', Pow(Number(2), Number(48))),
                   TRUE,
                   {
                       '0':
                       Variant(
                           [],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47)
                           })
                   })),
            ('Source',
             Field('Source',
                   ModularInteger('UINT48', Pow(Number(2), Number(48))),
                   TRUE,
                   {
                       '00':
                       Variant(
                           [
                               ('Destination', '0')
                           ],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95)
                           })
                   })),
            ('TPID',
             Field('TPID',
                   RangeInteger('UINT16',
                                Number(0),
                                Sub(Pow(Number(2), Number(16)), Number(1)),
                                Number(16)),
                   Or(NotEqual(Value('TPID'), Number(0x8100)),
                      Equal(Value('TPID'), Number(0x8100))),
                   {
                       '000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00')
                           ],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111)
                           })
                   })),
            ('TCI',
             Field('TCI',
                   RangeInteger('UINT16',
                                Number(0),
                                Sub(Pow(Number(2), Number(16)), Number(1)),
                                Number(16)),
                   TRUE,
                   {
                       '0000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000')
                           ],
                           Equal(Value('TPID'), Number(0x8100)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127)
                           })
                   })),
            ('EtherType',
             Field('EtherType',
                   RangeInteger('UINT16',
                                Number(0),
                                Sub(Pow(Number(2), Number(16)), Number(1)),
                                Number(16)),
                   Or(GreaterEqual(Value('EtherType'), Number(1536)),
                      LessEqual(Value('EtherType'), Number(1500))),
                   {
                       '00000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('TCI', '0000')
                           ],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(128),
                               Last('EtherType'): Number(143)
                           }),
                       '0001':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000')
                           ],
                           NotEqual(Value('TPID'), Number(0x8100)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(96),
                               Last('EtherType'): Number(111)
                           })
                   })),
            ('Payload',
             Field('Payload',
                   Array('Payload_Array'),
                   And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                       LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                   {
                       '000000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('TCI', '0000'),
                               ('EtherType', '00000')
                           ],
                           LessEqual(Value('EtherType'), Number(1500)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(128),
                               Last('EtherType'): Number(143),
                               Length('Payload'): Mul(LengthValue('EtherType'), Number(8)),
                               First('Payload'): Number(144),
                               Last('Payload'): Add(Mul(LengthValue('EtherType'), Number(8)),
                                                    Number(143))
                           }),
                       '000001':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('TCI', '0000'),
                               ('EtherType', '00000')
                           ],
                           GreaterEqual(Value('EtherType'), Number(1536)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(128),
                               Last('EtherType'): Number(143),
                               Length('Payload'): Add(Last('Message'), Number(-143)),
                               First('Payload'): Number(144),
                               Last('Payload'): Last('Message')
                           }),
                       '00010':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('EtherType', '0001')
                           ],
                           LessEqual(Value('EtherType'), Number(1500)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(96),
                               Last('EtherType'): Number(111),
                               Length('Payload'): Mul(LengthValue('EtherType'), Number(8)),
                               First('Payload'): Number(112),
                               Last('Payload'): Add(Mul(LengthValue('EtherType'), Number(8)),
                                                    Number(111))
                           }),
                       '00011':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('EtherType', '0001')
                           ],
                           GreaterEqual(Value('EtherType'), Number(1536)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(96),
                               Last('EtherType'): Number(111),
                               Length('Payload'): Add(Last('Message'), Number(-111)),
                               First('Payload'): Number(112),
                               Last('Payload'): Last('Message')
                           })
                   })),
            ('FINAL',
             Field('FINAL',
                   Null(),
                   TRUE,
                   {
                       '0000000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('TCI', '0000'),
                               ('EtherType', '00000'),
                               ('Payload', '000000')
                           ],
                           And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                               LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(128),
                               Last('EtherType'): Number(143),
                               Length('Payload'): Mul(LengthValue('EtherType'), Number(8)),
                               First('Payload'): Number(144),
                               Last('Payload'): Add(Mul(LengthValue('EtherType'), Number(8)),
                                                    Number(143))
                           }),
                       '0000010':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('TCI', '0000'),
                               ('EtherType', '00000'),
                               ('Payload', '000001')
                           ],
                           And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                               LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(128),
                               Last('EtherType'): Number(143),
                               Length('Payload'): Add(Last('Message'), Number(-143)),
                               First('Payload'): Number(144),
                               Last('Payload'): Last('Message')
                           }),
                       '000100':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('EtherType', '0001'),
                               ('Payload', '00010')
                           ],
                           And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                               LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(96),
                               Last('EtherType'): Number(111),
                               Length('Payload'): Mul(LengthValue('EtherType'), Number(8)),
                               First('Payload'): Number(112),
                               Last('Payload'): Add(Mul(LengthValue('EtherType'), Number(8)),
                                                    Number(111))
                           }),
                       '000110':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('TPID', '000'),
                               ('EtherType', '0001'),
                               ('Payload', '00011')
                           ],
                           And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                               LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('EtherType'): Number(16),
                               First('EtherType'): Number(96),
                               Last('EtherType'): Number(111),
                               Length('Payload'): Add(Last('Message'), Number(-111)),
                               First('Payload'): Number(112),
                               Last('Payload'): Last('Message')
                           })
                   }))
        ])

        self.assertEqual(ETHERNET_PDU.fields(), expected)
