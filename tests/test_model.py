import unittest
from collections import OrderedDict

from rflx.expression import (TRUE, UNDEFINED, Add, And, Div, Equal, ExpressionError, First,
                             GreaterEqual, Last, Length, LengthValue, LessEqual, Mul, NotEqual,
                             Number, Or, Pow, Sub, Value)
from rflx.model import (FINAL, Array, Edge, Field, InitialNode, Message, ModelError, ModularInteger,
                        Node, Null, RangeInteger, Variant)
from tests.models import ETHERNET_FRAME

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
            Message('Z', initial).fields()

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
            Message('Z', initial).fields()

    def test_pdu_fields_invalid_self_reference(self) -> None:
        t = ModularInteger('T', Number(2))

        initial = InitialNode()
        n1 = Node('X', t)
        n2 = Node('Y', t)

        initial.edges = [Edge(n1, TRUE)]
        n1.edges = [Edge(n2, first=First('Y'))]
        n2.edges = [Edge(FINAL)]

        with self.assertRaisesRegex(ExpressionError, 'self-reference to "Y\'First"'):
            Message('Z', initial).fields()

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

        pdu = Message('Foo', initial)

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
                   ModularInteger('Address_Type', Pow(Number(2), Number(48))),
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
                   ModularInteger('Address_Type', Pow(Number(2), Number(48))),
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
            ('Type_Length_TPID',
             Field('Type_Length_TPID',
                   RangeInteger('Type_Length_Type',
                                Number(46),
                                Sub(Pow(Number(2), Number(16)), Number(1)),
                                Number(16)),
                   Or(NotEqual(Value('Type_Length_TPID'), Number(0x8100)),
                      Equal(Value('Type_Length_TPID'), Number(0x8100))),
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
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111)
                           })
                   })),
            ('TPID',
             Field('TPID',
                   RangeInteger('TPID_Type',
                                Number(0x8100),
                                Number(0x8100),
                                Number(16)),
                   TRUE,
                   {
                       '0000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000')
                           ],
                           Equal(Value('Type_Length_TPID'), Number(0x8100)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                           })
                   })),
            ('TCI',
             Field('TCI',
                   ModularInteger('TCI_Type',
                                  Pow(Number(2), Number(16))),
                   TRUE,
                   {
                       '00000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000')
                           ],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127)
                           })
                   })),
            ('Type_Length',
             Field('Type_Length',
                   RangeInteger('Type_Length_Type',
                                Number(46),
                                Sub(Pow(Number(2), Number(16)), Number(1)),
                                Number(16)),
                   Or(GreaterEqual(Value('Type_Length'), Number(1536)),
                      LessEqual(Value('Type_Length'), Number(1500))),
                   {
                       '000000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000'),
                               ('TCI', '00000')
                           ],
                           TRUE,
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(128),
                               Last('Type_Length'): Number(143)
                           }),
                       '0001':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                           ],
                           NotEqual(Value('Type_Length_TPID'), Number(0x8100)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(96),
                               Last('Type_Length'): Number(111)
                           })
                   })),
            ('Payload',
             Field('Payload',
                   Array('Payload_Type'),
                   And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                       LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                   {
                       '0000000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000'),
                               ('TCI', '00000'),
                               ('Type_Length', '000000')
                           ],
                           LessEqual(Value('Type_Length'), Number(1500)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(128),
                               Last('Type_Length'): Number(143),
                               Length('Payload'): Mul(LengthValue('Type_Length'), Number(8)),
                               First('Payload'): Number(144),
                               Last('Payload'): Add(Mul(LengthValue('Type_Length'), Number(8)),
                                                    Number(143))
                           }),
                       '0000001':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000'),
                               ('TCI', '00000'),
                               ('Type_Length', '000000')
                           ],
                           GreaterEqual(Value('Type_Length'), Number(1536)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(128),
                               Last('Type_Length'): Number(143),
                               Length('Payload'): Add(Last('Message'), Number(-143)),
                               First('Payload'): Number(144),
                               Last('Payload'): Last('Message')
                           }),
                       '00010':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('Type_Length', '0001')
                           ],
                           LessEqual(Value('Type_Length'), Number(1500)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(96),
                               Last('Type_Length'): Number(111),
                               Length('Payload'): Mul(LengthValue('Type_Length'), Number(8)),
                               First('Payload'): Number(112),
                               Last('Payload'): Add(Mul(LengthValue('Type_Length'), Number(8)),
                                                    Number(111))
                           }),
                       '00011':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('Type_Length', '0001')
                           ],
                           GreaterEqual(Value('Type_Length'), Number(1536)),
                           {
                               Length('Destination'): Number(48),
                               First('Destination'): Number(0),
                               Last('Destination'): Number(47),
                               Length('Source'): Number(48),
                               First('Source'): Number(48),
                               Last('Source'): Number(95),
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(96),
                               Last('Type_Length'): Number(111),
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
                       '00000000':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000'),
                               ('TCI', '00000'),
                               ('Type_Length', '000000'),
                               ('Payload', '0000000')
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
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(128),
                               Last('Type_Length'): Number(143),
                               Length('Payload'): Mul(LengthValue('Type_Length'), Number(8)),
                               First('Payload'): Number(144),
                               Last('Payload'): Add(Mul(LengthValue('Type_Length'), Number(8)),
                                                    Number(143))
                           }),
                       '00000010':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('TPID', '0000'),
                               ('TCI', '00000'),
                               ('Type_Length', '000000'),
                               ('Payload', '0000001')
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
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('TPID'): Number(16),
                               First('TPID'): Number(96),
                               Last('TPID'): Number(111),
                               Length('TCI'): Number(16),
                               First('TCI'): Number(112),
                               Last('TCI'): Number(127),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(128),
                               Last('Type_Length'): Number(143),
                               Length('Payload'): Add(Last('Message'), Number(-143)),
                               First('Payload'): Number(144),
                               Last('Payload'): Last('Message')
                           }),
                       '000100':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('Type_Length', '0001'),
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
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(96),
                               Last('Type_Length'): Number(111),
                               Length('Payload'): Mul(LengthValue('Type_Length'), Number(8)),
                               First('Payload'): Number(112),
                               Last('Payload'): Add(Mul(LengthValue('Type_Length'), Number(8)),
                                                    Number(111))
                           }),
                       '000110':
                       Variant(
                           [
                               ('Destination', '0'),
                               ('Source', '00'),
                               ('Type_Length_TPID', '000'),
                               ('Type_Length', '0001'),
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
                               Length('Type_Length_TPID'): Number(16),
                               First('Type_Length_TPID'): Number(96),
                               Last('Type_Length_TPID'): Number(111),
                               Length('Type_Length'): Number(16),
                               First('Type_Length'): Number(96),
                               Last('Type_Length'): Number(111),
                               Length('Payload'): Add(Last('Message'), Number(-111)),
                               First('Payload'): Number(112),
                               Last('Payload'): Last('Message')
                           })
                   }))
        ])

        self.assertEqual(ETHERNET_FRAME.fields(), expected)
