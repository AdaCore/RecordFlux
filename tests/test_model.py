import unittest
from collections import OrderedDict
from typing import Dict

from model import (Add, And, Array, Div, Edge, Equal, FINAL, Field, First, Greater, GreaterEqual,
                   Last, Length, Less, LessEqual, ModelError, ModularInteger, Mul, Node, NotEqual,
                   Number, Or, PDU, Pow, RangeInteger, Sub, TRUE, UNDEFINED, Value, Variant)

from tests.models import ETHERNET_PDU


SOME_LOG_EXPR = Equal(UNDEFINED, UNDEFINED)


# pylint: disable=too-many-public-methods
class TestModel(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true_simplified(self) -> None:
        self.assertEqual(TRUE.simplified(),
                         TRUE)

    def test_and_simplified(self) -> None:
        self.assertEqual(And(TRUE, TRUE).simplified(),
                         TRUE)
        self.assertEqual(And(TRUE, SOME_LOG_EXPR).simplified(),
                         SOME_LOG_EXPR)
        self.assertEqual(And(SOME_LOG_EXPR, TRUE).simplified(),
                         SOME_LOG_EXPR)

    def test_or_simplified(self) -> None:
        self.assertEqual(Or(TRUE, TRUE).simplified(),
                         TRUE)
        self.assertEqual(Or(TRUE, SOME_LOG_EXPR).simplified(),
                         TRUE)
        self.assertEqual(Or(SOME_LOG_EXPR, TRUE).simplified(),
                         TRUE)

    def test_undefined_neg(self) -> None:
        self.assertEqual(-UNDEFINED,
                         UNDEFINED)

    def test_undefined_simplified(self) -> None:
        self.assertEqual(UNDEFINED.simplified(),
                         UNDEFINED)

    def test_undefined_to_bytes(self) -> None:
        self.assertEqual(UNDEFINED.to_bytes(),
                         UNDEFINED)

    def test_number_neg(self) -> None:
        self.assertEqual(-Number(42),
                         Number(-42))

    def test_number_simplified(self) -> None:
        self.assertEqual(Number(42).simplified(),
                         Number(42))

    def test_number_to_bytes(self) -> None:
        self.assertEqual(Number(48).to_bytes(),
                         Number(6))
        self.assertEqual(Number(47).to_bytes(),
                         Number(5))

    def test_number_add(self) -> None:
        self.assertEqual(Number(5) + Number(3), Number(8))

    def test_number_sub(self) -> None:
        self.assertEqual(Number(5) - Number(3), Number(2))

    def test_number_mul(self) -> None:
        self.assertEqual(Number(4) * Number(2), Number(8))

    def test_number_div(self) -> None:
        self.assertEqual(Number(4) // Number(2), Number(2))

    def test_number_pow(self) -> None:
        self.assertEqual(Number(2)**Number(4), Number(16))

    def test_number_lt(self) -> None:
        self.assertEqual(Number(1) < Number(2),
                         True)
        self.assertEqual(Number(2) < Number(2),
                         False)
        self.assertEqual(Number(3) < Number(2),
                         False)
        self.assertEqual(Value('X') < Number(2),
                         False)
        self.assertEqual(Number(2) < Value('X'),
                         False)

    def test_number_le(self) -> None:
        self.assertEqual(Number(1) <= Number(2),
                         True)
        self.assertEqual(Number(2) <= Number(2),
                         True)
        self.assertEqual(Number(3) <= Number(2),
                         False)
        self.assertEqual(Value('X') <= Number(2),
                         False)
        self.assertEqual(Number(2) <= Value('X'),
                         False)

    def test_number_gt(self) -> None:
        self.assertEqual(Number(1) > Number(2),
                         False)
        self.assertEqual(Number(2) > Number(2),
                         False)
        self.assertEqual(Number(3) > Number(2),
                         True)
        self.assertEqual(Value('X') > Number(2),
                         False)
        self.assertEqual(Number(2) > Value('X'),
                         False)

    def test_number_ge(self) -> None:
        self.assertEqual(Number(1) >= Number(2),
                         False)
        self.assertEqual(Number(2) >= Number(2),
                         True)
        self.assertEqual(Number(3) >= Number(2),
                         True)
        self.assertEqual(Value('X') >= Number(2),
                         False)
        self.assertEqual(Number(2) >= Value('X'),
                         False)

    def test_add_neg(self) -> None:
        self.assertEqual(-Add(Value('X'), Number(1)),
                         Add(Value('X', True), Number(-1)))

    def test_add_simplified(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)).simplified(),
                         Add(Value('X'), Number(1)))
        self.assertEqual(Add(Value('X'), Number(0)).simplified(),
                         Value('X'))
        self.assertEqual(Add(Number(2), Number(3), Number(5)).simplified(),
                         Number(10))
        self.assertEqual(Add(Value('X'), Value('Y'), Value('X', True)).simplified(),
                         Value('Y'))
        self.assertEqual(Add(Value('X'), Value('Y'), Value('X'), -Value('X')).simplified(),
                         Add(Value('X'), Value('Y')))

    def test_add_to_bytes(self) -> None:
        self.assertEqual(Add(Value('X'), Number(8)).to_bytes(),
                         Add(Value('X'), Number(1)))

    def test_add_lt(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) < Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(3)) < Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(1)) < Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_le(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) <= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(3)) <= Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(1)) <= Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_gt(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) > Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(3)) > Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(1)) > Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_ge(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) >= Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(3)) >= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(1)) >= Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_mul_neg(self) -> None:
        self.assertEqual(-Mul(Value('X'), Number(2)),
                         Mul(Value('X'), Number(2), Number(-1)))

    def test_mul_simplified(self) -> None:
        self.assertEqual(Mul(Value('X'), Number(2)).simplified(),
                         Mul(Value('X'), Number(2)))
        self.assertEqual(Mul(Value('X'), Number(1)).simplified(),
                         Value('X'))
        self.assertEqual(Mul(Number(2), Number(3), Number(5)).simplified(),
                         Number(30))

    def test_mul_to_bytes(self) -> None:
        self.assertEqual(Mul(Value('X'), Number(8)).to_bytes(),
                         Mul(Value('X'), Number(1)))

    def test_sub_neg(self) -> None:
        self.assertEqual(-Sub(Number(1), Value('X')),
                         Sub(Number(-1), Value('X')))

    def test_sub_simplified(self) -> None:
        self.assertEqual(Sub(Number(1), Value('X')).simplified(),
                         Add(Value('X'), Number(-1)))
        self.assertEqual(Sub(Value('X'), Number(1)).simplified(),
                         Add(Value('X'), Number(-1)))
        self.assertEqual(Sub(Number(6), Number(2)).simplified(),
                         Number(4))
        self.assertEqual(Sub(Value('X'), Value('Y')).simplified(),
                         Add(Value('X'), Value('Y', True)))

    def test_sub_to_bytes(self) -> None:
        self.assertEqual(Sub(Value('X'), Number(8)).to_bytes(),
                         Sub(Value('X'), Number(1)))

    def test_div_neg(self) -> None:
        self.assertEqual(-Div(Value('X'), Number(1)),
                         Div(Value('X', True), Number(1)))

    def test_div_simplified(self) -> None:
        self.assertEqual(Div(Value('X'), Number(1)).simplified(),
                         Div(Value('X'), Number(1)))
        self.assertEqual(Div(Number(6), Number(2)).simplified(),
                         Number(3))
        self.assertEqual(Div(Number(9), Number(2)).simplified(),
                         Div(Number(9), Number(2)))

    def test_div_to_bytes(self) -> None:
        self.assertEqual(Div(Value('X'), Number(8)).to_bytes(),
                         Div(Value('X'), Number(1)))

    def test_term_simplified(self) -> None:
        self.assertEqual(Add(Mul(Number(1), Number(6)),
                             Sub(Value('X'), Number(10)),
                             Add(Number(1), Number(3))).simplified(),
                         Value('X'))

    def test_term_to_bytes(self) -> None:
        self.assertEqual(Add(Mul(Number(8), Number(48)),
                             Sub(Value('X'), Number(80)),
                             Div(Number(8), Number(24))).to_bytes(),
                         Add(Mul(Number(1), Number(6)),
                             Sub(Value('X'), Number(10)),
                             Div(Number(1), Number(3))))

    def test_distributivity_simplified(self) -> None:
        self.assertEqual(Add(Sub(Value('X'), Add(Value('X'), Number(1))),
                             Add(Value('X'), Number(1))).simplified(),
                         Value('X'))
        self.assertEqual(Div(Add(Mul(Value('X'), Number(8)), Number(144)), Number(8)).simplified(),
                         Add(Value('X'), Number(18)))
        self.assertEqual(Div(Sub(Mul(Value('X'), Number(8)), Number(148)), Number(8)).simplified(),
                         Add(Value('X'), Div(Number(-148), Number(8))))

    def test_value_neg(self) -> None:
        self.assertEqual(-Value('X'),
                         Value('X', True))

    def test_value_simplified(self) -> None:
        self.assertEqual(Value('X').simplified(),
                         Value('X'))
        self.assertEqual(Value('X').simplified({Value('X'): Number(42)}),
                         Number(42))

    def test_length_simplified(self) -> None:
        self.assertEqual(Length('X').simplified(),
                         Length('X'))
        self.assertEqual(Length('X').simplified({Length('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-Length('X').simplified({Length('X'): Number(42)}),
                         Number(-42))

    def test_first_neg(self) -> None:
        self.assertEqual(-First('X'),
                         First('X', True))

    def test_first_simplified(self) -> None:
        self.assertEqual(First('X').simplified(),
                         First('X'))
        self.assertEqual(First('X').simplified({First('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-First('X').simplified({First('X'): Number(42)}),
                         Number(-42))

    def test_last_neg(self) -> None:
        self.assertEqual(-Last('X'),
                         Last('X', True))

    def test_last_simplified(self) -> None:
        self.assertEqual(Last('X').simplified(),
                         Last('X'))
        self.assertEqual(Last('X').simplified({Last('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-Last('X').simplified({Last('X'): Number(42)}),
                         Number(-42))

    def test_less_simplified(self) -> None:
        self.assertEqual(Less(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Less(Value('X'), Number(42)))

    def test_less_equal_simplified(self) -> None:
        self.assertEqual(LessEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         LessEqual(Value('X'), Number(42)))

    def test_equal_simplified(self) -> None:
        self.assertEqual(Equal(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Equal(Value('X'), Number(42)))

    def test_greater_simplified(self) -> None:
        self.assertEqual(Greater(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Greater(Value('X'), Number(42)))

    def test_greater_equal_simplified(self) -> None:
        self.assertEqual(GreaterEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         GreaterEqual(Value('X'), Number(42)))

    def test_not_equal_simplified(self) -> None:
        self.assertEqual(NotEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         NotEqual(Value('X'), Number(42)))

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

        n1 = Node('X', t)
        n2 = Node('Y', t)

        n1.edges = [Edge(n2, TRUE, Number(1))]
        n2.edges = [Edge(n1, TRUE, Number(1))]

        with self.assertRaises(ModelError):
            PDU('Z', n1).fields()

    def test_pdu_fields_invalid_dupe(self) -> None:
        t1 = ModularInteger('T1', Number(2))
        t2 = ModularInteger('T2', Number(4))

        n1 = Node('X', t1)
        n2 = Node('X', t2)

        n1.edges = [Edge(n2, TRUE)]
        n2.edges = [Edge(FINAL, TRUE)]

        with self.assertRaises(ModelError):
            PDU('Z', n1).fields()

    def test_pdu_fields_invalid_self_reference(self) -> None:
        t = ModularInteger('T', Number(2))

        n1 = Node('X', t)
        n2 = Node('Y', t)

        n1.edges = [Edge(n2, first=First('Y'))]
        n2.edges = [Edge(FINAL)]

        with self.assertRaisesRegex(ModelError, 'self-reference to "Y\'First"'):
            PDU('Z', n1).fields()

    def test_pdu_fields_length_after_payload(self) -> None:
        int_type = ModularInteger('T', Number(256))
        payload_type = Array('Payload_Type')

        version = Node('Version', int_type)
        payload = Node('Payload', payload_type)
        length = Node('Length', int_type)

        version.edges = [Edge(payload, length=Value('Length'))]
        payload.edges = [Edge(length, first=Add(Last('Buffer'),
                                                -Length('Length'),
                                                Number(1)))]
        length.edges = [Edge(FINAL)]

        pdu = PDU('Foo', version)

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
                       '0_0':
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
                       '0_0_0':
                       Variant(
                           [('Version', '0'),
                            ('Payload', '0_0')],
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
        expected: Dict[str, Field] = {
            'Destination':
            Field('Destination',
                  ModularInteger('UINT48', Pow(Number(2), Number(48))),
                  TRUE,
                  {
                      '0':
                      Variant(
                          [],
                          TRUE,
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48)
                          })
                  }),
            'Source':
            Field('Source',
                  ModularInteger('UINT48', Pow(Number(2), Number(48))),
                  TRUE,
                  {
                      '0_0':
                      Variant(
                          [
                              ('Destination', '0')
                          ],
                          TRUE,
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48)
                          })
                  }),
            'TPID':
            Field('TPID',
                  RangeInteger('UINT16',
                               Number(0),
                               Sub(Pow(Number(2), Number(16)), Number(1)),
                               Number(16)),
                  Or(NotEqual(Value('TPID'), Number(0x8100)),
                     Equal(Value('TPID'), Number(0x8100))),
                  {
                      '0_0_0':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0')
                          ],
                          TRUE,
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16)
                          })
                  }),
            'TCI':
            Field('TCI',
                  RangeInteger('UINT16',
                               Number(0),
                               Sub(Pow(Number(2), Number(16)), Number(1)),
                               Number(16)),
                  TRUE,
                  {
                      '0_0_0_0':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0')
                          ],
                          Equal(Value('TPID'), Number(0x8100)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('TCI'): Number(112),
                              Last('TCI'): Number(127),
                              Length('TCI'): Number(16)
                          })
                  }),
            'EtherType':
            Field('EtherType',
                  RangeInteger('UINT16',
                               Number(0),
                               Sub(Pow(Number(2), Number(16)), Number(1)),
                               Number(16)),
                  Or(GreaterEqual(Value('EtherType'), Number(1536)),
                     LessEqual(Value('EtherType'), Number(1500))),
                  {
                      '0_0_0_0_0':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0'),
                              ('TCI', '0_0_0_0')
                          ],
                          TRUE,
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('TCI'): Number(112),
                              Last('TCI'): Number(127),
                              Length('TCI'): Number(16),
                              First('EtherType'): Number(128),
                              Last('EtherType'): Number(143),
                              Length('EtherType'): Number(16)
                          }),
                      '0_0_0_1':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0')
                          ],
                          NotEqual(Value('TPID'), Number(0x8100)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('EtherType'): Number(96),
                              Last('EtherType'): Number(111),
                              Length('EtherType'): Number(16)
                          })
                  }),
            'Payload':
            Field('Payload',
                  Array('Payload_Array'),
                  And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                      LessEqual(Div(Length('Payload'), Number(8)), Number(1500))),
                  {
                      '0_0_0_0_0_0':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0'),
                              ('TCI', '0_0_0_0'),
                              ('EtherType', '0_0_0_0_0')
                          ],
                          LessEqual(Value('EtherType'), Number(1500)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('TCI'): Number(112),
                              Last('TCI'): Number(127),
                              Length('TCI'): Number(16),
                              First('EtherType'): Number(128),
                              Last('EtherType'): Number(143),
                              Length('EtherType'): Number(16),
                              First('Payload'): Number(144),
                              Last('Payload'): Add(Mul(Value('EtherType'), Number(8)), Number(143)),
                              Length('Payload'): Mul(Value('EtherType'), Number(8))
                          }),
                      '0_0_0_0_0_1':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0'),
                              ('TCI', '0_0_0_0'),
                              ('EtherType', '0_0_0_0_0')
                          ],
                          GreaterEqual(Value('EtherType'), Number(1536)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('TCI'): Number(112),
                              Last('TCI'): Number(127),
                              Length('TCI'): Number(16),
                              First('EtherType'): Number(128),
                              Last('EtherType'): Number(143),
                              Length('EtherType'): Number(16),
                              First('Payload'): Number(144),
                              Last('Payload'): Last('Message'),
                              Length('Payload'): Add(Last('Message'), Number(-143))
                          }),
                      '0_0_0_1_0':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0'),
                              ('EtherType', '0_0_0_1')
                          ],
                          LessEqual(Value('EtherType'), Number(1500)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('EtherType'): Number(96),
                              Last('EtherType'): Number(111),
                              Length('EtherType'): Number(16),
                              First('Payload'): Number(112),
                              Last('Payload'): Add(Mul(Value('EtherType'), Number(8)), Number(111)),
                              Length('Payload'): Mul(Value('EtherType'), Number(8))
                          }),
                      '0_0_0_1_1':
                      Variant(
                          [
                              ('Destination', '0'),
                              ('Source', '0_0'),
                              ('TPID', '0_0_0'),
                              ('EtherType', '0_0_0_1')
                          ],
                          GreaterEqual(Value('EtherType'), Number(1536)),
                          {
                              First('Destination'): Number(0),
                              Last('Destination'): Number(47),
                              Length('Destination'): Number(48),
                              First('Source'): Number(48),
                              Last('Source'): Number(95),
                              Length('Source'): Number(48),
                              First('TPID'): Number(96),
                              Last('TPID'): Number(111),
                              Length('TPID'): Number(16),
                              First('EtherType'): Number(96),
                              Last('EtherType'): Number(111),
                              Length('EtherType'): Number(16),
                              First('Payload'): Number(112),
                              Last('Payload'): Last('Message'),
                              Length('Payload'): Add(Last('Message'), Number(-111))
                          })
                  }),
        }

        self.assertEqual(ETHERNET_PDU.fields(), expected)
