#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
from typing import List

from model import (Add, And, Array, Div, Edge, Equal, FINAL, Field, First, Greater, GreaterEqual,
                   Last, Length, Less, LessEqual, ModelError, ModularInteger, Mul, Node, NotEqual,
                   Number, Or, PDU, RangeInteger, Sub, TRUE, UNDEFINED, Value)

from tests.models import ETHERNET_PDU


SOME_LOG_EXPR = Equal(UNDEFINED, UNDEFINED)


# pylint: disable=too-many-public-methods
class TestModel(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true_simplified(self) -> None:
        self.assertEqual(TRUE.simplified(), TRUE)

    def test_and_simplified(self) -> None:
        self.assertEqual(And(TRUE, TRUE).simplified(), TRUE)
        self.assertEqual(And(TRUE, SOME_LOG_EXPR).simplified(), SOME_LOG_EXPR)
        self.assertEqual(And(SOME_LOG_EXPR, TRUE).simplified(), SOME_LOG_EXPR)

    def test_or_simplified(self) -> None:
        self.assertEqual(Or(TRUE, TRUE).simplified(), TRUE)
        self.assertEqual(Or(TRUE, SOME_LOG_EXPR).simplified(), TRUE)
        self.assertEqual(Or(SOME_LOG_EXPR, TRUE).simplified(), TRUE)

    def test_undefined_simplified(self) -> None:
        self.assertEqual(UNDEFINED.simplified(), UNDEFINED)

    def test_number_simplified(self) -> None:
        self.assertEqual(Number(42).simplified(), Number(42))

    def test_number_add(self) -> None:
        self.assertEqual(Number(5) + Number(3), Number(8))

    def test_number_sub(self) -> None:
        self.assertEqual(Number(5) - Number(3), Number(2))

    def test_number_mul(self) -> None:
        self.assertEqual(Number(4) * Number(2), Number(8))

    def test_number_div(self) -> None:
        self.assertEqual(Number(4) // Number(2), Number(2))

    def test_add_simplified(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)).simplified(), Add(Value('X'), Number(1)))
        self.assertEqual(Add(Number(2), Number(3), Number(5)).simplified(), Number(10))

    def test_mul_simplified(self) -> None:
        self.assertEqual(Mul(Value('X'), Number(1)).simplified(), Mul(Value('X'), Number(1)))
        self.assertEqual(Mul(Number(2), Number(3), Number(5)).simplified(), Number(30))

    def test_sub_simplified(self) -> None:
        self.assertEqual(Sub(Number(1), Value('X')).simplified(), Sub(Number(1), Value('X')))
        self.assertEqual(Sub(Value('X'), Number(1)).simplified(), Add(Value('X'), Number(-1)))
        self.assertEqual(Sub(Number(6), Number(2)).simplified(), Number(4))

    def test_div_simplified(self) -> None:
        self.assertEqual(Div(Value('X'), Number(1)).simplified(), Div(Value('X'), Number(1)))
        self.assertEqual(Div(Number(6), Number(2)).simplified(), Number(3))

    def test_term_simplified(self) -> None:
        self.assertEqual(Add(Mul(Number(1), Number(6)),
                             Sub(Value('X'), Number(10)),
                             Add(Number(1), Number(3))).simplified(),
                         Value('X'))

    def test_value_simplified(self) -> None:
        self.assertEqual(Value('X').simplified(), Value('X'))
        self.assertEqual(Value('X').simplified({Value('X'): Number(42)}), Number(42))

    def test_length_simplified(self) -> None:
        self.assertEqual(Length('X').simplified(), Length('X'))
        self.assertEqual(Length('X').simplified({Length('X'): Number(42)}), Number(42))

    def test_first_simplified(self) -> None:
        self.assertEqual(First('X').simplified(), First('X'))
        self.assertEqual(First('X').simplified({First('X'): Number(42)}), Number(42))

    def test_last_simplified(self) -> None:
        self.assertEqual(Last('X').simplified(), Last('X'))
        self.assertEqual(Last('X').simplified({Last('X'): Number(42)}), Number(42))

    def test_less_simplified(self) -> None:
        self.assertEqual(Less(Value('X'), Number(42)).simplified(),
                         Less(Value('X'), Number(42)))

    def test_less_equal_simplified(self) -> None:
        self.assertEqual(LessEqual(Value('X'), Number(42)).simplified(),
                         LessEqual(Value('X'), Number(42)))

    def test_equal_simplified(self) -> None:
        self.assertEqual(Equal(Value('X'), Number(42)).simplified(),
                         Equal(Value('X'), Number(42)))

    def test_greater_simplified(self) -> None:
        self.assertEqual(Greater(Value('X'), Number(42)).simplified(),
                         Greater(Value('X'), Number(42)))

    def test_greater_equal_simplified(self) -> None:
        self.assertEqual(GreaterEqual(Value('X'), Number(42)).simplified(),
                         GreaterEqual(Value('X'), Number(42)))

    def test_not_equal_simplified(self) -> None:
        self.assertEqual(NotEqual(Value('X'), Number(42)).simplified(),
                         NotEqual(Value('X'), Number(42)))

    def test_modular_size(self) -> None:
        self.assertEqual(ModularInteger('UINT64', 2**64).size(), Number(64))

    def test_modular_invalid_modulus(self) -> None:
        with self.assertRaises(ModelError):
            ModularInteger('X', 255)

    def test_range_size(self) -> None:
        self.assertEqual(RangeInteger('UINT32', 0, 2**32 - 1, 32).size(), Number(32))

    def test_range_invalid_first(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', -1, 0, 1)

    def test_range_invalid_range(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', 1, 0, 1)

    def test_range_invalid_size(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', 0, 256, 8)

    def test_array_invalid_call(self) -> None:
        with self.assertRaises(RuntimeError):
            Array('X').size()

    def test_pdu_fields_invalid_cyclic(self) -> None:
        t = ModularInteger('T', 2)

        n1 = Node('X', t)
        n2 = Node('Y', t)

        n1.edges = [Edge(n2, TRUE, Number(1))]
        n2.edges = [Edge(n1, TRUE, Number(1))]

        with self.assertRaises(ModelError):
            PDU('Z', n1).fields()

    def test_pdu_fields_invalid_dupe(self) -> None:
        t1 = ModularInteger('T1', 2)
        t2 = ModularInteger('T2', 4)

        n1 = Node('X', t1)
        n2 = Node('X', t2)

        n1.edges = [Edge(n2, TRUE)]
        n2.edges = [Edge(FINAL, TRUE)]

        with self.assertRaises(ModelError):
            PDU('Z', n1).fields()

    def test_pdu_fields_ethernet(self) -> None:
        expected: List[Field] = [
            Field('Destination',
                  ModularInteger('UINT48', 2**48),
                  [
                      (TRUE,
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47)
                       })
                  ]),
            Field('Source',
                  ModularInteger('UINT48', 2**48),
                  [
                      (TRUE,
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95)
                       })
                  ]),
            Field('TPID',
                  RangeInteger('UINT16', 0, 2**16 - 1, 16),
                  [
                      (Or(NotEqual(Value('TPID'), Number(0x8100)),
                          Equal(Value('TPID'), Number(0x8100))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111)
                      })
                  ]),
            Field('TCI',
                  RangeInteger('UINT16', 0, 2**16 - 1, 16),
                  [
                      (Equal(Value('TPID'), Number(0x8100)),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('TCI'): Number(112),
                           Last('TCI'): Number(127)
                      })
                  ]),
            Field('EtherType',
                  RangeInteger('UINT16', 0, 2**16 - 1, 16),
                  [
                      (And(Equal(Value('TPID'), Number(0x8100)),
                           Or(GreaterEqual(Value('EtherType'), Number(1536)),
                              LessEqual(Value('EtherType'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('TCI'): Number(112),
                           Last('TCI'): Number(127),
                           First('EtherType'): Number(128),
                           Last('EtherType'): Number(143)
                      }),
                      (And(NotEqual(Value('TPID'), Number(0x8100)),
                           Or(GreaterEqual(Value('EtherType'), Number(1536)),
                              LessEqual(Value('EtherType'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('EtherType'): Number(96),
                           Last('EtherType'): Number(111)
                      })
                  ]),
            Field('Payload',
                  Array('Payload_Array'),
                  [
                      (And(And(Equal(Value('TPID'), Number(0x8100)),
                               LessEqual(Value('EtherType'), Number(1500))),
                           And(GreaterEqual(Length('Payload'), Number(46)),
                               LessEqual(Length('Payload'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('TCI'): Number(112),
                           Last('TCI'): Number(127),
                           First('EtherType'): Number(128),
                           Last('EtherType'): Number(143),
                           First('Payload'): Number(144),
                           Last('Payload'): Add(Value('EtherType'), Number(143))
                      }),
                      (And(And(Equal(Value('TPID'), Number(0x8100)),
                               GreaterEqual(Value('EtherType'), Number(1536))),
                           And(GreaterEqual(Length('Payload'), Number(46)),
                               LessEqual(Length('Payload'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('TCI'): Number(112),
                           Last('TCI'): Number(127),
                           First('EtherType'): Number(128),
                           Last('EtherType'): Number(143),
                           First('Payload'): Number(144),
                           Last('Payload'): Last('Buffer')
                      }),
                      (And(And(NotEqual(Value('TPID'), Number(0x8100)),
                               LessEqual(Value('EtherType'), Number(1500))),
                           And(GreaterEqual(Length('Payload'), Number(46)),
                               LessEqual(Length('Payload'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('EtherType'): Number(96),
                           Last('EtherType'): Number(111),
                           First('Payload'): Number(112),
                           Last('Payload'): Add(Value('EtherType'), Number(111))
                      }),
                      (And(And(NotEqual(Value('TPID'), Number(0x8100)),
                               GreaterEqual(Value('EtherType'), Number(1536))),
                           And(GreaterEqual(Length('Payload'), Number(46)),
                               LessEqual(Length('Payload'), Number(1500)))),
                       {
                           First('Destination'): Number(0),
                           Last('Destination'): Number(47),
                           First('Source'): Number(48),
                           Last('Source'): Number(95),
                           First('TPID'): Number(96),
                           Last('TPID'): Number(111),
                           First('EtherType'): Number(96),
                           Last('EtherType'): Number(111),
                           First('Payload'): Number(112),
                           Last('Payload'): Last('Buffer')
                      })
                  ]),
        ]

        self.assertEqual(ETHERNET_PDU.fields(), expected)


if __name__ == "__main__":
    unittest.main()
