import unittest

from rflx.expression import (TRUE, UNDEFINED, Add, And, Div, Equal, First, GreaterEqual, Last,
                             Length, LessEqual, Mul, NotEqual, Number, Or, Pow, Sub, Variable)
from rflx.model import (FINAL, INITIAL, Array, Enumeration, Field, Link, Message, ModelError,
                        ModularInteger, RangeInteger)
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
            ModularInteger('X', Pow(Number(2), Variable('X')))

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
            RangeInteger('X', Add(Number(1), Variable('X')), Number(15), Number(4))

    def test_range_invalid_last_variable(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(1), Add(Number(1), Variable('X')), Number(4))

    def test_range_invalid_first_negative(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(-1), Number(0), Number(1))

    def test_range_invalid_range(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(1), Number(0), Number(1))

    def test_range_invalid_size_variable(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(0), Number(256), Add(Number(8), Variable('X')))

    def test_range_invalid_size_too_small(self) -> None:
        with self.assertRaises(ModelError):
            RangeInteger('X', Number(0), Number(256), Number(8))

    def test_array_invalid_call(self) -> None:
        with self.assertRaises(ModelError):
            Array('X', ModularInteger('B', Number(256))).size  # pylint: disable=expression-not-assigned

    def test_message_missing_type(self) -> None:
        structure = [
            Link(INITIAL, Field('X')),
            Link(Field('X'), FINAL),
        ]

        with self.assertRaisesRegex(ModelError, '^missing type for field "X" of "M"$'):
            Message('M', structure, {})

    def test_message_superfluous_type(self) -> None:
        t = ModularInteger('T', Number(2))

        structure = [
            Link(INITIAL, Field('X')),
            Link(Field('X'), FINAL),
        ]

        types = {
            Field('X'): t,
            Field('Y'): t
        }

        with self.assertRaisesRegex(ModelError, '^superfluous field "Y" in field types of "M"$'):
            Message('M', structure, types)

    def test_message_ambiguous_first_field(self) -> None:
        t = ModularInteger('T', Number(2))

        structure = [
            Link(INITIAL, Field('X')),
            Link(INITIAL, Field('Y')),
            Link(Field('X'), Field('Z')),
            Link(Field('Y'), Field('Z')),
            Link(Field('Z'), FINAL)
        ]

        types = {
            Field('X'): t,
            Field('Y'): t,
            Field('Z'): t
        }

        with self.assertRaisesRegex(ModelError, '^ambiguous first field in "M"$'):
            Message('M', structure, types)

    def test_message_cycle(self) -> None:
        t = ModularInteger('T', Number(2))

        structure = [
            Link(INITIAL, Field('X')),
            Link(Field('X'), Field('Y')),
            Link(Field('Y'), Field('Z')),
            Link(Field('Z'), Field('X')),
            Link(Field('X'), FINAL)
        ]

        types = {
            Field('X'): t,
            Field('Y'): t,
            Field('Z'): t
        }

        with self.assertRaisesRegex(ModelError, '^structure of "M" contains cycle$'):
            Message('M', structure, types)

    def test_message_fields(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.fields,
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('TPID'),
             Field('TCI'),
             Field('Type_Length'),
             Field('Payload')))

    def test_message_definite_fields(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_fields,
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('Type_Length'),
             Field('Payload')))

    def test_message_field_condition(self) -> None:
        self.assertEqual(
            ETHERNET_FRAME.field_condition(INITIAL),
            TRUE)
        self.assertEqual(
            ETHERNET_FRAME.field_condition(Field('TPID')),
            Equal(
                Variable('Type_Length_TPID'),
                Number(33024)))
        self.assertEqual(
            ETHERNET_FRAME.field_condition(Field('Type_Length')),
            Or(
                NotEqual(
                    Variable('Type_Length_TPID'),
                    Number(33024)),
                Equal(
                    Variable('Type_Length_TPID'),
                    Number(33024))))
        self.assertEqual(
            ETHERNET_FRAME.field_condition(Field('Payload')),
            Or(
                And(
                    Or(
                        NotEqual(
                            Variable('Type_Length_TPID'),
                            Number(33024)),
                        Equal(
                            Variable('Type_Length_TPID'),
                            Number(33024))),
                    LessEqual(
                        Variable('Type_Length'),
                        Number(1500))),
                And(
                    Or(
                        NotEqual(
                            Variable('Type_Length_TPID'),
                            Number(33024)),
                        Equal(
                            Variable('Type_Length_TPID'),
                            Number(33024))),
                    GreaterEqual(
                        Variable('Type_Length'),
                        Number(1536)))))

    def test_message_incoming(self) -> None:
        self.assertEqual(
            ETHERNET_FRAME.incoming(INITIAL),
            [])
        self.assertEqual(
            ETHERNET_FRAME.incoming(Field('Type_Length')),
            [Link(Field('Type_Length_TPID'), Field('Type_Length'),
                  NotEqual(Variable('Type_Length_TPID'), Number(0x8100)),
                  first=First('Type_Length_TPID')),
             Link(Field('TCI'), Field('Type_Length'))])
        self.assertEqual(
            ETHERNET_FRAME.incoming(FINAL),
            [Link(Field('Payload'), FINAL,
                  And(GreaterEqual(Div(Length('Payload'), Number(8)), Number(46)),
                  LessEqual(Div(Length('Payload'), Number(8)), Number(1500))))])

    def test_message_outgoing(self) -> None:
        self.assertEqual(
            ETHERNET_FRAME.outgoing(INITIAL),
            [Link(INITIAL, Field('Destination'))])
        self.assertEqual(
            ETHERNET_FRAME.outgoing(Field('Type_Length')),
            [Link(Field('Type_Length'), Field('Payload'),
                  LessEqual(Variable('Type_Length'), Number(1500)),
                  Mul(Variable('Type_Length'), Number(8))),
             Link(Field('Type_Length'), Field('Payload'),
                  GreaterEqual(Variable('Type_Length'), Number(1536)),
                  Sub(Last('Message'), Last('Type_Length')))])
        self.assertEqual(
            ETHERNET_FRAME.outgoing(FINAL),
            [])

    def test_message_direct_predecessors(self) -> None:
        self.assertEqual(
            ETHERNET_FRAME.direct_predecessors(INITIAL),
            [])
        self.assertEqual(
            ETHERNET_FRAME.direct_predecessors(Field('Type_Length')),
            [Field('Type_Length_TPID'), Field('TCI')])
        self.assertEqual(
            ETHERNET_FRAME.direct_predecessors(FINAL),
            [Field('Payload')])

    def test_message_direct_successors(self) -> None:
        self.assertEqual(
            ETHERNET_FRAME.direct_successors(INITIAL),
            [Field('Destination')])
        self.assertEqual(
            ETHERNET_FRAME.direct_successors(Field('Type_Length')),
            [Field('Payload')])
        self.assertEqual(
            ETHERNET_FRAME.direct_successors(FINAL),
            [])

    def test_message_definite_predecessors(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_predecessors(FINAL),
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('Type_Length'),
             Field('Payload')))
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_predecessors(Field('TCI')),
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('TPID')))

    def test_message_predecessors(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.predecessors(FINAL),
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('TPID'),
             Field('TCI'),
             Field('Type_Length'),
             Field('Payload')))
        self.assertTupleEqual(
            ETHERNET_FRAME.predecessors(Field('TCI')),
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('TPID')))
        self.assertTupleEqual(
            ETHERNET_FRAME.predecessors(Field('Destination')),
            ())
        self.assertTupleEqual(
            ETHERNET_FRAME.predecessors(INITIAL),
            ())

    def test_message_successors(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(INITIAL),
            (Field('Destination'),
             Field('Source'),
             Field('Type_Length_TPID'),
             Field('TPID'),
             Field('TCI'),
             Field('Type_Length'),
             Field('Payload')))
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(Field('Source')),
            (Field('Type_Length_TPID'),
             Field('TPID'),
             Field('TCI'),
             Field('Type_Length'),
             Field('Payload')))
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(Field('TPID')),
            (Field('TCI'),
             Field('Type_Length'),
             Field('Payload')))
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(Field('Payload')),
            ())
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(FINAL),
            ())

    def disabled_test_nonexistent_variable(self) -> None:
        foo_type = ModularInteger('Foo', Pow(Number(2), Number(32)))
        enum_type = Enumeration('Bar', {'Val1': Number(0), 'Val2': Number(1)}, Number(8), True)
        structure = [
            Link(INITIAL, Field('F1')),
            Link(Field('F1'), Field('F2'),
                 Equal(Variable('F1'), Variable('Val3'))),
            Link(Field('F2'), FINAL)]

        types = {
            Field('F1'): enum_type,
            Field('F2'): foo_type
        }
        with self.assertRaisesRegex(ModelError, 'undefined variables (Val3)'):
            Message('X', structure, types)
