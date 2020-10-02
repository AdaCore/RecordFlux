# pylint: disable=too-many-lines

from itertools import zip_longest
from pathlib import Path
from typing import Any, Dict, Sequence

import pytest

import rflx.declaration as decl
import rflx.expression as expr
import rflx.statement as stmt
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import (
    UNDEFINED,
    Add,
    And,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    Last,
    Length,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Pow,
    Sub,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
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
    Opaque,
    Private,
    RangeInteger,
    Refinement,
    State,
    Transition,
)
from rflx.parser import grammar, parser
from rflx.parser.ast import (
    ArraySpec,
    ContextSpec,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    ReferenceSpec,
    RefinementSpec,
    SessionSpec,
    Specification,
    Then,
)
from rflx.parser.parser import Component, ParseFatalException, Parser
from tests.models import ETHERNET_FRAME
from tests.utils import assert_equal

TESTDIR = "tests"
SPECDIR = "specs"


def assert_specifications_files(
    filenames: Sequence[str], specifications: Dict[str, Specification]
) -> None:
    p = Parser()
    for filename in filenames:
        p.parse(Path(filename))
    assert p.specifications == specifications, filenames


def assert_specifications_string(string: str, specifications: Dict[str, Specification]) -> None:
    p = Parser()
    p.parse_string(string)
    assert p.specifications == specifications


def assert_messages_files(filenames: Sequence[str], messages: Sequence[Message]) -> None:
    p = Parser()
    for filename in filenames:
        p.parse(Path(filename))
    model = p.create_model()
    assert_messages(model.messages, messages)


def assert_messages_string(string: str, messages: Sequence[Message]) -> None:
    p = Parser()
    p.parse_string(string)
    model = p.create_model()
    assert_messages(model.messages, messages)


def assert_messages(
    actual_messages: Sequence[Message], expected_messages: Sequence[Message]
) -> None:
    for actual, expected in zip_longest(actual_messages, expected_messages):
        assert actual.full_name == expected.full_name
        assert actual.structure == expected.structure, expected.full_name
        assert actual.types == expected.types, expected.full_name
        assert actual.fields == expected.fields, expected.full_name
    assert actual_messages == expected_messages


def assert_refinements_string(string: str, refinements: Sequence[Refinement]) -> None:
    p = Parser()
    p.parse_string(string)
    model = p.create_model()
    assert model.refinements == refinements


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        p = Parser()
        for filename in filenames:
            p.parse(Path(filename))
        p.create_model()


def assert_error_string(string: str, regex: str) -> None:
    p = Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse_string(string)
        p.create_model()


def raise_parser_error() -> None:
    fail("TEST", Subsystem.PARSER, Severity.ERROR)


@pytest.mark.parametrize(
    "string,expected",
    [("X", ID("X")), ("X2", ID("X2")), ("X_Y", ID("X_Y")), ("X_Y_3", ID("X_Y_3"))],
)
def test_grammar_unqualified_identifier(string: str, expected: ID) -> None:
    actual = grammar.unqualified_identifier().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X", ID("X")),
        ("X2", ID("X2")),
        ("X_Y", ID("X_Y")),
        ("X_Y_3", ID("X_Y_3")),
        ("X::Y", ID("X::Y")),
        ("X2::Y2", ID("X2::Y2")),
        ("X_Y::Z", ID("X_Y::Z")),
        ("X_Y_3::Z_4", ID("X_Y_3::Z_4")),
        ("X::Y::Z", ID("X::Y::Z")),
    ],
)
def test_grammar_qualified_identifier(string: str, expected: ID) -> None:
    actual = grammar.qualified_identifier().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("1000", expr.Number(1000)),
        ("1_000", expr.Number(1000)),
        ("16#6664#", expr.Number(26212)),
        ("16#66_64#", expr.Number(26212)),
        ("-1000", expr.Number(-1000)),
        ("-1_000", expr.Number(-1000)),
        ("-16#6664#", expr.Number(-26212)),
        ("-16#66_64#", expr.Number(-26212)),
    ],
)
def test_grammar_expression_numeric_literal(string: str, expected: Expr) -> None:
    actual = grammar.numeric_literal().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected", [("X", expr.Variable("X")), ("X::Y", expr.Variable("X::Y"))]
)
def test_grammar_variable(string: str, expected: decl.Declaration) -> None:
    actual = grammar.variable().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X'First", expr.First(expr.Variable("X"))),
        ("X'Last", expr.Last(expr.Variable("X"))),
        ("X'Length", expr.Length(expr.Variable("X"))),
        ("X'Head", expr.Head(expr.Variable("X"))),
        ("X'Opaque", expr.Opaque(expr.Variable("X"))),
        ("X'Present", expr.Present(expr.Variable("X"))),
        ("X'Valid", expr.Valid(expr.Variable("X"))),
        ("X'Valid_Checksum", expr.ValidChecksum(expr.Variable("X"))),
        ("X where X = 42", expr.Binding(expr.Variable("X"), {ID("X"): expr.Number(42)})),
        ("X'Head.Y", expr.Selected(expr.Head(expr.Variable("X")), "Y")),
    ],
)
def test_grammar_expression_suffix(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A - B * 2**3 - 1",
            Sub(Sub(Variable("A"), Mul(Variable("B"), Pow(Number(2), Number(3)))), Number(1)),
        ),
        (
            "(A - B) * 2**3 - 1",
            Sub(Mul(Sub(Variable("A"), Variable("B")), Pow(Number(2), Number(3))), Number(1)),
        ),
        (
            "A - B * 2**(3 - 1)",
            Sub(Variable("A"), Mul(Variable("B"), Pow(Number(2), Sub(Number(3), Number(1))))),
        ),
        (
            "A - (B * 2)**3 - 1",
            Sub(Sub(Variable("A"), Pow(Mul(Variable("B"), Number(2)), Number(3))), Number(1)),
        ),
        (
            "A - (B * 2**3 - 1)",
            Sub(Variable("A"), Sub(Mul(Variable("B"), Pow(Number(2), Number(3))), Number(1))),
        ),
        (
            "A + B * (-8)",
            Add(Variable("A"), Mul(Variable("B"), Number(-8))),
        ),
    ],
)
def test_grammar_expression_mathematical(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X = Y", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
        ("X /= Y", expr.NotEqual(expr.Variable("X"), expr.Variable("Y"))),
        ("42 < X", expr.Less(expr.Number(42), expr.Variable("X"))),
        ("42 <= X", expr.LessEqual(expr.Number(42), expr.Variable("X"))),
        ("X > 42", expr.Greater(expr.Variable("X"), expr.Number(42))),
        ("X >= 42", expr.GreaterEqual(expr.Variable("X"), expr.Number(42))),
        ("X in Y", expr.In(expr.Variable("X"), expr.Variable("Y"))),
        ("X not in Y", expr.NotIn(expr.Variable("X"), expr.Variable("Y"))),
        ("((X = 42))", expr.Equal(expr.Variable("X"), expr.Number(42))),
    ],
)
def test_grammar_expression_relation(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X or Y", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
        ("((X or Y))", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_grammar_expression_boolean(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X + Y", expr.Add(expr.Variable("X"), expr.Variable("Y"))),
        ("X + Y (Z)", expr.Add(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_grammar_mathematical_expression(string: str, expected: Expr) -> None:
    actual = grammar.mathematical_expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42 > X", 'unexpected expression type "Greater".*'),
        ("X and Y", 'unexpected expression type "And".*'),
    ],
)
def test_grammar_mathematical_expression_error(string: str, error: Expr) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.mathematical_expression().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X and Y (Z)", expr.And(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_grammar_boolean_expression(string: str, expected: Expr) -> None:
    actual = grammar.boolean_expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42", 'unexpected expression type "Number".*'),
        ("X", 'unexpected expression type "Variable".*'),
    ],
)
def test_grammar_boolean_expression_error(string: str, error: Expr) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.boolean_expression().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("42", expr.Number(42)),
        ('"Foo Bar"', expr.String("Foo Bar")),
        ("[1]", expr.Aggregate(expr.Number(1))),
        ("[1, 2]", expr.Aggregate(expr.Number(1), expr.Number(2))),
        (
            '[137] & "PNG" & [13, 10, 26, 10]',
            expr.Aggregate(
                Number(137),
                Number(80),
                Number(78),
                Number(71),
                Number(13),
                Number(10),
                Number(26),
                Number(10),
            ),
        ),
        (
            "for all X in Y => X = Z",
            expr.ForAllIn(
                "X", expr.Variable("Y"), expr.Equal(expr.Variable("X"), expr.Variable("Z"))
            ),
        ),
        (
            "for some X in Y => X = Z",
            expr.ForSomeIn(
                "X", expr.Variable("Y"), expr.Equal(expr.Variable("X"), expr.Variable("Z"))
            ),
        ),
        (
            "[for X in Y => X.A]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.TRUE,
            ),
        ),
        (
            "[for X in Y => X.A when X.B = Z]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.Equal(expr.Selected(expr.Variable("X"), "B"), expr.Variable("Z")),
            ),
        ),
        (
            'X (A, "S", 42)',
            expr.Call("X", [expr.Variable("A"), expr.String("S"), expr.Number(42)]),
        ),
        ("X::Y (A)", expr.Conversion("X::Y", expr.Variable("A"))),
        ("X'(Y => Z)", expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z")})),
        (
            "X'(Y => Z, A => B)",
            expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z"), ID("A"): expr.Variable("B")}),
        ),
        ("X'(null message)", expr.MessageAggregate("X", {})),
        ("X", expr.Variable("X")),
    ],
)
def test_grammar_expression_base(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X'Valid = True", expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True"))),
        ("X::Y /= Z", expr.NotEqual(expr.Variable("X::Y"), expr.Variable("Z"))),
        ("X.Y /= Z", expr.NotEqual(expr.Selected(expr.Variable("X"), "Y"), expr.Variable("Z"))),
        (
            "X = Y and Y /= Z",
            expr.And(
                expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                expr.NotEqual(expr.Variable("Y"), expr.Variable("Z")),
            ),
        ),
        (
            "X'Valid and Y'Valid",
            expr.And(expr.Valid(expr.Variable("X")), expr.Valid(expr.Variable("Y"))),
        ),
        (
            "X'Valid = True and (Y'Valid = False or Z'Valid = False)",
            expr.And(
                expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True")),
                expr.Or(
                    expr.Equal(expr.Valid(expr.Variable("Y")), expr.Variable("False")),
                    expr.Equal(expr.Valid(expr.Variable("Z")), expr.Variable("False")),
                ),
            ),
        ),
        (
            "X'Valid = False or X.A /= A or X.B /= B or (X.C = 0 and X.D not in X.E)",
            expr.Or(
                expr.Or(
                    expr.Or(
                        expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("False")),
                        expr.NotEqual(expr.Selected(expr.Variable("X"), "A"), expr.Variable("A")),
                    ),
                    expr.NotEqual(expr.Selected(expr.Variable("X"), "B"), expr.Variable("B")),
                ),
                And(
                    expr.Equal(expr.Selected(expr.Variable("X"), "C"), expr.Number(0)),
                    expr.NotIn(
                        expr.Selected(expr.Variable("X"), "D"),
                        expr.Selected(expr.Variable("X"), "E"),
                    ),
                ),
            ),
        ),
        (
            "for some A in X.B => (A.T = P.E and (G::E not in P::S (A.D).V))",
            expr.ForSomeIn(
                "A",
                expr.Selected(expr.Variable("X"), "B"),
                And(
                    expr.Equal(
                        expr.Selected(expr.Variable("A"), "T"),
                        expr.Selected(expr.Variable("P"), "E"),
                    ),
                    expr.NotIn(
                        expr.Variable("G::E"),
                        expr.Selected(
                            expr.Conversion("P::S", expr.Selected(expr.Variable("A"), "D")),
                            "V",
                        ),
                    ),
                ),
            ),
        ),
        ("X::Y (Z) = 42", expr.Equal(expr.Conversion("X::Y", expr.Variable("Z")), expr.Number(42))),
        ("X (Y).Z", expr.Selected(expr.Call("X", [expr.Variable("Y")]), "Z")),
        ("X (Y).Z'Length", Length(expr.Selected(expr.Call("X", [expr.Variable("Y")]), "Z"))),
        (
            "G::E not in P::S (E.D).V",
            expr.NotIn(
                expr.Variable("G::E"),
                expr.Selected(expr.Conversion("P::S", expr.Selected(expr.Variable("E"), "D")), "V"),
            ),
        ),
        (
            "[for E in L => E.B when E.T = A]'Head",
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Variable("L"),
                    expr.Selected(expr.Variable("E"), "B"),
                    expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                )
            ),
        ),
        ("A'Head.D", expr.Selected(expr.Head(expr.Variable("A")), "D")),
        (
            "[for E in L => E.B when E.T = A]'Head.D",
            expr.Selected(
                expr.Head(
                    expr.Comprehension(
                        "E",
                        expr.Variable("L"),
                        expr.Selected(expr.Variable("E"), "B"),
                        expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                    )
                ),
                "D",
            ),
        ),
        (
            "(for some S in P::X ([for E in C.A => E when E.T = P.L]'Head.D).H => S.G = G) = False",
            expr.Equal(
                expr.ForSomeIn(
                    "S",
                    expr.Selected(
                        expr.Conversion(
                            "P::X",
                            expr.Selected(
                                expr.Head(
                                    expr.Comprehension(
                                        "E",
                                        expr.Selected(expr.Variable("C"), "A"),
                                        expr.Variable("E"),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("E"), "T"),
                                            expr.Selected(expr.Variable("P"), "L"),
                                        ),
                                    )
                                ),
                                "D",
                            ),
                        ),
                        "H",
                    ),
                    expr.Equal(expr.Selected(expr.Variable("S"), "G"), expr.Variable("G")),
                ),
                expr.Variable("False"),
            ),
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2)",
            expr.Binding(
                expr.MessageAggregate("M1", {ID("D"): expr.Variable("B1")}),
                {ID("B1"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")})},
            ),
        ),
        (
            "M1'(D1 => B1, D2 => B2) where B1 = M2'(D => B2), B2 = M2'(D => B3)",
            expr.Binding(
                expr.MessageAggregate(
                    "M1", {ID("D1"): expr.Variable("B1"), ID("D2"): expr.Variable("B2")}
                ),
                {
                    ID("B1"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")}),
                    ID("B2"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B3")}),
                },
            ),
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2) where B2 = M3'(D => B3)",
            expr.Binding(
                expr.MessageAggregate("M1", {ID("D"): expr.Variable("B1")}),
                {
                    ID("B1"): expr.Binding(
                        expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")}),
                        {ID("B2"): expr.MessageAggregate("M3", {ID("D"): expr.Variable("B3")})},
                    )
                },
            ),
        ),
    ],
)
def test_grammar_expression_complex(string: str, expected: Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


def test_grammar_private_type_declaration() -> None:
    string = "type X is private"
    expected = decl.TypeDeclaration(Private("X", location=Location((1, 1), None, (1, 17))))
    actual = grammar.private_type_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X : Channel with Readable", decl.ChannelDeclaration("X", readable=True)),
        ("X : Channel with Writable", decl.ChannelDeclaration("X", writable=True)),
        (
            "X : Channel with Readable, Writable",
            decl.ChannelDeclaration("X", readable=True, writable=True),
        ),
    ],
)
def test_grammar_channel_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.channel_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("with function X return Y", decl.FunctionDeclaration("X", [], "Y")),
        (
            "with function X (A : B; C : D) return Y",
            decl.FunctionDeclaration("X", [decl.Argument("A", "B"), decl.Argument("C", "D")], "Y"),
        ),
    ],
)
def test_grammar_formal_function_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.formal_function_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A : B", decl.VariableDeclaration("A", "B")),
        ("A : B := C", decl.VariableDeclaration("A", "B", expr.Variable("C"))),
        ("A : B := 1", decl.VariableDeclaration("A", "B", Number(1))),
    ],
)
def test_grammar_variable_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.variable_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A : B renames C.D",
            decl.RenamingDeclaration("A", "B", expr.Selected(expr.Variable("C"), "D")),
        ),
    ],
)
def test_grammar_renaming_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.renaming_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [("A := B", stmt.Assignment("A", expr.Variable("B")))],
)
def test_grammar_assignment_statement(string: str, expected: stmt.Statement) -> None:
    actual = grammar.assignment_statement().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A'Append (B)", stmt.Append("A", expr.Variable("B"))),
        ("A'Extend (B)", stmt.Extend("A", expr.Variable("B"))),
        ("C'Reset", stmt.Reset("C")),
    ],
)
def test_grammar_attribute_statement(string: str, expected: stmt.Statement) -> None:
    actual = grammar.attribute_statement().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            """
               state A is
               begin
               transition
                  then B
               end A
         """,
            State("A", transitions=[Transition("B")]),
        ),
        (
            """
               state A is
               begin
               transition
                  then B
                     if X = Y
                  then C
               end A
         """,
            State(
                "A",
                transitions=[Transition("B", Equal(Variable("X"), Variable("Y"))), Transition("C")],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  then B
                     with Desc => "rfc2549.txt+12:3-45:6"
                     if X = Y
                  then C
                     with Desc => "rfc2549.txt+123:45-678:9"
               end A
         """,
            State(
                "A",
                transitions=[
                    Transition(
                        "B",
                        Equal(Variable("X"), Variable("Y")),
                        description="rfc2549.txt+12:3-45:6",
                    ),
                    Transition("C", description="rfc2549.txt+123:45-678:9"),
                ],
            ),
        ),
        (
            """
               state A is
                  Z : Boolean := Y;
               begin
               transition
                  then B
               end A
         """,
            State(
                "A",
                transitions=[Transition("B")],
                declarations=[decl.VariableDeclaration("Z", "Boolean", Variable("Y"))],
                actions=[],
            ),
        ),
    ],
)
def test_grammar_state(string: str, expected: decl.Declaration) -> None:
    actual = grammar.state().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        (
            """
               state A is
               begin
               transition
                  then B
               end C
         """,
            "inconsistent state identifier: A /= C.*",
        )
    ],
)
def test_grammar_state_error(string: str, error: str) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.state().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            """
               generic
                  X : Channel with Readable, Writable;
                  type T is private;
                  with function F return T;
               session Session with
                  Initial => A,
                  Final => B
               is
                  Y : Boolean := 0;
               begin
                  state A is
                     Z : Boolean := Y;
                  begin
                     Z := 1;
                  transition
                     then B
                        if Z = 1
                     then A
                  end A;

                  state B is null state;
               end Session;
         """,
            SessionSpec(
                ID("Session"),
                ID("A"),
                ID("B"),
                [
                    State(
                        "A",
                        declarations=[decl.VariableDeclaration("Z", "Boolean", Variable("Y"))],
                        actions=[stmt.Assignment("Z", Number(1))],
                        transitions=[
                            Transition("B", condition=Equal(Variable("Z"), Number(1))),
                            Transition("A"),
                        ],
                    ),
                    State("B"),
                ],
                [decl.VariableDeclaration("Y", "Boolean", Number(0))],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.TypeDeclaration(Private("T")),
                    decl.FunctionDeclaration("F", [], "T"),
                ],
                Location((2, 16), None, (23, 27)),
            ),
        ),
    ],
)
def test_grammar_session_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.session_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        (
            """
               generic
               session X with
                  Initial => A,
                  Final => A
               is
               begin
                  state A is null state;
               end Y;
         """,
            "inconsistent session identifier: X /= Y.*",
        )
    ],
)
def test_grammar_session_declaration_error(string: str, error: str) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.session_declaration().parseString(string, parseAll=True)


def test_session() -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is

               generic
               session Session with
                  Initial => A,
                  Final => C
               is
               begin
                  state A is
                  begin
                  transition
                     then B
                  end A;

                  state B is
                  begin
                  transition
                     then C
                  end B;

                  state C is null state;
               end Session;

            end Test;
        """
    )
    p.create_model()


def test_grammar_unexpected_exception(monkeypatch: Any) -> None:
    with pytest.raises(
        ParseFatalException,
        match=r"ZeroDivisionError",
    ):
        monkeypatch.setattr(
            grammar,
            "parse_mathematical_operator",
            grammar.fatalexceptions(lambda x, y, z: [1, 1 / 0, 1]),
        )
        grammar.expression().parseString("1 + 1")


def test_grammar_expression_aggregate_no_number() -> None:
    with pytest.raises(ParseFatalException, match=r"^Expected Number"):
        grammar.expression().parseString("[1, Foo]")


def test_grammar_unexpected_suffix() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected suffix .*$"):
        grammar.parse_suffix(
            "",
            0,
            [[Variable(ID("X")), ("X", None)]],
        )


def test_grammar_unexpected_relation_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_relational_operator(
            "",
            0,
            [[Number(1, location=Location((1, 1))), "<>", Number(1, location=Location((1, 1)))]],
        )


def test_grammar_unexpected_boolean_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_boolean_operator(
            "",
            0,
            [[Number(1, location=Location((1, 8))), "xor", Number(1, location=Location((2, 25)))]],
        )


def test_grammar_unexpected_mathematical_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_mathematical_operator(
            "",
            0,
            [[Number(1, location=Location((1, 1))), "//", Number(1, location=Location((1, 8)))]],
        )


def test_grammar_unexpected_quantified_expression(monkeypatch: Any) -> None:
    monkeypatch.setattr(grammar, "evaluate_located_expression", lambda s, t: (t, Location((1, 1))))
    with pytest.raises(ParseFatalException, match=r"^unexpected quantified expression"):
        grammar.parse_quantified_expression(
            "",
            0,
            [[0, "any", ID("X"), Variable("Y"), Equal(Variable("X"), Variable("Z")), 24]],
        )


def test_grammar_unexpected_type() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected type"):
        grammar.parse_type("type T is X;", 0, [0, "type", "T", "is", "X", 8])


def test_illegal_package_identifiers() -> None:
    assert_error_string(
        """
            package RFLX_Types is
            end RFLX_Types;
        """,
        r'^<stdin>:2:21: parser: error: illegal prefix "RFLX" in package identifier "RFLX_Types"',
    )


def test_inconsistent_package_identifiers() -> None:
    assert_error_string(
        """
            package A is
            end B;
        """,
        r'^<stdin>:3:17: parser: error: inconsistent package identifier "B"\n'
        r'<stdin>:2:21: parser: info: previous identifier was "A"',
    )


def test_empty_file_spec() -> None:
    assert_specifications_files([f"{TESTDIR}/empty_file.rflx"], {})


def test_empty_file_message() -> None:
    assert_messages_files([f"{TESTDIR}/empty_file.rflx"], [])


def test_comment_only_spec() -> None:
    assert_specifications_files([f"{TESTDIR}/comment_only.rflx"], {})


def test_comment_only_message() -> None:
    assert_messages_files([f"{TESTDIR}/comment_only.rflx"], [])


def test_incorrect_name() -> None:
    assert_error_files(
        [f"{TESTDIR}/incorrect_name.rflx"],
        f"^{TESTDIR}/incorrect_name.rflx:1:9: parser: error: file name does not match unit name"
        r' "Test", should be "test.rflx"$',
    )


def test_incorrect_specification() -> None:
    assert_error_files(
        [f"{TESTDIR}/incorrect_specification.rflx"],
        f'{TESTDIR}/incorrect_specification.rflx:3:10: parser: error: Expected "is"',
    )


def test_unexpected_exception_in_parser(monkeypatch: Any) -> None:
    p = Parser()
    with pytest.raises(RecordFluxError, match=r"parser: error: TEST"):
        monkeypatch.setattr(parser, "check_naming", lambda x, e: raise_parser_error())
        p.parse_string(
            """
                package Test is
                   type T is mod 256;
                end Test;
            """
        )
        p.create_model()


def test_package_spec() -> None:
    assert_specifications_files(
        [f"{TESTDIR}/empty_package.rflx"],
        {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", [], []))},
    )


def test_package_message() -> None:
    assert_messages_files([f"{TESTDIR}/empty_package.rflx"], [])


def test_duplicate_specifications() -> None:
    files = [f"{TESTDIR}/empty_package.rflx", f"{TESTDIR}/empty_package.rflx"]
    assert_specifications_files(
        files,
        {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", [], []))},
    )
    assert_messages_files(files, [])


def test_context_spec() -> None:
    assert_specifications_files(
        [f"{TESTDIR}/context.rflx"],
        {
            "Context": Specification(
                ContextSpec(["Empty_File", "Empty_Package"]),
                PackageSpec("Context", [], []),
            ),
            "Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", [], [])),
        },
    )


def test_context_message() -> None:
    assert_messages_files([f"{TESTDIR}/context.rflx"], [])


def test_context_dependency_cycle() -> None:
    assert_error_files(
        [f"{TESTDIR}/context_cycle.rflx"],
        f"^"
        f"{TESTDIR}/context_cycle.rflx:1:6: parser: error: dependency cycle when "
        f'including "Context_Cycle_1"\n'
        f'{TESTDIR}/context_cycle_1.rflx:1:6: parser: info: when including "Context_Cycle_2"\n'
        f'{TESTDIR}/context_cycle_2.rflx:1:6: parser: info: when including "Context_Cycle_3"\n'
        f'{TESTDIR}/context_cycle_3.rflx:1:6: parser: info: when including "Context_Cycle_1"'
        f"$",
    )


def test_duplicate_type() -> None:
    assert_error_files(
        [f"{TESTDIR}/duplicate_type.rflx"],
        f'{TESTDIR}/duplicate_type.rflx:3:4: parser: error: duplicate type "Duplicate_Type::T"\n'
        f"{TESTDIR}/duplicate_type.rflx:2:4: parser: info:"
        f' previous occurrence of "Duplicate_Type::T"',
    )


def test_message_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:5:22: model: error: missing type for field "Foo" in "Test::PDU"$',
    )


def test_message_undefined_component() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T
                        then Bar;
                  end message;
            end Test;
        """,
        r'^<stdin>:7:30: parser: error: undefined field "Bar"$',
    )


def test_invalid_location_expression() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T
                        then Bar
                            with Foo => 1;
                    Bar : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:8:38: parser: error: Expected {{"First" - "=>" - MathematicalExpression}'
        r' | {"Length" - "=>" - MathematicalExpression}}$',
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """
            package Test is
               type Boolean is mod 2;
            end Test;
        """,
        r'^<stdin>:3:16: model: error: illegal redefinition of built-in type "Boolean"',
    )


def test_invalid_modular_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 2**128;
            end Test;
        """,
        r'^<stdin>:3:30: model: error: modulus of "T" exceeds limit \(2\*\*64\)',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Bar, Baz) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:16: model: error: size of "T" too small',
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Bar => 0) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:44: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Foo_1 => 1, Bar => 0, Bar_1 => 1) with Size => 8;
            end Test;
        """,
        r'<stdin>:3:56: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence\n"
        r'<stdin>:3:68: model: error: duplicate enumeration value "1" in "T"\n'
        r"<stdin>:3:46: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_identical_literals_location() -> None:
    assert_error_files(
        [f"{TESTDIR}/identical_literals.rflx"],
        f"{TESTDIR}/identical_literals.rflx:3:4: model: error: conflicting literals: Bar\n"
        f'{TESTDIR}/identical_literals.rflx:2:21: model: info: previous occurrence of "Bar"',
    )


def test_array_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is array of Foo;
            end Test;
        """,
        r'^<stdin>:3:35: parser: error: undefined element type "Test::Foo"$',
    )


def test_duplicate_message() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'parser: error: duplicate type "Test::PDU"',
    )


def test_duplicate_refinement() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     null
                        then Foo
                           with Length => 8;
                     Foo : Opaque;
                  end message;
               for Test::PDU use (Foo => Test::PDU);
               for PDU use (Foo => PDU);
            end Test;
        """,
        r'^<stdin>:11:16: parser: error: duplicate refinement with "Test::PDU"\n'
        r"<stdin>:10:16: parser: info: previous occurrence",
    )


def test_refinement_undefined_message() -> None:
    assert_error_string(
        """
            package Test is
               for PDU use (Foo => Bar);
            end Test;
        """,
        r'^<stdin>:3:16: parser: error: undefined type "Test::PDU" in refinement$',
    )


def test_refinement_undefined_sdu() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Foo => Bar);
            end Test;
        """,
        r'^<stdin>:8:36: parser: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
    )


def test_refinement_invalid_field() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Bar => PDU);
            end Test;
        """,
        r'^<stdin>:8:29: model: error: invalid field "Bar" in refinement',
    )


def test_refinement_invalid_condition() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     null
                        then Foo
                           with Length => 8;
                     Foo : Opaque;
                  end message;
               for PDU use (Foo => PDU)
                  if X < Y + 1;
            end Test;
        """,
        r"^"
        r'<stdin>:11:22: parser: error: unknown field or literal "X"'
        r' in refinement condition of "Test::PDU"\n'
        r'<stdin>:11:26: parser: error: unknown field or literal "Y"'
        r' in refinement condition of "Test::PDU"'
        r"$",
    )


def test_derivation_duplicate_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     Foo : T;
                  end message;
               type Bar is new Test::Foo;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:9:16: parser: error: duplicate type "Test::Bar"\n'
        r'<stdin>:8:16: parser: info: previous occurrence of "Test::Bar"',
    )


def test_derivation_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:3:16: parser: error: undefined base message "Test::Foo" in derived message$',
    )


def test_derivation_unsupported_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is mod 256;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:4:16: parser: error: illegal derivation "Test::Bar"\n'
        r'<stdin>:3:16: parser: info: invalid base message type "Test::Foo"',
    )


def test_derivation_of_derived_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is null message;
               type Bar is new Foo;
               type Baz is new Bar;
            end Test;
        """,
        r'^<stdin>:5:16: parser: error: illegal derivation "Test::Baz"\n'
        r'<stdin>:4:16: parser: info: invalid base message "Test::Bar"$',
    )


def test_invalid_first_in_initial_node() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     null
                        then Foo
                           with First => 0;
                     Foo : T;
                  end message;
            end Test;
        """,
        r"^<stdin>:8:42: parser: error: invalid first expression$",
    )


def test_multiple_initial_node_edges() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     null
                        then Foo,
                        then Bar;
                     Foo : T;
                     Bar : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:7:33: parser: error: Expected ";"',
    )


def test_multiple_initial_nodes() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     null
                        then Foo;
                     null
                        then Bar;
                     Foo : T;
                     Bar : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:8:22: parser: error: reserved word "null" used as identifier',
    )


def test_reserved_word_in_type_name() -> None:
    assert_error_string(
        """
            package Test is
               type Type is mod 256;
            end Test;
        """,
        r'^<stdin>:3:21: parser: error: reserved word "Type" used as identifier',
    )


def test_reserved_word_in_message_component() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Message : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:6:22: parser: error: Found unwanted token, "Message"',
    )


def test_session_name_conflict() -> None:
    assert_error_string(
        """
            package Test is
               type X is mod 2**8;

               generic
               session X with
                  Initial => A,
                  Final => A
               is
               begin
                  state A is null state;
               end X;
            end Test;
        """,
        r'^<stdin>:5:16: parser: error: name conflict for session "Test::X"\n'
        r'<stdin>:3:16: parser: info: previous occurrence of "Test::X"$',
    )


def test_integer_type_spec() -> None:
    spec = {
        "Integer_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Integer_Type",
                [
                    RangeInteger("__PACKAGE__::Page_Num", Number(1), Number(2000), Number(16)),
                    RangeInteger("__PACKAGE__::Line_Size", Number(0), Number(255), Number(8)),
                    ModularInteger("__PACKAGE__::Byte", Number(256)),
                    ModularInteger("__PACKAGE__::Hash_Index", Number(64)),
                ],
                [],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/integer_type.rflx"], spec)


def test_enumeration_type_spec() -> None:
    spec = {
        "Enumeration_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Enumeration_Type",
                [
                    Enumeration(
                        "__PACKAGE__::Day",
                        [
                            ("Mon", Number(1)),
                            ("Tue", Number(2)),
                            ("Wed", Number(3)),
                            ("Thu", Number(4)),
                            ("Fri", Number(5)),
                            ("Sat", Number(6)),
                            ("Sun", Number(7)),
                        ],
                        Number(3),
                        False,
                    ),
                    Enumeration(
                        "__PACKAGE__::Gender",
                        [("M", Number(0)), ("F", Number(1))],
                        Number(1),
                        False,
                    ),
                    Enumeration(
                        "__PACKAGE__::Priority",
                        [("LOW", Number(1)), ("MEDIUM", Number(4)), ("HIGH", Number(7))],
                        Number(8),
                        True,
                    ),
                ],
                [],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/enumeration_type.rflx"], spec)


def test_array_type_spec() -> None:
    spec = {
        "Array_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Array_Type",
                [
                    ModularInteger("__PACKAGE__::Byte", Number(256)),
                    ArraySpec("__PACKAGE__::Bytes", ReferenceSpec("__PACKAGE__::Byte")),
                    MessageSpec(
                        "__PACKAGE__::Foo",
                        [
                            Component(
                                "Length",
                                "Byte",
                                [Then("Bytes", UNDEFINED, Mul(Variable("Length"), Number(8)))],
                            ),
                            Component("Bytes", "Bytes"),
                        ],
                    ),
                    ArraySpec("__PACKAGE__::Bar", ReferenceSpec("__PACKAGE__::Foo")),
                ],
                [],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/array_type.rflx"], spec)


def test_message_type_spec() -> None:
    spec = {
        "Message_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Message_Type",
                [
                    ModularInteger("__PACKAGE__::T", Number(256)),
                    MessageSpec(
                        "__PACKAGE__::PDU",
                        [
                            Component(
                                "Foo",
                                "T",
                                [
                                    Then(
                                        "Bar",
                                        UNDEFINED,
                                        UNDEFINED,
                                        LessEqual(Variable("Foo"), Number(30, 16)),
                                    ),
                                    Then(
                                        "Baz",
                                        UNDEFINED,
                                        UNDEFINED,
                                        Greater(Variable("Foo"), Number(30, 16)),
                                    ),
                                ],
                            ),
                            Component("Bar", "T"),
                            Component("Baz", "T"),
                        ],
                    ),
                    MessageSpec(
                        "__PACKAGE__::Simple_PDU",
                        [Component("Bar", "T"), Component("Baz", "T")],
                    ),
                    MessageSpec("__PACKAGE__::Empty_PDU", []),
                ],
                [],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/message_type.rflx"], spec)


def test_message_type_message() -> None:
    simple_structure = [
        Link(INITIAL, Field("Bar")),
        Link(Field("Bar"), Field("Baz")),
        Link(Field("Baz"), FINAL),
    ]

    simple_types = {
        Field("Bar"): ModularInteger("Message_Type::T", Number(256)),
        Field("Baz"): ModularInteger("Message_Type::T", Number(256)),
    }

    simple_message = Message("Message_Type::Simple_PDU", simple_structure, simple_types)

    structure = [
        Link(INITIAL, Field("Foo")),
        Link(Field("Foo"), Field("Bar"), LessEqual(Variable("Foo"), Number(30, 16))),
        Link(Field("Foo"), Field("Baz"), Greater(Variable("Foo"), Number(30, 16))),
        Link(Field("Bar"), Field("Baz")),
        Link(Field("Baz"), FINAL),
    ]

    types = {
        **simple_types,
        **{Field("Foo"): ModularInteger("Message_Type::T", Number(256))},
    }

    message = Message("Message_Type::PDU", structure, types)

    empty_message = Message("Message_Type::Empty_PDU", [], {})

    assert_messages_files(
        [f"{TESTDIR}/message_type.rflx"], [message, simple_message, empty_message]
    )


def test_message_in_message() -> None:
    length = ModularInteger("Message_In_Message::Length", Pow(Number(2), Number(16)))

    length_value = Message(
        "Message_In_Message::Length_Value",
        [
            Link(INITIAL, Field("Length")),
            Link(Field("Length"), Field("Value"), length=Mul(Number(8), Variable("Length"))),
            Link(Field("Value"), FINAL),
        ],
        {Field("Length"): length, Field("Value"): Opaque()},
    )

    derived_length_value = DerivedMessage("Message_In_Message::Derived_Length_Value", length_value)

    message = Message(
        "Message_In_Message::Message",
        [
            Link(INITIAL, Field("Foo_Length")),
            Link(Field("Foo_Value"), Field("Bar_Length")),
            Link(Field("Bar_Value"), FINAL),
            Link(
                Field("Foo_Length"),
                Field("Foo_Value"),
                length=Mul(Variable("Foo_Length"), Number(8)),
            ),
            Link(
                Field("Bar_Length"),
                Field("Bar_Value"),
                length=Mul(Variable("Bar_Length"), Number(8)),
            ),
        ],
        {
            Field("Foo_Length"): length,
            Field("Foo_Value"): Opaque(),
            Field("Bar_Length"): length,
            Field("Bar_Value"): Opaque(),
        },
    )

    derived_message = DerivedMessage("Message_In_Message::Derived_Message", message)

    assert_messages_files(
        [f"{TESTDIR}/message_in_message.rflx"],
        [length_value, derived_length_value, message, derived_message],
    )


def test_type_refinement_spec() -> None:
    spec = {
        "Message_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Message_Type",
                [
                    ModularInteger("__PACKAGE__::T", Number(256)),
                    MessageSpec(
                        "__PACKAGE__::PDU",
                        [
                            Component(
                                "Foo",
                                "T",
                                [
                                    Then(
                                        "Bar",
                                        UNDEFINED,
                                        UNDEFINED,
                                        LessEqual(Variable("Foo"), Number(30, 16)),
                                    ),
                                    Then(
                                        "Baz",
                                        UNDEFINED,
                                        UNDEFINED,
                                        Greater(Variable("Foo"), Number(30, 16)),
                                    ),
                                ],
                            ),
                            Component("Bar", "T"),
                            Component("Baz", "T"),
                        ],
                    ),
                    MessageSpec(
                        "__PACKAGE__::Simple_PDU",
                        [Component("Bar", "T"), Component("Baz", "T")],
                    ),
                    MessageSpec("__PACKAGE__::Empty_PDU", []),
                ],
                [],
            ),
        ),
        "Type_Refinement": Specification(
            ContextSpec(["Message_Type"]),
            PackageSpec(
                "Type_Refinement",
                [
                    RefinementSpec(
                        "Message_Type::Simple_PDU",
                        "Bar",
                        "Message_Type::PDU",
                        Equal(Variable("Baz"), Number(42)),
                    ),
                    RefinementSpec("Message_Type::PDU", "Bar", "Message_Type::Simple_PDU"),
                ],
                [],
            ),
        ),
    }
    assert_specifications_files(
        [f"{TESTDIR}/message_type.rflx", f"{TESTDIR}/type_refinement.rflx"], spec
    )


def test_type_derivation_spec() -> None:
    assert_specifications_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     N : T;
                  end message;
               type Bar is new Foo;
            end Test;
        """,
        {
            "Test": Specification(
                ContextSpec([]),
                PackageSpec(
                    "Test",
                    [
                        ModularInteger("__PACKAGE__::T", Number(256)),
                        MessageSpec("__PACKAGE__::Foo", [Component("N", "T")]),
                        DerivationSpec("__PACKAGE__::Bar", "Foo"),
                    ],
                    [],
                ),
            )
        },
    )


def test_type_derivation_message() -> None:
    t = ModularInteger("Test::T", Number(256))

    structure = [Link(INITIAL, Field("Baz")), Link(Field("Baz"), FINAL)]

    types = {Field("Baz"): t}

    message_foo = Message("Test::Foo", structure, types)
    message_bar = DerivedMessage("Test::Bar", message_foo)

    assert_messages_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     Baz : T;
                  end message;
               type Bar is new Foo;
            end Test;
        """,
        [message_foo, message_bar],
    )


def test_type_derivation_refinements() -> None:
    message_foo = Message(
        "Test::Foo",
        [Link(INITIAL, Field("Baz"), length=Number(48)), Link(Field("Baz"), FINAL)],
        {Field("Baz"): Opaque()},
    )
    message_bar = DerivedMessage("Test::Bar", message_foo)

    assert_refinements_string(
        """
            package Test is
               type Foo is
                  message
                     null
                        then Baz
                           with Length => 48;
                     Baz : Opaque;
                  end message;
               for Foo use (Baz => Foo);
               type Bar is new Foo;
               for Bar use (Baz => Bar);
            end Test;
        """,
        [
            Refinement("Test", message_foo, Field("Baz"), message_foo),
            Refinement("Test", message_bar, Field("Baz"), message_bar),
        ],
    )


def test_ethernet_spec() -> None:
    spec = {
        "Ethernet": Specification(
            ContextSpec([]),
            PackageSpec(
                "Ethernet",
                [
                    ModularInteger("__PACKAGE__::Address", Pow(Number(2), Number(48))),
                    RangeInteger(
                        "__PACKAGE__::Type_Length",
                        Number(46),
                        Sub(Pow(Number(2), Number(16)), Number(1)),
                        Number(16),
                    ),
                    RangeInteger(
                        "__PACKAGE__::TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16)
                    ),
                    ModularInteger("__PACKAGE__::TCI", Pow(Number(2), Number(16))),
                    MessageSpec(
                        "__PACKAGE__::Frame",
                        [
                            Component("Destination", "Address"),
                            Component("Source", "Address"),
                            Component(
                                "Type_Length_TPID",
                                "Type_Length",
                                [
                                    Then(
                                        "TPID",
                                        First("Type_Length_TPID"),
                                        UNDEFINED,
                                        Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
                                    ),
                                    Then(
                                        "Type_Length",
                                        First("Type_Length_TPID"),
                                        UNDEFINED,
                                        NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                                    ),
                                ],
                            ),
                            Component("TPID", "TPID"),
                            Component("TCI", "TCI"),
                            Component(
                                "Type_Length",
                                "Type_Length",
                                [
                                    Then(
                                        "Payload",
                                        UNDEFINED,
                                        Mul(Variable("Type_Length"), Number(8)),
                                        LessEqual(Variable("Type_Length"), Number(1500)),
                                    ),
                                    Then(
                                        "Payload",
                                        UNDEFINED,
                                        Sub(Last("Message"), Last("Type_Length")),
                                        GreaterEqual(Variable("Type_Length"), Number(1536)),
                                    ),
                                ],
                            ),
                            Component(
                                "Payload",
                                "Opaque",
                                [
                                    Then(
                                        condition=And(
                                            GreaterEqual(
                                                Div(Length("Payload"), Number(8)),
                                                Number(46),
                                            ),
                                            LessEqual(
                                                Div(Length("Payload"), Number(8)),
                                                Number(1500),
                                            ),
                                        ),
                                    )
                                ],
                            ),
                        ],
                    ),
                ],
                [],
            ),
        )
    }

    assert_specifications_files([f"{SPECDIR}/ethernet.rflx"], spec)


def test_ethernet_message() -> None:
    assert_messages_files([f"{SPECDIR}/ethernet.rflx"], [ETHERNET_FRAME])


def test_tls() -> None:
    p = Parser()
    for f in ["tls_alert.rflx", "tls_handshake.rflx", "tls_heartbeat.rflx", "tls_record.rflx"]:
        p.parse(Path(f"{SPECDIR}/{f}"))
    p.create_model()


def test_message_with_two_length_fields() -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is
              type Length is mod 2**8;
              type Packet is
                 message
                    Length_1 : Length;
                    Length_2 : Length
                       then Payload
                          with Length => 8 * (Length_1 + Length_2);
                    Payload : Opaque;
                 end message;
           end Test;
        """
    )
    p.create_model()


def test_feature_integration() -> None:
    p = Parser()
    p.parse(Path(f"{TESTDIR}/feature_integration.rflx"))
    p.create_model()


def test_parsed_field_locations() -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is
              type T is mod 2**8;
              type M is
                 message
                    F1 : T;
                    F2 : T;
                 end message;
           end Test;
        """
    )
    m = p.create_model()
    assert m.messages[0].fields == (
        Field(ID("F1", Location((6, 21), end=(6, 22)))),
        Field(ID("F2", Location((7, 21), end=(7, 22)))),
    )


def test_array_with_imported_element_type() -> None:
    p = Parser()
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::T;
           end Array_Test;
        """
    )
    p.parse_string(
        """
           package Test is
              type T is mod 256;
           end Test;
        """
    )
    m = p.create_model()
    arrays = [t for t in m.types if isinstance(t, Array)]
    assert len(arrays) == 1
    assert arrays[0].identifier == ID("Array_Test::T")
    assert arrays[0].element_type == ModularInteger("Test::T", Number(256))


def test_parse_checksum() -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is
              type T is mod 2**8;
              type M is
                 message
                    F1 : T;
                    F2 : T;
                    C1 : T;
                    F3 : T;
                    C2 : T
                       then F4
                          if C1'Valid_Checksum and C2'Valid_Checksum;
                    F4 : T;
                 end message
                    with Checksum => (C1 => (F3, F1'First .. F2'Last),
                                      C2 => (C1'Last + 1 .. C2'First - 1));
            end Test;
        """
    )
    m = p.create_model()

    assert_equal(
        m.messages[0].checksums,
        {
            ID("C1"): [Variable("F3"), ValueRange(First("F1"), Last("F2"))],
            ID("C2"): [ValueRange(Add(Last("C1"), Number(1)), Sub(First("C2"), Number(1)))],
        },
    )
