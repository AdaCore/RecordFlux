from pathlib import Path

import pytest
import rflx_lang as lang

from rflx import expression as expr, model
from rflx.common import STDIN
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.model import State, declaration as decl, statement as stmt
from rflx.specification.parser import (
    Parser,
    create_declaration,
    create_formal_declaration,
    create_id,
    create_session,
    create_state,
    create_statement,
    diagnostics_to_error,
)
from rflx.typing_ import BOOLEAN
from tests.utils import parse, parse_bool_expression, parse_expression, parse_math_expression


def parse_statement(data: str) -> stmt.Statement:
    parser_statement, filename = parse(data, lang.GrammarRule.action_rule)
    assert isinstance(parser_statement, lang.Statement)
    error = RecordFluxError()
    statement = create_statement(error, parser_statement, filename)
    error.propagate()
    assert isinstance(statement, stmt.Statement)
    return statement


def parse_declaration(data: str) -> decl.Declaration:
    parser_declaration, filename = parse(data, lang.GrammarRule.declaration_rule)
    assert isinstance(parser_declaration, lang.LocalDecl)
    error = RecordFluxError()
    declaration = create_declaration(error, parser_declaration, ID("Package"), filename)
    error.propagate()
    assert isinstance(declaration, decl.Declaration)
    return declaration


def parse_formal_declaration(data: str) -> decl.Declaration:
    error = RecordFluxError()
    parser_declaration, filename = parse(data, lang.GrammarRule.session_parameter_rule)
    assert isinstance(parser_declaration, lang.FormalDecl)
    declaration = create_formal_declaration(error, parser_declaration, ID("Package"), filename)
    error.propagate()
    assert isinstance(declaration, decl.Declaration)
    return declaration


def parse_state(data: str) -> State:
    parser_state, source = parse(data, lang.GrammarRule.state_rule)
    assert isinstance(parser_state, lang.State)
    error = RecordFluxError()
    state = create_state(error, parser_state, ID("Package"), source)
    error.propagate()
    assert isinstance(state, State)
    return state


def check_diagnostics_error(unit: lang.AnalysisUnit, error: RecordFluxError) -> None:
    if diagnostics_to_error(unit.diagnostics, error, STDIN):
        error.propagate()


def parse_session_error(string: str) -> None:
    unit = lang.AnalysisContext().get_from_buffer(
        "<stdin>", string, rule=lang.GrammarRule.session_declaration_rule
    )
    error = RecordFluxError()
    check_diagnostics_error(unit, error)
    assert isinstance(unit.root, lang.SessionDecl)
    result = create_session(error, unit.root, ID("Package"), Path("<stdin>"))
    error.propagate()
    assert isinstance(result, model.UncheckedSession)


def parse_session(string: str) -> model.UncheckedSession:
    unit = lang.AnalysisContext().get_from_buffer(
        "<stdin>", string, rule=lang.GrammarRule.session_declaration_rule
    )
    error = RecordFluxError()
    check_diagnostics_error(unit, error)
    assert isinstance(unit.root, lang.SessionDecl)
    result = create_session(error, unit.root, ID("Package"), Path("<stdin>"))
    error.propagate()
    return result


def parse_id(data: str, rule: str) -> ID:
    unit = lang.AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    assert isinstance(unit.root, lang.AbstractID)
    error = RecordFluxError()
    result = create_id(error, unit.root, Path("<stdin>"))
    error.propagate()
    return result


@pytest.mark.parametrize(
    ("string", "expected"),
    [("X", ID("X")), ("X2", ID("X2")), ("X_Y", ID("X_Y")), ("X_Y_3", ID("X_Y_3"))],
)
def test_unqualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, lang.GrammarRule.unqualified_identifier_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X", ID("X")),
        ("X2", ID("X2")),
        ("X_Y", ID("X_Y")),
        ("X_Y_3", ID("X_Y_3")),
        ("X::Y", ID("X::Y")),
        ("X2::Y2", ID("X2::Y2")),
        ("X_Y::Z", ID("X_Y::Z")),
        ("X_Y_3::Z_4", ID("X_Y_3::Z_4")),
    ],
)
def test_qualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, lang.GrammarRule.qualified_identifier_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
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
def test_expression_numeric_literal(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"), [("X", expr.Variable("X")), ("X::Y", expr.Variable("X::Y"))]
)
def test_variable(string: str, expected: decl.Declaration) -> None:
    actual = parse_expression(string, lang.GrammarRule.variable_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X'First", expr.First(expr.Variable("X"))),
        ("X'Last", expr.Last(expr.Variable("X"))),
        ("X'Size", expr.Size(expr.Variable("X"))),
        ("X'Head", expr.Head(expr.Variable("X"))),
        ("X'Opaque", expr.Opaque(expr.Variable("X"))),
        ("X'Present", expr.Present(expr.Variable("X"))),
        ("X'Valid", expr.Valid(expr.Variable("X"))),
        ("X'Valid_Checksum", expr.ValidChecksum(expr.Variable("X"))),
        ("X'Has_Data", expr.HasData(expr.Variable("X"))),
        ("X'Head.Y", expr.Selected(expr.Head(expr.Variable("X")), "Y")),
    ],
)
def test_expression_suffix(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, lang.GrammarRule.extended_expression_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        (
            "A - B * 2**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                ),
                expr.Number(1),
            ),
        ),
        (
            "(A - B) * 2**3 - 1",
            expr.Sub(
                expr.Mul(
                    expr.Sub(expr.Variable("A"), expr.Variable("B")),
                    expr.Pow(expr.Number(2), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - B * 2**(3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Mul(
                    expr.Variable("B"),
                    expr.Pow(expr.Number(2), expr.Sub(expr.Number(3), expr.Number(1))),
                ),
            ),
        ),
        (
            "A - (B * 2)**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Pow(expr.Mul(expr.Variable("B"), expr.Number(2)), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - (B * 2**3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Sub(
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                    expr.Number(1),
                ),
            ),
        ),
        (
            "A + B * (-8)",
            expr.Add(expr.Variable("A"), expr.Mul(expr.Variable("B"), expr.Number(-8))),
        ),
        (
            "A + B mod 8 * 2",
            expr.Add(
                expr.Variable("A"),
                expr.Mul(expr.Mod(expr.Variable("B"), expr.Number(8)), expr.Number(2)),
            ),
        ),
    ],
)
def test_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X + Y", expr.Add(expr.Variable("X"), expr.Variable("Y"))),
        ("X + Y (Z)", expr.Add(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_extended_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X = Y", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
        ("X /= Y", expr.NotEqual(expr.Variable("X"), expr.Variable("Y"))),
        ("42 < X", expr.Less(expr.Number(42), expr.Variable("X"))),
        ("42 <= X", expr.LessEqual(expr.Number(42), expr.Variable("X"))),
        ("X > 42", expr.Greater(expr.Variable("X"), expr.Number(42))),
        ("X >= 42", expr.GreaterEqual(expr.Variable("X"), expr.Number(42))),
        ("((X = 42))", expr.Equal(expr.Variable("X"), expr.Number(42))),
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X or Y", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
        ("((X or Y))", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X and Y (Z)", expr.And(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_extended_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "error"),
    [
        ("42 > X", "<stdin>:1:1: parser: error: boolean expression in math context"),
        ("X and Y", "<stdin>:1:1: parser: error: boolean expression in math context"),
    ],
)
def test_mathematical_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_math_expression(string, extended=False)


@pytest.mark.parametrize(
    ("string", "error"),
    [
        ("42", "<stdin>:1:1: parser: error: math expression in boolean context"),
        ("X * 3", "<stdin>:1:1: parser: error: math expression in boolean context"),
    ],
)
def test_boolean_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_bool_expression(string, extended=False)


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("42", expr.Number(42)),
        ('"Foo Bar"', expr.String("Foo Bar")),
        ("[1]", expr.Aggregate(expr.Number(1))),
        ("[1, 2]", expr.Aggregate(expr.Number(1), expr.Number(2))),
        (
            '[137] & "PNG" & [13, 10, 26, 10]',
            expr.Aggregate(
                expr.Number(137),
                expr.Number(80),
                expr.Number(78),
                expr.Number(71),
                expr.Number(13),
                expr.Number(10),
                expr.Number(26),
                expr.Number(10),
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
            "[for X in Y if X.B = Z => X.A]",
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
        ("X in Y", expr.In(expr.Variable("X"), expr.Variable("Y"))),
        ("X not in Y", expr.NotIn(expr.Variable("X"), expr.Variable("Y"))),
        ("X < Y", expr.Less(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_expression_base(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, lang.GrammarRule.extended_expression_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
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
                expr.And(
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
                expr.And(
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
        ("X (Y).Z'Size", expr.Size(expr.Selected(expr.Call("X", [expr.Variable("Y")]), "Z"))),
        (
            "G::E not in P::S (E.D).V",
            expr.NotIn(
                expr.Variable("G::E"),
                expr.Selected(expr.Conversion("P::S", expr.Selected(expr.Variable("E"), "D")), "V"),
            ),
        ),
        (
            "[for E in L if E.T = A => E.B]'Head",
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
            "[for E in L if E.T = A => E.B]'Head.D",
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
            "(for some S in P::X ([for E in C.A if E.T = P.L => E]'Head.D).H => S.G = G) = False",
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
            "(case C is when V1 | V2 => 8, when V3 => 16)",
            expr.CaseExpr(
                expr.Variable("C"),
                [
                    ([ID("V1"), ID("V2")], expr.Number(8)),
                    ([ID("V3")], expr.Number(16)),
                ],
            ),
        ),
    ],
)
def test_expression_complex(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X : Channel with Readable", decl.ChannelDeclaration("X", readable=True)),
        ("X : Channel with Writable", decl.ChannelDeclaration("X", writable=True)),
        (
            "X : Channel with Readable, Writable",
            decl.ChannelDeclaration("X", readable=True, writable=True),
        ),
        ("with function X return Y", decl.FunctionDeclaration("X", [], "Package::Y")),
        ("with function X return Package::Y", decl.FunctionDeclaration("X", [], "Package::Y")),
        (
            "with function X (A : B; C : D) return Y",
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("A", "Package::B"), decl.Argument("C", "Package::D")],
                "Package::Y",
            ),
        ),
        (
            "with function X (A : Boolean) return Boolean",
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("A", str(model.BOOLEAN.identifier))],
                str(model.BOOLEAN.identifier),
            ),
        ),
    ],
)
def test_formal_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_formal_declaration(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A : B", decl.VariableDeclaration("A", "Package::B")),
        ("A : B := C", decl.VariableDeclaration("A", "Package::B", expr.Variable("C"))),
        ("A : B := 1", decl.VariableDeclaration("A", "Package::B", expr.Number(1))),
    ],
)
def test_variable_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_declaration(string)
    assert actual == expected
    assert actual.location


def test_renaming_declaration() -> None:
    expected = decl.RenamingDeclaration("A", "Package::B", expr.Selected(expr.Variable("C"), "D"))
    actual = parse_declaration("A : B renames C.D")
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A := B", stmt.VariableAssignment("A", expr.Variable("B"))),
        ("A.B := C", stmt.MessageFieldAssignment("A", "B", expr.Variable("C"))),
    ],
)
def test_assignment_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A'Append (B)", stmt.Append("A", expr.Variable("B"))),
        ("A'Extend (B)", stmt.Extend("A", expr.Variable("B"))),
        ("A'Read (B)", stmt.Read("A", expr.Variable("B"))),
        ("A'Write (B)", stmt.Write("A", expr.Variable("B"))),
        ("A'Reset", stmt.Reset("A")),
        (
            "A'Reset (B => 1, C => 2)",
            stmt.Reset("A", {ID("B"): expr.Number(1), ID("C"): expr.Number(2)}),
        ),
    ],
)
def test_attribute_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        (
            """
               state A is
               begin
               transition
                  goto B
               end A
         """,
            model.State("A", transitions=[model.Transition("B")]),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
                     if X = Y
                  goto C
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition("B", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
                    model.Transition("C"),
                ],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
                     with Desc => "rfc2549.txt+12:3-45:6"
                     if X = Y
                  goto C
                     with Desc => "rfc2549.txt+123:45-678:9"
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition(
                        "B",
                        expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                        description="rfc2549.txt+12:3-45:6",
                    ),
                    model.Transition("C", description="rfc2549.txt+123:45-678:9"),
                ],
            ),
        ),
        (
            """
               state A is
                  Z : Boolean := Y;
               begin
               transition
                  goto B
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                declarations=[
                    decl.VariableDeclaration("Z", "__BUILTINS__::Boolean", expr.Variable("Y"))
                ],
                actions=[],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
               exception
                  goto C
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                exception_transition=model.Transition("C"),
                declarations=[],
                actions=[],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
               exception
                  goto C
                     with Desc => "rfc2549.txt+12:3-45:6"
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                exception_transition=model.Transition("C", description="rfc2549.txt+12:3-45:6"),
                declarations=[],
                actions=[],
            ),
        ),
    ],
    ids=range(1, 7),
)
def test_state(string: str, expected: decl.Declaration) -> None:
    actual = parse_state(string)
    assert actual == expected
    assert actual.location


def test_state_error() -> None:
    string = """
       state A is
       begin
       transition
          goto B
       end C
    """
    error = "<stdin>:2:8: parser: error: inconsistent state identifier: A /= C.*"
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_state(string)


def test_session_declaration() -> None:
    string = """
           generic
              X : Channel with Readable, Writable;
              with function F return Boolean;
           session Session is
              Y : Boolean := True;
           begin
              state A is
                 Z : Boolean := Y;
              begin
                 Z := False;
              transition
                 goto null
                    if Z = False
                 goto A
              end A;
           end Session
    """
    actual = parse_session(string)
    expected = model.UncheckedSession(
        ID("Package::Session"),
        [
            model.State(
                "A",
                declarations=[
                    decl.VariableDeclaration("Z", BOOLEAN.identifier, expr.Variable("Y"))
                ],
                actions=[stmt.VariableAssignment("Z", expr.FALSE)],
                transitions=[
                    model.Transition(
                        "null",
                        condition=expr.Equal(expr.Variable("Z"), expr.FALSE),
                    ),
                    model.Transition("A"),
                ],
            ),
        ],
        [decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.TRUE)],
        [
            decl.ChannelDeclaration("X", readable=True, writable=True),
            decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
        ],
        location=Location((2, 12), STDIN, (17, 23)),
    )
    assert actual == expected
    assert actual.location


def test_parse_session() -> None:
    string = """
       generic
       session X is
       begin
          state A is
          begin
          transition
             goto null
          end A;
       end X
    """
    parse_session_error(string)


@pytest.mark.parametrize(
    ("string", "error"),
    [
        (
            """
               generic
               session X is
               begin
                  state A is
                  begin
                  transition
                     goto null
                  end A;
               end Y
         """,
            "<stdin>:2:16: parser: error: inconsistent session identifier: X /= Y.*",
        ),
        (
            """
               generic
               session X is
               begin
                  state A is
                  begin
                  transition
                     goto null
                  end A
               end Y
         """,
            "<stdin>:10:16: parser: error: Expected ';', got 'end'",
        ),
    ],
)
def test_session_error(string: str, error: str) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_session_error(string)


def test_session() -> None:
    p = Parser()
    p.parse_string(
        """\
        package Test is

           generic
           session Session is
           begin
              state A is
              begin
              transition
                 goto B
              end A;

              state B is
              begin
              transition
                 goto null
              end B;
           end Session;

        end Test;
        """
    )
    p.create_model()


def test_expression_aggregate_no_number() -> None:
    with pytest.raises(
        RecordFluxError, match=(r"^<stdin>:1:5: parser: error: Expected Numeral, got 'First'$")
    ):
        parse_expression("[1, Foo]", lang.GrammarRule.expression_rule)
