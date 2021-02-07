import pytest
from librflxlang import AnalysisContext

from rflx import declaration as decl, expression as expr, model, statement as stmt
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.specification.parser import (
    GrammarRule,
    Parser,
    create_declaration,
    create_formal_declaration,
    create_id,
    create_session,
    create_state,
    create_statement,
    diagnostics_to_error,
)
from tests.utils import parse, parse_bool_expression, parse_expression, parse_math_expression


def parse_statement(data: str) -> stmt.Statement:
    return parse(data, GrammarRule.action_rule, create_statement)


def parse_declaration(data: str) -> decl.Declaration:
    return parse(data, GrammarRule.declaration_rule, create_declaration)


def parse_formal_declaration(data: str) -> decl.Declaration:
    return parse(data, GrammarRule.session_parameter_rule, create_formal_declaration)


def parse_state(data: str) -> decl.Declaration:
    return parse(data, GrammarRule.state_rule, create_state)


def parse_session(string: str, skip_validation: bool = False) -> model.Session:
    unit = AnalysisContext().get_from_buffer(
        "<stdin>", string, rule=GrammarRule.session_declaration_rule
    )
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error):
        error.propagate()
    return create_session(unit.root, ID("Package"), skip_validation=skip_validation)


def parse_id(data: str, rule: GrammarRule) -> ID:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    assert unit.root, "\n".join(str(d) for d in unit.diagnostics)
    return create_id(unit.root)


@pytest.mark.parametrize(
    "string,expected",
    [("X", ID("X")), ("X2", ID("X2")), ("X_Y", ID("X_Y")), ("X_Y_3", ID("X_Y_3"))],
)
def test_unqualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, GrammarRule.unqualified_identifier_rule)
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
    ],
)
def test_qualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, GrammarRule.qualified_identifier_rule)
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
def test_expression_numeric_literal(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected", [("X", expr.Variable("X")), ("X::Y", expr.Variable("X::Y"))]
)
def test_variable(string: str, expected: decl.Declaration) -> None:
    actual = parse_expression(string, GrammarRule.variable_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X'First", expr.First(expr.Variable("X"))),
        ("X'Last", expr.Last(expr.Variable("X"))),
        ("X'Size", expr.Size(expr.Variable("X"))),
        ("X'Head", expr.Head(expr.Variable("X"))),
        ("X'Opaque", expr.Opaque(expr.Variable("X"))),
        ("X'Present", expr.Present(expr.Variable("X"))),
        ("X'Valid", expr.Valid(expr.Variable("X"))),
        ("X'Valid_Checksum", expr.ValidChecksum(expr.Variable("X"))),
        ("X where X = 42", expr.Binding(expr.Variable("X"), {ID("X"): expr.Number(42)})),
        ("X'Head.Y", expr.Selected(expr.Head(expr.Variable("X")), "Y")),
    ],
)
def test_expression_suffix(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, GrammarRule.extended_expression_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
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
def test_expression_mathematical(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
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
        ("((X = 42))", expr.Equal(expr.Variable("X"), expr.Number(42))),
    ],
)
def test_expression_relation(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=False)
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
def test_expression_boolean(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X + Y", expr.Add(expr.Variable("X"), expr.Variable("Y"))),
        ("X + Y (Z)", expr.Add(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42 > X", "Invalid math BinOp OpGt.*"),
        ("X and Y", "Invalid math BinOp OpAnd.*"),
    ],
)
def test_mathematical_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises(NotImplementedError, match=rf"^{error}$"):
        parse_math_expression(string, extended=False)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X and Y (Z)", expr.And(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42", "'NumericLiteral'"),
        ("X * 3", "Invalid bool BinOp OpMul => X [*] 3"),
    ],
)
def test_boolean_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises((KeyError, NotImplementedError), match=rf"^{error}$"):
        parse_bool_expression(string, extended=False)


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
        ("X in Y", expr.In(expr.Variable("X"), expr.Variable("Y"))),
        ("X not in Y", expr.NotIn(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_expression_base(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, GrammarRule.extended_expression_rule)
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
def test_expression_complex(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


def test_private_type_declaration() -> None:
    string = "type X is private"
    expected = decl.TypeDeclaration(model.Private("X", location=Location((1, 1), None, (1, 17))))
    actual = parse_formal_declaration(string)
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
def test_channel_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_formal_declaration(string)
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
def test_formal_function_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_formal_declaration(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A : B", decl.VariableDeclaration("A", "B")),
        ("A : B := C", decl.VariableDeclaration("A", "B", expr.Variable("C"))),
        ("A : B := 1", decl.VariableDeclaration("A", "B", expr.Number(1))),
    ],
)
def test_variable_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_declaration(string)
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
def test_renaming_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_declaration(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [("A := B", stmt.Assignment("A", expr.Variable("B")))],
)
def test_assignment_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A'Append (B)", stmt.Append("A", expr.Variable("B"))),
        ("A'Extend (B)", stmt.Extend("A", expr.Variable("B"))),
        ("A'Read (B)", stmt.Read("A", expr.Variable("B"))),
        ("A'Write (B)", stmt.Write("A", expr.Variable("B"))),
        ("C'Reset", stmt.Reset("C")),
    ],
)
def test_attribute_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
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
            model.State("A", transitions=[model.Transition("B")]),
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
                  then B
                     with Desc => "rfc2549.txt+12:3-45:6"
                     if X = Y
                  then C
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
                  then B
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
    ],
    ids=range(1, 5),
)
def test_state(string: str, expected: decl.Declaration) -> None:
    actual = parse_state(string)
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
            "<stdin>:2:16: parser: error: inconsistent state identifier: A /= C.*",
        )
    ],
    ids=[1],
)
def test_state_error(string: str, error: str) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_state(string)


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
                  Y : Boolean := True;
               begin
                  state A is
                     Z : Boolean := Y;
                  begin
                     Z := False;
                  transition
                     then B
                        if Z = False
                     then A
                  end A;

                  state B is null state;
               end Session
         """,
            model.Session(
                ID("Package::Session"),
                ID("A"),
                ID("B"),
                [
                    model.State(
                        "A",
                        declarations=[
                            decl.VariableDeclaration(
                                "Z", "__BUILTINS__::Boolean", expr.Variable("Y")
                            )
                        ],
                        actions=[stmt.Assignment("Z", expr.Variable("False"))],
                        transitions=[
                            model.Transition(
                                "B",
                                condition=expr.Equal(expr.Variable("Z"), expr.Variable("False")),
                            ),
                            model.Transition("A"),
                        ],
                    ),
                    model.State("B"),
                ],
                [decl.VariableDeclaration("Y", "__BUILTINS__::Boolean", expr.Variable("True"))],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.TypeDeclaration(model.Private("T")),
                    decl.FunctionDeclaration("F", [], "T"),
                ],
                [],
                Location((2, 16), None, (23, 27)),
                skip_validation=True,
            ),
        ),
    ],
    ids=[1],
)
def test_session_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_session(string, skip_validation=True)
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
               end Y
         """,
            "<stdin>:2:16: parser: error: inconsistent session identifier: X /= Y.*",
        )
    ],
)
def test_session_declaration_error(string: str, error: str) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_session(string)


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


def test_expression_aggregate_no_number() -> None:
    with pytest.raises(RecordFluxError, match=r"^<stdin>:1:5: parser: error: Expected Numeral"):
        parse_expression("[1, Foo]", GrammarRule.expression_rule)
