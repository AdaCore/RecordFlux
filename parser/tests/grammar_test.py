# pylint: disable=too-many-lines

from typing import Dict

import pytest
from librflxlang import AnalysisContext, GrammarRule

from tests.utils import to_dict


def parse(string: str, rule: str) -> object:
    unit = AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)
    assert len(unit.diagnostics) == 0, str(unit.diagnostics)
    return to_dict(unit.root)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X", {"_kind": "UnqualifiedID", "_value": "X"}),
        ("X2", {"_kind": "UnqualifiedID", "_value": "X2"}),
        ("X_Y", {"_kind": "UnqualifiedID", "_value": "X_Y"}),
        ("X_Y_3", {"_kind": "UnqualifiedID", "_value": "X_Y_3"}),
    ],
)
def test_unqualified_identifier(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.unqualified_identifier_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "X"},
                "package": None,
            },
        ),
        (
            "X2",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "X2"},
                "package": None,
            },
        ),
        (
            "X_Y",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "X_Y"},
                "package": None,
            },
        ),
        (
            "X_Y_3",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "X_Y_3"},
                "package": None,
            },
        ),
        (
            "X::Y",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                "package": {"_kind": "UnqualifiedID", "_value": "X"},
            },
        ),
        (
            "X2::Y2",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "Y2"},
                "package": {"_kind": "UnqualifiedID", "_value": "X2"},
            },
        ),
        (
            "X_Y::Z",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                "package": {"_kind": "UnqualifiedID", "_value": "X_Y"},
            },
        ),
        (
            "X_Y_3::Z_4",
            {
                "_kind": "ID",
                "name": {"_kind": "UnqualifiedID", "_value": "Z_4"},
                "package": {"_kind": "UnqualifiedID", "_value": "X_Y_3"},
            },
        ),
    ],
)
def test_qualified_identifier(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.qualified_identifier_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        ("1000", {"_kind": "NumericLiteral", "_value": "1000"}),
        ("1_000", {"_kind": "NumericLiteral", "_value": "1_000"}),
        ("16#6664#", {"_kind": "NumericLiteral", "_value": "16#6664#"}),
        ("16#66_64#", {"_kind": "NumericLiteral", "_value": "16#66_64#"}),
        (
            "-1000",
            {
                "_kind": "Negation",
                "data": {"_kind": "NumericLiteral", "_value": "1000"},
            },
        ),
        (
            "-1_000",
            {
                "_kind": "Negation",
                "data": {"_kind": "NumericLiteral", "_value": "1_000"},
            },
        ),
        (
            "-16#6664#",
            {
                "_kind": "Negation",
                "data": {"_kind": "NumericLiteral", "_value": "16#6664#"},
            },
        ),
        (
            "-16#66_64#",
            {
                "_kind": "Negation",
                "data": {"_kind": "NumericLiteral", "_value": "16#66_64#"},
            },
        ),
    ],
)
def test_expression_numeric_literal(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X",
            {
                "_kind": "Variable",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                    "package": None,
                },
            },
        ),
        (
            "X::Y",
            {
                "_kind": "Variable",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                    "package": {"_kind": "UnqualifiedID", "_value": "X"},
                },
            },
        ),
    ],
)
def test_variable(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.variable_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X'First",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrFirst", "_value": "First"},
            },
        ),
        (
            "X'Last",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrLast", "_value": "Last"},
            },
        ),
        (
            "X'Size",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrSize", "_value": "Size"},
            },
        ),
        (
            "X'Has_Data",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrHasData", "_value": "Has_Data"},
            },
        ),
        (
            "X'Head",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrHead", "_value": "Head"},
            },
        ),
        (
            "X'Opaque",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrOpaque", "_value": "Opaque"},
            },
        ),
        (
            "X'Present",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrPresent", "_value": "Present"},
            },
        ),
        (
            "X'Valid",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrValid", "_value": "Valid"},
            },
        ),
        (
            "X'Valid_Checksum",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "kind": {"_kind": "AttrValidChecksum", "_value": "Valid_Checksum"},
            },
        ),
        (
            "X where X = 42",
            {
                "_kind": "Binding",
                "bindings": [
                    {
                        "_kind": "TermAssoc",
                        "expression": {"_kind": "NumericLiteral", "_value": "42"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                    }
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X'Head.Y",
            {
                "_kind": "SelectNode",
                "expression": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "kind": {"_kind": "AttrHead", "_value": "Head"},
                },
                "selector": {"_kind": "UnqualifiedID", "_value": "Y"},
            },
        ),
    ],
)
def test_expression_suffix(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.extended_expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A - B * 2**3 - 1",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "A"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpSub", "_value": "-"},
                    "right": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                "package": None,
                            },
                        },
                        "op": {"_kind": "OpMul", "_value": "*"},
                        "right": {
                            "_kind": "BinOp",
                            "left": {"_kind": "NumericLiteral", "_value": "2"},
                            "op": {"_kind": "OpPow", "_value": "**"},
                            "right": {"_kind": "NumericLiteral", "_value": "3"},
                        },
                    },
                },
                "op": {"_kind": "OpSub", "_value": "-"},
                "right": {"_kind": "NumericLiteral", "_value": "1"},
            },
        ),
        (
            "(A - B) * 2**3 - 1",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "ParenExpression",
                        "data": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "A"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpSub", "_value": "-"},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                    "package": None,
                                },
                            },
                        },
                    },
                    "op": {"_kind": "OpMul", "_value": "*"},
                    "right": {
                        "_kind": "BinOp",
                        "left": {"_kind": "NumericLiteral", "_value": "2"},
                        "op": {"_kind": "OpPow", "_value": "**"},
                        "right": {"_kind": "NumericLiteral", "_value": "3"},
                    },
                },
                "op": {"_kind": "OpSub", "_value": "-"},
                "right": {"_kind": "NumericLiteral", "_value": "1"},
            },
        ),
        (
            "A - B * 2**(3 - 1)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpSub", "_value": "-"},
                "right": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "B"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpMul", "_value": "*"},
                    "right": {
                        "_kind": "BinOp",
                        "left": {"_kind": "NumericLiteral", "_value": "2"},
                        "op": {"_kind": "OpPow", "_value": "**"},
                        "right": {
                            "_kind": "ParenExpression",
                            "data": {
                                "_kind": "BinOp",
                                "left": {"_kind": "NumericLiteral", "_value": "3"},
                                "op": {"_kind": "OpSub", "_value": "-"},
                                "right": {"_kind": "NumericLiteral", "_value": "1"},
                            },
                        },
                    },
                },
            },
        ),
        (
            "A - (B * 2)**3 - 1",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "A"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpSub", "_value": "-"},
                    "right": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "ParenExpression",
                            "data": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "B",
                                        },
                                        "package": None,
                                    },
                                },
                                "op": {"_kind": "OpMul", "_value": "*"},
                                "right": {"_kind": "NumericLiteral", "_value": "2"},
                            },
                        },
                        "op": {"_kind": "OpPow", "_value": "**"},
                        "right": {"_kind": "NumericLiteral", "_value": "3"},
                    },
                },
                "op": {"_kind": "OpSub", "_value": "-"},
                "right": {"_kind": "NumericLiteral", "_value": "1"},
            },
        ),
        (
            "A - (B * 2**3 - 1)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpSub", "_value": "-"},
                "right": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpMul", "_value": "*"},
                            "right": {
                                "_kind": "BinOp",
                                "left": {"_kind": "NumericLiteral", "_value": "2"},
                                "op": {"_kind": "OpPow", "_value": "**"},
                                "right": {"_kind": "NumericLiteral", "_value": "3"},
                            },
                        },
                        "op": {"_kind": "OpSub", "_value": "-"},
                        "right": {"_kind": "NumericLiteral", "_value": "1"},
                    },
                },
            },
        ),
        (
            "A + B * (-8)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAdd", "_value": "+"},
                "right": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "B"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpMul", "_value": "*"},
                    "right": {
                        "_kind": "ParenExpression",
                        "data": {
                            "_kind": "Negation",
                            "data": {"_kind": "NumericLiteral", "_value": "8"},
                        },
                    },
                },
            },
        ),
        (
            "A + B mod 8 * 2",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAdd", "_value": "+"},
                "right": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                "package": None,
                            },
                        },
                        "op": {"_kind": "OpMod", "_value": "mod"},
                        "right": {"_kind": "NumericLiteral", "_value": "8"},
                    },
                    "op": {"_kind": "OpMul", "_value": "*"},
                    "right": {"_kind": "NumericLiteral", "_value": "2"},
                },
            },
        ),
    ],
)
def test_expression_mathematical(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X = Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpEq", "_value": "="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X /= Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpNeq", "_value": "/="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "42 < X",
            {
                "_kind": "BinOp",
                "left": {"_kind": "NumericLiteral", "_value": "42"},
                "op": {"_kind": "OpLt", "_value": "<"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "42 <= X",
            {
                "_kind": "BinOp",
                "left": {"_kind": "NumericLiteral", "_value": "42"},
                "op": {"_kind": "OpLe", "_value": "<="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X > 42",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpGt", "_value": ">"},
                "right": {"_kind": "NumericLiteral", "_value": "42"},
            },
        ),
        (
            "X >= 42",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpGe", "_value": ">="},
                "right": {"_kind": "NumericLiteral", "_value": "42"},
            },
        ),
        (
            "((X = 42))",
            {
                "_kind": "ParenExpression",
                "data": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                "package": None,
                            },
                        },
                        "op": {"_kind": "OpEq", "_value": "="},
                        "right": {"_kind": "NumericLiteral", "_value": "42"},
                    },
                },
            },
        ),
    ],
)
def test_expression_relation(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X and Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X or Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpOr", "_value": "or"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "((X or Y))",
            {
                "_kind": "ParenExpression",
                "data": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                "package": None,
                            },
                        },
                        "op": {"_kind": "OpOr", "_value": "or"},
                        "right": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                                "package": None,
                            },
                        },
                    },
                },
            },
        ),
    ],
)
def test_expression_boolean(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X + Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAdd", "_value": "+"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X + Y (Z)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAdd", "_value": "+"},
                "right": {
                    "_kind": "Call",
                    "arguments": [
                        {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                                "package": None,
                            },
                        }
                    ],
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Y"},
                },
            },
        ),
    ],
)
def test_mathematical_expression(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.extended_expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X and Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X and Y (Z)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "Call",
                    "arguments": [
                        {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                                "package": None,
                            },
                        }
                    ],
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Y"},
                },
            },
        ),
    ],
)
def test_boolean_expression(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.extended_expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        ("42", {"_kind": "NumericLiteral", "_value": "42"}),
        ('"Foo Bar"', {"_kind": "StringLiteral", "_value": '"Foo Bar"'}),
        (
            "[1]",
            {
                "_kind": "SequenceAggregate",
                "values": [{"_kind": "NumericLiteral", "_value": "1"}],
            },
        ),
        (
            "[1, 2]",
            {
                "_kind": "SequenceAggregate",
                "values": [
                    {"_kind": "NumericLiteral", "_value": "1"},
                    {"_kind": "NumericLiteral", "_value": "2"},
                ],
            },
        ),
        (
            '[137] & "PNG" & [13, 10, 26, 10]',
            {
                "_kind": "Concatenation",
                "left": {
                    "_kind": "Concatenation",
                    "left": {
                        "_kind": "SequenceAggregate",
                        "values": [{"_kind": "NumericLiteral", "_value": "137"}],
                    },
                    "right": {"_kind": "StringLiteral", "_value": '"PNG"'},
                },
                "right": {
                    "_kind": "SequenceAggregate",
                    "values": [
                        {"_kind": "NumericLiteral", "_value": "13"},
                        {"_kind": "NumericLiteral", "_value": "10"},
                        {"_kind": "NumericLiteral", "_value": "26"},
                        {"_kind": "NumericLiteral", "_value": "10"},
                    ],
                },
            },
        ),
        (
            "for all X in Y => X = Z",
            {
                "_kind": "QuantifiedExpression",
                "iterable": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
                "operation": {"_kind": "QuantifierAll", "_value": "all"},
                "parameter_identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "predicate": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpEq", "_value": "="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "package": None,
                        },
                    },
                },
            },
        ),
        (
            "for some X in Y => X = Z",
            {
                "_kind": "QuantifiedExpression",
                "iterable": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
                "operation": {"_kind": "QuantifierSome", "_value": "some"},
                "parameter_identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "predicate": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpEq", "_value": "="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "package": None,
                        },
                    },
                },
            },
        ),
        (
            "[for X in Y => X.A]",
            {
                "_kind": "Comprehension",
                "sequence": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
                "condition": None,
                "iterator": {"_kind": "UnqualifiedID", "_value": "X"},
                "selector": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "A"},
                },
            },
        ),
        (
            "[for X in Y if X.B = Z => X.A]",
            {
                "_kind": "Comprehension",
                "sequence": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
                "condition": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "SelectNode",
                        "expression": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                "package": None,
                            },
                        },
                        "selector": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                    "op": {"_kind": "OpEq", "_value": "="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "package": None,
                        },
                    },
                },
                "iterator": {"_kind": "UnqualifiedID", "_value": "X"},
                "selector": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "A"},
                },
            },
        ),
        (
            'X (A, "S", 42)',
            {
                "_kind": "Call",
                "arguments": [
                    {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "A"},
                            "package": None,
                        },
                    },
                    {"_kind": "StringLiteral", "_value": '"S"'},
                    {"_kind": "NumericLiteral", "_value": "42"},
                ],
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
            },
        ),
        (
            "X::Y (A)",
            {
                "_kind": "Conversion",
                "argument": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
                "target_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                    "package": {"_kind": "UnqualifiedID", "_value": "X"},
                },
            },
        ),
        (
            "X'(Y => Z)",
            {
                "_kind": "MessageAggregate",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                    "package": None,
                },
                "values": {
                    "_kind": "MessageAggregateAssociations",
                    "associations": [
                        {
                            "_kind": "MessageAggregateAssociation",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                                    "package": None,
                                },
                            },
                            "identifier": {"_kind": "UnqualifiedID", "_value": "Y"},
                        }
                    ],
                },
            },
        ),
        (
            "X'(Y => Z, A => B)",
            {
                "_kind": "MessageAggregate",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                    "package": None,
                },
                "values": {
                    "_kind": "MessageAggregateAssociations",
                    "associations": [
                        {
                            "_kind": "MessageAggregateAssociation",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                                    "package": None,
                                },
                            },
                            "identifier": {"_kind": "UnqualifiedID", "_value": "Y"},
                        },
                        {
                            "_kind": "MessageAggregateAssociation",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                    "package": None,
                                },
                            },
                            "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                        },
                    ],
                },
            },
        ),
        (
            "X'(null message)",
            {
                "_kind": "MessageAggregate",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                    "package": None,
                },
                "values": {"_kind": "NullMessageAggregate", "_value": "null message"},
            },
        ),
        (
            "X",
            {
                "_kind": "Variable",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                    "package": None,
                },
            },
        ),
        (
            "X in Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpIn", "_value": "in"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X not in Y",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "X"},
                        "package": None,
                    },
                },
                "op": {"_kind": "OpNotin", "_value": "not in"},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": None,
                    },
                },
            },
        ),
    ],
)
def test_expression_base(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.extended_expression_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X'Valid = True",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "kind": {"_kind": "AttrValid", "_value": "Valid"},
                },
                "op": {"_kind": "OpEq", "_value": "="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "True"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X::Y /= Z",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": {"_kind": "UnqualifiedID", "_value": "X"},
                    },
                },
                "op": {"_kind": "OpNeq", "_value": "/="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X.Y /= Z",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "Y"},
                },
                "op": {"_kind": "OpNeq", "_value": "/="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "X = Y and Y /= Z",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpEq", "_value": "="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                            "package": None,
                        },
                    },
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                            "package": None,
                        },
                    },
                    "op": {"_kind": "OpNeq", "_value": "/="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "package": None,
                        },
                    },
                },
            },
        ),
        (
            "X'Valid and Y'Valid",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "kind": {"_kind": "AttrValid", "_value": "Valid"},
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                            "package": None,
                        },
                    },
                    "kind": {"_kind": "AttrValid", "_value": "Valid"},
                },
            },
        ),
        (
            "X'Valid = True and (Y'Valid = False or Z'Valid = False)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "Attribute",
                        "expression": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                "package": None,
                            },
                        },
                        "kind": {"_kind": "AttrValid", "_value": "Valid"},
                    },
                    "op": {"_kind": "OpEq", "_value": "="},
                    "right": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "True"},
                            "package": None,
                        },
                    },
                },
                "op": {"_kind": "OpAnd", "_value": "and"},
                "right": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Attribute",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Y",
                                        },
                                        "package": None,
                                    },
                                },
                                "kind": {"_kind": "AttrValid", "_value": "Valid"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "False",
                                    },
                                    "package": None,
                                },
                            },
                        },
                        "op": {"_kind": "OpOr", "_value": "or"},
                        "right": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Attribute",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Z",
                                        },
                                        "package": None,
                                    },
                                },
                                "kind": {"_kind": "AttrValid", "_value": "Valid"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "False",
                                    },
                                    "package": None,
                                },
                            },
                        },
                    },
                },
            },
        ),
        (
            "X'Valid = False or X.A /= A or X.B /= B or (X.C = 0 and X.D not in X.E)",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "BinOp",
                    "left": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Attribute",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "kind": {"_kind": "AttrValid", "_value": "Valid"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "False",
                                    },
                                    "package": None,
                                },
                            },
                        },
                        "op": {"_kind": "OpOr", "_value": "or"},
                        "right": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "A"},
                            },
                            "op": {"_kind": "OpNeq", "_value": "/="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "A"},
                                    "package": None,
                                },
                            },
                        },
                    },
                    "op": {"_kind": "OpOr", "_value": "or"},
                    "right": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "SelectNode",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                    "package": None,
                                },
                            },
                            "selector": {"_kind": "UnqualifiedID", "_value": "B"},
                        },
                        "op": {"_kind": "OpNeq", "_value": "/="},
                        "right": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                "package": None,
                            },
                        },
                    },
                },
                "op": {"_kind": "OpOr", "_value": "or"},
                "right": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "C"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {"_kind": "NumericLiteral", "_value": "0"},
                        },
                        "op": {"_kind": "OpAnd", "_value": "and"},
                        "right": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "D"},
                            },
                            "op": {"_kind": "OpNotin", "_value": "not in"},
                            "right": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "E"},
                            },
                        },
                    },
                },
            },
        ),
        (
            "for some A in X.B => (A.T = P.E and (G::E not in P::S (A.D).V))",
            {
                "_kind": "QuantifiedExpression",
                "iterable": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "X"},
                            "package": None,
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "B"},
                },
                "operation": {"_kind": "QuantifierSome", "_value": "some"},
                "parameter_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "predicate": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "A",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "T"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "P",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "E"},
                            },
                        },
                        "op": {"_kind": "OpAnd", "_value": "and"},
                        "right": {
                            "_kind": "ParenExpression",
                            "data": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "E",
                                        },
                                        "package": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "G",
                                        },
                                    },
                                },
                                "op": {"_kind": "OpNotin", "_value": "not in"},
                                "right": {
                                    "_kind": "SelectNode",
                                    "expression": {
                                        "_kind": "Conversion",
                                        "argument": {
                                            "_kind": "SelectNode",
                                            "expression": {
                                                "_kind": "Variable",
                                                "identifier": {
                                                    "_kind": "ID",
                                                    "name": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "A",
                                                    },
                                                    "package": None,
                                                },
                                            },
                                            "selector": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "D",
                                            },
                                        },
                                        "target_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "S",
                                            },
                                            "package": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "P",
                                            },
                                        },
                                    },
                                    "selector": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "V",
                                    },
                                },
                            },
                        },
                    },
                },
            },
        ),
        (
            "X::Y (Z) = 42",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Conversion",
                    "argument": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "package": None,
                        },
                    },
                    "target_identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "package": {"_kind": "UnqualifiedID", "_value": "X"},
                    },
                },
                "op": {"_kind": "OpEq", "_value": "="},
                "right": {"_kind": "NumericLiteral", "_value": "42"},
            },
        ),
        (
            "X (Y).Z",
            {
                "_kind": "SelectNode",
                "expression": {
                    "_kind": "Call",
                    "arguments": [
                        {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                                "package": None,
                            },
                        }
                    ],
                    "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                },
                "selector": {"_kind": "UnqualifiedID", "_value": "Z"},
            },
        ),
        (
            "X (Y).Z'Size",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Call",
                        "arguments": [
                            {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                                    "package": None,
                                },
                            }
                        ],
                        "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "Z"},
                },
                "kind": {"_kind": "AttrSize", "_value": "Size"},
            },
        ),
        (
            "G::E not in P::S (E.D).V",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "E"},
                        "package": {"_kind": "UnqualifiedID", "_value": "G"},
                    },
                },
                "op": {"_kind": "OpNotin", "_value": "not in"},
                "right": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Conversion",
                        "argument": {
                            "_kind": "SelectNode",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "E"},
                                    "package": None,
                                },
                            },
                            "selector": {"_kind": "UnqualifiedID", "_value": "D"},
                        },
                        "target_identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "S"},
                            "package": {"_kind": "UnqualifiedID", "_value": "P"},
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "V"},
                },
            },
        ),
        (
            "[for E in L if E.T = A => E.B]'Head",
            {
                "_kind": "Attribute",
                "expression": {
                    "_kind": "Comprehension",
                    "sequence": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "L"},
                            "package": None,
                        },
                    },
                    "condition": {
                        "_kind": "BinOp",
                        "left": {
                            "_kind": "SelectNode",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "E"},
                                    "package": None,
                                },
                            },
                            "selector": {"_kind": "UnqualifiedID", "_value": "T"},
                        },
                        "op": {"_kind": "OpEq", "_value": "="},
                        "right": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "A"},
                                "package": None,
                            },
                        },
                    },
                    "iterator": {"_kind": "UnqualifiedID", "_value": "E"},
                    "selector": {
                        "_kind": "SelectNode",
                        "expression": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "E"},
                                "package": None,
                            },
                        },
                        "selector": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                },
                "kind": {"_kind": "AttrHead", "_value": "Head"},
            },
        ),
        (
            "A'Head.D",
            {
                "_kind": "SelectNode",
                "expression": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "A"},
                            "package": None,
                        },
                    },
                    "kind": {"_kind": "AttrHead", "_value": "Head"},
                },
                "selector": {"_kind": "UnqualifiedID", "_value": "D"},
            },
        ),
        (
            "[for E in L if E.T = A => E.B]'Head.D",
            {
                "_kind": "SelectNode",
                "expression": {
                    "_kind": "Attribute",
                    "expression": {
                        "_kind": "Comprehension",
                        "sequence": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "L"},
                                "package": None,
                            },
                        },
                        "condition": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "E",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "T"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "A"},
                                    "package": None,
                                },
                            },
                        },
                        "iterator": {"_kind": "UnqualifiedID", "_value": "E"},
                        "selector": {
                            "_kind": "SelectNode",
                            "expression": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "E"},
                                    "package": None,
                                },
                            },
                            "selector": {"_kind": "UnqualifiedID", "_value": "B"},
                        },
                    },
                    "kind": {"_kind": "AttrHead", "_value": "Head"},
                },
                "selector": {"_kind": "UnqualifiedID", "_value": "D"},
            },
        ),
        (
            "(for some S in P::X ([for E in C.A if E.T = P.L => E]'Head.D).H => S.G = G) = False",
            {
                "_kind": "BinOp",
                "left": {
                    "_kind": "ParenExpression",
                    "data": {
                        "_kind": "QuantifiedExpression",
                        "iterable": {
                            "_kind": "SelectNode",
                            "expression": {
                                "_kind": "Conversion",
                                "argument": {
                                    "_kind": "SelectNode",
                                    "expression": {
                                        "_kind": "Attribute",
                                        "expression": {
                                            "_kind": "Comprehension",
                                            "sequence": {
                                                "_kind": "SelectNode",
                                                "expression": {
                                                    "_kind": "Variable",
                                                    "identifier": {
                                                        "_kind": "ID",
                                                        "name": {
                                                            "_kind": "UnqualifiedID",
                                                            "_value": "C",
                                                        },
                                                        "package": None,
                                                    },
                                                },
                                                "selector": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "A",
                                                },
                                            },
                                            "condition": {
                                                "_kind": "BinOp",
                                                "left": {
                                                    "_kind": "SelectNode",
                                                    "expression": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "E",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "selector": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "T",
                                                    },
                                                },
                                                "op": {"_kind": "OpEq", "_value": "="},
                                                "right": {
                                                    "_kind": "SelectNode",
                                                    "expression": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "P",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "selector": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "L",
                                                    },
                                                },
                                            },
                                            "iterator": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "E",
                                            },
                                            "selector": {
                                                "_kind": "Variable",
                                                "identifier": {
                                                    "_kind": "ID",
                                                    "name": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "E",
                                                    },
                                                    "package": None,
                                                },
                                            },
                                        },
                                        "kind": {"_kind": "AttrHead", "_value": "Head"},
                                    },
                                    "selector": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "D",
                                    },
                                },
                                "target_identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "X"},
                                    "package": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "P",
                                    },
                                },
                            },
                            "selector": {"_kind": "UnqualifiedID", "_value": "H"},
                        },
                        "operation": {"_kind": "QuantifierSome", "_value": "some"},
                        "parameter_identifier": {
                            "_kind": "UnqualifiedID",
                            "_value": "S",
                        },
                        "predicate": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "SelectNode",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "S",
                                        },
                                        "package": None,
                                    },
                                },
                                "selector": {"_kind": "UnqualifiedID", "_value": "G"},
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "G"},
                                    "package": None,
                                },
                            },
                        },
                    },
                },
                "op": {"_kind": "OpEq", "_value": "="},
                "right": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "False"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2)",
            {
                "_kind": "Binding",
                "bindings": [
                    {
                        "_kind": "TermAssoc",
                        "expression": {
                            "_kind": "MessageAggregate",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "M2"},
                                "package": None,
                            },
                            "values": {
                                "_kind": "MessageAggregateAssociations",
                                "associations": [
                                    {
                                        "_kind": "MessageAggregateAssociation",
                                        "expression": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "B2",
                                                },
                                                "package": None,
                                            },
                                        },
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "D",
                                        },
                                    }
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B1"},
                    }
                ],
                "expression": {
                    "_kind": "MessageAggregate",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "M1"},
                        "package": None,
                    },
                    "values": {
                        "_kind": "MessageAggregateAssociations",
                        "associations": [
                            {
                                "_kind": "MessageAggregateAssociation",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "B1",
                                        },
                                        "package": None,
                                    },
                                },
                                "identifier": {"_kind": "UnqualifiedID", "_value": "D"},
                            }
                        ],
                    },
                },
            },
        ),
        (
            "M1'(D1 => B1, D2 => B2) where B1 = M2'(D => B2), B2 = M2'(D => B3)",
            {
                "_kind": "Binding",
                "bindings": [
                    {
                        "_kind": "TermAssoc",
                        "expression": {
                            "_kind": "MessageAggregate",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "M2"},
                                "package": None,
                            },
                            "values": {
                                "_kind": "MessageAggregateAssociations",
                                "associations": [
                                    {
                                        "_kind": "MessageAggregateAssociation",
                                        "expression": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "B2",
                                                },
                                                "package": None,
                                            },
                                        },
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "D",
                                        },
                                    }
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B1"},
                    },
                    {
                        "_kind": "TermAssoc",
                        "expression": {
                            "_kind": "MessageAggregate",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "M2"},
                                "package": None,
                            },
                            "values": {
                                "_kind": "MessageAggregateAssociations",
                                "associations": [
                                    {
                                        "_kind": "MessageAggregateAssociation",
                                        "expression": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "B3",
                                                },
                                                "package": None,
                                            },
                                        },
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "D",
                                        },
                                    }
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B2"},
                    },
                ],
                "expression": {
                    "_kind": "MessageAggregate",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "M1"},
                        "package": None,
                    },
                    "values": {
                        "_kind": "MessageAggregateAssociations",
                        "associations": [
                            {
                                "_kind": "MessageAggregateAssociation",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "B1",
                                        },
                                        "package": None,
                                    },
                                },
                                "identifier": {
                                    "_kind": "UnqualifiedID",
                                    "_value": "D1",
                                },
                            },
                            {
                                "_kind": "MessageAggregateAssociation",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "B2",
                                        },
                                        "package": None,
                                    },
                                },
                                "identifier": {
                                    "_kind": "UnqualifiedID",
                                    "_value": "D2",
                                },
                            },
                        ],
                    },
                },
            },
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2) where B2 = M3'(D => B3)",
            {
                "_kind": "Binding",
                "bindings": [
                    {
                        "_kind": "TermAssoc",
                        "expression": {
                            "_kind": "Binding",
                            "bindings": [
                                {
                                    "_kind": "TermAssoc",
                                    "expression": {
                                        "_kind": "MessageAggregate",
                                        "identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "M3",
                                            },
                                            "package": None,
                                        },
                                        "values": {
                                            "_kind": "MessageAggregateAssociations",
                                            "associations": [
                                                {
                                                    "_kind": "MessageAggregateAssociation",
                                                    "expression": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "B3",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "identifier": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "D",
                                                    },
                                                }
                                            ],
                                        },
                                    },
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "B2",
                                    },
                                }
                            ],
                            "expression": {
                                "_kind": "MessageAggregate",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "M2"},
                                    "package": None,
                                },
                                "values": {
                                    "_kind": "MessageAggregateAssociations",
                                    "associations": [
                                        {
                                            "_kind": "MessageAggregateAssociation",
                                            "expression": {
                                                "_kind": "Variable",
                                                "identifier": {
                                                    "_kind": "ID",
                                                    "name": {
                                                        "_kind": "UnqualifiedID",
                                                        "_value": "B2",
                                                    },
                                                    "package": None,
                                                },
                                            },
                                            "identifier": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "D",
                                            },
                                        }
                                    ],
                                },
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B1"},
                    }
                ],
                "expression": {
                    "_kind": "MessageAggregate",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "M1"},
                        "package": None,
                    },
                    "values": {
                        "_kind": "MessageAggregateAssociations",
                        "associations": [
                            {
                                "_kind": "MessageAggregateAssociation",
                                "expression": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "B1",
                                        },
                                        "package": None,
                                    },
                                },
                                "identifier": {"_kind": "UnqualifiedID", "_value": "D"},
                            }
                        ],
                    },
                },
            },
        ),
    ],
)
def test_expression_complex(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.extended_expression_rule)
    assert actual == expected


def test_private_type_declaration() -> None:
    string = "type X is private"
    expected = {
        "_kind": "FormalPrivateTypeDecl",
        "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
    }
    actual = parse(string, GrammarRule.session_parameter_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "X : Channel with Readable",
            {
                "_kind": "FormalChannelDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "parameters": [{"_kind": "Readable", "_value": "Readable"}],
            },
        ),
        (
            "X : Channel with Writable",
            {
                "_kind": "FormalChannelDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "parameters": [{"_kind": "Writable", "_value": "Writable"}],
            },
        ),
        (
            "X : Channel with Readable, Writable",
            {
                "_kind": "FormalChannelDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "parameters": [
                    {"_kind": "Readable", "_value": "Readable"},
                    {"_kind": "Writable", "_value": "Writable"},
                ],
            },
        ),
    ],
)
def test_channel_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.session_parameter_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "with function X return Y",
            {
                "_kind": "FormalFunctionDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "parameters": None,
                "return_type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                    "package": None,
                },
            },
        ),
        (
            "with function X (A : B; C : D) return Y",
            {
                "_kind": "FormalFunctionDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                "parameters": {
                    "_kind": "Parameters",
                    "parameters": [
                        {
                            "_kind": "Parameter",
                            "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                            "type_identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                "package": None,
                            },
                        },
                        {
                            "_kind": "Parameter",
                            "identifier": {"_kind": "UnqualifiedID", "_value": "C"},
                            "type_identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "D"},
                                "package": None,
                            },
                        },
                    ],
                },
                "return_type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                    "package": None,
                },
            },
        ),
    ],
)
def test_formal_function_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.session_parameter_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A : B",
            {
                "_kind": "VariableDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "initializer": None,
                "type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                    "package": None,
                },
            },
        ),
        (
            "A : B := C",
            {
                "_kind": "VariableDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "initializer": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "C"},
                        "package": None,
                    },
                },
                "type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                    "package": None,
                },
            },
        ),
        (
            "A : B := 1",
            {
                "_kind": "VariableDecl",
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "initializer": {"_kind": "NumericLiteral", "_value": "1"},
                "type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                    "package": None,
                },
            },
        ),
    ],
)
def test_variable_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.declaration_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A : B renames C.D",
            {
                "_kind": "RenamingDecl",
                "expression": {
                    "_kind": "SelectNode",
                    "expression": {
                        "_kind": "Variable",
                        "identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "C"},
                            "package": None,
                        },
                    },
                    "selector": {"_kind": "UnqualifiedID", "_value": "D"},
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "type_identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                    "package": None,
                },
            },
        ),
    ],
)
def test_renaming_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.declaration_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A := B",
            {
                "_kind": "Assignment",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "B"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        )
    ],
)
def test_assignment_statement(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.assignment_statement_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A.B := C",
            {
                "_kind": "MessageFieldAssignment",
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "C"},
                        "package": None,
                    },
                },
                "message": {"_kind": "UnqualifiedID", "_value": "A"},
                "field": {"_kind": "UnqualifiedID", "_value": "B"},
            },
        )
    ],
)
def test_message_field_assignment_statement(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.message_field_assignment_statement_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A'Append (B)",
            {
                "_kind": "AttributeStatement",
                "attr": {"_kind": "AttrStmtAppend", "_value": "Append"},
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "B"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        ),
        (
            "A'Extend (B)",
            {
                "_kind": "AttributeStatement",
                "attr": {"_kind": "AttrStmtExtend", "_value": "Extend"},
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "B"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        ),
        (
            "A'Read (B)",
            {
                "_kind": "AttributeStatement",
                "attr": {"_kind": "AttrStmtRead", "_value": "Read"},
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "B"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        ),
        (
            "A'Write (B)",
            {
                "_kind": "AttributeStatement",
                "attr": {"_kind": "AttrStmtWrite", "_value": "Write"},
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "B"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        ),
        (
            "C'Reset",
            {
                "_kind": "Reset",
                "identifier": {"_kind": "UnqualifiedID", "_value": "C"},
                "associations": [],
            },
        ),
        (
            "C'Reset (A => 1, B => 2)",
            {
                "_kind": "Reset",
                "identifier": {"_kind": "UnqualifiedID", "_value": "C"},
                "associations": [
                    {
                        "_kind": "MessageAggregateAssociation",
                        "expression": {"_kind": "NumericLiteral", "_value": "1"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    },
                    {
                        "_kind": "MessageAggregateAssociation",
                        "expression": {"_kind": "NumericLiteral", "_value": "2"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                ],
            },
        ),
    ],
)
def test_attribute_statement(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.action_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            """
               state A is
               begin
               transition
                  goto B
               end A
         """,
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [],
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": None,
                    "final_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
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
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [
                        {
                            "_kind": "ConditionalTransition",
                            "condition": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "op": {"_kind": "OpEq", "_value": "="},
                                "right": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Y",
                                        },
                                        "package": None,
                                    },
                                },
                            },
                            "description": None,
                            "target": {"_kind": "UnqualifiedID", "_value": "B"},
                        }
                    ],
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": None,
                    "final_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "C"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
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
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [
                        {
                            "_kind": "ConditionalTransition",
                            "condition": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "X",
                                        },
                                        "package": None,
                                    },
                                },
                                "op": {"_kind": "OpEq", "_value": "="},
                                "right": {
                                    "_kind": "Variable",
                                    "identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Y",
                                        },
                                        "package": None,
                                    },
                                },
                            },
                            "description": {
                                "_kind": "Description",
                                "content": {
                                    "_kind": "StringLiteral",
                                    "_value": '"rfc2549.txt+12:3-45:6"',
                                },
                            },
                            "target": {"_kind": "UnqualifiedID", "_value": "B"},
                        }
                    ],
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": None,
                    "final_transition": {
                        "_kind": "Transition",
                        "description": {
                            "_kind": "Description",
                            "content": {
                                "_kind": "StringLiteral",
                                "_value": '"rfc2549.txt+123:45-678:9"',
                            },
                        },
                        "target": {"_kind": "UnqualifiedID", "_value": "C"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
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
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [],
                    "declarations": [
                        {
                            "_kind": "VariableDecl",
                            "identifier": {"_kind": "UnqualifiedID", "_value": "Z"},
                            "initializer": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Y"},
                                    "package": None,
                                },
                            },
                            "type_identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Boolean"},
                                "package": None,
                            },
                        }
                    ],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": None,
                    "final_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
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
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [],
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "C"},
                    },
                    "final_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
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
            {
                "_kind": "State",
                "body": {
                    "_kind": "StateBody",
                    "actions": [],
                    "conditional_transitions": [],
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    "exception_transition": {
                        "_kind": "Transition",
                        "description": {
                            "_kind": "Description",
                            "content": {
                                "_kind": "StringLiteral",
                                "_value": '"rfc2549.txt+12:3-45:6"',
                            },
                        },
                        "target": {"_kind": "UnqualifiedID", "_value": "C"},
                    },
                    "final_transition": {
                        "_kind": "Transition",
                        "description": None,
                        "target": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                },
                "description": None,
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
            },
        ),
    ],
    ids=range(1, 7),
)
def test_state(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.state_rule)
    assert actual == expected


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
                     goto B
                        if Z = False
                     goto A
                  end A;

                  state B is null state;
               end Session
         """,
            {
                "_kind": "SessionDecl",
                "aspects": {
                    "_kind": "SessionAspects",
                    "final": {"_kind": "UnqualifiedID", "_value": "B"},
                    "initial": {"_kind": "UnqualifiedID", "_value": "A"},
                },
                "declarations": [
                    {
                        "_kind": "VariableDecl",
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Y"},
                        "initializer": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "True"},
                                "package": None,
                            },
                        },
                        "type_identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Boolean"},
                            "package": None,
                        },
                    }
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Session"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Session"},
                "parameters": [
                    {
                        "_kind": "FormalChannelDecl",
                        "identifier": {"_kind": "UnqualifiedID", "_value": "X"},
                        "parameters": [
                            {"_kind": "Readable", "_value": "Readable"},
                            {"_kind": "Writable", "_value": "Writable"},
                        ],
                    },
                    {
                        "_kind": "FormalPrivateTypeDecl",
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                    },
                    {
                        "_kind": "FormalFunctionDecl",
                        "identifier": {"_kind": "UnqualifiedID", "_value": "F"},
                        "parameters": None,
                        "return_type_identifier": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                            "package": None,
                        },
                    },
                ],
                "states": [
                    {
                        "_kind": "State",
                        "body": {
                            "_kind": "StateBody",
                            "actions": [
                                {
                                    "_kind": "Assignment",
                                    "expression": {
                                        "_kind": "Variable",
                                        "identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "False",
                                            },
                                            "package": None,
                                        },
                                    },
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "Z",
                                    },
                                }
                            ],
                            "conditional_transitions": [
                                {
                                    "_kind": "ConditionalTransition",
                                    "condition": {
                                        "_kind": "BinOp",
                                        "left": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Z",
                                                },
                                                "package": None,
                                            },
                                        },
                                        "op": {"_kind": "OpEq", "_value": "="},
                                        "right": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "False",
                                                },
                                                "package": None,
                                            },
                                        },
                                    },
                                    "description": None,
                                    "target": {"_kind": "UnqualifiedID", "_value": "B"},
                                }
                            ],
                            "declarations": [
                                {
                                    "_kind": "VariableDecl",
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "Z",
                                    },
                                    "initializer": {
                                        "_kind": "Variable",
                                        "identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Y",
                                            },
                                            "package": None,
                                        },
                                    },
                                    "type_identifier": {
                                        "_kind": "ID",
                                        "name": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Boolean",
                                        },
                                        "package": None,
                                    },
                                }
                            ],
                            "end_identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                            "exception_transition": None,
                            "final_transition": {
                                "_kind": "Transition",
                                "description": None,
                                "target": {"_kind": "UnqualifiedID", "_value": "A"},
                            },
                        },
                        "description": None,
                        "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                    },
                    {
                        "_kind": "State",
                        "body": {"_kind": "NullStateBody", "_value": "null state"},
                        "description": None,
                        "identifier": {"_kind": "UnqualifiedID", "_value": "B"},
                    },
                ],
            },
        ),
    ],
    ids=[1],
)
def test_session_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.session_declaration_rule)
    assert actual == expected


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "type T is mod 8",
            {
                "_kind": "TypeDecl",
                "definition": {
                    "_kind": "ModularTypeDef",
                    "mod": {"_kind": "NumericLiteral", "_value": "8"},
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                "parameters": None,
            },
        ),
        (
            "type T is sequence of E",
            {
                "_kind": "TypeDecl",
                "definition": {
                    "_kind": "SequenceTypeDef",
                    "element_type": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "E"},
                        "package": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                "parameters": None,
            },
        ),
        (
            "type T is message F : A::B (C => D); end message",
            {
                "_kind": "TypeDecl",
                "definition": {
                    "_kind": "MessageTypeDef",
                    "aspects": [],
                    "message_fields": {
                        "_kind": "MessageFields",
                        "fields": [
                            {
                                "_kind": "MessageField",
                                "aspects": [],
                                "condition": None,
                                "identifier": {"_kind": "UnqualifiedID", "_value": "F"},
                                "thens": [],
                                "type_identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                    "package": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "A",
                                    },
                                },
                                "type_arguments": [
                                    {
                                        "_kind": "TypeArgument",
                                        "expression": {
                                            "_kind": "Variable",
                                            "identifier": {
                                                "_kind": "ID",
                                                "name": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "D",
                                                },
                                                "package": None,
                                            },
                                        },
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "C",
                                        },
                                    }
                                ],
                            }
                        ],
                        "initial_field": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                "parameters": None,
            },
        ),
        (
            "type T (P : A::B) is message F : C::D; end message",
            {
                "_kind": "TypeDecl",
                "definition": {
                    "_kind": "MessageTypeDef",
                    "aspects": [],
                    "message_fields": {
                        "_kind": "MessageFields",
                        "fields": [
                            {
                                "_kind": "MessageField",
                                "aspects": [],
                                "condition": None,
                                "identifier": {"_kind": "UnqualifiedID", "_value": "F"},
                                "thens": [],
                                "type_identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "D"},
                                    "package": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "C",
                                    },
                                },
                                "type_arguments": [],
                            }
                        ],
                        "initial_field": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                "parameters": {
                    "_kind": "Parameters",
                    "parameters": [
                        {
                            "_kind": "Parameter",
                            "identifier": {"_kind": "UnqualifiedID", "_value": "P"},
                            "type_identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "B"},
                                "package": {"_kind": "UnqualifiedID", "_value": "A"},
                            },
                        }
                    ],
                },
            },
        ),
        (
            "type T is message A : Integer; end message with Byte_Order => Low_Order_First",
            {
                "_kind": "TypeDecl",
                "definition": {
                    "_kind": "MessageTypeDef",
                    "aspects": [
                        {
                            "_kind": "ByteOrderAspect",
                            "byte_order": {
                                "_kind": "ByteOrderTypeLoworderfirst",
                                "_value": "Low_Order_First",
                            },
                        }
                    ],
                    "message_fields": {
                        "_kind": "MessageFields",
                        "fields": [
                            {
                                "_kind": "MessageField",
                                "aspects": [],
                                "condition": None,
                                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                                "thens": [],
                                "type_arguments": [],
                                "type_identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Integer"},
                                    "package": None,
                                },
                            }
                        ],
                        "initial_field": None,
                    },
                },
                "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                "parameters": None,
            },
        ),
    ],
)
def test_type_declaration(string: str, expected: Dict[str, str]) -> None:
    actual = parse(string, GrammarRule.type_declaration_rule)
    assert actual == expected
