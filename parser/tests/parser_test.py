from __future__ import annotations

import re

import librflxlang
import pytest

from language.lexer import rflx_lexer
from tests.utils import parse, to_dict


def parse_buffer(
    data: str, rule: str = librflxlang.GrammarRule.main_rule_rule
) -> librflxlang.AnalysisUnit:
    ctx = librflxlang.AnalysisContext()
    unit = ctx.get_from_buffer("text.rflx", data, rule=rule)
    del ctx
    return unit


def test_empty_file() -> None:
    unit = parse_buffer("")
    assert unit.root is None


def test_empty_package() -> None:
    unit = parse_buffer(
        """
            package Empty_Package is
            end Empty_Package;
        """,
    )
    assert to_dict(unit.root) == {
        "context_clause": [],
        "_kind": "Specification",
        "package_declaration": {
            "declarations": [],
            "end_identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
            "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
            "_kind": "PackageNode",
        },
    }


def test_modular_type() -> None:
    unit = parse_buffer(
        """
            type Modular_Type is mod 2 ** 9;
        """,
        rule=librflxlang.GrammarRule.type_declaration_rule,
    )
    assert to_dict(unit.root) == {
        "_kind": "TypeDecl",
        "definition": {
            "_kind": "ModularTypeDef",
            "mod": {
                "_kind": "BinOp",
                "left": {"_kind": "NumericLiteral", "_value": "2"},
                "op": {"_kind": "OpPow", "_value": "**"},
                "right": {"_kind": "NumericLiteral", "_value": "9"},
            },
        },
        "identifier": {"_kind": "UnqualifiedID", "_value": "Modular_Type"},
        "parameters": None,
    }


def test_checksum_attributes() -> None:
    unit = parse_buffer(
        """
            A'Valid_Checksum and B'Valid_Checksum;
        """,
        rule=librflxlang.GrammarRule.expression_rule,
    )
    assert to_dict(unit.root) == {
        "_kind": "BinOp",
        "left": {
            "_kind": "Attribute",
            "expression": {
                "_kind": "Variable",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "A"},
                    "package": None,
                },
            },
            "kind": {"_kind": "AttrValidChecksum", "_value": "Valid_Checksum"},
        },
        "op": {"_kind": "OpAnd", "_value": "and"},
        "right": {
            "_kind": "Attribute",
            "expression": {
                "_kind": "Variable",
                "identifier": {
                    "_kind": "ID",
                    "name": {"_kind": "UnqualifiedID", "_value": "B"},
                    "package": None,
                },
            },
            "kind": {"_kind": "AttrValidChecksum", "_value": "Valid_Checksum"},
        },
    }


def test_operator_precedence() -> None:
    unit = parse_buffer(
        """
            A / 8 >= 46 and A / 8 <= 1500
        """,
        rule=librflxlang.GrammarRule.expression_rule,
    )
    assert to_dict(unit.root) == {
        "_kind": "BinOp",
        "left": {
            "_kind": "BinOp",
            "left": {
                "_kind": "BinOp",
                "left": {
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                    "_kind": "Variable",
                },
                "op": {"_kind": "OpDiv", "_value": "/"},
                "right": {"_kind": "NumericLiteral", "_value": "8"},
            },
            "op": {"_kind": "OpGe", "_value": ">="},
            "right": {"_kind": "NumericLiteral", "_value": "46"},
        },
        "op": {"_kind": "OpAnd", "_value": "and"},
        "right": {
            "_kind": "BinOp",
            "left": {
                "_kind": "BinOp",
                "left": {
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                    "_kind": "Variable",
                },
                "op": {"_kind": "OpDiv", "_value": "/"},
                "right": {"_kind": "NumericLiteral", "_value": "8"},
            },
            "op": {"_kind": "OpLe", "_value": "<="},
            "right": {"_kind": "NumericLiteral", "_value": "1500"},
        },
    }


def test_negative_number() -> None:
    unit = parse_buffer(
        """
            -16#20_000#
        """,
        rule=librflxlang.GrammarRule.expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
        "_kind": "Negation",
        "data": {"_kind": "NumericLiteral", "_value": "16#20_000#"},
    }


def test_selector_precedence1() -> None:
    unit = parse_buffer(
        "X.B = Z",
        rule=librflxlang.GrammarRule.extended_expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
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
            "selector": {
                "_kind": "UnqualifiedID",
                "_value": "B",
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
    }


def test_selector_precedence2() -> None:
    unit = parse_buffer(
        "X.B'Size",
        rule=librflxlang.GrammarRule.extended_expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
        "_kind": "Attribute",
        "expression": {
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
        "kind": {"_kind": "AttrSize", "_value": "Size"},
    }


def test_selector_precedence3() -> None:
    unit = parse_buffer(
        "X'Head.B",
        rule=librflxlang.GrammarRule.extended_expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
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
        "selector": {"_kind": "UnqualifiedID", "_value": "B"},
    }


def test_suffix_precedence() -> None:
    unit = parse_buffer(
        "2**X'Size",
        rule=librflxlang.GrammarRule.extended_expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
        "_kind": "BinOp",
        "left": {"_kind": "NumericLiteral", "_value": "2"},
        "op": {"_kind": "OpPow", "_value": "**"},
        "right": {
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
    }


KEYWORDS = [l for l in rflx_lexer.literals_map if re.match("[A-Za-z_]+", l)]

KEYWORD_TESTS = [
    (keyword, t.format(keyword=keyword), r)
    for (t, r) in [
        (
            "for some {keyword} in {keyword} => {keyword} + 1",
            librflxlang.GrammarRule.quantified_expression_rule,
        ),
        (
            "[for {keyword} in {keyword} if {keyword} > {keyword} => {keyword} - 1]",
            librflxlang.GrammarRule.comprehension_rule,
        ),
        (
            "{keyword} (Variable + {keyword})",
            librflxlang.GrammarRule.call_rule,
        ),
        (
            "{keyword} => {keyword}",
            librflxlang.GrammarRule.message_aggregate_association_rule,
        ),
        (
            "Data.{keyword}",
            librflxlang.GrammarRule.extended_suffix_rule,
        ),
        (
            "{keyword} where {keyword} = {keyword}",
            librflxlang.GrammarRule.extended_suffix_rule,
        ),
        (
            "{keyword} => True",
            librflxlang.GrammarRule.aspect_rule,
        ),
        (
            "then {keyword} with First => {keyword}'First if {keyword} > {keyword}",
            librflxlang.GrammarRule.then_rule,
        ),
        (
            "{keyword} : {keyword};",
            librflxlang.GrammarRule.message_field_rule,
        ),
        (
            "{keyword} => (1, 30..34)",
            librflxlang.GrammarRule.checksum_association_rule,
        ),
        (
            "{keyword}, {keyword}, Elem_1, Elem_2",
            librflxlang.GrammarRule.positional_enumeration_rule,
        ),
        (
            "{keyword} => 42",
            librflxlang.GrammarRule.element_value_association_rule,
        ),
        (
            "type {keyword} is new {keyword}",
            librflxlang.GrammarRule.type_declaration_rule,
        ),
        (
            "for {keyword} use ({keyword} => {keyword}) if {keyword} > {keyword}",
            librflxlang.GrammarRule.type_refinement_rule,
        ),
        (
            "{keyword} : {keyword}",
            librflxlang.GrammarRule.parameter_rule,
        ),
        (
            "with function {keyword} return {keyword}",
            librflxlang.GrammarRule.formal_function_declaration_rule,
        ),
        (
            "{keyword} : Channel with Readable, Writable",
            librflxlang.GrammarRule.channel_declaration_rule,
        ),
        (
            "{keyword} : {keyword} renames {keyword}",
            librflxlang.GrammarRule.renaming_declaration_rule,
        ),
        (
            "{keyword} : {keyword} := {keyword}",
            librflxlang.GrammarRule.variable_declaration_rule,
        ),
        (
            "{keyword} := {keyword}",
            librflxlang.GrammarRule.assignment_statement_rule,
        ),
        (
            "{keyword}'Append ({keyword})",
            librflxlang.GrammarRule.list_attribute_rule,
        ),
        (
            "{keyword}'Reset",
            librflxlang.GrammarRule.reset_rule,
        ),
        (
            "goto {keyword} if {keyword} = {keyword}",
            librflxlang.GrammarRule.conditional_transition_rule,
        ),
        (
            'goto {keyword} with Desc => "foo"',
            librflxlang.GrammarRule.transition_rule,
        ),
        (
            'goto {keyword} with Desc => "foo"',
            librflxlang.GrammarRule.transition_rule,
        ),
        (
            "begin transition goto {keyword} end {keyword}",
            librflxlang.GrammarRule.state_body_rule,
        ),
        (
            "generic session {keyword} is begin end {keyword}",
            librflxlang.GrammarRule.session_declaration_rule,
        ),
        (
            """
                package {keyword} is
                end {keyword};
            """,
            librflxlang.GrammarRule.package_declaration_rule,
        ),
        (
            "with {keyword};",
            librflxlang.GrammarRule.context_item_rule,
        ),
    ]
    for keyword in [k.lower() for k in KEYWORDS] + [k.lower().capitalize() for k in KEYWORDS]
]


@pytest.mark.parametrize(
    "text,rule",
    [(t, r) for _, t, r in KEYWORD_TESTS],
    ids=[f"{k}->{r[:-5]}" for (k, _, r) in KEYWORD_TESTS],
)
def test_keyword_identifiers(text: str, rule: str) -> None:
    """
    Test that keywords can be used as identifiers.

    New keywords must be added to the unqualified_identifier rule in the parser module to allow the
    use as identifiers.
    """
    unit = parse_buffer(text, rule=rule)
    assert len(unit.diagnostics) == 0, text + "\n".join(str(d) for d in unit.diagnostics)


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "(case A is when V1 => 1, when V2 => 2)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "1"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V1"},
                                "package": None,
                            }
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "2"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V2"},
                                "package": None,
                            }
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when T::V1 => 1, when T::V2 => 2)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "1"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V1"},
                                "package": {"_kind": "UnqualifiedID", "_value": "T"},
                            }
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "2"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V2"},
                                "package": {"_kind": "UnqualifiedID", "_value": "T"},
                            }
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when V1 | T::V2 => 1, when V3 | T::V4 => 2)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "1"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V1"},
                                "package": None,
                            },
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V2"},
                                "package": {"_kind": "UnqualifiedID", "_value": "T"},
                            },
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "2"},
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V3"},
                                "package": None,
                            },
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V4"},
                                "package": {"_kind": "UnqualifiedID", "_value": "T"},
                            },
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when 1 => 8, when 2 => 16)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "8"},
                        "selectors": [{"_kind": "NumericLiteral", "_value": "1"}],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "16"},
                        "selectors": [{"_kind": "NumericLiteral", "_value": "2"}],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when 1 | 2 => 8, when 3 | 4 => 16)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "8"},
                        "selectors": [
                            {"_kind": "NumericLiteral", "_value": "1"},
                            {"_kind": "NumericLiteral", "_value": "2"},
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {"_kind": "NumericLiteral", "_value": "16"},
                        "selectors": [
                            {"_kind": "NumericLiteral", "_value": "3"},
                            {"_kind": "NumericLiteral", "_value": "4"},
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when V1 => Length + 2, when V2 => 2 * Length)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Length"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpAdd", "_value": "+"},
                            "right": {"_kind": "NumericLiteral", "_value": "2"},
                        },
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V1"},
                                "package": None,
                            }
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {
                            "_kind": "BinOp",
                            "left": {"_kind": "NumericLiteral", "_value": "2"},
                            "op": {"_kind": "OpMul", "_value": "*"},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Length"},
                                    "package": None,
                                },
                            },
                        },
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "V2"},
                                "package": None,
                            }
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
        (
            "(case A is when True => F1, when False => F2 or F3)",
            {
                "_kind": "CaseExpression",
                "choices": [
                    {
                        "_kind": "Choice",
                        "expression": {
                            "_kind": "Variable",
                            "identifier": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "F1"},
                                "package": None,
                            },
                        },
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "True"},
                                "package": None,
                            }
                        ],
                    },
                    {
                        "_kind": "Choice",
                        "expression": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "F2"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpOr", "_value": "or"},
                            "right": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "F3"},
                                    "package": None,
                                },
                            },
                        },
                        "selectors": [
                            {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "False"},
                                "package": None,
                            }
                        ],
                    },
                ],
                "expression": {
                    "_kind": "Variable",
                    "identifier": {
                        "_kind": "ID",
                        "name": {"_kind": "UnqualifiedID", "_value": "A"},
                        "package": None,
                    },
                },
            },
        ),
    ],
)
def test_extended_case_expression(string: str, expected: dict[str, str]) -> None:
    actual = parse(string, rule=librflxlang.GrammarRule.extended_expression_rule)
    assert actual == expected
