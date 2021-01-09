import librflxlang

from tests.utils import to_dict


def parse_buffer(
    data: str, rule: librflxlang.GrammarRule = librflxlang.GrammarRule.main_rule_rule
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
            "_kind": "PackageSpec",
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
