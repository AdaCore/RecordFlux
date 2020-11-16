from typing import Any

import librecordfluxdsllang as rflxdsl  # type: ignore


def parse_buffer(
    data: str, rule: rflxdsl.GrammarRule = rflxdsl.GrammarRule.main_rule_rule
) -> rflxdsl.AnalysisUnit:
    ctx = rflxdsl.AnalysisContext()
    unit = ctx.get_from_buffer("text.rflx", data, rule=rule)
    del ctx
    return unit


def to_dict(node: Any) -> Any:
    if node is None:
        return None
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        result["_kind"] = node.kind_name
        return result
    return {"_kind": node.kind_name, "_value": node.text}


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
        rule=rflxdsl.GrammarRule.type_declaration_rule,
    )
    assert to_dict(unit.root) == {
        "_kind": "TypeSpec",
        "definition": {
            "_kind": "ModularTypeDef",
            "mod": {
                "_kind": "MathematicalExpression",
                "data": {
                    "_kind": "BinOp",
                    "left": {"_kind": "NumericLiteral", "_value": "2"},
                    "op": {"_kind": "OpPow", "_value": "**"},
                    "right": {"_kind": "NumericLiteral", "_value": "9"},
                },
            },
        },
        "identifier": {"_kind": "UnqualifiedID", "_value": "Modular_Type"},
    }


def test_checksum_attributes() -> None:
    unit = parse_buffer(
        """
            A'Valid_Checksum and B'Valid_Checksum;
        """,
        rule=rflxdsl.GrammarRule.boolean_expression_rule,
    )
    assert to_dict(unit.root) == {
        "_kind": "BooleanExpression",
        "data": {
            "_kind": "BinOp",
            "left": {
                "identifier": {"_kind": "UnqualifiedID", "_value": "A"},
                "_kind": "Attribute",
                "kind": {"_kind": "AttrValidChecksum", "_value": "Valid_Checksum"},
            },
            "op": {"_kind": "OpAnd", "_value": "and"},
            "right": {
                "identifier": {"_kind": "UnqualifiedID", "_value": "B"},
                "_kind": "Attribute",
                "kind": {"_kind": "AttrValidChecksum", "_value": "Valid_Checksum"},
            },
        },
    }


def test_operator_precedence() -> None:
    unit = parse_buffer(
        """
            A / 8 >= 46 and A / 8 <= 1500
        """,
        rule=rflxdsl.GrammarRule.expression_rule,
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
        rule=rflxdsl.GrammarRule.expression_rule,
    )
    assert len(unit.diagnostics) == 0, "\n".join(str(d) for d in unit.diagnostics)
    assert to_dict(unit.root) == {
        "_kind": "Negation",
        "data": {"_kind": "NumericLiteral", "_value": "16#20_000#"},
    }
