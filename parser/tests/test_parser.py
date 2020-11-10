from typing import Any

import librecordfluxdsllang as rflxdsl  # type: ignore

ctx = rflxdsl.AnalysisContext()


def to_dict(node: Any) -> Any:
    if node is None:
        return ""
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        return result
    return node.text


def test_empty_file() -> None:
    unit = ctx.get_from_buffer("empty.rflx", "")
    assert unit.root is None


def test_empty_package() -> None:
    unit = ctx.get_from_buffer(
        "empty_package.rflx",
        """
            package Empty_Package is
            end Empty_Package;
        """,
    )
    assert to_dict(unit.root) == {
        "context_clause": [],
        "package_declaration": {
            "identifier": "Empty_Package",
            "declarations": [],
            "end_identifier": "Empty_Package",
        },
    }


def test_modular_type() -> None:
    unit = ctx.get_from_buffer(
        "modular.rflx",
        """
            type Modular_Type is mod 2 ** 9;
        """,
        rule=rflxdsl.GrammarRule.type_declaration_rule,
    )
    assert to_dict(unit.root) == {
        "identifier": "Modular_Type",
        "definition": {"mod": {"data": {"left": "2", "op": "**", "right": "9"}}},
    }


def test_checksum_attributes() -> None:
    unit = ctx.get_from_buffer(
        "test.rflx",
        """
            A'Valid_Checksum and B'Valid_Checksum;
        """,
        rule=rflxdsl.GrammarRule.boolean_expression_rule,
    )
    assert to_dict(unit.root) == {
        "data": {
            "left": {"identifier": "A", "kind": "Valid_Checksum"},
            "op": "and",
            "right": {"identifier": "B", "kind": "Valid_Checksum"},
        }
    }


def test_operator_precedence() -> None:
    unit = ctx.get_from_buffer(
        "test.rflx",
        """
            A / 8 >= 46 and A / 8 <= 1500
        """,
        rule=rflxdsl.GrammarRule.expression_rule,
    )
    assert to_dict(unit.root) == {
        "data": {
            "left": {
                "left": {
                    "left": {"identifier": {"name": "A", "package": ""}},
                    "op": "/",
                    "right": "8",
                },
                "op": ">=",
                "right": "46",
            },
            "op": "and",
            "right": {
                "left": {
                    "left": {"identifier": {"name": "A", "package": ""}},
                    "op": "/",
                    "right": "8",
                },
                "op": "<=",
                "right": "1500",
            },
        }
    }
