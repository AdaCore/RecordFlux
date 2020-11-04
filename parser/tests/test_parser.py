from typing import Any

import librecordfluxdsllang as rflxdsl  # type: ignore

ctx = rflxdsl.AnalysisContext()


def to_dict(node: Any) -> Any:
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
