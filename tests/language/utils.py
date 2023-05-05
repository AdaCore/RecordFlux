from typing import Any

from rflx_lang import AnalysisContext


def to_dict(node: Any) -> Any:  # type: ignore[misc]
    if node is None:
        return None
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        result["_kind"] = node.kind_name
        return result
    return {"_kind": node.kind_name, "_value": node.text}


def parse(string: str, rule: str) -> object:
    unit = AnalysisContext().get_from_buffer("<stdin>", string, rule=rule)
    assert len(unit.diagnostics) == 0, str(unit.diagnostics)
    return to_dict(unit.root)
