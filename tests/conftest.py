import re
from typing import Sequence

from rflx.expression import Expr


def pytest_assertrepr_compare(op: str, left: object, right: object) -> Sequence[str]:
    if isinstance(left, Expr) and isinstance(right, Expr) and op == "==":
        return [
            "Expr instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(left)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    return []
