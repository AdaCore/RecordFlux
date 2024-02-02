from __future__ import annotations

import os
import re
from collections.abc import Sequence

import hypothesis

from rflx import expression as expr, ir, model
from tests.const import FIXTURE_DIR

hypothesis.settings.register_profile(
    "default",
    max_examples=100,
    verbosity=hypothesis.Verbosity.verbose,
)
hypothesis.settings.register_profile(
    "ci",
    deadline=None,
    max_examples=200,
    verbosity=hypothesis.Verbosity.verbose,
)

hypothesis.settings.load_profile(os.environ.get("HYPOTHESIS_PROFILE", "default"))


def pytest_assertrepr_compare(op: str, left: object, right: object) -> Sequence[str]:
    if isinstance(left, expr.Expr) and isinstance(right, expr.Expr) and op == "==":
        return [
            "Expr instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    if isinstance(left, ir.Stmt) and isinstance(right, ir.Stmt) and op == "==":
        return [
            "Stmt instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    if isinstance(left, ir.Expr) and isinstance(right, ir.Expr) and op == "==":
        return [
            "Expr instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    if isinstance(left, model.Message) and isinstance(right, model.Message) and op == "==":
        return [
            "Message instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    if isinstance(left, model.Session) and isinstance(right, model.Session) and op == "==":
        return [
            "Session instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    if isinstance(left, ir.ComplexExpr) and isinstance(right, ir.ComplexExpr) and op == "==":
        return [
            "ComplexExpr instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    return []


pytest_plugins = [
    re.sub(r"[/\\]", ".", str(fixture).replace(".py", "")) for fixture in FIXTURE_DIR.glob("*.py")
]
