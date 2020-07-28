import os
import re
from typing import Any, Sequence

import hypothesis
import pytest

from rflx.expression import Expr
from tests.utils import BASE_TMP_DIR

hypothesis.settings.register_profile(
    "default", max_examples=100, verbosity=hypothesis.Verbosity.verbose
)
hypothesis.settings.register_profile(
    "ci", deadline=None, max_examples=1000, verbosity=hypothesis.Verbosity.verbose
)

hypothesis.settings.load_profile(os.environ.get("HYPOTHESIS_PROFILE", "default"))


def pytest_configure(config: Any) -> None:
    config.addinivalue_line(
        "markers", "verification: Tests which use formal verification. Not run by default."
    )
    config.addinivalue_line(
        "markers", "root: Tests which need root privileges. Not run by default."
    )
    if BASE_TMP_DIR:
        config.addinivalue_line("addopts", f"--basetemp={BASE_TMP_DIR}")
        os.makedirs(BASE_TMP_DIR, exist_ok=True)


def pytest_collection_modifyitems(config: Any, items: list) -> None:
    markexpr = config.getoption("-m")
    for marker in ["verification", "root"]:
        if marker not in markexpr.split(" "):
            for item in items:
                if any(m.name == marker for m in item.own_markers):
                    item.add_marker(pytest.mark.skip(reason="need '-m {marker}' option to run"))


def pytest_assertrepr_compare(op: str, left: object, right: object) -> Sequence[str]:
    if isinstance(left, Expr) and isinstance(right, Expr) and op == "==":
        return [
            "Expr instances",
            "repr:",
            *[f"    {l}" for l in ("Actual:   " + repr(left)).split("\n")],
            *[f"    {l}" for l in ("Expected: " + repr(right)).split("\n")],
            "str:",
            "    Actual:   " + re.sub(r"\n +", " ", str(left)),
            "    Expected: " + re.sub(r"\n +", " ", str(right)),
        ]
    return []
