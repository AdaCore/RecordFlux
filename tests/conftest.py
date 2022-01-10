import glob
import os
import re
from typing import Sequence

import hypothesis
from _pytest.config import Config

from rflx import expression as expr
from tests.const import FIXTURE_DIR

hypothesis.settings.register_profile(
    "default", max_examples=100, verbosity=hypothesis.Verbosity.verbose
)
hypothesis.settings.register_profile(
    "ci", deadline=None, max_examples=200, verbosity=hypothesis.Verbosity.verbose
)

hypothesis.settings.load_profile(os.environ.get("HYPOTHESIS_PROFILE", "default"))


def pytest_configure(config: Config) -> None:
    config.addinivalue_line(
        "markers", "compilation: Tests which use GNAT to compile Ada/SPARK code."
    )
    config.addinivalue_line(
        "markers", "verification: Tests which use GNATprove to formally verify SPARK code."
    )


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
    return []


pytest_plugins = [
    re.sub(r"[/\\]", ".", fixture.replace(".py", ""))
    for fixture in glob.glob(f"{FIXTURE_DIR}/*.py")
]
