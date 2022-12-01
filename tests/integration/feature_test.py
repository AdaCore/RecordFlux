from __future__ import annotations

from pathlib import Path

import pytest

from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.utils import assert_equal_code

MAIN = "main.adb"
FEATURES = [f for f in Path(__file__).parent.glob("*") if f.is_dir() and f.name != "__pycache__"]
INCOMPLETE_FEATURE_TESTS = [f for f in FEATURES if not (f / "test.rflx").is_file()]

assert not INCOMPLETE_FEATURE_TESTS


def create_model(feature: str) -> tuple[Model, Integration]:
    parser = Parser()
    parser.parse(Path("tests/integration") / feature / "test.rflx")
    return parser.create_model(), parser.get_integration()


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_parsability_and_model_creation(feature: str) -> None:
    create_model(feature)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_equality(feature: str, tmp_path: Path) -> None:
    generated_dir = Path(__file__).parent / feature / "generated"

    if not generated_dir.is_dir():
        pytest.skip()

    model, integration = create_model(feature)
    assert_equal_code(model, integration, generated_dir, tmp_path)
