from __future__ import annotations

from pathlib import Path

import pytest

from tests.const import FEATURE_DIR
from tests.feature import FEATURES, create_model
from tests.utils import assert_equal_code

INCOMPLETE_FEATURE_TESTS = [f for f in FEATURES if not (f / "test.rflx").is_file()]

assert not INCOMPLETE_FEATURE_TESTS


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_parsability_and_model_creation(feature: str) -> None:
    create_model(feature)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_equality(feature: str, tmp_path: Path) -> None:
    generated_dir = FEATURE_DIR / feature / "generated"

    assert generated_dir.is_dir()

    model, integration = create_model(feature)
    assert_equal_code(model, integration, generated_dir, tmp_path)
