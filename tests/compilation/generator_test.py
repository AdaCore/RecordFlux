from pathlib import Path

import pytest

from tests.unit.generator.generator_test import GENERATOR_TEST_CASES, TC
from tests.utils import assert_compilable_code


@pytest.mark.parametrize("tc", GENERATOR_TEST_CASES)
def test_compilability(tc: TC, tmp_path: Path) -> None:
    assert_compilable_code(tc.model(), tc.integration(), tmp_path)
