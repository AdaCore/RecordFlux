import math
from typing import Any

import pytest
from hypothesis import HealthCheck, given, settings

from rflx.model import Model
from tests import utils
from tests.property import strategies


@given(strategies.models())
@settings(
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
    max_examples=math.ceil(settings.default.max_examples / 10),
)
def test_code_compilation(tmp_path_factory: Any, model: Model) -> None:
    utils.assert_compilable_code(model, tmp_path_factory.mktemp("code_compilation"))


@pytest.mark.verification
@given(strategies.models())
@settings(
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
    max_examples=math.ceil(settings.default.max_examples / 200),
)
def test_code_verification(tmp_path_factory: Any, model: Model) -> None:
    utils.assert_provable_code(model, tmp_path_factory.mktemp("code_verification"))
