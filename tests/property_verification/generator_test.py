import math

import pytest
from hypothesis import HealthCheck, given, settings

from rflx.integration import Integration
from rflx.model import Model
from tests import utils
from tests.property import strategies


@given(strategies.models())
@settings(
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
    max_examples=math.ceil(settings.default.max_examples / 200),
)
def test_code_verification(tmp_path_factory: pytest.TempPathFactory, model: Model) -> None:
    utils.assert_provable_code(model, Integration(), tmp_path_factory.mktemp("code_verification"))
