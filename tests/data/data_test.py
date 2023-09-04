"""Verify correctness of test data."""

import pytest

from rflx import model
from tests.data import models


@pytest.mark.parametrize(
    "message",
    [
        models.null_message(),
        models.tlv_message(),
        models.ethernet_frame(),
        models.enumeration_message(),
        models.sequence_message(),
        models.expression_message(),
        models.derivation_message(),
    ],
)
def test_models(message: model.Message) -> None:
    message.verify()
