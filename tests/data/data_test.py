"""Verify correctness of test data."""

import pytest

from rflx import model
from tests.data import models


@pytest.mark.parametrize(
    "message",
    [
        models.NULL_MESSAGE,
        models.TLV_MESSAGE,
        models.ETHERNET_FRAME,
        models.ENUMERATION_MESSAGE,
        models.SEQUENCE_MESSAGE,
        models.EXPRESSION_MESSAGE,
        models.DERIVATION_MESSAGE,
    ],
)
def test_models(message: model.Message) -> None:
    message.verify()
