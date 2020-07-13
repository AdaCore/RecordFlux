from typing import Any, Mapping, Sequence

import pytest

from rflx.error import Location, RecordFluxError
from rflx.model import Field, Link, Message, Type


def assert_equal(left: Any, right: Any) -> None:
    assert left == right


def assert_message_model_error(
    structure: Sequence[Link], types: Mapping[Field, Type], regex: str, location: Location = None
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Message("P.M", structure, types, location)
