import pytest

from rflx.rapidflux import RecordFluxError


@pytest.fixture()
def _cleared_error_count() -> None:
    RecordFluxError.reset_errors()
