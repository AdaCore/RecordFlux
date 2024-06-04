import pytest

from rflx.rapidflux import RecordFluxError, source_code


@pytest.fixture(autouse=True)
def _clear_stored_source_code() -> None:
    source_code.clear()


@pytest.fixture()
def _cleared_error_count() -> None:
    RecordFluxError.reset_errors()
