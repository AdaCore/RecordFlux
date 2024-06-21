import pytest

from rflx.rapidflux import RecordFluxError, logging, source_code


@pytest.fixture(autouse=True)
def _clear_stored_source_code() -> None:
    source_code.clear()


@pytest.fixture(autouse=True)
def _clear_quiet_logging() -> None:
    logging.set_quiet(False)


@pytest.fixture()
def _cleared_error_count() -> None:
    RecordFluxError.reset_errors()
