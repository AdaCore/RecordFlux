import pytest

from rflx.rapidflux import RecordFluxError, logging, source_code


@pytest.fixture(autouse=True)
def _clear_stored_source_code() -> None:
    source_code.clear()


@pytest.fixture(autouse=True)
def _clear_quiet_logging() -> None:
    logging.set_quiet(False)


@pytest.fixture(autouse=True)
def _reset_errors() -> None:
    RecordFluxError.set_max_error(0)
    RecordFluxError.reset_errors()
