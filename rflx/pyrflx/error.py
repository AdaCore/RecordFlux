from __future__ import annotations

from rflx.rapidflux import ErrorEntry, RecordFluxError, Severity


class PyRFLXError(RecordFluxError):
    def __init__(self, entries: list[ErrorEntry] | None = None) -> None:
        super().__init__(entries if entries is not None else [])

    def push_msg(self, message: str) -> None:
        self.push(ErrorEntry(message, Severity.ERROR, None))
