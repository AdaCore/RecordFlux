from rflx.error import RecordFluxError, Severity, Subsystem


class PyRFLXError(RecordFluxError):
    def __init__(self, message: str) -> None:
        super().__init__()
        self.extend([(message, Subsystem.PYRFLX, Severity.ERROR, None)])
