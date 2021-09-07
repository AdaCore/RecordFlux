from rflx.common import Base
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID


class BasicDeclaration(Base):
    def __init__(
        self, identifier: StrID, location: Location = None, error: RecordFluxError = None
    ) -> None:
        identifier = ID(identifier)
        self.error = error or RecordFluxError()

        if len(identifier.parts) != 2:
            self.error.extend(
                [
                    (
                        f'invalid format for identifier "{identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        identifier.location,
                    )
                ],
            )

        self.identifier = identifier
        self.location = location

    def __hash__(self) -> int:
        return hash(self.identifier)

    @property
    def full_name(self) -> str:
        return str(self.identifier)

    @property
    def name(self) -> str:
        return str(self.identifier.name)

    @property
    def package(self) -> ID:
        return self.identifier.parent
