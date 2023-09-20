from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass
from typing import Optional, Sequence

from rflx.common import Base
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID


class TopLevelDeclaration(Base):
    def __init__(self, identifier: StrID, location: Optional[Location] = None) -> None:
        self.identifier = ID(identifier)
        self.location = location
        self.error = RecordFluxError()

        self._check_identifier()

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

    def _check_identifier(self) -> None:
        if len(self.identifier.parts) != 2:
            self.error.extend(
                [
                    (
                        f'invalid format for identifier "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.identifier.location,
                    ),
                ],
            )


@dataclass
class UncheckedTopLevelDeclaration(Base):
    identifier: ID

    @property
    def package(self) -> ID:
        return self.identifier.parent

    @abstractmethod
    def checked(
        self,
        declarations: Sequence[TopLevelDeclaration],
        skip_verification: bool = False,
        workers: int = 1,
    ) -> TopLevelDeclaration:
        raise NotImplementedError
