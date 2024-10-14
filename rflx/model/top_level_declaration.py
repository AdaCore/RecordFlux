from __future__ import annotations

from abc import abstractmethod
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from rflx import const
from rflx.common import Base
from rflx.identifier import ID, StrID
from rflx.rapidflux import NO_LOCATION, ErrorEntry, Location, RecordFluxError, Severity


class TopLevelDeclaration(Base):
    def __init__(self, identifier: StrID, location: Location = NO_LOCATION) -> None:
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

    def check_style(
        self,
        error: RecordFluxError,
        style_checks: dict[Path, frozenset[const.StyleCheck]],
    ) -> None:
        pass

    def _check_identifier(self) -> None:
        if len(self.identifier.parts) != 2:
            self.error.extend(
                [
                    ErrorEntry(
                        f'invalid format for identifier "{self.identifier}"',
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
