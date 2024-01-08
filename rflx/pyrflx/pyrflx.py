from __future__ import annotations

import logging
from collections.abc import Iterable, Iterator, Mapping
from pathlib import Path
from typing import Optional, Union

from rflx.error import FatalError
from rflx.identifier import ID, StrID
from rflx.model import Cache, Model
from rflx.specification import Parser

from .error import PyRFLXError
from .package import Package
from .typevalue import ChecksumFunction, MessageValue, RefinementValue

log = logging.getLogger(__name__)


class PyRFLX:
    def __init__(
        self,
        model: Model,
        checksum_functions: Optional[Mapping[StrID, Mapping[str, ChecksumFunction]]] = None,
        skip_message_verification: bool = False,
    ) -> None:
        """
        Initialize PyRFLX.

        Arguments:
        ---------
        model: The RecordFlux model.
        checksum_functions: A mapping of field identifiers to checksum functions.
        skip_message_verification: Disable some runtime checks for messages to optimize performance.
                                   This can lead to unexpected errors if an invalid field value is
                                   set.
        """
        self._packages: dict[str, Package] = {}
        messages: dict[ID, MessageValue] = {}

        for m in model.messages:
            p = str(m.package)
            if p not in self._packages:
                self._packages[p] = Package(p)
            message = MessageValue(m, skip_verification=skip_message_verification)
            messages[m.identifier] = message
            self._packages[p].set_message(m.name, message)

        for r in model.refinements:
            messages[r.pdu.identifier].add_refinement(
                RefinementValue(r, messages[r.sdu.identifier]),
            )

        self.set_checksum_functions(checksum_functions or {})

    @classmethod
    def from_specs(
        cls,
        files: Iterable[Union[str, Path]],
        cache: Optional[Cache] = None,
        skip_message_verification: bool = False,
    ) -> PyRFLX:
        paths = list(map(Path, files))
        for p in paths:
            if not p.is_file():
                raise FileNotFoundError(f'file not found: "{p}"')
        parser = Parser(cache)
        parser.parse(*paths)
        model = parser.create_model()
        return cls(model, None, skip_message_verification)

    def set_checksum_functions(
        self,
        functions: Mapping[StrID, Mapping[str, ChecksumFunction]],
    ) -> None:
        for identifier, checksum_field_function in functions.items():
            identifier_str = str(identifier)
            try:
                message_identifier = ID(identifier_str)
            except FatalError as e:
                raise PyRFLXError(f'"{identifier_str}" is not a valid identifier: {e}') from e

            if len(message_identifier.parts) < 2:
                raise PyRFLXError(f'"{identifier_str}" is not a valid identifier')

            if str(message_identifier.parent) in self._packages:
                package = self.package(message_identifier.parent)
                package.set_checksum_functions({message_identifier.name: checksum_field_function})

    def package(self, key: StrID) -> Package:
        return self._packages[str(key)]

    def __iter__(self) -> Iterator[Package]:
        return self._packages.values().__iter__()
