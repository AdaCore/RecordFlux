import logging
from pathlib import Path
from typing import Dict, Iterator, Sequence

from rflx.identifier import ID
from rflx.model import Model
from rflx.pyrflx.typevalue import MessageValue, RefinementValue
from rflx.specification import Parser

from .package import Package

log = logging.getLogger(__name__)


class PyRFLX:
    def __init__(
        self,
        model: Model,
        skip_message_verification: bool = False,
    ) -> None:
        self.__packages: Dict[str, Package] = {}
        messages: Dict[ID, MessageValue] = {}

        for m in model.messages:
            p = str(m.package)
            if p not in self.__packages:
                self.__packages[p] = Package(p)
            message = MessageValue(m, skip_verification=skip_message_verification)
            messages[m.identifier] = message
            self.__packages[p][str(m.name)] = message

        for r in model.refinements:
            messages[r.pdu.identifier].add_refinement(
                RefinementValue(r, messages[r.sdu.identifier])
            )

    @classmethod
    def from_specs(
        cls,
        files: Sequence[str],
        skip_model_verification: bool = False,
        skip_message_verification: bool = False,
    ) -> "PyRFLX":
        paths = list(map(Path, files))
        for p in paths:
            if not p.is_file():
                raise FileNotFoundError(f'file not found: "{p}"')
        parser = Parser(skip_model_verification)
        parser.parse(*paths)
        model = parser.create_model()
        return cls(model, skip_message_verification)

    def __getitem__(self, key: str) -> Package:
        return self.__packages[key]

    def __iter__(self) -> Iterator[Package]:
        return self.__packages.values().__iter__()
