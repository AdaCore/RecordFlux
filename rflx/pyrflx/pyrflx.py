import logging
from pathlib import Path
from typing import Dict, Iterator, Sequence

from rflx.model import Model
from rflx.pyrflx.typevalue import MessageValue
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
        packages = set(str(m.package) for m in model.messages)
        for p in packages:
            self.__packages[p] = Package(p)
            for m in [x for x in model.messages if str(x.package) == p]:
                self.__packages[p][str(m.name)] = MessageValue(
                    m, model.refinements, skip_message_verification
                )

    @classmethod
    def from_specs(
        cls,
        files: Sequence[str],
        skip_model_verification: bool = False,
        skip_message_verification: bool = False,
    ) -> "PyRFLX":
        parser = Parser(skip_model_verification)
        for f in files:
            if not Path(f).is_file():
                raise FileNotFoundError(f'file not found: "{f}"')
            parser.parse(Path(f))
        model = parser.create_model()
        return cls(model, skip_message_verification)

    def __getitem__(self, key: str) -> Package:
        return self.__packages[key]

    def __iter__(self) -> Iterator[Package]:
        return self.__packages.values().__iter__()
