import logging
from pathlib import Path
from typing import Dict, List

from rflx.parser import Parser
from rflx.pyrflx.typevalue import MessageValue

from .package import Package

log = logging.getLogger(__name__)


class PyRFLX:
    def __init__(self, files: List[str]) -> None:
        parser = Parser()
        self.__packages: Dict[str, Package] = {}

        for f in files:
            if not Path(f).is_file():
                raise FileNotFoundError(f'file not found: "{f}"')
            parser.parse(Path(f))
        model = parser.create_model()
        packages = set(str(m.package) for m in model.messages)
        for p in packages:
            self.__packages[p] = Package(p)
            for m in [x for x in model.messages if str(x.package) == p]:
                self.__packages[p][str(m.name)] = MessageValue(m, model.refinements)

    def __getitem__(self, key: str) -> Package:
        return self.__packages[key]
