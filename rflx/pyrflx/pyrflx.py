import logging
from pathlib import Path
from typing import Dict, List

from rflx.parser import Parser

from .message import Message
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
        packages = set(m.package for m in model.messages)
        for p in packages:
            self.__packages[p] = Package(p)
            for m in [x for x in model.messages if x.package == p]:
                try:
                    self.__packages[p][m.name] = Message(m)
                except ValueError as e:
                    log.warning("Ignoring message %s: %s", m.name, e)

    def __getitem__(self, key: str) -> Package:
        return self.__packages[key]
