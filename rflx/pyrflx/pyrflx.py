from pathlib import Path
from typing import Dict, List

from rflx.parser import Parser

from .message import Message
from .package import Package


class PyRFLX:
    def __init__(self, files: List[str]) -> None:
        parser = Parser()
        self.__packages: Dict[str, Package] = {}

        for f in files:
            if not Path(f).is_file():
                raise FileNotFoundError(f'file not found: "{f}"')
            parser.parse(f)
        messages = parser.messages
        packages = set(m.package for m in messages)
        for p in packages:
            self.__packages[p] = Package(p)
            for m in [x for x in messages if x.package == p]:
                self.__packages[p][m.name] = Message(m)

    def __getitem__(self, key: str) -> Package:
        return self.__packages[key]
