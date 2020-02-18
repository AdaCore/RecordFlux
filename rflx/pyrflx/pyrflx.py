from pathlib import Path
from typing import List

from rflx.parser import Parser

from .message import Message
from .package import Package


class PyRFLX:
    def __init__(self, files: List[str]) -> None:
        parser = Parser()

        for f in files:
            if not Path(f).is_file():
                raise FileNotFoundError(f'file not found: "{f}"')
            parser.parse(f)
        messages = parser.messages
        refinements = parser.refinements
        packages = set(m.package for m in messages)
        for p in packages:
            setattr(self, p, Package(p))
            for m in [x for x in messages if x.package == p]:
                getattr(self, p)[m.name] = Message(m)
