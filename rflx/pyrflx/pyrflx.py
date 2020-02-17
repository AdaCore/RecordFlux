
from pathlib import Path
from typing import List
from collections import namedtuple

from rflx.parser import Parser, ParserError

from .package import Package
from .message import Message

class PyRFLX(object):

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
            for m in filter(lambda x: x.package == p, messages):
                getattr(self, p)[m.name] = Message(m)
