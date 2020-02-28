from copy import copy
from typing import Dict, Iterator

from .message import Message


class Package:
    def __init__(self, name: str) -> None:
        self.__name = name
        self.__messages: Dict[str, Message] = {}

    @property
    def name(self) -> str:
        return self.__name

    def __getitem__(self, key: str) -> Message:
        return copy(self.__messages[key])

    def __setitem__(self, key: str, value: Message) -> None:
        self.__messages[key] = value

    def __iter__(self) -> Iterator:
        return self.__messages.values().__iter__()
