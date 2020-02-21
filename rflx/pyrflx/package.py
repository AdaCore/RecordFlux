from typing import Dict

from .message import Message


class Package:
    def __init__(self, name: str) -> None:
        self.__name = name
        self.__messages: Dict[str, Message] = {}

    @property
    def name(self) -> str:
        return self.__name

    def __getitem__(self, key: str) -> Message:
        return self.__messages[key]

    def __setitem__(self, key: str, value: Message) -> None:
        self.__messages[key] = value
