from typing import Dict, Iterator

from rflx.common import generic_repr
from rflx.pyrflx.typevalue import MessageValue


class Package:
    def __init__(self, name: str) -> None:
        self.__name = name
        self.__messages: Dict[str, MessageValue] = {}

    @property
    def name(self) -> str:
        return self.__name

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def __getitem__(self, key: str) -> MessageValue:
        return self.__messages[key].clone()

    def __setitem__(self, key: str, value: MessageValue) -> None:
        self.__messages[key] = value

    def __iter__(self) -> Iterator:
        return self.__messages.values().__iter__()
