from typing import Callable, Dict, Iterator

from rflx.common import Base
from rflx.pyrflx.typevalue import MessageValue


class Package(Base):
    def __init__(self, name: str) -> None:
        self.__name = name
        self.__messages: Dict[str, MessageValue] = {}

    @property
    def name(self) -> str:
        return self.__name

    def __getitem__(self, key: str) -> MessageValue:
        return self.__messages[key].clone()

    def __setitem__(self, key: str, value: MessageValue) -> None:
        self.__messages[key] = value

    def __iter__(self) -> Iterator:
        return self.__messages.values().__iter__()

    def set_checksum_functions(self, functions: Dict[str, Dict[str, Callable]]) -> None:
        for message_name, field_name in functions.items():
            self.__messages[message_name].set_checksum_function(field_name)
