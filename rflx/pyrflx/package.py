from typing import Dict, Iterator, Mapping, Union

from rflx.common import Base
from rflx.identifier import StrID

from .error import PyRFLXError
from .typevalue import ChecksumFunction, MessageValue


class Package(Base):
    def __init__(self, name: str) -> None:
        self._name = name
        self._messages: Dict[str, MessageValue] = {}

    @property
    def name(self) -> str:
        return self._name

    def new_message(
        self, key: StrID, parameters: Mapping[str, Union[bool, int, str]] = None
    ) -> MessageValue:
        message = self._messages[str(key)].clone()
        if parameters:
            message.add_parameters(parameters)
        return message

    def set_message(self, key: StrID, value: MessageValue) -> None:
        self._messages[str(key)] = value

    def __iter__(self) -> Iterator[MessageValue]:
        return self._messages.values().__iter__()

    def set_checksum_functions(self, functions: Dict[StrID, Dict[str, ChecksumFunction]]) -> None:
        for message_name, field_name_to_function_mapping in functions.items():
            message_name = str(message_name)
            if message_name not in self._messages:
                raise PyRFLXError(f'"{message_name}" is not a message in {self._name}')
            self._messages[message_name].set_checksum_function(field_name_to_function_mapping)
