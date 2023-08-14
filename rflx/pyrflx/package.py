from __future__ import annotations

from collections.abc import Iterator, Mapping
from typing import Optional, Union

from rflx.common import Base
from rflx.identifier import StrID

from .error import PyRFLXError
from .typevalue import ChecksumFunction, MessageValue


class Package(Base):
    def __init__(self, name: str) -> None:
        self._name = name
        self._messages: dict[str, MessageValue] = {}

    @property
    def name(self) -> str:
        return self._name

    def new_message(
        self,
        key: StrID,
        parameters: Optional[Mapping[str, Union[bool, int, str]]] = None,
    ) -> MessageValue:
        message = self._messages[str(key)].clone()
        if parameters:
            message.add_parameters(parameters)
        return message

    def set_message(self, key: StrID, value: MessageValue) -> None:
        self._messages[str(key)] = value

    def __iter__(self) -> Iterator[MessageValue]:
        return self._messages.values().__iter__()

    def set_checksum_functions(
        self,
        functions: Mapping[StrID, Mapping[str, ChecksumFunction]],
    ) -> None:
        for message_name, field_name_to_function_mapping in functions.items():
            name = str(message_name)
            if name not in self._messages:
                raise PyRFLXError(f'"{name}" is not a message in {self._name}')
            self._messages[name].set_checksum_function(field_name_to_function_mapping)
