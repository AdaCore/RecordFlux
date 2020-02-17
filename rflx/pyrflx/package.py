
import rflx.model as Model
from .message import Message

class Package:

    def __init__(self, name: str) -> None:
        self.__name = name

    @property
    def name(self) -> str:
        return self.__name

    def __getitem__(self, key: str) -> Message:
        return getattr(self, key)

    def __setitem__(self, key: str, value: Message) -> None:
        setattr(self, key, value)

