
import rflx.model as Model

class Message:

    def __init__(self, model: Model.Message) -> None:
        self.__model = model
        self.__mutable = False

    def new(self) -> "Message":
        m = Message(self.__model)
        m.__mutable = True
        return m

    @property
    def model(self) -> Model.Message:
        return self.__model
