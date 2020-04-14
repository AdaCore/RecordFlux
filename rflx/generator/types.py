from rflx.identifier import ID
from rflx.model import BUILTIN_TYPES


class Types:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def prefixed(self, type_name: ID) -> ID:
        if type_name in BUILTIN_TYPES:
            return type_name
        return self.prefix * type_name

    @property
    def prefixed_builtin_types(self) -> ID:
        return self.prefix * ID("Builtin_Types")

    @property
    def prefixed_builtin_conversions(self) -> ID:
        return self.prefix * ID("Builtin_Types.Conversions")

    @property
    def prefixed_generic_types(self) -> ID:
        return self.prefix * ID("Generic_Types")

    @property
    def prefixed_types(self) -> ID:
        return self.prefix * ID("Types")

    @property
    def types(self) -> ID:
        return ID("Types")

    @property
    def byte(self) -> ID:
        return self.types * "Byte"

    @property
    def bytes(self) -> ID:
        return self.types * "Bytes"

    @property
    def bytes_ptr(self) -> ID:
        return self.types * "Bytes_Ptr"

    @property
    def index(self) -> ID:
        return self.types * "Index"

    @property
    def length(self) -> ID:
        return self.types * "Length"

    @property
    def bit_index(self) -> ID:
        return self.types * "Bit_Index"

    @property
    def bit_length(self) -> ID:
        return self.types * "Bit_Length"

    @property
    def byte_index(self) -> ID:
        return self.types * "Byte_Index"

    @property
    def first_bit_index(self) -> ID:
        return self.types * "First_Bit_Index"

    @property
    def last_bit_index(self) -> ID:
        return self.types * "Last_Bit_Index"

    @property
    def offset(self) -> ID:
        return self.types * "Offset"

    @property
    def unreachable_bit_length(self) -> ID:
        return self.types * "Unreachable_Bit_Length"
