from rflx import ada
from rflx.common import file_name
from rflx.identifier import ID

REFINEMENT_PACKAGE = ID("Contains")

ARITHMETIC_PACKAGE = ID("RFLX_Arithmetic")
BUILTIN_TYPES_CONVERSIONS_PACKAGE = ID("RFLX_Builtin_Types.Conversions")
BUILTIN_TYPES_PACKAGE = ID("RFLX_Builtin_Types")
GENERIC_TYPES_PACKAGE = ID("RFLX_Generic_Types")
MESSAGE_SEQUENCE_PACKAGE = ID("RFLX_Message_Sequence")
SCALAR_SEQUENCE_PACKAGE = ID("RFLX_Scalar_Sequence")
TYPES_PACKAGE = ID("RFLX_Types")

LIBRARY_FILES = [
    file_name(str(p)) + ".ads"
    for p in [
        ARITHMETIC_PACKAGE,
        BUILTIN_TYPES_CONVERSIONS_PACKAGE,
        BUILTIN_TYPES_PACKAGE,
        GENERIC_TYPES_PACKAGE,
        MESSAGE_SEQUENCE_PACKAGE,
        SCALAR_SEQUENCE_PACKAGE,
        TYPES_PACKAGE,
    ]
] + [
    file_name(str(p)) + ".adb"
    for p in [
        ARITHMETIC_PACKAGE,
        GENERIC_TYPES_PACKAGE,
        MESSAGE_SEQUENCE_PACKAGE,
        SCALAR_SEQUENCE_PACKAGE,
    ]
]

TEMPLATE_DIR = ("rflx", "templates/")

TYPES = TYPES_PACKAGE
TYPES_BYTE = TYPES * "Byte"
TYPES_BYTES = TYPES * "Bytes"
TYPES_BYTES_PTR = TYPES * "Bytes_Ptr"
TYPES_INDEX = TYPES * "Index"
TYPES_LENGTH = TYPES * "Length"
TYPES_BIT_INDEX = TYPES * "Bit_Index"
TYPES_BIT_LENGTH = TYPES * "Bit_Length"
TYPES_TO_INDEX = TYPES * "To_Index"
TYPES_TO_LENGTH = TYPES * "To_Length"
TYPES_TO_BIT_LENGTH = TYPES * "To_Bit_Length"
TYPES_TO_FIRST_BIT_INDEX = TYPES * "To_First_Bit_Index"
TYPES_TO_LAST_BIT_INDEX = TYPES * "To_Last_Bit_Index"
TYPES_OFFSET = TYPES * "Offset"
TYPES_U64 = TYPES * "U64"
TYPES_BASE_INT = TYPES * "Base_Integer"
TYPES_BYTE_ORDER = TYPES * "Byte_Order"
TYPES_HIGH_ORDER_FIRST = TYPES * "High_Order_First"
TYPES_LOW_ORDER_FIRST = TYPES * "Low_Order_First"

UNREACHABLE = ada.Call(TYPES * "Unreachable")

CONFIGURATION_PRAGMAS = [
    ada.Pragma("Style_Checks", [ada.String("N3aAbCdefhiIklnOprStux")]),
    # https://github.com/Componolit/RecordFlux/issues/508
    ada.Pragma("Warnings", [ada.Variable("Off"), ada.String("redundant conversion")]),
]
