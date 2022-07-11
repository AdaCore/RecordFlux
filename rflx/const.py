from rflx.identifier import ID

BUILTINS_PACKAGE = ID("__BUILTINS__")
INTERNAL_PACKAGE = ID("__INTERNAL__")

# https://github.com/Componolit/RecordFlux/issues/1077
# size of integers is limited to 63bits

MAX_SCALAR_SIZE = 63
