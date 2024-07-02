import multiprocessing
from pathlib import Path
from typing import Final

from rflx.identifier import ID

BUILTINS_PACKAGE: Final = ID("__BUILTINS__")
INTERNAL_PACKAGE: Final = ID("__INTERNAL__")

# TODO(eng/recordflux/RecordFlux#1077): Size of integers is limited to 63 bits
MAX_SCALAR_SIZE: Final = 63

CACHE_PATH: Final = Path.cwd() / ".rflx_cache"

# The use of "forkserver" or "spawn" as start method for starting processes prevents deadlocks when
# RecordFlux is executed by another process, e.g., when the language server is started by VS Code.
MP_CONTEXT = multiprocessing.get_context("forkserver")

RESERVED_WORDS = [
    # Ada
    "abort",
    "abs",
    "abstract",
    "accept",
    "access",
    "aliased",
    "all",
    "and",
    "array",
    "at",
    "begin",
    "body",
    "case",
    "constant",
    "declare",
    "delay",
    "delta",
    "digits",
    "do",
    "else",
    "elsif",
    "end",
    "entry",
    "exception",
    "exit",
    "for",
    "function",
    "generic",
    "goto",
    "if",
    "in",
    "interface",
    "is",
    "limited",
    "loop",
    "mod",
    "new",
    "not",
    "null",
    "of",
    "or",
    "others",
    "out",
    "overriding",
    "package",
    "parallel",
    "pragma",
    "private",
    "procedure",
    "protected",
    "raise",
    "range",
    "record",
    "rem",
    "renames",
    "requeue",
    "return",
    "reverse",
    "select",
    "separate",
    "some",
    "subtype",
    "synchronized",
    "tagged",
    "task",
    "terminate",
    "then",
    "type",
    "until",
    "use",
    "when",
    "while",
    "with",
    "xor",
    # Code generation
    "initial",
    "final",
    "ctx",
    "val",
    "enum",
]
