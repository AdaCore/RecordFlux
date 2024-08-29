import multiprocessing
from pathlib import Path
from typing import Final

from rflx.rapidflux import ID, consts

BUILTINS_PACKAGE: Final = ID(consts.BUILTINS_PACKAGE)
INTERNAL_PACKAGE: Final = ID(consts.INTERNAL_PACKAGE)

MAX_SCALAR_SIZE: Final = consts.MAX_SCALAR_SIZE

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
