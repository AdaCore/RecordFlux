import multiprocessing
from pathlib import Path
from typing import Final

from rflx.identifier import ID

BUILTINS_PACKAGE: Final = ID("__BUILTINS__")
INTERNAL_PACKAGE: Final = ID("__INTERNAL__")

# TODO(eng/recordflux/RecordFlux#1077): Size of integers is limited to 63 bits
MAX_SCALAR_SIZE: Final = 63

CACHE_PATH: Final = Path.home() / ".cache" / "RecordFlux"

# The use of "forkserver" or "spawn" as start method for starting processes prevents deadlocks when
# RecordFlux is executed by another process, e.g., when the language server is started by VS Code.
MP_CONTEXT = multiprocessing.get_context("forkserver")
