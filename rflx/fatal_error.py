from __future__ import annotations

import sys
import traceback
import types
from typing import Callable, Optional

from rflx.version import version


class FatalErrorHandler:
    def __init__(self, output_func: Callable[[str], None], unsafe: bool = False) -> None:
        self._output_func = output_func
        self._unsafe = unsafe

    def __enter__(self) -> None:
        pass

    def __exit__(
        self,
        exc_type: Optional[type[BaseException]],
        exc_value: Optional[BaseException],
        tb: Optional[types.TracebackType],
    ) -> None:
        if exc_type is not None:
            self._output_func(fatal_error_message(self._unsafe))
            sys.exit(2)


def fatal_error_message(unsafe: bool) -> str:
    result = ""
    if unsafe:
        result += f"""
----------------------------------------------------------------------------
EXCEPTION IN UNSAFE MODE, PLEASE RERUN WITHOUT UNSAFE OPTIONS
----------------------------------------------------------------------------
{traceback.format_exc()}
----------------------------------------------------------------------------"""
    result += f"""
------------------------------ RecordFlux Bug ------------------------------
{version()}

Optimized: {not __debug__}

Command: {' '.join(sys.argv)}

{traceback.format_exc()}
----------------------------------------------------------------------------

A bug was detected. Please report this issue on GitHub:

https://github.com/AdaCore/RecordFlux/issues/new?labels=bug

Include the complete content of the bug box shown above and all input files
in the report."""
    return result
