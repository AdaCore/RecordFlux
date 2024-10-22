from __future__ import annotations

import sys
import traceback
import types
from collections.abc import Callable
from typing import Final

from rflx.version import is_gnat_tracker_release, version

BUG_BOX_MSG_GITHUB: Final = """
A bug was detected. Please report this issue on GitHub:

https://github.com/AdaCore/RecordFlux/issues/new?labels=bug
"""

BUG_BOX_MSG_GNAT_TRACKER: Final = """
A bug was detected. Please submit a bug report using GNATtracker at
https://support.adacore.com/csm by logging in and clicking the
'Create A New Case' button. Alternatively, submit a bug report by email to
support@adacore.com, including your account number #xxxx in the subject line.
Use a subject line meaningful to you and us to track the bug.
"""


class FatalErrorHandler:
    def __init__(self, output_func: Callable[[str], None], unsafe: bool = False) -> None:
        self._output_func = output_func
        self._unsafe = unsafe

    def __enter__(self) -> None:
        pass

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        tb: types.TracebackType | None,
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

{BUG_BOX_MSG_GNAT_TRACKER if is_gnat_tracker_release() else BUG_BOX_MSG_GITHUB}

Include the complete content of the bug box shown above and all input files
in the report. Use plain ASCII or MIME attachment(s)."""
    return result
