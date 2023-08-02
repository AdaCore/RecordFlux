from __future__ import annotations

import os
import re
import textwrap
from collections.abc import Iterable, Iterator, Sequence
from pathlib import Path
from typing import NoReturn, TypeVar


def format_repr(string: str) -> str:
    """
    Improve formatting of string representation.

    When `RFLX_TESTING` is set, Black is used to format the string representation. This makes
    pytest diffs of complex objects more readable, especially when using
    [pytest-clarity](https://github.com/darrenburns/pytest-clarity) or
    [pytest-icdiff](https://github.com/hjwp/pytest-icdiff).
    """
    if os.environ.get("RFLX_TESTING"):
        from black import FileMode, format_str

        return format_str(string, mode=FileMode(line_length=60))

    return string


STDIN = Path("<stdin>")


class Base:
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            for k in other.__dict__:
                if k not in ["location", "error"] and k not in self.__dict__:
                    return False
            for k, v in self.__dict__.items():
                if k not in ["location", "error"] and (
                    k not in other.__dict__ or v != other.__dict__[k]
                ):
                    return False
            return True
        return NotImplemented

    def __repr__(self) -> str:
        args = "\n" + ",\n".join(f"{k}={v!r}" for k, v in self.__dict__.items() if k != "location")
        return format_repr(indent_next(f"\n{self.__class__.__name__}({indent(args, 4)})", 4))


def verbose_repr(obj: object, attributes: Sequence[str]) -> str:
    def prefixed_str(obj: object) -> str:
        obj_str = str(obj)
        return (
            "\n" + "\n".join(f"# {l}" if l else "#" for l in obj_str.split("\n")) + "\n"
            if obj_str
            else ""
        )

    indentation = len(obj.__class__.__name__) + 1
    args = "".join(f"{a}={getattr(obj, a)!r},\n" for a in attributes)
    return format_repr(
        indent_next(
            f"\n{obj.__class__.__name__}({indent_next(args, indentation)}\n){prefixed_str(obj)}",
            4,
        ),
    )


def indent(string: str, indentation: int) -> str:
    return textwrap.indent(string, indentation * " ")


def indent_next(string: str, indentation: int) -> str:
    return string.replace("\n", "\n" + indentation * " ")


def file_name(name: str) -> str:
    return re.sub(r"(?:\.|::)", "-", name.lower())


T = TypeVar("T")


def unique(iterable: Iterable[T]) -> Iterator[T]:
    # In Python 3.7+, standard `dict` is guaranteed to preserve order:
    # https://stackoverflow.com/a/39980744
    return iter(dict.fromkeys(iterable))


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f'unhandled value "{value}" ({type(value).__name__})'
