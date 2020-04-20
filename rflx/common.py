from typing import Iterable, Iterator, Set, TypeVar


def generic_repr(class_name: str, obj_dict: dict) -> str:
    args = "\n" + ",\n".join(f"{k}={v!r}" for k, v in obj_dict.items())
    return indent_next(f"\n{class_name}({indent(args, 4)})", 4)


def indent(string: str, indentation: int) -> str:
    return "\n".join((indentation * " " + l if l else "") for l in string.split("\n"))


def indent_next(string: str, indentation: int) -> str:
    return string.replace("\n", "\n" + indentation * " ")


def flat_name(full_name: str) -> str:
    return full_name.replace(".", "_")


def file_name(identifier: str) -> str:
    return identifier.lower().replace(".", "-")


T = TypeVar("T")  # pylint: disable=invalid-name


def unique(iterable: Iterable[T]) -> Iterator[T]:
    seen: Set[T] = set()
    for e in iterable:
        if e not in seen:
            seen.add(e)
            yield e
