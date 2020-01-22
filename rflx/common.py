from typing import Iterator, List, Set, TypeVar


def indent(string: str, indentation: int) -> str:
    return "\n".join((indentation * " " + l if l else "") for l in string.split("\n"))


def indent_next(string: str, indentation: int) -> str:
    return string.replace("\n", "\n" + indentation * " ")


def verify_identifier(name: str) -> None:
    assert " " not in name, f'whitespace in identifier "{name}"'


T = TypeVar("T")  # pylint: disable=invalid-name


def unique(iterable: List[T]) -> Iterator[T]:
    seen: Set[T] = set()
    for e in iterable:
        if e not in seen:
            seen.add(e)
            yield e
