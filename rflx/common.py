from typing import Iterable, Iterator, Sequence, Set, TypeVar


def generic_eq(self: object, other: object) -> bool:
    if isinstance(other, self.__class__):
        for k in other.__dict__:
            if k != "location" and k not in self.__dict__:
                return False
        for k, v in self.__dict__.items():
            if k != "location" and (k not in other.__dict__ or v != other.__dict__[k]):
                return False
        return True
    return NotImplemented


def generic_repr(class_name: str, obj_dict: dict) -> str:
    args = "\n" + ",\n".join(f"{k}={v!r}" for k, v in obj_dict.items() if k != "location")
    return indent_next(f"\n{class_name}({indent(args, 4)})", 4)


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
    return indent_next(
        f"\n{obj.__class__.__name__}({indent_next(args, indentation)}\n){prefixed_str(obj)}", 4
    )


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
