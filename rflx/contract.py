from __future__ import annotations

if __debug__:  # pragma: no cover
    from icontract import (
        DBC as DBC,
        DBCMeta as DBCMeta,
        ViolationError as ViolationError,
        ensure as ensure,
        invariant as invariant,
        require as require,
        snapshot as snapshot,
    )

else:
    import abc
    import reprlib
    import typing as ty
    from collections.abc import Callable
    from typing import Any, Optional, Union

    class DBCMeta(abc.ABCMeta):  # type: ignore[no-redef]
        pass

    class DBC(metaclass=DBCMeta):  # type: ignore[no-redef]
        pass

    class ViolationError(AssertionError):  # type: ignore[no-redef]
        pass

    def require(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: ARG001
        description: Optional[str] = None,  # noqa: ARG001
        a_repr: Optional[reprlib.Repr] = None,  # noqa: ARG001
        enabled: bool = __debug__,  # noqa: ARG001
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: ARG001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def snapshot(  # type: ignore[no-redef,misc]
        capture: ty.Callable[..., Any],  # noqa: ARG001
        name: Optional[str] = None,  # noqa: ARG001
        enabled: bool = __debug__,  # noqa: ARG001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def ensure(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: ARG001
        description: Optional[str] = None,  # noqa: ARG001
        a_repr: Optional[reprlib.Repr] = None,  # noqa: ARG001
        enabled: bool = __debug__,  # noqa: ARG001
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: ARG001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def invariant(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: ARG001
        description: Optional[str] = None,  # noqa: ARG001
        a_repr: Optional[reprlib.Repr] = None,  # noqa: ARG001
        enabled: bool = __debug__,  # noqa: ARG001
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: ARG001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x
