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

    class DBCMeta(abc.ABCMeta):  # type: ignore
        pass

    class DBC(metaclass=DBCMeta):  # type: ignore
        pass

    class ViolationError(AssertionError):  # type: ignore
        pass

    def require(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def snapshot(  # type: ignore[no-redef,misc]
        capture: ty.Callable[..., Any],
        name: Optional[str] = None,
        enabled: bool = __debug__,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def ensure(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def invariant(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x
