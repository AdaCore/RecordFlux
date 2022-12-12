if __debug__:  # pragma: no cover

    # pylint: disable=unused-import
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

    # pylint: disable=unused-argument

    def require(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: PEA001
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: PEA001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def snapshot(  # type: ignore[no-redef,misc]
        capture: ty.Callable[..., Any],  # noqa: PEA001
        name: Optional[str] = None,
        enabled: bool = __debug__,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def ensure(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: PEA001
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: PEA001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def invariant(  # type: ignore[no-redef,misc]
        condition: ty.Callable[..., Any],  # noqa: PEA001
        description: Optional[str] = None,
        a_repr: Optional[reprlib.Repr] = None,
        enabled: bool = __debug__,
        error: Optional[Union[ty.Callable[..., Exception], type]] = None,  # noqa: PEA001
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x
