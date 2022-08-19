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
    from typing import Any, Callable, Optional, Union

    class DBCMeta(abc.ABCMeta):  # type: ignore
        pass

    class DBC(metaclass=DBCMeta):  # type: ignore
        pass

    class ViolationError(AssertionError):  # type: ignore
        pass

    # pylint: disable=unused-argument

    def require(  # type: ignore[no-redef,misc]
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def snapshot(  # type: ignore[no-redef,misc]
        capture: Callable[..., Any], name: Optional[str] = None, enabled: bool = __debug__
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def ensure(  # type: ignore[no-redef,misc]
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x

    def invariant(  # type: ignore[no-redef,misc]
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:  # type: ignore[type-arg]
        return lambda x: x
