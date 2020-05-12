if __debug__:

    # pylint: disable=unused-import
    from icontract import DBC, DBCMeta, ViolationError, ensure, invariant, require, snapshot

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

    def require(  # type: ignore
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:
        return lambda x: x

    def snapshot(  # type: ignore
        capture: Callable[..., Any], name: Optional[str] = None, enabled: bool = __debug__
    ) -> Callable:
        return lambda x: x

    def ensure(  # type: ignore
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:
        return lambda x: x

    def invariant(  # type: ignore
        condition: Callable[..., Any],
        description: Optional[str] = None,
        a_repr: reprlib.Repr = None,
        enabled: bool = __debug__,
        error: Optional[Union[Callable[..., Exception], type]] = None,
    ) -> Callable:
        return lambda x: x
