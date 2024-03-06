from pathlib import Path

from rflx.error import Location


def test_location() -> None:
    l = Location((1, 2), Path("foo"), (3, 4))
    assert l.start == (1, 2)
    assert l.source == Path("foo")
    assert l.end == (3, 4)

    l = Location((1, 2))
    assert l.start == (1, 2)
    assert l.source is None
    assert l.end is None


def test_location_repr() -> None:
    assert repr(Location((1, 2))) == "Location((1, 2), None, None)"
    assert repr(Location((1, 2), None, (3, 4))) == "Location((1, 2), None, (3, 4))"
    assert repr(Location((1, 2), Path("foo"), (3, 4))) == 'Location((1, 2), "foo", (3, 4))'
