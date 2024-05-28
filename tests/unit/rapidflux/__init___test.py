import pickle
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from rflx.rapidflux import (
    Annotation,
    ErrorEntry,
    Location,
    RecordFluxError,
    Severity,
)


@pytest.mark.parametrize(
    "obj",
    [
        Severity.ERROR,
        Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
        ErrorEntry(
            "Some error",
            Severity.ERROR,
            Location((1, 1)),
            annotations=[
                Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
            ],
        ),
        RecordFluxError(
            [
                ErrorEntry(
                    "Some error",
                    Severity.ERROR,
                    Location((1, 1)),
                    annotations=[
                        Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
                    ],
                ),
            ],
        ),
    ],
)
def test_pickle(obj: object) -> None:
    with NamedTemporaryFile("w+b") as f:
        pickle.dump(obj, f)
        f.flush()

        with Path(f.name).open("rb") as read_file:
            loaded = pickle.load(read_file)  # noqa: S301
            assert loaded == obj


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
