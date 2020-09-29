from pathlib import Path

import pytest

from rflx.error import RecordFluxError
from rflx.parser import cache, parser
from tests.models import INVALID_MESSAGE, VALID_MESSAGE

TEST_DIR = Path("specs")
SPEC_DIR = Path("specs")


def test_create_model() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_create_model_cached() -> None:
    p = parser.Parser(cached=True)
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_create_proven_message(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    assert parser.create_proven_message(VALID_MESSAGE, False, c)
    assert c.is_verified(VALID_MESSAGE)


def test_create_proven_message_error(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    with pytest.raises(RecordFluxError):
        parser.create_proven_message(INVALID_MESSAGE, False, c)
    assert not c.is_verified(INVALID_MESSAGE)
