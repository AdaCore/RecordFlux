from pathlib import Path

from rflx.parser import cache
from tests.data.models import TLV_MESSAGE


def test_init(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path / "Test"
    cache.Cache()
    assert (tmp_path / "Test").is_dir()
    assert (tmp_path / "Test" / cache.VERIFICATION_FILE).is_file()


def test_init_existing(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    with open(tmp_path / cache.VERIFICATION_FILE, "x") as f:
        f.write("{}")
    cache.Cache()
    assert tmp_path.is_dir()
    assert (tmp_path / cache.VERIFICATION_FILE).is_file()


def test_init_disabled(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path / "Test"
    cache.Cache(enabled=False)
    assert not (tmp_path / "Test").is_dir()
    assert not (tmp_path / "Test" / cache.VERIFICATION_FILE).is_file()


def test_verified(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    assert not c.is_verified(TLV_MESSAGE)
    c.add_verified(TLV_MESSAGE)
    assert c.is_verified(TLV_MESSAGE)
    c.add_verified(TLV_MESSAGE)
    assert c.is_verified(TLV_MESSAGE)


def test_verified_disabled(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache(enabled=False)
    assert not c.is_verified(TLV_MESSAGE)
    c.add_verified(TLV_MESSAGE)
    assert not c.is_verified(TLV_MESSAGE)
