from pathlib import Path

from rflx.rapidflux import source_code


def test_source_code_register_and_retrieve() -> None:
    path = Path("some/path")
    source_code_str = "Foo bar"
    source_code.register(path, source_code_str)
    assert source_code.retrieve(path) == source_code_str


def test_source_code_register_and_retrieve_multiple_files() -> None:
    sources = {
        Path("foo.rflx"): "foo",
        Path("bar.rflx"): "bar",
        Path("baz.rflx"): "baz",
    }

    for path, source_string in sources.items():
        source_code.register(path, source_string)

    for path, source_string in sources.items():
        assert source_code.retrieve(path) == source_string


def test_source_code_retrieve_non_existent() -> None:
    assert source_code.retrieve(Path("non_existent.rflx")) is None
