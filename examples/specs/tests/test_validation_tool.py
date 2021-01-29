import re
from pathlib import Path
from typing import Iterator

import pytest

from tools.validate_spec import cli


def test_cli_error_msg_not_in_package() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Message",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )
        == 'message "Message" could not be found in package "Ethernet"'
    )


def test_cli_spec_file_not_found() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "non_existent_file.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )
        == 'specification file not found: "non_existent_file.rflx"'
    )


def test_cli_required_arg_not_provided() -> None:
    with pytest.raises(SystemExit, match="2"):
        cli(
            [
                "validate_spec",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )

    with pytest.raises(SystemExit, match="2"):
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )


def test_cli_no_test_data_provided() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
            ]
        )
        == "must provide directory with valid and/or invalid messages"
    )


def test_cli_output_file_exists(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.json"
    tmp_file.write_text("")
    assert tmp_file.is_file()
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
                "-o",
                f"{tmp_file}",
            ]
        )
    ) == f"output file already exists: {tmp_file}"


def test_cli_path_does_not_exist() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/data/ethernet/non_existent_dir",
            ]
        )
    ) == "tests/data/ethernet/non_existent_dir does not exist or is not a directory"


def test_cli_path_is_not_directory(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.txt"
    tmp_file.write_text("")
    assert tmp_file.is_file()
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                f"{tmp_file}",
            ]
        )
    ) == f"{tmp_file} does not exist or is not a directory"


@pytest.fixture(name="tmp_path_restricted")
def fixture_tmp_path_restricted(tmp_path: Path) -> Iterator[Path]:
    tmp_path.chmod(0o100)
    yield tmp_path
    tmp_path.chmod(0o700)


def test_cli_cannot_open_output_file(tmp_path_restricted: Path) -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
                "-o",
                f"{tmp_path_restricted}/test.json",
            ]
        )
        == f"cannot open output file {tmp_path_restricted}/test.json:"
        f" [Errno 13] Permission denied: '{tmp_path_restricted}/test.json'"
    )


def test_cli_abort_on_error() -> None:
    ret = cli(
        [
            "validate_spec",
            "-s",
            "in_ethernet.rflx",
            "-m",
            "Ethernet::Frame",
            "-v",
            "tests/data/ethernet/frame/invalid",
            "-i",
            "tests/data/ethernet/frame/valid",
            "--abort-on-error",
        ]
    )
    assert isinstance(ret, str)
    assert re.match(
        r"^(tests/data/ethernet/frame/invalid/).+(\.raw) (classified as FalseNegative)$", ret
    )


def test_cli_not_regular_file(tmpdir: Path) -> None:
    subdir = tmpdir / "test"
    subdir.mkdir()
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                f"{tmpdir}",
            ]
        )
        == f"{subdir} is not a regular file"
    )


def test_cli_invalid_identifier() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet.Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )
        == 'invalid identifier "Ethernet.Frame" : id: error: "." in identifier parts'
    )


def test_validation_positive() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/valid",
                "-i",
                "tests/data/ethernet/frame/invalid",
            ]
        )
        == 0
    )


def test_validation_negative() -> None:
    number = len(
        list(Path("tests/data/ethernet/frame/invalid").glob("*"))
        + list(Path("tests/data/ethernet/frame/valid").glob("*"))
    )
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/invalid",
                "-i",
                "tests/data/ethernet/frame/valid",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )


def test_cli_full_json_output(tmp_path: Path) -> None:
    number = len(
        list(Path("tests/data/ethernet/frame/invalid").glob("*"))
        + list(Path("tests/data/ethernet/frame/valid").glob("*"))
    )
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/data/ethernet/frame/invalid",
                "-i",
                "tests/data/ethernet/frame/valid",
                "-o",
                f"{tmp_path}/output.json",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )
