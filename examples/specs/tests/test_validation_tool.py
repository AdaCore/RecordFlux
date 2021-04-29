import os
import re
from filecmp import cmp
from pathlib import Path
from typing import Any, Iterator

import pytest
from rflx.pyrflx import PyRFLX

from tools.validate_spec import _validate_message, cli


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
                "--no-verification",
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
                "--no-verification",
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
                "--no-verification",
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
            "--no-verification",
        ]
    )
    assert isinstance(ret, str)
    assert re.match(
        r"^aborted: message tests/data/ethernet/frame/invalid/.+\.raw "
        r"was classified incorrectly$",
        ret,
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


def test_validation_original_and_parsed_not_equal() -> None:
    ethernet_too_short_value = PyRFLX.from_specs(["ethernet.rflx"], skip_model_verification=True)[
        "Ethernet"
    ]["Frame"]
    validation_result = _validate_message(
        Path("tests/data/ethernet/frame/invalid/ethernet_invalid_too_long.raw"),
        True,
        ethernet_too_short_value,
    )
    assert (
        validation_result.parser_error
        == "message parsed by PyRFLX is shorter than the original message"
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
                "--no-verification",
            ]
        )
        == 0
    )


def test_validation_positive_full_output(tmp_path: Path) -> None:
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
                f"{tmp_path}/output.json",
                "--no-verification",
            ]
        )
        == 0
    )
    assert cmp(f"{tmp_path}/output.json", "tests/data/valid_full_output_positive.json")


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
                "--no-verification",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )


def test_validation_negative_full_output(tmp_path: Path) -> None:
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
                "--no-verification",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )
    assert cmp(f"{tmp_path}/output.json", "tests/data/valid_full_output_negative.json")


def test_validation_coverage(capsys: Any) -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/coverage/ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/coverage/ethernet/frame/valid",
                "-i",
                "tests/coverage/ethernet/frame/invalid",
                "--coverage",
                "--target-coverage=100",
                "--no-verification",
            ]
        )
        == 0
    )
    captured_output = capsys.readouterr()
    with open("tests/coverage/valid_cov_output_ethernet.txt", "r") as f:
        valid_output = valid_output = f.read().replace("Directory: .", f"Directory: {os.getcwd()}")
    assert captured_output.out == valid_output


def test_coverage_threshold_missed(capsys: Any) -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/coverage/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/coverage/ethernet/frame/invalid",
                "-v",
                "tests/coverage/ethernet/frame/valid",
                "-c",
                "--target-coverage=80",
                "--no-verification",
            ]
        )
        == "missed target coverage of 80.00%, reached 22.06%"
    )
    captured_output = capsys.readouterr()
    with open("tests/coverage/valid_cov_output_in_ethernet.txt", "r") as f:
        valid_output = f.read().replace("Directory: .", f"Directory: {os.getcwd()}")
    assert captured_output.out == valid_output
