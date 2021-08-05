import os
import re
from filecmp import cmp
from pathlib import Path
from typing import Iterator

import pytest
from _pytest.capture import CaptureFixture
from rflx.pyrflx import PyRFLX

from tools.validate_spec import _validate_message, cli


def test_cli_error_msg_not_in_package() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Message",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-f",
                "tests.checksum",
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
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
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
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
            ]
        )

    with pytest.raises(SystemExit, match="2"):
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
            ]
        )


def test_cli_no_test_data_provided() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-o",
                f"{tmp_file}",
                "-f",
                "tests.checksum",
            ]
        )
    ) == f"output file already exists: {tmp_file}"


def test_cli_path_does_not_exist() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/validation_tool/ethernet/non_existent_dir",
                "-f",
                "tests.checksum",
            ]
        )
    ) == "tests/validation_tool/ethernet/non_existent_dir does not exist or is not a directory"


def test_cli_path_is_not_directory(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.txt"
    tmp_file.write_text("")
    assert tmp_file.is_file()
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                f"{tmp_file}",
                "-f",
                "tests.checksum",
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-o",
                f"{tmp_path_restricted}/test.json",
                "-f",
                "tests.checksum",
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
            "tests/validation_tool/in_ethernet.rflx",
            "-m",
            "Ethernet::Frame",
            "-v",
            "tests/validation_tool/ethernet/frame/invalid",
            "-i",
            "tests/validation_tool/ethernet/frame/valid",
            "-f",
            "tests.checksum",
            "--abort-on-error",
            "--no-verification",
        ]
    )
    assert isinstance(ret, str)
    assert re.match(
        r"^aborted: message tests/validation_tool/ethernet/frame/invalid.+\.raw "
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                f"{tmpdir}",
                "-f",
                "tests.checksum",
                "--no-verification",
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
            ]
        )
        == 'invalid identifier: id: error: " " in identifier parts of "Ethernet Frame"'
    )


def test_validation_original_and_parsed_not_equal() -> None:
    ethernet_too_short_value = PyRFLX.from_specs(["ethernet.rflx"], skip_model_verification=True)[
        "Ethernet"
    ]["Frame"]
    validation_result = _validate_message(
        Path("tests/validation_tool/ethernet/frame/invalid/ethernet_invalid_too_long.raw"),
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-f",
                "tests.checksum",
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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-o",
                f"{tmp_path}/output.json",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == 0
    )
    assert cmp(f"{tmp_path}/output.json", "tests/validation_tool/valid_full_output_positive.json")


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
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/invalid",
                "-i",
                "tests/validation_tool/ethernet/frame/valid",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )


def test_validation_negative_full_output(tmp_path: Path) -> None:
    number = len(
        list(Path("tests/validation_tool/ethernet/frame/invalid").glob("*"))
        + list(Path("tests/validation_tool/ethernet/frame/valid").glob("*"))
    )
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/invalid",
                "-i",
                "tests/validation_tool/ethernet/frame/valid",
                "-o",
                f"{tmp_path}/output.json",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == f"{number} messages were classified incorrectly"
    )
    assert cmp(f"{tmp_path}/output.json", "tests/validation_tool/valid_full_output_negative.json")


def test_validation_coverage(capsys: CaptureFixture[str]) -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "--coverage",
                "--target-coverage=100",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == 0
    )
    captured_output = capsys.readouterr()
    with open("tests/validation_tool/valid_cov_output_ethernet.txt", "r") as f:
        valid_output = f.read().replace("Directory: .", f"Directory: {os.getcwd()}")
    assert captured_output.out == valid_output


def test_coverage_threshold_missed(capsys: CaptureFixture[str]) -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-c",
                "--target-coverage=90",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == "missed target coverage of 90.00%, reached 81.08%"
    )
    captured_output = capsys.readouterr()
    with open("tests/validation_tool/valid_cov_output_in_ethernet.txt", "r") as f:
        valid_output = f.read().replace("Directory: .", f"Directory: {os.getcwd()}")
    assert captured_output.out == valid_output


def test_validation_coverage_threshold_invalid() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/in_ethernet.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/validation_tool/ethernet/frame/invalid",
                "-v",
                "tests/validation_tool/ethernet/frame/valid",
                "-c",
                "--target-coverage=110",
                "-f",
                "tests.checksum",
                "--no-verification",
            ]
        )
        == "target coverage must be between 0 and 100, got 110.0"
    )


def test_cli_checksum_positive() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-f",
                "tests.validation_tool.checksum_message_checksum_function",
            ]
        )
        == 0
    )


def test_cli_checksum_negative() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-v",
                "tests/validation_tool/checksum_message/invalid",
                "-i",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.checksum_message_checksum_function",
            ]
        )
        == "3 messages were classified incorrectly"
    )


def test_cli_checksum_import_error() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests/checksum",
            ]
        )
        == "The provided module tests/checksum cannot be imported. "
        "Make sure the module name is provided as package.module and not as a file system path. "
        "No module named 'tests/checksum'"
    )


def test_cli_checksum_missing_attribute() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.missing_checksum_functions_attrib",
                "--no-verification",
            ]
        )
        == "The checksum module at tests.validation_tool.missing_checksum_functions_attrib"
        ' does not contain an attribute with the name "checksum_function".'
    )


def test_cli_checksum_functions_no_checksum_provided() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "--no-verification",
            ]
        )
        == "The following messages define checksum fields, "
        "but no checksum function has been provided: "
        'Checksum_Message::Message at field "Checksum".'
    )


def test_cli_checksum_functions_missing_key() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Ethernet::Frame",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.missing_key",
                "--no-verification",
            ]
        )
        == "The following messages define checksum fields, but no checksum function has been "
        'provided: Checksum_Message::Message at field "Checksum".'
    )


def test_cli_no_callable_checksum() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.missing_checksum_callable",
                "--no-verification",
            ]
        )
        == 'The value at key "Checksum" is not a callable checksum function.'
    )


def test_cli_no_checksum_func_dict() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.missing_checksum_func_dict",
                "--no-verification",
            ]
        )
        == "The value at key Checksum_Message::Message is not a dict."
    )


def test_cli_no_cannot_set_checksum_to_pyrflx() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.message_not_in_package",
                "--no-verification",
            ]
        )
        == "Could not set checksum function to pyrflx: "
        'pyrflx: error: "Package" is not a message in Checksum_Message'
    )


def test_cli_checksum_function_attribute_not_dict() -> None:
    assert (
        cli(
            [
                "validate_spec",
                "-s",
                "tests/validation_tool/checksum_message.rflx",
                "-m",
                "Checksum_Message::Message",
                "-i",
                "tests/validation_tool/checksum_message/invalid",
                "-v",
                "tests/validation_tool/checksum_message/valid",
                "-f",
                "tests.validation_tool.checksum_attribute_not_dict",
                "--no-verification",
            ]
        )
        == "The attribute checksum_function of tests.validation_tool.checksum_attribute_not_dict"
        " is not a dict."
    )
