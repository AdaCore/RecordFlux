import os
from pathlib import Path
from typing import Iterator, Sequence

import pytest
from _pytest.capture import CaptureFixture

from rflx import expression as expr
from rflx.identifier import ID
from rflx.pyrflx import PyRFLX
from rflx.validator import (
    ValidationError,
    _expand_expression,
    _validate_message,
    initialize_pyrflx,
    validate,
)
from tests.const import SPEC_DIR

TEST_DIR = Path("tests/data/validator")
CHECKSUM_MODULE = "tests.data.validator.checksum"


def test_initialize_pyrflx_spec_file_not_found() -> None:
    with pytest.raises(
        ValidationError, match=r'^specification file not found: "non_existent_file.rflx"$'
    ):
        initialize_pyrflx(["non_existent_file.rflx"], CHECKSUM_MODULE, skip_model_verification=True)


def test_initialize_pyrflx_checksum_import_error() -> None:
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            r'provided module "tests/checksum" cannot be imported, make sure module name is'
            r' provided as "package.module" and not as file system path:'
            r" No module named 'tests/checksum'"
            r"$"
        ),
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests/checksum",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_no_checksum_provided() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^missing checksum definition for field "C" of "Checksum_Message::Message"$',
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_missing_attribute() -> None:
    checksum_module = "tests.data.validator.checksum_missing_attribute"
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf'missing attribute "checksum_function" in checksum module "{checksum_module}"'
            r"$"
        ),
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            checksum_module,
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_missing_field() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^missing checksum definition for field "C" of "Checksum_Message::Message"$',
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_missing_field",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_function_type() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^value at key "Checksum" is not a callable checksum function$',
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_function_type",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_field_dict_type() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^value at key "Checksum_Message::Message" is not a dict$',
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_field_dict_type",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_message() -> None:
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            r"invalid checksum definition: "
            r'pyrflx: error: "Invalid_Message" is not a message in Checksum_Message'
            r"$"
        ),
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_message",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_field() -> None:
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            r"invalid checksum definition: "
            r"pyrflx: error: cannot set checksum function: field Invalid_Field is not defined"
            r"$"
        ),
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_field",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_attribute_type() -> None:
    checksum_module = "tests.data.validator.checksum_invalid_attribute_type"
    with pytest.raises(
        ValidationError,
        match=rf'^attribute "checksum_function" of "{checksum_module}" is not a dict$',
    ):
        initialize_pyrflx(
            [SPEC_DIR / "checksum_message.rflx"],
            checksum_module,
            skip_model_verification=True,
        )


def test_validate_error_msg_not_in_package() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError, match=r'^message "Message" could not be found in package "Ethernet"$'
    ):
        validate(ID("Ethernet::Message"), pyrflx, None, None, None)


@pytest.fixture(name="tmp_path_restricted")
def fixture_tmp_path_restricted(tmp_path: Path) -> Iterator[Path]:
    tmp_path.chmod(0o100)
    yield tmp_path
    tmp_path.chmod(0o700)


def test_validate_cannot_open_output_file(tmp_path_restricted: Path) -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"cannot open output file {tmp_path_restricted}/test.json:"
            rf" \[Errno 13\] Permission denied: '{tmp_path_restricted}/test.json'"
            r"$"
        ),
    ):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/valid",
            TEST_DIR / "ethernet/frame/invalid",
            tmp_path_restricted / "test.json",
        )


def test_validate_abort_on_error() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"aborted: message {TEST_DIR}/ethernet/frame/invalid.+\.raw "
            r"was classified incorrectly"
            r"$"
        ),
    ):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/valid",
            TEST_DIR / "ethernet/frame/invalid",
            abort_on_error=True,
        )


def test_validate_not_regular_file(tmp_path: Path) -> None:
    subdir = tmp_path / "test.raw"
    subdir.mkdir()
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError,
        match=(r"^" rf"{subdir} is not a regular file" r"$"),
    ):
        validate(ID("Ethernet::Frame"), pyrflx, tmp_path)


def test_validate_positive() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    validate(
        ID("Ethernet::Frame"),
        pyrflx,
        TEST_DIR / "ethernet/frame/invalid",
        TEST_DIR / "ethernet/frame/valid",
    )


def test_validate_positive_output(tmp_path: Path) -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    validate(
        ID("Ethernet::Frame"),
        pyrflx,
        TEST_DIR / "ethernet/frame/invalid",
        TEST_DIR / "ethernet/frame/valid",
        tmp_path / "output.json",
    )
    assert (tmp_path / "output.json").read_text() == (TEST_DIR / "output_positive.json").read_text(
        encoding="utf-8"
    )


def test_validate_negative() -> None:
    number = len(
        list((TEST_DIR / "ethernet/frame/invalid").glob("*.raw"))
        + list((TEST_DIR / "ethernet/frame/valid").glob("*.raw"))
    )
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{number} messages were classified incorrectly$",
    ):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/valid",
            TEST_DIR / "ethernet/frame/invalid",
        )


def test_validate_negative_output(tmp_path: Path) -> None:
    number = len(
        list((TEST_DIR / "ethernet/frame/invalid").glob("*.raw"))
        + list((TEST_DIR / "ethernet/frame/valid").glob("*.raw"))
    )
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{number} messages were classified incorrectly$",
    ):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/valid",
            TEST_DIR / "ethernet/frame/invalid",
            tmp_path / "output.json",
        )
    assert (tmp_path / "output.json").read_text() == (TEST_DIR / "output_negative.json").read_text(
        encoding="utf-8"
    )


def test_validate_coverage(capsys: CaptureFixture[str]) -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    validate(
        ID("Ethernet::Frame"),
        pyrflx,
        TEST_DIR / "ethernet/frame/invalid",
        TEST_DIR / "ethernet/frame/valid",
        coverage=True,
        target_coverage=100,
    )
    expected_output = f"""model: warning: model verification skipped
{TEST_DIR}/ethernet/frame/valid/802.3-LLC-CDP.raw                      PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1AD-802.1Q-IPv4.raw            PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-802.1Q-IPv4-ICMP.raw        PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-CDP.raw                 PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-STP.raw                 PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_802.3.raw                     PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_double_vlan_tag.raw           PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_ipv4_udp.raw                  PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_vlan_tag.raw                  PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_802.3_invalid_length.raw    PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_invalid_too_long.raw        PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_invalid_too_short.raw       PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_undefined.raw               PASSED


--------------------------------------------------------------------------------
                     RecordFlux Validation Coverage Report
Directory: {os.getcwd()}
--------------------------------------------------------------------------------
File                                          Links       Used        Coverage
ethernet.rflx                                    10         10         100.00%
--------------------------------------------------------------------------------
TOTAL                                            10         10         100.00%
--------------------------------------------------------------------------------
"""
    assert capsys.readouterr().out == expected_output


def test_coverage_threshold_missed(capsys: CaptureFixture[str]) -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(ValidationError, match=r"missed target coverage of 90.00%, reached 73.68%"):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/invalid",
            TEST_DIR / "ethernet/frame/valid",
            coverage=True,
            target_coverage=90,
        )
    expected_output = f"""model: warning: model verification skipped
{TEST_DIR}/ethernet/frame/valid/802.3-LLC-CDP.raw                      PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1AD-802.1Q-IPv4.raw            PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-802.1Q-IPv4-ICMP.raw        PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-CDP.raw                 PASSED
{TEST_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-STP.raw                 PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_802.3.raw                     PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_double_vlan_tag.raw           PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_ipv4_udp.raw                  PASSED
{TEST_DIR}/ethernet/frame/valid/ethernet_vlan_tag.raw                  PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_802.3_invalid_length.raw    PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_invalid_too_long.raw        PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_invalid_too_short.raw       PASSED
{TEST_DIR}/ethernet/frame/invalid/ethernet_undefined.raw               PASSED


--------------------------------------------------------------------------------
                     RecordFlux Validation Coverage Report
Directory: {os.getcwd()}
--------------------------------------------------------------------------------
File                                          Links       Used        Coverage
ipv4.rflx                                        28         18          64.29%
ethernet.rflx                                    10         10         100.00%
--------------------------------------------------------------------------------
TOTAL                                            38         28          73.68%
--------------------------------------------------------------------------------


================================================================================
                                Uncovered Links
================================================================================


                                   ipv4.rflx
--------------------------------------------------------------------------------
{SPEC_DIR}/ipv4.rflx:21:10: missing link          Initial          ->        Copied
{SPEC_DIR}/ipv4.rflx:21:27: missing link          Copied           ->     Option_Class
{SPEC_DIR}/ipv4.rflx:22:38: missing link       Option_Class        ->    Option_Number
{SPEC_DIR}/ipv4.rflx:24:13: missing link       Option_Number       ->        Final
{SPEC_DIR}/ipv4.rflx:26:13: missing link       Option_Number       ->    Option_Length
{SPEC_DIR}/ipv4.rflx:29:17: missing link       Option_Length       ->     Option_Data
{SPEC_DIR}/ipv4.rflx:30:20: missing link       Option_Length       ->     Option_Data
{SPEC_DIR}/ipv4.rflx:31:20: missing link       Option_Length       ->     Option_Data
{SPEC_DIR}/ipv4.rflx:32:20: missing link       Option_Length       ->     Option_Data
{SPEC_DIR}/ipv4.rflx:34:50: missing link        Option_Data        ->        Final
"""
    assert capsys.readouterr().out == expected_output


def test_validate_coverage_threshold_invalid() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "in_ethernet.rflx"], CHECKSUM_MODULE, skip_model_verification=True
    )
    with pytest.raises(
        ValidationError, match=r"^target coverage must be between 0 and 100, got 110$"
    ):
        validate(
            ID("Ethernet::Frame"),
            pyrflx,
            TEST_DIR / "ethernet/frame/invalid",
            TEST_DIR / "ethernet/frame/valid",
            coverage=True,
            target_coverage=110,
        )


def test_validate_checksum_positive() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "checksum_message.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    validate(
        ID("Checksum_Message::Message"),
        pyrflx,
        TEST_DIR / "checksum_message/invalid",
        TEST_DIR / "checksum_message/valid",
    )


def test_validate_pyrflx_checksum_negative() -> None:
    pyrflx = initialize_pyrflx(
        [SPEC_DIR / "checksum_message.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(ValidationError, match=r"^3 messages were classified incorrectly$"):
        validate(
            ID("Checksum_Message::Message"),
            pyrflx,
            TEST_DIR / "checksum_message/valid",
            TEST_DIR / "checksum_message/invalid",
        )


def test_validate_message_original_and_parsed_not_equal() -> None:
    ethernet_too_short_value = (
        PyRFLX.from_specs([SPEC_DIR / "ethernet.rflx"], skip_model_verification=True)
        .package("Ethernet")
        .new_message("Frame")
    )
    validation_result = _validate_message(
        Path(TEST_DIR / "ethernet/frame/invalid/ethernet_invalid_too_long.raw"),
        valid_original_message=True,
        message_value=ethernet_too_short_value,
    )
    assert (
        validation_result.parser_error
        == "message parsed by PyRFLX is shorter than the original message"
    )


def test_validate_message_parameterized_message() -> None:
    message = (
        PyRFLX.from_specs(["tests/data/specs/parameterized.rflx"], skip_model_verification=True)
        .package("Parameterized")
        .new_message("Message")
    )
    validation_result = _validate_message(
        Path(TEST_DIR / "parameterized/message/valid/parameterized_message.raw"),
        valid_original_message=True,
        message_value=message,
    )
    assert validation_result.validation_success


@pytest.mark.parametrize(
    "expression,expected",
    [
        (
            expr.Or(expr.Variable("A"), expr.Variable("B")),
            [
                expr.Variable("A"),
                expr.Variable("B"),
            ],
        ),
        (
            expr.And(expr.Variable("A"), expr.Variable("B")),
            [
                expr.And(expr.Variable("A"), expr.Variable("B")),
            ],
        ),
        (
            expr.And(expr.Or(expr.Variable("A"), expr.Variable("B")), expr.Variable("C")),
            [
                expr.And(expr.Variable("A"), expr.Variable("C")),
                expr.And(expr.Variable("B"), expr.Variable("C")),
            ],
        ),
        (
            expr.And(
                expr.Or(expr.Variable("A"), expr.Variable("B")),
                expr.Variable("C"),
                expr.Variable("D"),
            ),
            [
                expr.And(expr.Variable("A"), expr.Variable("C"), expr.Variable("D")),
                expr.And(expr.Variable("B"), expr.Variable("C"), expr.Variable("D")),
            ],
        ),
        (
            expr.And(
                expr.Or(expr.Variable("A"), expr.Variable("B")),
                expr.Or(expr.Variable("C"), expr.Variable("D")),
                expr.Variable("E"),
            ),
            [
                expr.And(expr.Variable("A"), expr.Variable("C"), expr.Variable("E")),
                expr.And(expr.Variable("A"), expr.Variable("D"), expr.Variable("E")),
                expr.And(expr.Variable("B"), expr.Variable("C"), expr.Variable("E")),
                expr.And(expr.Variable("B"), expr.Variable("D"), expr.Variable("E")),
            ],
        ),
    ],
)
def test_expand_expression(expression: expr.Expr, expected: Sequence[expr.Expr]) -> None:
    assert _expand_expression(expression) == expected
