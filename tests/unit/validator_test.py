from __future__ import annotations

from collections.abc import Sequence
from pathlib import Path

import pytest

from rflx import expression as expr
from rflx.identifier import ID
from rflx.pyrflx import PyRFLX
from rflx.validator import ValidationError, Validator
from tests.const import SPEC_DIR, VALIDATOR_DIR

CHECKSUM_MODULE = "tests.data.validator.checksum"


def test_initialize_pyrflx_spec_file_not_found() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^specification file not found: "non_existent_file.rflx"$',
    ):
        Validator(["non_existent_file.rflx"], CHECKSUM_MODULE, skip_model_verification=True)


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
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests/checksum",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_no_checksum_provided() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^missing checksum definition for field "C" of "Checksum_Message::Message"$',
    ):
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_missing_attribute() -> None:
    checksum_module = "tests.data.validator.checksum_missing_attribute"
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf'missing attribute "checksum_functions" in checksum module "{checksum_module}"'
            r"$"
        ),
    ):
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            checksum_module,
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_missing_field() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^missing checksum definition for field "C" of "Checksum_Message::Message"$',
    ):
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_missing_field",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_function_type() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^value at key "Checksum" is not a callable checksum function$',
    ):
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_function_type",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_field_dict_type() -> None:
    with pytest.raises(
        ValidationError,
        match=r'^value at key "Checksum_Message::Message" is not a dict$',
    ):
        Validator(
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
        Validator(
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
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            "tests.data.validator.checksum_invalid_field",
            skip_model_verification=True,
        )


def test_initialize_pyrflx_checksum_invalid_attribute_type() -> None:
    checksum_module = "tests.data.validator.checksum_invalid_attribute_type"
    with pytest.raises(
        ValidationError,
        match=rf'^attribute "checksum_functions" of "{checksum_module}" is not a dict$',
    ):
        Validator(
            [SPEC_DIR / "checksum_message.rflx"],
            checksum_module,
            skip_model_verification=True,
        )


def test_validate_error_msg_not_in_package() -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=r'^message "Message" could not be found in package "Ethernet"$',
    ):
        validator.validate(
            ID("Ethernet::Message"),
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            None,
            None,
        )


def test_validate_no_raw_files_in_valid_dir(tmp_path: Path) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"{tmp_path} contains no files with a .raw file extension, please provide a directory "
            "with .raw files or a list of individual files with any extension"
            r"$"
        ),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [
                VALIDATOR_DIR / "ethernet/frame/valid",
                tmp_path,
            ],
            [VALIDATOR_DIR / "ethernet/frame/invalid"],
        )


def test_validate_no_raw_files_in_invalid_dir(tmp_path: Path) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"{tmp_path} contains no files with a .raw file extension, please provide a directory "
            "with .raw files or a list of individual files with any extension"
            r"$"
        ),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            [
                VALIDATOR_DIR / "ethernet/frame/invalid",
                tmp_path,
            ],
        )


def test_validate_no_test_data_provided() -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=r"^must provide directory with valid and/or invalid messages$",
    ):
        validator.validate(ID("Ethernet::Message"), None, None, None)


def test_validate_valid_samples_dir_does_not_exist(tmp_path: Path) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{tmp_path}/non_existent_dir does not exist$",
    ):
        validator.validate(ID("Ethernet::Message"), [tmp_path / "non_existent_dir"], None, None)


def test_validate_invalid_samples_dir_does_not_exist(tmp_path: Path) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{tmp_path}/non_existent_dir does not exist$",
    ):
        validator.validate(ID("Ethernet::Message"), None, [tmp_path / "non_existent_dir"], None)


def test_validate_output_is_a_dir(tmp_path: Path) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=(rf"^{tmp_path} is a directory, please specify a file for the validation report$"),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            None,
            tmp_path,
        )


def test_validate_output_file_exists(tmp_path: Path) -> None:
    tmp_file = tmp_path / "test.json"
    tmp_file.write_text("")
    assert tmp_file.is_file()
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^output file already exists: {tmp_file}$",
    ):
        validator.validate(ID("Ethernet::Message"), [tmp_path], None, tmp_file)


def test_validate_output_not_writable(tmp_path: Path) -> None:
    tmp_path.chmod(0o555)
    output_file = tmp_path / "output.txt"
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"cannot open output file {output_file}: \[Errno 13\] Permission denied: "
            rf"'{output_file}'"
            r"$"
        ),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            None,
            output_file,
        )


def test_validate_abort_on_error() -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            rf"aborted: message {VALIDATOR_DIR}/ethernet/frame/invalid.+\.raw "
            r"was classified incorrectly"
            r"$"
        ),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            [VALIDATOR_DIR / "ethernet/frame/invalid"],
            abort_on_error=True,
        )


def test_validate_not_regular_file(tmp_path: Path) -> None:
    subdir = tmp_path / "test.raw"
    subdir.mkdir()
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{subdir} is not a regular file$",
    ):
        validator.validate(ID("Ethernet::Frame"), [tmp_path])


def test_validate_positive_only() -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    validator.validate(
        ID("Ethernet::Frame"),
        None,
        [VALIDATOR_DIR / "ethernet/frame/valid"],
    )


def test_validate_positive_output(tmp_path: Path) -> None:
    """
    Test the JSON output on correctly classified samples.

    Test the detailed validation report on a set of correctly classified samples that are collected
    from multiple directories. The report should contain expected output for each file.
    """
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    validator.validate(
        ID("Ethernet::Frame"),
        [
            VALIDATOR_DIR / "ethernet/frame/invalid",
            VALIDATOR_DIR / "ethernet/frame/invalid2",
            VALIDATOR_DIR / "ethernet/frame/invalid3/ethernet_invalid_too_long.bin",
            VALIDATOR_DIR / "ethernet/frame/invalid3/ethernet_invalid_too_short.dat",
        ],
        [
            VALIDATOR_DIR / "ethernet/frame/valid",
            VALIDATOR_DIR / "ethernet/frame/valid2",
            VALIDATOR_DIR / "ethernet/frame/valid3/ethernet_802.3.bin",
            VALIDATOR_DIR / "ethernet/frame/valid3/ethernet_ipv4_udp.dat",
        ],
        tmp_path / "output.json",
    )
    assert (tmp_path / "output.json").read_text() == (
        VALIDATOR_DIR / "output_positive.json"
    ).read_text(
        encoding="utf-8",
    )


def test_validate_negative_only() -> None:
    number = len(list((VALIDATOR_DIR / "ethernet/frame/invalid").glob("*.raw")))
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{number} messages were classified incorrectly$",
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            None,
            [VALIDATOR_DIR / "ethernet/frame/invalid"],
        )


def test_validate_negative_output(tmp_path: Path) -> None:
    """
    Test the JSON output on incorrectly classified samples.

    Test the detailed validation report on a set of incorrectly classified samples that are
    collected from multiple directories. The report should contain expected output for each file.
    """
    number = len(
        list((VALIDATOR_DIR / "ethernet/frame/invalid").glob("*.raw"))
        + list((VALIDATOR_DIR / "ethernet/frame/invalid2").glob("*.raw"))
        + list((VALIDATOR_DIR / "ethernet/frame/invalid3").iterdir())
        + list((VALIDATOR_DIR / "ethernet/frame/valid").glob("*.raw"))
        + list((VALIDATOR_DIR / "ethernet/frame/valid2").glob("*.raw"))
        + list((VALIDATOR_DIR / "ethernet/frame/valid3").iterdir()),
    )
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=rf"^{number} messages were classified incorrectly$",
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [
                VALIDATOR_DIR / "ethernet/frame/valid",
                VALIDATOR_DIR / "ethernet/frame/valid2",
                VALIDATOR_DIR / "ethernet/frame/valid3/ethernet_802.3.bin",
                VALIDATOR_DIR / "ethernet/frame/valid3/ethernet_ipv4_udp.dat",
            ],
            [
                VALIDATOR_DIR / "ethernet/frame/invalid",
                VALIDATOR_DIR / "ethernet/frame/invalid2",
                VALIDATOR_DIR / "ethernet/frame/invalid3/ethernet_invalid_too_long.bin",
                VALIDATOR_DIR / "ethernet/frame/invalid3/ethernet_invalid_too_short.dat",
            ],
            tmp_path / "output.json",
        )
    assert (tmp_path / "output.json").read_text() == (
        VALIDATOR_DIR / "output_negative.json"
    ).read_text(
        encoding="utf-8",
    )


def test_validate_coverage(capsys: pytest.CaptureFixture[str]) -> None:
    validator = Validator(
        [SPEC_DIR / "ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    validator.validate(
        ID("Ethernet::Frame"),
        [VALIDATOR_DIR / "ethernet/frame/invalid"],
        [VALIDATOR_DIR / "ethernet/frame/valid"],
        coverage=True,
        target_coverage=100,
    )
    expected_output = f"""model: warning: model verification skipped
{VALIDATOR_DIR}/ethernet/frame/valid/802.3-LLC-CDP.raw                      PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1AD-802.1Q-IPv4.raw            PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-802.1Q-IPv4-ICMP.raw        PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-CDP.raw                 PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-STP.raw                 PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_802.3.raw                     PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_double_vlan_tag.raw           PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_ipv4_udp.raw                  PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_vlan_tag.raw                  PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_802.3_invalid_length.raw    PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_invalid_too_long.raw        PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_invalid_too_short.raw       PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_undefined.raw               PASSED


--------------------------------------------------------------------------------
                     RecordFlux Validation Coverage Report
Directory: {Path.cwd()}
--------------------------------------------------------------------------------
File                                          Links       Used        Coverage
ethernet.rflx                                    10         10         100.00%
--------------------------------------------------------------------------------
TOTAL                                            10         10         100.00%
--------------------------------------------------------------------------------
"""
    assert capsys.readouterr().out == expected_output


def test_coverage_threshold_missed(capsys: pytest.CaptureFixture[str]) -> None:
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
        split_disjunctions=True,
    )
    with pytest.raises(
        ValidationError,
        match=(r"^missed target coverage of 90.00%, reached 73.68%$"),
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/invalid"],
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            coverage=True,
            target_coverage=90,
        )
    expected_output = f"""model: warning: model verification skipped
{VALIDATOR_DIR}/ethernet/frame/valid/802.3-LLC-CDP.raw                      PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1AD-802.1Q-IPv4.raw            PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-802.1Q-IPv4-ICMP.raw        PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-CDP.raw                 PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/EII-802.1Q-LLC-STP.raw                 PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_802.3.raw                     PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_double_vlan_tag.raw           PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_ipv4_udp.raw                  PASSED
{VALIDATOR_DIR}/ethernet/frame/valid/ethernet_vlan_tag.raw                  PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_802.3_invalid_length.raw    PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_invalid_too_long.raw        PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_invalid_too_short.raw       PASSED
{VALIDATOR_DIR}/ethernet/frame/invalid/ethernet_undefined.raw               PASSED


--------------------------------------------------------------------------------
                     RecordFlux Validation Coverage Report
Directory: {Path.cwd()}
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
    validator = Validator(
        [SPEC_DIR / "in_ethernet.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(
        ValidationError,
        match=r"^target coverage must be between 0 and 100, got 110$",
    ):
        validator.validate(
            ID("Ethernet::Frame"),
            [VALIDATOR_DIR / "ethernet/frame/invalid"],
            [VALIDATOR_DIR / "ethernet/frame/valid"],
            coverage=True,
            target_coverage=110,
        )


def test_validate_checksum_positive() -> None:
    validator = Validator(
        [SPEC_DIR / "checksum_message.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    validator.validate(
        ID("Checksum_Message::Message"),
        [VALIDATOR_DIR / "checksum_message/invalid"],
        [VALIDATOR_DIR / "checksum_message/valid"],
    )


def test_validate_pyrflx_checksum_negative() -> None:
    validator = Validator(
        [SPEC_DIR / "checksum_message.rflx"],
        CHECKSUM_MODULE,
        skip_model_verification=True,
    )
    with pytest.raises(ValidationError, match=r"^3 messages were classified incorrectly$"):
        validator.validate(
            ID("Checksum_Message::Message"),
            [VALIDATOR_DIR / "checksum_message/valid"],
            [VALIDATOR_DIR / "checksum_message/invalid"],
        )


def test_validate_message_original_and_parsed_not_equal() -> None:
    validator = Validator([], skip_model_verification=True)
    ethernet_too_short_value = (
        PyRFLX.from_specs([SPEC_DIR / "ethernet.rflx"], skip_model_verification=True)
        .package("Ethernet")
        .new_message("Frame")
    )
    validation_result = validator._validate_message(  # noqa: SLF001
        Path(VALIDATOR_DIR / "ethernet/frame/invalid/ethernet_invalid_too_long.raw"),
        valid_original_message=True,
        message_value=ethernet_too_short_value,
    )
    assert (
        validation_result.parser_error
        == "message parsed by PyRFLX is shorter than the original message"
    )


def test_validate_message_parameterized_message() -> None:
    validator = Validator([], skip_model_verification=True)
    message = (
        PyRFLX.from_specs(["tests/data/specs/parameterized.rflx"], skip_model_verification=True)
        .package("Parameterized")
        .new_message("Message")
    )
    validation_result = validator._validate_message(  # noqa: SLF001
        Path(VALIDATOR_DIR / "parameterized/message/valid/parameterized_message.raw"),
        valid_original_message=True,
        message_value=message,
    )
    assert validation_result.validation_success


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        (
            expr.Or(
                expr.Equal(expr.Variable("A"), expr.Number(1)),
                expr.Equal(expr.Variable("B"), expr.Number(1)),
            ),
            [
                expr.Equal(expr.Variable("A"), expr.Number(1)),
                expr.Equal(expr.Variable("B"), expr.Number(1)),
            ],
        ),
        (
            expr.And(
                expr.Equal(expr.Variable("A"), expr.Number(1)),
                expr.Equal(expr.Variable("B"), expr.Number(1)),
            ),
            [
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
            ],
        ),
        (
            expr.And(
                expr.Or(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
                expr.Equal(expr.Variable("C"), expr.Number(1)),
            ),
            [
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
            ],
        ),
        (
            expr.And(
                expr.Or(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
                expr.Equal(expr.Variable("C"), expr.Number(1)),
                expr.Equal(expr.Variable("D"), expr.Number(1)),
            ),
            [
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("D"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("D"), expr.Number(1)),
                ),
            ],
        ),
        (
            expr.And(
                expr.Or(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
                expr.Or(
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("D"), expr.Number(1)),
                ),
                expr.Equal(expr.Variable("E"), expr.Number(1)),
            ),
            [
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("E"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("D"), expr.Number(1)),
                    expr.Equal(expr.Variable("E"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("E"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("D"), expr.Number(1)),
                    expr.Equal(expr.Variable("E"), expr.Number(1)),
                ),
            ],
        ),
        (
            expr.And(
                expr.Or(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
                expr.Or(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
                expr.Or(
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
            ),
            [
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("A"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                ),
                expr.And(
                    expr.Equal(expr.Variable("C"), expr.Number(1)),
                    expr.Equal(expr.Variable("B"), expr.Number(1)),
                ),
            ],
        ),
    ],
)
def test_expand_expression(expression: expr.Expr, expected: Sequence[expr.Expr]) -> None:
    assert Validator._expand_expression(expression) == expected  # noqa: SLF001


def test_parameterized_message_missing_parameter() -> None:
    validator = Validator([], skip_model_verification=True)
    message = (
        PyRFLX.from_specs(["tests/data/specs/parameterized.rflx"], skip_model_verification=True)
        .package("Parameterized")
        .new_message("Message")
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            f"{VALIDATOR_DIR}/parameterized/message/invalid/parameterized_message_missing_parameter.raw:"
            r" pyrflx: error: missing parameter values: Tag_Mode"
            r"$"
        ),
    ):
        validator._validate_message(  # noqa: SLF001
            Path(
                VALIDATOR_DIR
                / "parameterized/message/invalid/parameterized_message_missing_parameter.raw",
            ),
            valid_original_message=True,
            message_value=message,
        )


def test_parameterized_message_excess_parameter() -> None:
    validator = Validator([], skip_model_verification=True)
    message = (
        PyRFLX.from_specs(["tests/data/specs/parameterized.rflx"], skip_model_verification=True)
        .package("Parameterized")
        .new_message("Message")
    )
    with pytest.raises(
        ValidationError,
        match=(
            r"^"
            f"{VALIDATOR_DIR}/parameterized/message/invalid/parameterized_message_excess_parameter.raw:"
            r" pyrflx: error: unexpected parameter values: Excess"
            r"$"
        ),
    ):
        validator._validate_message(  # noqa: SLF001
            Path(
                VALIDATOR_DIR
                / "parameterized/message/invalid/parameterized_message_excess_parameter.raw",
            ),
            valid_original_message=True,
            message_value=message,
        )
