#!/usr/bin/env -S python3 -O

import argparse
import importlib
import json
import os
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import Dict, List, Optional, TextIO, Type, Union

from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.model import Link
from rflx.pyrflx import Package, PyRFLX, PyRFLXError
from rflx.pyrflx.typevalue import MessageValue


def cli(argv: List[str]) -> Union[int, str]:
    # pylint: disable=too-many-return-statements, too-many-branches
    parser = argparse.ArgumentParser(
        description="validate specification against a set of known valid or invalid messages"
    )

    parser.add_argument(
        "-s", "--specification", type=Path, help="specification file", required=True
    )

    parser.add_argument(
        "-m",
        "--message-identifier",
        type=str,
        help="identifier of the top-level message: <PackageName>::<MessageName>",
        required=True,
    )

    parser.add_argument(
        "-v",
        "--directory-valid",
        type=Path,
        help="path to directory with known valid examples",
        default=None,
    )

    parser.add_argument(
        "-i",
        "--directory-invalid",
        type=Path,
        help="path to directory with known valid examples",
        default=None,
    )

    parser.add_argument(
        "-f",
        "--checksum_functions",
        type=Path,
        help="path to the module containing the checksum functions",
        default=None,
    )

    parser.add_argument(
        "-o",
        "--json-output",
        type=Path,
        help="path to output file - file must not exist",
        default=None,
    )

    parser.add_argument(
        "--abort-on-error",
        action="store_true",
        help=(
            "abort with exitcode 1 if a message is classified "
            "as a false positive or false negative"
        ),
    )

    parser.add_argument(
        "-c",
        "--coverage",
        action="store_true",
        help="enable coverage calculation and print the "
        "combined link-coverage of all provided messages",
    )

    parser.add_argument(
        "--target-coverage",
        type=float,
        default=0,
        help="abort with exitcode 1 if the coverage threshold is not reached; "
        "target coverage is expected in percentage",
    )

    parser.add_argument("--no-verification", action="store_true", help="skip model verification")

    args = parser.parse_args(argv[1:])
    if args.directory_valid is None and args.directory_invalid is None:
        return "must provide directory with valid and/or invalid messages"

    for path in [args.directory_valid, args.directory_invalid]:
        if path is not None and not path.is_dir():
            return f"{path} does not exist or is not a directory"

    if args.json_output is not None and args.json_output.exists():
        return f"output file already exists: {args.json_output}"

    try:
        identifier = ID(args.message_identifier)
    except RecordFluxError as e:
        return f"invalid identifier: {e}"

    try:
        pyrflx = PyRFLX.from_specs(
            [str(args.specification)], skip_model_verification=args.no_verification
        )
    except FileNotFoundError as e:
        return f"specification {e}"

    if args.checksum_functions is not None:
        try:
            checksum_module = importlib.import_module(str(args.checksum_functions))
        except ImportError as e:
            return (
                f"The provided module {args.checksum_functions} cannot be "
                f"imported. Make sure the module name is provided as "
                f"package.module and not as a file system path. {e}"
            )

        try:
            all_checksum_functions = checksum_module.checksum_functions # type: ignore[attr-defined]
        except AttributeError:
            return (
                f"The checksum module at {args.checksum_functions} "
                f'does not contain an attribute with the name "checksum_function".'
            )
        if not isinstance(all_checksum_functions, dict):
            return f"The attribute checksum_function of {args.checksum_functions} is not a dict."
        try:
            checksum_functions = all_checksum_functions[args.message_identifier]
        except KeyError:
            return f"The checksum_function dict does not contain a key for {identifier}"

        for message_name, checksum_func_callable in checksum_functions.items():
            if not callable(checksum_func_callable):
                return f'The value at key "{message_name}" is not a callable checksum function.'

        try:
            pyrflx.set_checksum_functions(all_checksum_functions)
        except PyRFLXError as e:
            return f"Could not set checksum function to pyrflx: {e}"

    try:
        validate(
            identifier,
            pyrflx,
            args.directory_invalid,
            args.directory_valid,
            args.json_output,
            args.abort_on_error,
            args.coverage,
            args.target_coverage,
        )
    except ValidationError as e:
        return f"{e}"

    return 0


def validate(
    msg_identifier: ID,
    pyrflx: PyRFLX,
    directory_invalid: Optional[Path],
    directory_valid: Optional[Path],
    json_output: Optional[Path],
    abort_on_error: bool = False,
    coverage: bool = False,
    target_coverage: float = 0.00,
) -> None:
    # pylint: disable = too-many-arguments, too-many-locals

    if target_coverage < 0 or target_coverage > 100:
        raise ValidationError(f"target coverage must be between 0 and 100, got {target_coverage}")

    try:
        message_value = pyrflx[str(msg_identifier.parent)][str(msg_identifier.name)]
    except KeyError as e:
        raise ValidationError(
            f'message "{msg_identifier.name}" could not be found '
            f'in package "{msg_identifier.parent}"'
        ) from e

    incorrectly_classified = 0
    coverage_info = CoverageInformation(list(pyrflx), coverage)

    with OutputWriter(json_output) as output_writer:
        for directory_path, is_valid_directory in [
            (directory_valid, True),
            (directory_invalid, False),
        ]:
            directory = sorted(directory_path.glob("*")) if directory_path is not None else []
            for path in directory:
                validation_result = _validate_message(path, is_valid_directory, message_value)
                coverage_info.update(validation_result.parser_result)
                validation_result.print_console_output()
                output_writer.write_result(validation_result)
                if not validation_result.validation_success:
                    incorrectly_classified += 1
                    if abort_on_error:
                        raise ValidationError(f"aborted: message {path} was classified incorrectly")
        coverage_info.print_coverage()

    error_msgs = []
    if incorrectly_classified != 0:
        error_msgs.append(f"{incorrectly_classified} messages were classified incorrectly")
    if (
        coverage
        and coverage_info.total_covered_links / coverage_info.total_links < target_coverage / 100
    ):
        error_msgs.append(
            f"missed target coverage of {target_coverage/100:.2%}, "
            f"reached {coverage_info.total_covered_links / coverage_info.total_links:.2%}"
        )
    if len(error_msgs) > 0:
        raise ValidationError("\n".join(e for e in error_msgs))


def _validate_message(
    message_path: Path, valid_original_message: bool, message_value: MessageValue
) -> "ValidationResult":
    if not message_path.is_file():
        raise ValidationError(f"{message_path} is not a regular file")
    original_message = message_path.read_bytes()
    parser_error: Optional[str] = None
    parser_result: MessageValue = message_value.clone()
    try:
        parser_result.parse(original_message)
        valid_parser_result = parser_result.bytestring == original_message
        if not valid_parser_result:
            assert parser_result.valid_message
            assert len(parser_result.bytestring) <= len(original_message)
            assert original_message.startswith(parser_result.bytestring)
            parser_error = "message parsed by PyRFLX is shorter than the original message"

    except PyRFLXError as e:
        parser_error = str(e)
        valid_parser_result = False

    return ValidationResult(
        valid_original_message == valid_parser_result,
        parser_result,
        parser_error,
        message_path,
        original_message,
        valid_original_message,
        valid_parser_result,
    )


class CoverageInformation:
    def __init__(self, packages: List[Package], coverage: bool) -> None:
        self.__total_message_coverage: Dict[ID, Dict[Link, bool]] = {}
        self.__spec_files: Dict[str, List[ID]] = defaultdict(list)
        self.__coverage = coverage

        if not self.__coverage:
            return

        for package in packages:
            for message in package:
                assert isinstance(message, MessageValue)
                self.__total_message_coverage[message.identifier] = {
                    link: False for link in message.model.structure
                }

                assert message.model.location is not None
                assert message.model.location.source is not None
                file_name = message.model.location.source.name
                self.__spec_files[file_name].append(message.identifier)

        self.total_links = sum(
            len(structure) for structure in self.__total_message_coverage.values()
        )
        self.total_covered_links = 0

    def update(self, message_value: MessageValue) -> None:
        if self.__coverage:
            messages = message_value.inner_messages() + [message_value]
            for message in messages:
                for link in message.path:
                    if not self.__total_message_coverage[message.identifier][link]:
                        self.total_covered_links += 1
                        self.__total_message_coverage[message.identifier][link] = True

    def file_total_links(self, file_name: str) -> int:
        assert file_name in self.__spec_files
        return sum(
            len(self.__total_message_coverage[message]) for message in self.__spec_files[file_name]
        )

    def file_covered_links(self, file_name: str) -> int:
        assert file_name in self.__spec_files
        return sum(
            list(self.__total_message_coverage[message].values()).count(True)
            for message in self.__spec_files[file_name]
        )

    def file_uncovered_links(self, file_name: str) -> List[Link]:
        assert file_name in self.__spec_files
        return [
            link
            for message in self.__spec_files[file_name]
            for link, covered in self.__total_message_coverage[message].items()
            if not covered
        ]

    def print_coverage(self) -> None:
        if self.__coverage:
            self.__print_coverage_overview()
            if self.total_covered_links / self.total_links != 1:
                self.__print_link_coverage()

    def __print_coverage_overview(self) -> None:
        print("\n")
        print("-" * 80)
        print(f"{'RecordFlux Validation Coverage Report' : ^80}")
        print(f"Directory: {os.getcwd()}")
        print("-" * 80)
        print(f"{'File' : <40} {'Links' : >10} {'Used' : >10} {'Coverage' : >15}")
        for file in self.__spec_files:
            file_links = self.file_total_links(file)
            file_covered_links = self.file_covered_links(file)
            print(
                f"{file : <40} {file_links : >10} {file_covered_links : >10} "
                f"{file_covered_links / file_links :>15.2%}"
            )
        print("-" * 80)
        print(
            f"{'TOTAL' : <40} {self.total_links: >10} {self.total_covered_links : >10} "
            f"{self.total_covered_links / self.total_links :15.2%}"
        )
        print("-" * 80)

    def __print_link_coverage(self) -> None:
        print("\n")
        print("=" * 80)
        print(f"{'Uncovered Links' : ^80}")
        print("=" * 80)
        for file in self.__spec_files:
            uncovered_links = self.file_uncovered_links(file)
            if len(uncovered_links) != 0:
                print("\n")
                print(f"{file : ^80}")
                print("-" * 80)
                for link in uncovered_links:
                    print(
                        f"{str(link.location):<17}"
                        f": missing link {link.source.name:^25} -> {link.target.name:^20}"
                    )


@dataclass
class ValidationResult:
    validation_success: bool
    parser_result: MessageValue
    parser_error: Optional[str]
    message_file_path: Path
    original_message: bytes
    valid_original_message: bool
    valid_parser_result: bool

    def __get_field_values(self) -> Dict[str, object]:
        parsed_field_values: Dict[str, object] = {}
        for field_name in self.parser_result.valid_fields:
            field_value = self.parser_result.get(field_name)
            if isinstance(field_value, MessageValue):
                field_value = field_value.bytestring.hex()
            if isinstance(field_value, bytes):
                field_value = field_value.hex()
            parsed_field_values[field_name] = field_value
        return parsed_field_values

    def as_json(self) -> Dict[str, object]:
        output = {
            "file name": str(self.message_file_path),
            "provided as": self.valid_original_message,
            "recognized as": self.valid_parser_result,
            "original": self.original_message.hex(),
        }
        if self.parser_result.valid_message:
            output["parsed"] = self.parser_result.bytestring.hex()
        output["parsed field values"] = self.__get_field_values()
        if self.parser_error is not None:
            output["error"] = self.parser_error

        return output

    def print_console_output(self) -> None:
        if self.validation_success:
            print(f"{str(self.message_file_path):<80} PASSED")
        else:
            print(f"{str(self.message_file_path):<80} FAILED")
            print(
                f"provided as: {self.valid_original_message}\t "
                f"recognized as: {self.valid_parser_result}"
            )
            if self.parser_error is not None:
                print(self.parser_error)


class OutputWriter:
    file: Optional[TextIO]

    def __init__(self, file: Optional[Path]) -> None:
        if file is not None:
            try:
                self.file = open(file, "w")  # pylint: disable=consider-using-with
            except OSError as e:
                raise ValidationError(f"cannot open output file {file}: {e}") from e
            self.file.write("[\n")
            self.count = 0
        else:
            self.file = file
        self.classified_incorrectly = 0

    def __enter__(self) -> "OutputWriter":
        return self

    def __exit__(
        self,
        exception_type: Optional[Type[BaseException]],
        exception_value: Optional[BaseException],
        traceback: Optional[TracebackType],
    ) -> None:
        if self.file is not None:
            self.file.write("\n]")
            self.file.close()

    def write_result(self, validation_result: ValidationResult) -> None:
        if self.file is not None:
            if self.count != 0:
                self.file.write(",\n")
            json.dump(
                validation_result.as_json(),
                self.file,
                indent="\t",
            )
            self.count += 1


class ValidationError(Exception):
    pass


if __name__ == "__main__":
    sys.exit(cli(sys.argv))
