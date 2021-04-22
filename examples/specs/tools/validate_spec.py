#!/usr/bin/env -S python3 -O

import argparse
import json
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import Dict, List, Optional, TextIO, Type, Union

from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.model import Link
from rflx.pyrflx import Package, PyRFLX, PyRFLXError
from rflx.pyrflx.typevalue import MessageValue


def cli(argv: List[str]) -> Union[int, str]:
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
        help="print the combined link-coverage of all provided messages",
    )

    parser.add_argument(
        "--target-coverage",
        type=float,
        default=0,
        help="abort with exitcode 1 if the coverage threshold is not reached",
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
        return f'invalid identifier "{args.message_identifier}" : {e}'

    try:
        pyrflx = PyRFLX.from_specs(
            [str(args.specification)], skip_model_verification=args.no_verification
        )
    except FileNotFoundError as e:
        return f"specification {e}"

    try:
        message_value = pyrflx[str(identifier.parent)][str(identifier.name)]
    except KeyError:
        return f'message "{identifier.name}" could not be found in package "{identifier.parent}"'

    coverage_info = (
        CoverageInformation(list(pyrflx), args.target_coverage) if args.coverage else None
    )

    try:
        validate(
            message_value,
            args.directory_invalid,
            args.directory_valid,
            args.json_output,
            args.abort_on_error,
            coverage_info,
        )
    except ValidationError as e:
        return f"{e}"

    return 0


def validate(
    message_value: MessageValue,
    directory_invalid: Optional[Path],
    directory_valid: Optional[Path],
    json_output: Optional[Path],
    abort_on_error: bool,
    coverage_info: Optional["CoverageInformation"],
) -> int:

    with OutputWriter(json_output) as output_writer:
        for directory_path, is_valid_directory in [
            (directory_valid, True),
            (directory_invalid, False),
        ]:
            directory = sorted(directory_path.glob("*")) if directory_path is not None else []
            for path in directory:
                validation_result = _validate_message(path, is_valid_directory, message_value)
                if coverage_info is not None:
                    coverage_info.update(validation_result.parser_result)
                output_writer.write_result(validation_result)
                if not validation_result.validation_success and abort_on_error:
                    raise ValidationError(f"aborted: message {path} was classified incorrectly")
        if coverage_info is not None:
            output_writer.write_coverage(coverage_info)
    return 0


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
    def __init__(self, packages: List[Package], target_coverage: float) -> None:
        self._total_message_coverage: Dict[str, Dict[Link, bool]] = {}
        self.spec_files: Dict[str, List[str]] = {}
        self.target_coverage = target_coverage
        for package in packages:
            for message in package:
                assert isinstance(message, MessageValue)
                self._total_message_coverage[str(message.identifier)] = {
                    link: False for link in message.model.structure
                }

                assert isinstance(message.model.location, Location)
                assert isinstance(message.model.location.source, Path)
                file_name = message.model.location.source.name
                if file_name in self.spec_files:
                    self.spec_files[file_name].append(str(message.identifier))
                else:
                    self.spec_files[file_name] = [str(message.identifier)]

    def update(self, message_value: MessageValue) -> None:
        messages = message_value.inner_messages() + [message_value]
        for message in messages:
            for link in message.path:
                self._total_message_coverage[str(message.identifier)][link] = True

    @property
    def total_links(self) -> int:
        return sum([len(structure) for structure in self._total_message_coverage.values()])

    @property
    def total_covered_links(self) -> int:
        return sum(
            [
                list(structure.values()).count(True)
                for structure in self._total_message_coverage.values()
            ]
        )

    def file_total_links(self, file_name: str) -> int:
        assert file_name in self.spec_files
        return sum(
            [len(self._total_message_coverage[message]) for message in self.spec_files[file_name]]
        )

    def file_covered_links(self, file_name: str) -> int:
        assert file_name in self.spec_files
        return sum(
            [
                list(self._total_message_coverage[message].values()).count(True)
                for message in self.spec_files[file_name]
            ]
        )

    def file_uncovered_links(self, file_name: str) -> List[Link]:
        assert file_name in self.spec_files
        uncovered = []
        for message in self.spec_files[file_name]:
            uncovered.extend(
                [
                    link
                    for link, covered in self._total_message_coverage[message].items()
                    if not covered
                ]
            )
        return uncovered


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
        self.reached_coverage = 0.00
        self.target_coverage = 0.00

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

        if exception_value is None:
            err = ValidationError()
            if self.classified_incorrectly != 0:
                err.append(f"{self.classified_incorrectly} messages were classified incorrectly")
            if self.reached_coverage < self.target_coverage:
                err.append(
                    f"missed target coverage of {self.target_coverage:.2%}, "
                    f"reached {self.reached_coverage:.2%}"
                )
            if len(err.messages) > 0:
                raise err

    def write_coverage(self, coverage_info: CoverageInformation) -> None:
        self.__write_coverage_short(coverage_info)
        self.__write_coverage_full(coverage_info)
        self.reached_coverage = coverage_info.total_covered_links / coverage_info.total_links
        self.target_coverage = coverage_info.target_coverage

    @staticmethod
    def __write_coverage_short(total_coverage: CoverageInformation) -> None:
        print("\n")
        print("-" * 80)
        print(f"{'RecordFlux Validation Coverage Report' : ^80}")
        print(f"Directory: {os.getcwd()}")
        print("-" * 80)
        print(f"{'File' : <40} {'Links' : >10} {'Used' : >10} {'Coverage' : >15}")
        for file, _ in total_coverage.spec_files.items():
            file_links = total_coverage.file_total_links(file)
            file_covered_links = total_coverage.file_covered_links(file)
            print(
                f"{file : <40} {file_links : >10} {file_covered_links : >10} "
                f"{file_covered_links / file_links :>15.2%}"
            )
        print("-" * 80)
        total_links = total_coverage.total_links
        total_covered_links = total_coverage.total_covered_links
        print(
            f"{'TOTAL' : <40} {total_links: >10} {total_covered_links : >10} "
            f"{total_covered_links / total_links :15.2%}"
        )
        print("-" * 80)

    @staticmethod
    def __write_coverage_full(total_coverage: CoverageInformation) -> None:
        print("\n")
        print("=" * 80)
        print(f"{'Link Coverage' : ^80}")
        print("=" * 80)
        for file, _ in total_coverage.spec_files.items():
            print("\n")
            print(f"{file : ^80}")
            print("-" * 80)
            uncovered_links = total_coverage.file_uncovered_links(file)
            if len(uncovered_links) == 0:
                print(f"{'all links covered':^80}")
                continue
            for link in uncovered_links:
                print(
                    f"{str(link.location) if link.location is not None else '':<17}"
                    f": missing link {link.source.name:^25} -> {link.target.name:^20}"
                )

    def write_result(self, validation_result: ValidationResult) -> None:
        self.__write_console_output(validation_result)
        self.__write_json_output(validation_result)

    def __write_console_output(self, result: ValidationResult) -> None:
        if result.validation_success:
            print(f"{str(result.message_file_path):<80} PASSED")
        else:
            print(f"{str(result.message_file_path):<80} FAILED")
            print(
                f"provided as: {result.valid_original_message}\t "
                f"recognized as: {result.valid_parser_result}"
            )
            if result.parser_error is not None:
                print(result.parser_error)
            self.classified_incorrectly += 1

    def __write_json_output(self, result: ValidationResult) -> None:
        if self.file is not None:
            if self.count != 0:
                self.file.write(",\n")
            json.dump(
                result.as_json(),
                self.file,
                indent="\t",
            )
            self.count += 1


class ValidationError(Exception):
    def __init__(self, message: Optional[str] = None) -> None:
        super().__init__()
        self.messages: List[str] = []
        if isinstance(message, str):
            self.messages.append(message)

    def __str__(self) -> str:
        return "\n".join(e for e in self.messages)

    def append(self, message: str) -> None:
        self.messages.append(message)


if __name__ == "__main__":
    sys.exit(cli(sys.argv))
