#!/usr/bin/env -S python3 -O

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import Dict, List, Optional, TextIO, Type, Union

from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.pyrflx import PyRFLX, PyRFLXError
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
        message_value = PyRFLX.from_specs(
            [str(args.specification)], skip_model_verification=args.no_verification
        )[str(identifier.parent)][str(identifier.name)]
    except KeyError:
        return f'message "{identifier.name}" could not be found in package "{identifier.parent}"'

    except FileNotFoundError as e:
        return f"specification {e}"

    try:
        validate(
            message_value,
            args.directory_invalid,
            args.directory_valid,
            args.json_output,
            args.abort_on_error,
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
) -> int:

    with OutputWriter(json_output) as output_writer:
        for directory_path, is_valid_directory in [
            (directory_valid, True),
            (directory_invalid, False),
        ]:
            directory = sorted(directory_path.glob("*")) if directory_path is not None else []
            for path in directory:
                validation_result = _validate_message(path, is_valid_directory, message_value)
                output_writer.write_result(validation_result)
                if not validation_result.validation_success and abort_on_error:
                    raise ValidationError(f"aborted: message {path} was classified incorrectly")

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
                self.file = open(file, "w")
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

        if exception_value is None and self.classified_incorrectly != 0:
            raise ValidationError(
                f"{self.classified_incorrectly} messages were classified incorrectly"
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
    def __init__(self, message: str) -> None:
        super().__init__()
        self.message = message

    def __str__(self) -> str:
        return self.message


if __name__ == "__main__":
    sys.exit(cli(sys.argv))
