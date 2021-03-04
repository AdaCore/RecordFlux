#!/usr/bin/env -S python3 -O

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import IO, Dict, Iterator, List, Optional, Sequence, Type, Union

from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.pyrflx import PyRFLX, PyRFLXError, TypeValue
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
        pdu_message = PyRFLX.from_specs(
            [str(args.specification)], skip_model_verification=args.no_verification
        )[str(identifier.parent)][str(identifier.name)]
    except KeyError:
        return f'message "{identifier.name}" could not be found in package "{identifier.parent}"'

    except FileNotFoundError as e:
        return f"specification {e}"

    try:
        validation_main(
            pdu_message,
            args.directory_invalid,
            args.directory_valid,
            args.json_output,
            args.abort_on_error,
        )
    except ValidationError as e:
        return f"{e}"

    return 0


def validation_main(
    pdu_message: MessageValue,
    path_invalid: Optional[Path],
    path_valid: Optional[Path],
    full_output_path: Optional[Path],
    abort_on_error: bool,
) -> None:
    def validate_directory(path_iterator: Iterator[Path], is_valid_directory: bool) -> int:
        classified_incorrectly = 0
        for path in path_iterator:
            validation_result = __validate_message(path, is_valid_directory, pdu_message)
            output_writer.write_result(validation_result)
            if not validation_result.validation_success and abort_on_error:
                raise ValidationError(f"aborted: message {path} was classified incorrectly")
        return classified_incorrectly

    valid_message_paths: Optional[Iterator[Path]] = (
        path_valid.glob("*") if path_valid is not None else path_valid
    )
    invalid_message_paths: Optional[Iterator[Path]] = (
        path_invalid.glob("*") if path_invalid is not None else path_invalid
    )

    with OutputWriter(full_output_path) as output_writer:
        if valid_message_paths is not None:
            validate_directory(valid_message_paths, True)
        if invalid_message_paths is not None:
            validate_directory(invalid_message_paths, False)


def __validate_message(
    message_path: Path, valid_original_message: bool, model: MessageValue
) -> "ValidationResult":
    if not message_path.is_file():
        raise ValidationError(f"{message_path} is not a regular file")
    original_message = message_path.read_bytes()

    parser_result: Optional[MessageValue] = None
    parser_error: Optional[str] = None

    pdu_model: MessageValue = model.clone()
    try:
        pdu_model.parse(original_message)
        parser_result = pdu_model
        valid_parser_result = (
            parser_result.bytestring == original_message and parser_result.valid_message
        )
        if not valid_parser_result:
            parser_error = "Invalid message"
    except RecordFluxError as e:
        parser_error = str(e)
        valid_parser_result = False

    validation_success: bool
    if valid_original_message:
        validation_success = valid_parser_result
    else:
        validation_success = not valid_parser_result

    result = ValidationResult(
        validation_success,
        parser_result,
        parser_error,
        message_path,
        original_message,
        valid_original_message,
        valid_parser_result,
    )

    return result


@dataclass
class ValidationResult:
    validation_success: bool
    parser_result: Optional[MessageValue]
    parser_error: Optional[str]
    message_file_path: Path
    original_message: bytes
    valid_original_message: bool
    valid_parser_result: bool

    def as_json(self) -> Dict[str, object]:
        def get_field_values(
            msg: MessageValue,
        ) -> Dict[str, Union[MessageValue, Sequence[TypeValue], int, str, bytes]]:
            parsed_field_values: Dict[
                str, Union[MessageValue, Sequence[TypeValue], int, str, bytes]
            ] = {}
            parsed_field_values.fromkeys(msg.fields)
            for field_name in msg.fields:
                try:
                    field_value = msg.get(field_name)
                except PyRFLXError as e:
                    field_value = str(f"{e} or not set")
                if isinstance(field_value, MessageValue):
                    field_value = field_value.bytestring.hex()
                elif isinstance(field_value, bytes):
                    field_value = field_value.hex()
                parsed_field_values[field_name] = field_value
            return parsed_field_values

        output = {
            "file name": str(self.message_file_path),
            "provided as": self.valid_original_message,
            "recognized as": self.valid_parser_result,
            "original": self.original_message.hex(),
        }

        if self.parser_result is not None:
            output["parsed"] = self.parser_result.bytestring.hex()
            output["parsed field values"] = get_field_values(self.parser_result)
        if self.parser_error is not None:
            output["error"] = self.parser_error

        return output


class OutputWriter:
    file: Optional[IO]

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
        if self.file is not None:
            self.__write_json_output(validation_result)

    def __write_console_output(self, result: ValidationResult) -> None:
        if result.validation_success:
            print(f"{str(result.message_file_path):<80} {'PASSED'}")
        else:
            print(f"{str(result.message_file_path):<80} FAILED")
            print(
                f"provided as: {result.valid_original_message}\t "
                f"recognized as: {result.valid_parser_result}"
            )
            if result.parser_error is not None:
                print(f"{result.parser_error}")
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
