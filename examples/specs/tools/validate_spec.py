#!/usr/bin/env python3

import argparse
import json
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from types import TracebackType
from typing import IO, Dict, Iterator, List, Optional, Sequence, Type, Union

from rflx.identifier import ID
from rflx.pyrflx import PyRFLX, TypeValue
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

    args = parser.parse_args(argv[1:])
    if args.directory_valid is None and args.directory_invalid is None:
        parser.error("must provide directory with valid and/or invalid messages")

    if args.json_output is not None and args.json_output.exists():
        parser.error(f"output file already exists: {args.json_output}")

    try:
        validation_main(args)
    except ValidationError as e:
        sys.exit(e)

    return 0


def validation_main(args: argparse.Namespace) -> None:
    # pylint: disable=too-many-locals
    def get_message_to_validate(message_path: Path, is_valid: bool) -> "OriginalMessage":
        if not message_path.is_file():
            raise ValidationError(f"{message_path} is not a regular file")
        message_bytes = message_path.read_bytes()
        message = OriginalMessage(message_bytes, is_valid, message_path.name)
        return message

    root_message_id: str = args.message_identifier
    path_spec: Path = args.specification
    path_invalid: Optional[Path] = args.directory_invalid
    path_valid: Optional[Path] = args.directory_valid
    full_output_path: Optional[Path] = args.json_output
    abort_on_error: bool = args.abort_on_error

    path_generators: Dict[Path, Iterator[Path]] = {}
    for path in [path_valid, path_invalid]:
        if path is not None:
            path_generators[path] = path.glob("*")

    try:
        identifier = ID(root_message_id)
    except AssertionError as e:
        raise ValidationError(f'invalid identifier "{root_message_id}" : {str(e)}') from e

    try:
        pdu_message = PyRFLX.from_specs([str(path_spec)])[str(identifier.parent)][
            str(identifier.name)
        ]
    except KeyError as e:
        raise ValidationError(
            f'message "{str(identifier.name)}" could not be '
            f'found in package "{str(identifier.parent)}"'
        ) from e
    except FileNotFoundError as e:
        raise ValidationError(f"specification {str(e)}") from e

    classified_incorrectly = 0
    with JsonOutputWriter(full_output_path) as json_output_writer:
        validator = Validator(pdu_message)
        for path, path_generator in path_generators.items():
            for message_file_path in path_generator:
                msg = get_message_to_validate(message_file_path, path == path_valid)
                validation_result = validator.validate_message(msg)
                json_output_writer.write(validation_result.get_json_output())
                print(validation_result.get_abbreviated_output())
                if validation_result.classification in [
                    Classification.FP,
                    Classification.FN,
                ]:
                    if abort_on_error:
                        sys.exit(1)
                    else:
                        classified_incorrectly += 1

    print(f"Summary: {classified_incorrectly} messages were classified incorrectly.")
    if classified_incorrectly != 0:
        sys.exit(1)


class JsonOutputWriter:
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

    def __enter__(self) -> "JsonOutputWriter":
        return self

    def __exit__(
        self,
        exception_type: Optional[Type[BaseException]],
        exception_value: Optional[BaseException],
        traceback: Optional[TracebackType],
    ) -> None:

        if exception_value is not None:
            raise ValidationError(str(exception_value))

        if self.file is not None:
            self.file.write("\n]")
            self.file.close()

    def write(self, content: Dict[str, object]) -> None:
        if self.file is not None:
            if self.count != 0:
                self.file.write(",\n")
            json.dump(
                content,
                self.file,
                indent="\t",
            )
            self.count += 1


class Validator:
    def __init__(self, pdu_message: MessageValue) -> None:
        self.__pdu_message = pdu_message

    def validate_message(self, original_message: "OriginalMessage") -> "ValidationResult":
        parser_result = self.__parse_message(original_message.bytes)
        validation_result = ValidationResult(parser_result, original_message)
        parsed_message = parser_result.parsed_message

        if isinstance(parsed_message, MessageValue):
            validation_result.correct_serialization = (
                parsed_message.bytestring == original_message.bytes
            )
        else:
            validation_result.correct_serialization = False

        if parser_result.is_valid and original_message.is_valid:
            validation_result.classification = Classification.TP
        elif parser_result.is_valid and not original_message.is_valid:
            validation_result.classification = Classification.FP
        elif not parser_result.is_valid and original_message.is_valid:
            validation_result.classification = Classification.FN
        else:
            validation_result.classification = Classification.TN

        return validation_result

    def __parse_message(self, message: bytes) -> "ParserResult":
        pdu_model: MessageValue = self.__pdu_message.clone()
        result = ParserResult()
        try:
            pdu_model.parse(message)
            result.parsed_message = pdu_model
            # ISSUE: Componolit/RecordFlux#510
        except Exception as e:  # pylint: disable=broad-except
            result.error_message = str(e)
        return result


@dataclass
class OriginalMessage:
    bytes: bytes
    is_valid: bool
    file_name: str


class ParserResult:
    def __init__(self) -> None:
        self.__parsed_message: Optional[MessageValue] = None
        self.__error_message: str = ""
        self.__msg_valid: Optional[bool] = None

    @property
    def parsed_message(self) -> Optional[MessageValue]:
        return self.__parsed_message

    @parsed_message.setter
    def parsed_message(self, parsed_message: MessageValue) -> None:
        self.__parsed_message = parsed_message
        self.__msg_valid = parsed_message.valid_message

    @property
    def error_message(self) -> str:
        return self.__error_message

    @error_message.setter
    def error_message(self, error_message: str) -> None:
        self.__error_message = error_message
        self.__msg_valid = False

    @property
    def is_valid(self) -> Optional[bool]:
        return self.__msg_valid


class ValidationResult:
    def __init__(self, parser_result: ParserResult, original_message: OriginalMessage):
        self.__parser_result: ParserResult = parser_result
        self.__original_message: OriginalMessage = original_message
        self.__classification: Classification = Classification.NI
        self.__correct_serialization: Optional[bool] = None

    @property
    def classification(self) -> "Classification":
        return self.__classification

    @classification.setter
    def classification(self, classifier: "Classification") -> None:
        self.__classification = classifier

    @property
    def correct_serialization(self) -> Optional[bool]:
        return self.__correct_serialization

    @correct_serialization.setter
    def correct_serialization(self, is_correct: bool) -> None:
        self.__correct_serialization = is_correct

    def get_json_output(self) -> Dict[str, object]:
        parsed_message = self.__parser_result.parsed_message
        parsed_field_values: Dict[
            str, Union[MessageValue, Sequence[TypeValue], int, str, bytes]
        ] = {}
        if parsed_message is not None:
            parsed_field_values.fromkeys(parsed_message.fields)
            for field_name in parsed_message.fields:
                try:
                    field_value = parsed_message.get(field_name)
                except ValueError as e:
                    field_value = str(f"{e} or not set")
                if isinstance(field_value, MessageValue):
                    field_value = field_value.bytestring.hex()
                elif isinstance(field_value, bytes):
                    field_value = field_value.hex()
                parsed_field_values[field_name] = field_value

        return {
            "file name": self.__original_message.file_name,
            "original": self.__original_message.bytes.hex(),
            "parsed": parsed_message.bytestring.hex() if parsed_message is not None else "",
            "parsed_field_values": parsed_field_values,
            "error_message": self.__parser_result.error_message,
            "provided_as": self.__original_message.is_valid,
            "recognized_as": self.__parser_result.is_valid,
            "classification": self.__classification.value,
            "serialized_correctly": self.__correct_serialization,
        }

    def get_abbreviated_output(self) -> str:
        parsed_message = self.__parser_result.parsed_message
        if parsed_message is None:
            identifier = "Message could not be parsed"
        else:
            identifier = parsed_message.identifier.__str__()

        output = (
            f"classification: {self.classification.value}, "
            f"identifier: {identifier}, "
            f"file name: {self.__original_message.file_name}, "
            f"error: {self.__parser_result.error_message}"
        )
        return output


class ValidationError(BaseException):
    def __init__(self, message: str) -> None:
        super().__init__()
        self.message = message

    def __str__(self) -> str:
        return self.message


class Classification(Enum):
    TN = "TrueNegative"
    TP = "TruePositive"
    FP = "FalsePositive"
    FN = "FalseNegative"
    NI = "NotInitialized"


if __name__ == "__main__":
    sys.exit(cli(sys.argv))
