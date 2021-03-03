#!/usr/bin/env -S python3 -O

import argparse
import json
import sys
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
    def get_message_from_path(message_path: Path) -> bytes:
        if not message_path.is_file():
            raise ValidationError(f"{message_path} is not a regular file")
        return message_path.read_bytes()

    def rflx_parse_message_from_bytes(message: bytes) -> Union[str, MessageValue]:
        pdu_model: MessageValue = pdu_message.clone()
        try:
            pdu_model.parse(message)
            return pdu_model
        except RecordFluxError as e:
            return str(e)

    def validate(original_message: bytes, original_message_is_valid: bool, msg_path: Path) -> bool:
        parser_result = rflx_parse_message_from_bytes(original_message)
        parsed_message_is_valid = False

        if isinstance(parser_result, MessageValue):
            parsed_message_is_valid = (
                parser_result.bytestring == original_message and parser_result.valid_message
            )

        if original_message_is_valid:
            validation_success = parsed_message_is_valid
        else:
            validation_success = not parsed_message_is_valid

        if validation_success:
            output_writer.passed(
                parser_result,
                msg_path,
                original_message,
                original_message_is_valid,
                parsed_message_is_valid,
            )
        else:
            output_writer.failed(
                parser_result,
                msg_path,
                original_message,
                original_message_is_valid,
                parsed_message_is_valid,
            )

        return validation_success

    valid_message_paths: Optional[Iterator[Path]] = (
        path_valid.glob("*") if path_valid is not None else path_valid
    )
    invalid_message_paths: Optional[Iterator[Path]] = (
        path_invalid.glob("*") if path_invalid is not None else path_invalid
    )

    with OutputWriter(full_output_path) as output_writer:
        for paths in [valid_message_paths, invalid_message_paths]:
            if paths is None:
                continue
            for path in paths:
                original_message = get_message_from_path(path)
                val_success = validate(original_message, paths == valid_message_paths, path)
                if not val_success and abort_on_error:
                    return


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

        if self.classified_incorrectly != 0:
            raise ValidationError(
                f"{self.classified_incorrectly} messages were classified incorrectly"
            )

    def failed(
        self,
        parser_result: Union[str, MessageValue],
        message_file_path: Path,
        original_message: bytes,
        valid_original_message: bool,
        valid_parser_result: bool,
    ) -> None:
        print(f"{str(message_file_path):<80} FAILED")
        print(f"provided as: {valid_original_message}\t recognized as: {valid_parser_result}")
        if isinstance(parser_result, str):
            print(f"{parser_result}")
        else:
            print("error: invalid message")
        self.classified_incorrectly += 1
        if self.file is not None:
            json_out = self.__get_json_output(
                message_file_path,
                original_message,
                parser_result,
                valid_original_message,
                valid_parser_result,
                False,
            )
            self.__write_json_output(json_out)

    def passed(
        self,
        parser_result: Union[str, MessageValue],
        message_file_path: Path,
        original_message: bytes,
        valid_original_message: bool,
        valid_parser_result: bool,
    ) -> None:
        print(f"{str(message_file_path):<80} {'PASSED'}")

        if self.file is not None:
            json_out = self.__get_json_output(
                message_file_path,
                original_message,
                parser_result,
                valid_original_message,
                valid_parser_result,
                True,
            )
            self.__write_json_output(json_out)

    def __write_json_output(self, values: Dict[str, object]) -> None:
        if self.file is not None:
            if self.count != 0:
                self.file.write(",\n")
            json.dump(
                values,
                self.file,
                indent="\t",
            )
            self.count += 1

    @staticmethod
    def __get_json_output(
        file_name: Path,
        original_message: bytes,
        parsed_message: Union[MessageValue, str],
        valid_original_message: bool,
        valid_parser_result: bool,
        correct_serialization: bool,
    ) -> Dict[str, object]:
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
            "file name": str(file_name),
            "provided as": valid_original_message,
            "recognized as": valid_parser_result,
            "original": original_message.hex(),
        }

        if isinstance(parsed_message, MessageValue):
            output["parsed"] = parsed_message.bytestring.hex()
            output["parsed field values"] = get_field_values(parsed_message)
            output["serialized correctly"] = correct_serialization
        else:
            output["parsed"] = parsed_message

        return output


class ValidationError(Exception):
    def __init__(self, message: str) -> None:
        super().__init__()
        self.message = message

    def __str__(self) -> str:
        return self.message


if __name__ == "__main__":
    sys.exit(cli(sys.argv))
