from __future__ import annotations

import importlib
import json
from collections import defaultdict
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from itertools import product
from pathlib import Path
from types import TracebackType
from typing import Optional, TextIO, Union

from ruamel.yaml.main import YAML

from rflx import expression as expr
from rflx.identifier import ID, StrID
from rflx.model import AlwaysVerify, Cache, Link, Message, Model, Refinement, type_ as mty
from rflx.pyrflx import ChecksumFunction, Package, PyRFLX, PyRFLXError
from rflx.pyrflx.typevalue import MessageValue
from rflx.specification import Parser


class Validator:
    def __init__(
        self,
        files: Iterable[Union[str, Path]],
        checksum_module: Optional[str] = None,
        cache: Optional[Cache] = None,
        split_disjunctions: bool = False,
    ):
        model = self._create_model(
            [Path(f) for f in files],
            AlwaysVerify() if cache is None else cache,
            split_disjunctions,
        )
        checksum_functions = self._parse_checksum_module(checksum_module)
        missing_checksum_definitions = {
            (str(message.identifier), str(field_identifier))
            for message in model.messages
            for field_identifier in message.checksums
        } - {
            (str(message_name), field_name)
            for message_name, checksum_mapping in checksum_functions.items()
            for field_name in checksum_mapping
        }

        if len(missing_checksum_definitions) != 0:
            raise ValidationError(
                "missing checksum definition for "
                + ", ".join(
                    [
                        f'field "{field_name}" of "{message_name}"'
                        for message_name, field_name in missing_checksum_definitions
                    ],
                ),
            )

        try:
            self._pyrflx = PyRFLX(model, checksum_functions)
        except PyRFLXError as e:
            raise ValidationError(f"invalid checksum definition: {e}") from e

    def validate(  # noqa: PLR0913
        self,
        message_identifier: ID,
        paths_invalid: Optional[list[Path]] = None,
        paths_valid: Optional[list[Path]] = None,
        json_output: Optional[Path] = None,
        abort_on_error: bool = False,
        coverage: bool = False,
        target_coverage: float = 0.00,
    ) -> None:
        self._check_arguments(
            message_identifier,
            paths_invalid,
            paths_valid,
            json_output,
            abort_on_error,
            coverage,
            target_coverage,
        )

        try:
            message_value = self._pyrflx.package(message_identifier.parent).new_message(
                message_identifier.name,
            )
        except KeyError as e:
            raise ValidationError(
                f'message "{message_identifier.name}" could not be found '
                f'in package "{message_identifier.parent}"',
            ) from e

        incorrectly_classified = 0
        coverage_info = CoverageInformation(list(self._pyrflx), coverage)

        paths = [
            *[(p, True) for p in paths_valid or []],
            *[(p, False) for p in paths_invalid or []],
        ]

        with OutputWriter(json_output) as output_writer:
            for provided_path, is_valid in paths:
                if provided_path.is_dir():
                    files = sorted(provided_path.glob("*.raw"))
                    if not files:
                        raise ValidationError(
                            f"{provided_path} contains no files with a .raw file extension, please "
                            "provide a directory with .raw files or a list of individual files with"
                            " any extension",
                        )
                else:
                    files = [provided_path]
                for path in files:
                    validation_result = self._validate_message(
                        path,
                        is_valid,
                        message_value,
                    )
                    coverage_info.update(validation_result.parsed_message)
                    validation_result.print_console_output()
                    output_writer.write_result(validation_result)
                    if not validation_result.validation_success:
                        incorrectly_classified += 1
                        if abort_on_error:
                            raise ValidationError(
                                f"aborted: message {path} was classified incorrectly",
                            )
            coverage_info.print_coverage()

        error_msgs = []
        if incorrectly_classified != 0:
            error_msgs.append(f"{incorrectly_classified} messages were classified incorrectly")
        if (
            coverage
            and coverage_info.total_covered_links / coverage_info.total_links
            < target_coverage / 100
        ):
            error_msgs.append(
                f"missed target coverage of {target_coverage/100:.2%}, "
                f"reached {coverage_info.total_covered_links / coverage_info.total_links:.2%}",
            )
        if len(error_msgs) > 0:
            raise ValidationError("\n".join(e for e in error_msgs))

    def _check_arguments(
        self,
        _message_identifier: ID,
        paths_invalid: Optional[list[Path]] = None,
        paths_valid: Optional[list[Path]] = None,
        json_output: Optional[Path] = None,
        _abort_on_error: bool = False,
        _coverage: bool = False,
        target_coverage: float = 0.00,
    ) -> None:
        """Perform some additional sanity checks of validator specific arguments."""
        if target_coverage < 0 or target_coverage > 100:
            raise ValidationError(
                f"target coverage must be between 0 and 100, got {target_coverage}",
            )

        if paths_valid is None and paths_invalid is None:
            raise ValidationError("must provide directory with valid and/or invalid messages")

        for path in [*(paths_valid or []), *(paths_invalid or [])]:
            if not path.exists():
                raise ValidationError(f"{path} does not exist")

        if json_output is not None and json_output.exists():
            if json_output.is_dir():
                raise ValidationError(
                    f"{json_output} is a directory, please specify a file for the validation"
                    " report",
                )
            raise ValidationError(f"output file already exists: {json_output}")

    def _create_model(
        self,
        files: Sequence[Path],
        cache: Cache,
        split_disjunctions: bool,
    ) -> Model:
        for f in files:
            if not f.is_file():
                raise ValidationError(f'specification file not found: "{f}"')
        parser = Parser(cache)
        parser.parse(*files)
        model = parser.create_model()
        if split_disjunctions:
            messages: dict[ID, Message] = {}
            for t in model.types:
                if isinstance(t, Message):
                    messages[t.identifier] = self._expand_message_links(t, messages)
            model = Model([self._replace_messages(t, messages) for t in model.types])
        return model

    def _expand_message_links(self, message: Message, messages: Mapping[ID, Message]) -> Message:
        """Split disjunctions in link conditions."""
        structure = []
        for link in message.structure:
            conditions = self._expand_expression(link.condition.simplified())

            if len(conditions) == 1:
                structure.append(link)
                continue

            structure.extend(
                Link(
                    link.source,
                    link.target,
                    condition,
                    link.size,
                    link.first,
                    condition.location,
                )
                for condition in conditions
            )

        types = {f: self._replace_messages(t, messages) for f, t in message.types.items()}

        return message.copy(structure=structure, types=types)

    @staticmethod
    def _replace_messages(type_: mty.Type, messages: Mapping[ID, Message]) -> mty.Type:
        """Recursively replace messages."""
        if isinstance(type_, Message):
            return messages[type_.identifier]
        if isinstance(type_, Refinement):
            return Refinement(
                type_.package,
                messages[type_.pdu.identifier],
                type_.field,
                messages[type_.sdu.identifier],
                type_.condition,
                type_.location,
            )
        if isinstance(type_, mty.Sequence) and isinstance(type_.element_type, Message):
            return mty.Sequence(
                type_.identifier,
                messages[type_.element_type.identifier],
                type_.location,
            )
        return type_

    @staticmethod
    def _expand_expression(expression: expr.Expr) -> list[expr.Expr]:
        """Create disjunction by expanding the expression and return it as a list."""
        if isinstance(expression, expr.Or):
            return expression.terms

        if not isinstance(expression, expr.And):
            return [expression]

        atoms = []
        disjunctions = []

        for e in expression.terms:
            if isinstance(e, expr.Or):
                disjunctions.append(e.terms)
            else:
                atoms.append(e)

        disjunctions.append([expr.And(*atoms)])

        result: list[expr.Expr] = []
        for value in (expr.And(*dict.fromkeys(p)).simplified() for p in product(*disjunctions)):
            for seen in result:
                if expr.Not(expr.Equal(value, seen)).check().result == expr.ProofResult.UNSAT:
                    break
            else:
                result.append(value)
        return result

    @staticmethod
    def _parse_checksum_module(name: Optional[str]) -> dict[StrID, dict[str, ChecksumFunction]]:
        if name is None:
            return {}

        checksum_functions = {}

        try:
            checksum_module = importlib.import_module(name)
        except ImportError as e:
            raise ValidationError(
                f'provided module "{name}" cannot be '
                f"imported, make sure module name is provided as "
                f'"package.module" and not as file system path: {e}',
            ) from e

        try:
            checksum_functions = checksum_module.checksum_functions
        except AttributeError as e:
            raise ValidationError(
                f'missing attribute "checksum_functions" in checksum module "{name}"',
            ) from e

        if not isinstance(checksum_functions, dict):
            raise ValidationError(f'attribute "checksum_functions" of "{name}" is not a dict')

        for message_id, checksum_field_mapping in checksum_functions.items():
            if not isinstance(checksum_field_mapping, dict):
                raise ValidationError(f'value at key "{message_id}" is not a dict')
            for field_name, checksum_func_callable in checksum_field_mapping.items():
                if not callable(checksum_func_callable):
                    raise ValidationError(
                        f'value at key "{field_name}" is not a callable checksum function',
                    )

        return checksum_functions

    @staticmethod
    def _validate_message(
        message_path: Path,
        valid_original_message: bool,
        message_value: MessageValue,
    ) -> ValidationResult:
        if not message_path.is_file():
            raise ValidationError(f"{message_path} is not a regular file")

        parameters_path = message_path.with_suffix(".yaml")
        message_parameters: dict[str, Union[bool, int, str]] = {}

        if parameters_path.is_file():
            yaml = YAML()
            message_parameters = yaml.load(parameters_path)

        original_message = message_path.read_bytes()
        parsed_message = message_value.clone()
        parser_error = None

        try:
            parsed_message.add_parameters(message_parameters)
        except PyRFLXError as e:
            raise ValidationError(f"{message_path}: {e}") from e

        try:
            parsed_message.parse(original_message)
            valid_parser_result = parsed_message.bytestring == original_message
            if not valid_parser_result:
                assert parsed_message.valid_message
                assert len(parsed_message.bytestring) <= len(original_message)
                assert original_message.startswith(parsed_message.bytestring)
                parser_error = "message parsed by PyRFLX is shorter than the original message"
        except PyRFLXError as e:
            parser_error = str(e)
            valid_parser_result = False

        return ValidationResult(
            valid_original_message == valid_parser_result,
            parsed_message,
            parser_error,
            message_path,
            original_message,
            valid_original_message,
            valid_parser_result,
        )


class CoverageInformation:
    def __init__(self, packages: Sequence[Package], coverage: bool) -> None:
        self._total_message_coverage: dict[ID, dict[Link, bool]] = {}
        self._spec_files: dict[str, list[ID]] = defaultdict(list)
        self._coverage = coverage

        if not self._coverage:
            return

        for package in packages:
            for message in package:
                assert isinstance(message, MessageValue)
                self._total_message_coverage[message.identifier] = {
                    link: False for link in message.model.structure
                }

                assert message.model.location is not None
                assert message.model.location.source is not None
                file_name = message.model.location.source.name
                self._spec_files[file_name].append(message.identifier)

        self.total_links = sum(
            len(structure) for structure in self._total_message_coverage.values()
        )
        self.total_covered_links = 0

    def update(self, message_value: MessageValue) -> None:
        if self._coverage:
            messages = [*message_value.inner_messages(), message_value]
            for message in messages:
                for link in message.path:
                    if not self._total_message_coverage[message.identifier][link]:
                        self.total_covered_links += 1
                        self._total_message_coverage[message.identifier][link] = True

    def file_total_links(self, file_name: str) -> int:
        assert file_name in self._spec_files
        return sum(
            len(self._total_message_coverage[message]) for message in self._spec_files[file_name]
        )

    def file_covered_links(self, file_name: str) -> int:
        assert file_name in self._spec_files
        return sum(
            list(self._total_message_coverage[message].values()).count(True)
            for message in self._spec_files[file_name]
        )

    def file_uncovered_links(self, file_name: str) -> list[Link]:
        assert file_name in self._spec_files
        return [
            link
            for message in self._spec_files[file_name]
            for link, covered in self._total_message_coverage[message].items()
            if not covered
        ]

    def print_coverage(self) -> None:
        if self._coverage:
            self._print_coverage_overview()
            if self.total_covered_links / self.total_links != 1:
                self._print_link_coverage()

    def _print_coverage_overview(self) -> None:
        print("\n")  # noqa: T201
        print("-" * 80)  # noqa: T201
        print(f"{'RecordFlux Validation Coverage Report' : ^80}".rstrip())  # noqa: T201
        print(f"Directory: {Path.cwd()}")  # noqa: T201
        print("-" * 80)  # noqa: T201
        print(f"{'File' : <40} {'Links' : >10} {'Used' : >10} {'Coverage' : >15}")  # noqa: T201
        for file in self._spec_files:
            file_links = self.file_total_links(file)
            file_covered_links = self.file_covered_links(file)
            print(  # noqa: T201
                f"{file : <40} {file_links : >10} {file_covered_links : >10} "
                f"{file_covered_links / file_links :>15.2%}",
            )
        print("-" * 80)  # noqa: T201
        print(  # noqa: T201
            f"{'TOTAL' : <40} {self.total_links: >10} {self.total_covered_links : >10} "
            f"{self.total_covered_links / self.total_links :15.2%}",
        )
        print("-" * 80)  # noqa: T201

    def _print_link_coverage(self) -> None:
        print("\n")  # noqa: T201
        print("=" * 80)  # noqa: T201
        print(f"{'Uncovered Links' : ^80}".rstrip())  # noqa: T201
        print("=" * 80)  # noqa: T201
        for file in self._spec_files:
            uncovered_links = self.file_uncovered_links(file)
            if len(uncovered_links) != 0:
                print("\n")  # noqa: T201
                print(f"{file : ^80}".rstrip())  # noqa: T201
                print("-" * 80)  # noqa: T201
                for link in sorted(uncovered_links, key=lambda x: str(x.location)):
                    print(  # noqa: T201
                        f"{link.location!s:<17}"
                        f": missing link {link.source.name:^25} -> {link.target.name:^20}".rstrip(),
                    )


@dataclass
class ValidationResult:
    validation_success: bool
    parsed_message: MessageValue
    parser_error: Optional[str]
    message_path: Path
    original_message: bytes
    valid_original_message: bool
    valid_parser_result: bool

    def as_json(self) -> dict[str, object]:
        output = {
            "file name": str(self.message_path),
            "provided as": self.valid_original_message,
            "recognized as": self.valid_parser_result,
            "original": self.original_message.hex(),
        }
        if self.parsed_message.valid_message:
            output["parsed"] = self.parsed_message.bytestring.hex()
        output["parsed fields"] = self.parsed_message.as_json()
        if self.parser_error is not None:
            output["error"] = self.parser_error

        return output

    def print_console_output(self) -> None:
        if self.validation_success:
            print(f"{self.message_path!s:<80} PASSED")  # noqa: T201
        else:
            print(  # noqa: T201
                f"{self.message_path!s:<80} FAILED\n"
                f"provided as: {self.valid_original_message}\t "
                f"recognized as: {self.valid_parser_result}",
            )
            if self.parser_error is not None:
                print(self.parser_error)  # noqa: T201


class OutputWriter:
    file: Optional[TextIO]

    def __init__(self, file: Optional[Path]) -> None:
        if file is not None:
            try:
                self.file = file.open("w", encoding="utf-8")
            except OSError as e:
                raise ValidationError(f"cannot open output file {file}: {e}") from e
            self.file.write("[\n")
            self.count = 0
        else:
            self.file = file
        self.classified_incorrectly = 0

    def __enter__(self) -> OutputWriter:
        return self

    def __exit__(
        self,
        exception_type: Optional[type[BaseException]],
        exception_value: Optional[BaseException],
        traceback: Optional[TracebackType],
    ) -> None:
        if self.file is not None:
            self.file.write("\n]\n")
            self.file.close()

    def write_result(self, validation_result: ValidationResult) -> None:
        if self.file is not None:
            if self.count != 0:
                self.file.write(",\n")
            json.dump(
                validation_result.as_json(),
                self.file,
                indent="    ",
            )
            self.count += 1


class ValidationError(Exception):
    pass
