"""Ensure that the test specifications contain the expected errors."""

from __future__ import annotations

import pathlib
from collections.abc import Sequence

import pytest

from rflx import specification
from rflx.error import RecordFluxError
from tests.const import IDE_DIR


def assert_error(filename: pathlib.Path, expected: Sequence[str]) -> None:
    p = specification.Parser()
    with pytest.raises(RecordFluxError) as excinfo:
        error = RecordFluxError()

        try:
            p.parse(filename)
        except RecordFluxError as e:
            error.extend(e)

        try:
            p.create_model()
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()

    assert str(excinfo.value).split("\n") == expected


def test_multiple_errors() -> None:
    path = IDE_DIR / "multiple_errors.rflx"
    assert_error(
        path,
        [
            f'{path}:6:9: parser: error: illegal prefix "RFLX" in package identifier'
            ' "RFLX_Invalid"',
            f'{path}:298:5: parser: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: parser: info: previous identifier was "RFLX_Invalid"',
            f'{path}:6:9: parser: error: file name does not match unit name "RFLX_Invalid",'
            ' should be "rflx_invalid.rflx"',
            f'{path}:40:4: model: error: illegal redefinition of built-in type "Boolean"',
            f'{path}:6:9: parser: error: illegal prefix "RFLX" in package identifier'
            ' "RFLX_Invalid"',
            f'{path}:298:5: parser: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: parser: info: previous identifier was "RFLX_Invalid"',
            f'{path}:6:9: parser: error: file name does not match unit name "RFLX_Invalid", should'
            ' be "rflx_invalid.rflx"',
            f'{path}:40:4: model: error: illegal redefinition of built-in type "Boolean"',
            f'{path}:13:9: model: error: first of "Range2" contains variable',
            f'{path}:13:9: model: error: last of "Range2" contains variable',
            f'{path}:16:9: model: error: range of "Range3" negative',
            f'{path}:19:9: model: error: size of "Range4" contains variable',
            f'{path}:22:9: model: error: size of "Range5" too small',
            f'{path}:25:27: parser: error: undefined element type "RFLX_Invalid::Undef"',
            f'{path}:28:18: model: error: modulus of "S" exceeds limit (2**63)',
            f'{path}:31:9: model: error: modulus of "T" not power of two',
            f'{path}:34:9: model: error: modulus of "Mod3" contains variable',
            f'{path}:47:28: model: error: duplicate literal "E2_2"',
            f"{path}:47:22: model: info: previous occurrence",
            f'{path}:47:9: model: error: size of "E2" too small',
            f'{path}:50:46: model: error: duplicate enumeration value "2" in "E3"',
            f"{path}:50:35: model: info: previous occurrence",
            f'{path}:73:10: model: error: unreachable field "Z" in "RFLX_Invalid::M1"',
            f'{path}:73:10: model: error: duplicate link from "Z" to "Final"',
            f"{path}:73:16: model: info: duplicate link",
            f"{path}:73:26: model: info: duplicate link",
            f'{path}:79:14: parser: error: undefined type "RFLX_Invalid::Undef_Type"',
            f'{path}:81:18: parser: error: undefined field "X"',
            f'{path}:79:10: model: error: missing type for field "Z" in "RFLX_Invalid::M2"',
            f'{path}:86:9: model: error: unsupported element type size of sequence "A3"',
            f'{path}:85:9: model: info: type "E6" has size 4, must be multiple of 8',
            f'{path}:93:30: model: error: invalid First for field "Final"',
            f"{path}:96:9: model: error: message size must be multiple of 8 bit",
            f"{path}:96:9: model: info: on path Field",
            f'{path}:103:26: model: error: size of opaque field "Data" not multiple of 8 bit'
            " (Length -> Data)",
            f'{path}:106:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:106:40: parser: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:109:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:109:26: parser: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:112:4: parser: error: undefined type "RFLX_Invalid::Undef_PDU" in refinement',
            f'{path}:112:31: parser: error: undefined type "RFLX_Invalid::R" in refinement '
            'of "RFLX_Invalid::Undef_PDU"',
            f'{path}:115:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:115:26: parser: error: undefined type "RFLX_Invalid::Undef_Type" in '
            'refinement of "RFLX_Invalid::PDU1"',
            f'{path}:118:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:118:33: parser: error: undefined type "RFLX_Invalid::R" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:124:21: parser: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:125:21: parser: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:128:21: parser: error: undefined base message "RFLX_Invalid::Undef_Type"'
            " in derived message",
            f'{path}:131:9: parser: error: illegal derivation "RFLX_Invalid::Ref3"',
            f'{path}:8:9: parser: info: invalid base message type "RFLX_Invalid::R"',
            f'{path}:134:21: parser: error: undefined base message "RFLX_Invalid::Ref1"'
            " in derived message",
            f'{path}:142:19: model: error: undefined variable "Undef_Var"',
            f"{path}:142:19: model: info: on path Length -> Data",
            f"{path}:149:25: model: error: expected integer type",
            f"{path}:149:25: model: info: found aggregate with element type universal integer"
            " (5 .. 20)",
            f"{path}:149:19: model: info: on path Length -> Data -> F1",
            f'{path}:157:24: model: error: expected integer type "RFLX_Invalid::R" (5 .. 23)',
            f"{path}:157:24: model: info: found aggregate with element type universal integer"
            " (1 .. 100)",
            f"{path}:157:19: model: info: on path Length -> Data -> F1 -> F2 -> Final",
            f'{path}:153:19: model: error: invalid use of size attribute for "1"',
            f'{path}:169:10: model: error: name conflict for "F2_F1" in "RFLX_Invalid::M5"',
            f'{path}:160:9: model: info: when merging message "RFLX_Invalid::M4"',
            f'{path}:167:10: model: info: into field "F2"',
            f'{path}:181:27: model: error: expected sequence type "RFLX_Invalid::R_Sequence"'
            ' with element integer type "RFLX_Invalid::R" (5 .. 23)',
            f"{path}:181:27: model: info: found aggregate with element type universal integer"
            " (1 .. 1000)",
            f"{path}:181:19: model: info: on path Length -> Data1 -> Data2",
            f'{path}:185:27: model: error: expected sequence type "__INTERNAL__::Opaque"'
            ' with element integer type "Byte" (0 .. 255)',
            f"{path}:185:27: model: info: found aggregate with element type universal integer"
            " (1 .. 1000)",
            f"{path}:185:19: model: info: on path Length -> Data1 -> Data2 -> Final",
            f'{path}:188:36: parser: error: undefined element type "RFLX_Invalid::M5"',
            f'{path}:194:17: parser: error: undefined type "RFLX_Invalid::M5_Sequence"',
            f'{path}:194:10: model: error: missing type for field "Data" in "RFLX_Invalid::M7"',
            f'{path}:205:10: model: error: conflicting conditions for field "F1"',
            f"{path}:209:19: model: info: condition 0 (F1 -> F2): F1 < 80",
            f"{path}:207:19: model: info: condition 1 (F1 -> Final): F1 > 50",
            f'{path}:217:10: model: error: unreachable field "F2" in "RFLX_Invalid::M9"',
            f'{path}:232:18: model: error: fixed size field "F1" with size aspect',
            f"{path}:242:30: model: error: illegal first aspect at initial link",
            f'{path}:251:19: model: error: undefined variable "F1"',
            f"{path}:251:19: model: info: on path F1",
            f'{path}:258:18: model: error: negative size for field "F2" (F1 -> F2)',
            f"{path}:258:18: model: error: size of opaque field "
            f'"F2" not multiple of 8 bit (F1 -> F2)',
            f'{path}:255:9: model: error: negative start for field "Final" (F1 -> F2 -> Final)',
            f"{path}:258:18: model: info: unsatisfied \"F2'Last ="
            " (F1'Last + 1 + (F1 - 2 ** 33)) - 1\"",
            f"{path}:257:10: model: info: unsatisfied \"F1'Last = (Message'First + 32) - 1\"",
            f'{path}:200:9: model: info: unsatisfied "F1 < 2 ** 32"',
            f"{path}:261:10: model: info: unsatisfied \"F2'Last + 1 >= Message'First\"",
            f'{path}:267:10: model: error: unconstrained field "F1" without size aspect',
            f'{path}:273:9: model: error: field "F3" not congruent with overlaid field "F1"',
            f"{path}:273:9: model: info: unsatisfied \"F1'First = Message'First\"",
            f"{path}:275:10: model: info: unsatisfied \"F1'Last = (Message'First + 8) - 1\"",
            f"{path}:277:13: model: info: unsatisfied \"(F1'First + 16) - 1 = F1'Last\"",
            f'{path}:289:29: model: error: size aspect for final field in "RFLX_Invalid::M17"',
            f"{path}:292:9: model: error: unnecessary always-valid aspect"
            ' on "Unnecessary_Always_Valid_Enum"',
            f'{path}:37:9: model: error: name conflict for type "RFLX_Invalid::R"',
            f'{path}:8:9: model: info: previous occurrence of "RFLX_Invalid::R"',
            f'{path}:40:9: model: error: name conflict for type "__BUILTINS__::Boolean"',
            '__BUILTINS__:0:0: model: info: previous occurrence of "__BUILTINS__::Boolean"',
            f"{path}:53:9: model: error: conflicting literals: False, True",
            '__BUILTINS__:0:0: model: info: previous occurrence of "False"',
            '__BUILTINS__:0:0: model: info: previous occurrence of "True"',
            f"{path}:43:9: model: error: conflicting literals: Foo",
            f'{path}:40:21: model: info: previous occurrence of "Foo"',
            f"{path}:56:9: model: error: conflicting literals: Bar, Foo",
            f'{path}:40:26: model: info: previous occurrence of "Bar"',
            f'{path}:40:21: model: info: previous occurrence of "Foo"',
            f"{path}:56:9: model: error: conflicting literals: Foo",
            f'{path}:43:30: model: info: previous occurrence of "Foo"',
            f'{path}:56:21: model: error: literal "E4" conflicts with type declaration',
            f'{path}:53:9: model: info: conflicting type "RFLX_Invalid::E4"',
        ],
    )


def test_parse_error_aggregate_non_number() -> None:
    path = IDE_DIR / "parse_error_aggregate_non_number.rflx"
    assert_error(
        path,
        [
            f"{path}:10:26: parser: error: Cannot parse <relation>",
            f"{path}:10:39: parser: error: Expected Numeral, got 'First'",
            f"{path}:10:39: parser: error: Expected ';', got 'First'",
        ],
    )


def test_parse_error_invalid_location() -> None:
    path = IDE_DIR / "parse_error_invalid_location.rflx"
    assert_error(
        path,
        [
            f'{path}:7:21: parser: error: invalid aspect "Invalid"',
            f"{path}:3:9: model: error: message size must be multiple of 8 bit",
            f"{path}:3:9: model: info: on path Y",
        ],
    )
