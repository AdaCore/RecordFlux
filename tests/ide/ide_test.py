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
            f'{path}:295:5: parser: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: parser: info: previous identifier was "RFLX_Invalid"',
            f'{path}:6:9: parser: error: file name does not match unit name "RFLX_Invalid",'
            ' should be "rflx_invalid.rflx"',
            f'{path}:31:4: model: error: illegal redefinition of built-in type "Boolean"',
            f'{path}:6:9: parser: error: illegal prefix "RFLX" in package identifier'
            ' "RFLX_Invalid"',
            f'{path}:295:5: parser: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: parser: info: previous identifier was "RFLX_Invalid"',
            f'{path}:6:9: parser: error: file name does not match unit name "RFLX_Invalid", should'
            ' be "rflx_invalid.rflx"',
            f'{path}:31:4: model: error: illegal redefinition of built-in type "Boolean"',
            f'{path}:72:18: parser: error: undefined field "X"',
            f'{path}:13:9: model: error: first of "Range2" contains variable',
            f'{path}:13:9: model: error: last of "Range2" contains variable',
            f'{path}:16:9: model: error: range of "Range3" negative',
            f'{path}:19:9: model: error: size of "Range4" contains variable',
            f'{path}:22:9: model: error: size of "Range5" too small',
            f'{path}:25:27: model: error: undefined element type "RFLX_Invalid::Undef"',
            f'{path}:38:28: model: error: duplicate literal "E2_2"',
            f"{path}:38:22: model: info: previous occurrence",
            f'{path}:38:9: model: error: size of "E2" too small',
            f'{path}:41:46: model: error: duplicate enumeration value "2" in "E3"',
            f"{path}:41:35: model: info: previous occurrence",
            f'{path}:64:10: model: error: unreachable field "Z" in "RFLX_Invalid::M1"',
            f'{path}:64:10: model: error: duplicate link from "Z" to "Final"',
            f"{path}:64:16: model: info: duplicate link",
            f"{path}:64:26: model: info: duplicate link",
            f'{path}:70:14: model: error: undefined type "RFLX_Invalid::Undef_Type"',
            f'{path}:70:10: model: error: missing type for field "Z" in "RFLX_Invalid::M2"',
            f'{path}:77:9: model: error: unsupported element type size of sequence "A3"',
            f'{path}:76:9: model: info: type "E6" has size 4, must be multiple of 8',
            f'{path}:84:30: model: error: invalid First for field "Final"',
            f"{path}:87:9: model: error: message size must be multiple of 8 bit",
            f"{path}:87:9: model: info: on path Field",
            f'{path}:94:44: model: error: size of opaque field "Data" not multiple of 8 bit'
            " (Length -> Data)",
            f'{path}:97:8: model: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:97:40: model: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:100:8: model: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:100:26: model: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:103:8: model: error: undefined type "RFLX_Invalid::Undef_PDU" in refinement',
            f'{path}:103:31: model: error: undefined type "RFLX_Invalid::R" in refinement '
            'of "RFLX_Invalid::Undef_PDU"',
            f'{path}:106:8: model: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:106:26: model: error: undefined type "RFLX_Invalid::Undef_Type" in '
            'refinement of "RFLX_Invalid::PDU1"',
            f'{path}:109:8: model: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:109:33: model: error: undefined type "RFLX_Invalid::R" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:115:21: model: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:116:21: model: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:119:21: model: error: undefined base message "RFLX_Invalid::Undef_Type"'
            " in derived message",
            f'{path}:122:9: model: error: illegal derivation "RFLX_Invalid::Ref3"',
            f'{path}:8:9: model: info: invalid base message type "RFLX_Invalid::R"',
            f'{path}:125:21: model: error: undefined base message "RFLX_Invalid::Ref1"'
            " in derived message",
            f'{path}:132:19: model: error: undefined variable "F1"',
            f"{path}:132:19: model: info: on path Length -> Data",
            f'{path}:136:19: model: error: undefined variable "F1"',
            f"{path}:136:25: model: error: expected integer type",
            f"{path}:136:25: model: info: found aggregate with element type universal integer"
            " (5 .. 20)",
            f"{path}:136:19: model: info: on path Length -> Data -> F1",
            f'{path}:144:24: model: error: expected integer type "RFLX_Invalid::R" (5 .. 23)',
            f"{path}:144:24: model: info: found aggregate with element type universal integer"
            " (1 .. 100)",
            f"{path}:144:19: model: info: on path Length -> Data -> F1 -> F2 -> Final",
            f'{path}:140:19: model: error: invalid use of size attribute for "1"',
            f'{path}:156:10: model: error: name conflict for "F2_F1" in "RFLX_Invalid::M5"',
            f'{path}:147:9: model: info: when merging message "RFLX_Invalid::M4"',
            f'{path}:154:10: model: info: into field "F2"',
            f'{path}:168:27: model: error: expected sequence type "RFLX_Invalid::R_Sequence"'
            ' with element integer type "RFLX_Invalid::R" (5 .. 23)',
            f"{path}:168:27: model: info: found aggregate with element type universal integer"
            " (1 .. 1000)",
            f"{path}:168:19: model: info: on path Length -> Data1 -> Data2",
            f'{path}:172:27: model: error: expected sequence type "__INTERNAL__::Opaque"'
            ' with element integer type "Byte" (0 .. 255)',
            f"{path}:172:27: model: info: found aggregate with element type universal integer"
            " (1 .. 1000)",
            f"{path}:172:19: model: info: on path Length -> Data1 -> Data2 -> Final",
            f'{path}:175:36: model: error: undefined element type "RFLX_Invalid::M5"',
            f'{path}:181:17: model: error: undefined type "RFLX_Invalid::M5_Sequence"',
            f'{path}:181:10: model: error: missing type for field "Data" in "RFLX_Invalid::M7"',
            f'{path}:192:10: model: error: conflicting conditions for field "F1"',
            f"{path}:196:19: model: info: condition 0 (F1 -> F2): F1 < 80",
            f"{path}:194:19: model: info: condition 1 (F1 -> Final): F1 > 50",
            f'{path}:204:10: model: error: unreachable field "F2" in "RFLX_Invalid::M9"',
            f'{path}:219:18: model: error: fixed size field "F1" with size aspect',
            f"{path}:229:30: model: error: illegal first aspect at initial link",
            f'{path}:238:19: model: error: undefined variable "F1"',
            f"{path}:238:19: model: info: on path F1",
            f'{path}:247:29: model: error: negative size for field "F2" (F1 -> F2)',
            f"{path}:247:29: model: error: size of opaque field "
            f'"F2" not multiple of 8 bit (F1 -> F2)',
            f'{path}:242:9: model: error: negative start for field "Final" (F1 -> F2 -> Final)',
            f"{path}:245:18: model: info: unsatisfied \"F2'Last ="
            " (F1'Last + 1 + (F1 - 2 ** 33)) - 1\"",
            f"{path}:244:10: model: info: unsatisfied \"F1'Last = (Message'First + 32) - 1\"",
            f'{path}:187:9: model: info: unsatisfied "F1 <= 4294967295"',
            f"{path}:248:10: model: info: unsatisfied \"F2'Last + 1 >= Message'First\"",
            f'{path}:254:10: model: error: unconstrained field "F1" without size aspect',
            f'{path}:260:9: model: error: field "F3" not congruent with overlaid field "F2"',
            f"{path}:263:10: model: info: unsatisfied \"F2'Last = (F1'Last + 1 + 8) - 1\"",
            f"{path}:262:18: model: info: unsatisfied \"F2'First = F1'Last + 1\"",
            f"{path}:264:13: model: info: unsatisfied \"(F2'First + 16) - 1 = F2'Last\"",
            f'{path}:276:29: model: error: size aspect for final field in "RFLX_Invalid::M17"',
            f"{path}:279:9: model: error: unnecessary always-valid aspect"
            ' on "Unnecessary_Always_Valid_Enum"',
            f"{path}:290:30: model: error: undefined variable " '"Undef_Var"',
            f'{path}:28:9: model: error: name conflict for type "RFLX_Invalid::R"',
            f'{path}:8:9: model: info: previous occurrence of "RFLX_Invalid::R"',
            f'{path}:31:9: model: error: name conflict for type "__BUILTINS__::Boolean"',
            '__BUILTINS__:0:0: model: info: previous occurrence of "__BUILTINS__::Boolean"',
            f"{path}:44:9: model: error: conflicting literals: False, True",
            '__BUILTINS__:0:0: model: info: previous occurrence of "False"',
            '__BUILTINS__:0:0: model: info: previous occurrence of "True"',
            f"{path}:34:9: model: error: conflicting literals: Foo",
            f'{path}:31:21: model: info: previous occurrence of "Foo"',
            f"{path}:47:9: model: error: conflicting literals: Bar, Foo",
            f'{path}:31:26: model: info: previous occurrence of "Bar"',
            f'{path}:31:21: model: info: previous occurrence of "Foo"',
            f"{path}:47:9: model: error: conflicting literals: Foo",
            f'{path}:34:30: model: info: previous occurrence of "Foo"',
            f'{path}:47:21: model: error: literal "E4" conflicts with type declaration',
            f'{path}:44:9: model: info: conflicting type "RFLX_Invalid::E4"',
        ],
    )


def test_parse_error_aggregate_non_number() -> None:
    path = IDE_DIR / "parse_error_aggregate_non_number.rflx"
    assert_error(
        path,
        [
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
