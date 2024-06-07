"""Ensure that the test specifications contain the expected errors."""

from __future__ import annotations

import pathlib
from collections.abc import Sequence

import pytest

from rflx import specification
from rflx.rapidflux import RecordFluxError
from tests.const import IDE_DIR


def assert_error(filename: pathlib.Path, expected: Sequence[str]) -> None:
    p = specification.Parser()
    with pytest.raises(RecordFluxError) as excinfo:  # noqa: PT012
        error = RecordFluxError()

        try:
            p.parse(filename)
        except RecordFluxError as e:
            error.extend(e.entries)

        try:
            p.create_model()
        except RecordFluxError as e:
            error.extend(e.entries)

        error.propagate()

    assert str(excinfo.value).split("\n") == expected


def test_multiple_errors() -> None:
    path = IDE_DIR / "multiple_errors.rflx"
    assert_error(
        path,
        [
            f'{path}:6:9: error: illegal prefix "RFLX" in package identifier "RFLX_Invalid"',
            f'{path}:295:5: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: info: previous identifier was "RFLX_Invalid"',
            f"{path}:6:9: error: source file name does not match the package name"
            ' "RFLX_Invalid"',
            f"{path}:6:9: help: either rename the file to "
            '"rflx_invalid.rflx" or change the package name to "Multiple_Errors"',
            f'{path}:6:9: help: rename to "Multiple_Errors"',
            f'{path}:295:5: help: rename to "Multiple_Errors"',
            f'{path}:6:9: error: illegal prefix "RFLX" in package identifier "RFLX_Invalid"',
            f'{path}:295:5: error: inconsistent package identifier "Inconsistent"',
            f'{path}:6:9: info: previous identifier was "RFLX_Invalid"',
            f"{path}:6:9: error: source file name does not match the package name"
            ' "RFLX_Invalid"',
            f"{path}:6:9: help: either rename the file to "
            '"rflx_invalid.rflx" or change the package name to "Multiple_Errors"',
            f'{path}:6:9: help: rename to "Multiple_Errors"',
            f'{path}:295:5: help: rename to "Multiple_Errors"',
            f'{path}:6:9: error: illegal identifier "RFLX_Invalid"',
            f'{path}:6:9: info: identifiers starting with "RFLX_" are reserved for internal use',
            f'{path}:72:18: error: undefined field "X"',
            f'{path}:13:9: error: first of "Range2" contains variable',
            f'{path}:13:9: error: last of "Range2" contains variable',
            f'{path}:16:9: error: range of "Range3" negative',
            f'{path}:19:9: error: size of "Range4" contains variable',
            f'{path}:22:9: error: size of "Range5" too small',
            f'{path}:25:27: error: undefined element type "RFLX_Invalid::Undef"',
            f'{path}:38:28: error: duplicate literal "E2_2"',
            f"{path}:38:22: info: previous occurrence",
            f'{path}:38:9: error: size of "E2" too small',
            f'{path}:41:46: error: duplicate enumeration value "2" in "E3"',
            f"{path}:41:35: info: previous occurrence",
            f'{path}:64:10: error: unreachable field "Z" in "RFLX_Invalid::M1"',
            f'{path}:64:10: error: duplicate link from "Z" to "Final"',
            f"{path}:64:16: info: duplicate link",
            f"{path}:64:26: info: duplicate link",
            f'{path}:70:14: error: undefined type "RFLX_Invalid::Undef_Type"',
            f'{path}:77:9: error: unsupported element type size of sequence "A3"',
            f'{path}:76:9: info: type "E6" has size 4, must be multiple of 8',
            f'{path}:84:30: error: invalid First for field "Final"',
            f"{path}:87:9: error: message size must be multiple of 8 bit",
            f'{path}:89:10: note: on path "Field"',
            f'{path}:94:44: error: size of opaque field "Data" not multiple of 8 bit',
            f"{path}:94:44: help: sizes are expressed in bits, not bytes",
            f'{path}:94:44: help: did you mean "Length * 8"?',
            f'{path}:97:8: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:97:40: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:100:8: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:100:26: error: undefined type "RFLX_Invalid::SDU2" in refinement '
            'of "RFLX_Invalid::PDU1"',
            f'{path}:103:8: error: undefined type "RFLX_Invalid::Undef_PDU" in refinement',
            f'{path}:103:31: error: type "RFLX_Invalid::R" cannot be used in refinement '
            "because it's not a message type",
            f'{path}:106:8: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:106:26: error: undefined type "RFLX_Invalid::Undef_Type" in '
            'refinement of "RFLX_Invalid::PDU1"',
            f'{path}:109:8: error: undefined type "RFLX_Invalid::PDU1" in refinement',
            f'{path}:109:33: error: type "RFLX_Invalid::R" cannot be used in refinement '
            "because it's not a message type",
            f'{path}:115:21: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:116:21: error: undefined base message "RFLX_Invalid::PDU1"'
            " in derived message",
            f'{path}:119:21: error: undefined base message "RFLX_Invalid::Undef_Type"'
            " in derived message",
            f'{path}:122:9: error: illegal derivation "RFLX_Invalid::Ref3"',
            f'{path}:8:9: info: invalid base message type "RFLX_Invalid::R"',
            f'{path}:125:21: error: undefined base message "RFLX_Invalid::Ref1"'
            " in derived message",
            f'{path}:140:19: error: invalid use of size attribute for "1"',
            f'{path}:156:10: error: name conflict for "F2_F1" in "RFLX_Invalid::M5"',
            f'{path}:147:9: info: when merging message "RFLX_Invalid::M4"',
            f'{path}:154:10: info: into field "F2"',
            f'{path}:164:10: error: unreachable field "Data1"',
            f'{path}:161:10: note: on path "Length"',
            f'{path}:162:18: note: on path "Data1"',
            f"{path}:168:19: info: unsatisfied \"3 * R'Size = Data1'Size\"",
            f'{path}:8:9: info: unsatisfied "R\'Size = 16"',
            f'{path}:8:9: info: unsatisfied "Length <= 23"',
            f'{path}:163:29: info: unsatisfied "Data1\'Size = Length"',
            f'{path}:175:36: error: undefined element type "RFLX_Invalid::M5"',
            f'{path}:181:17: error: undefined type "RFLX_Invalid::M5_Sequence"',
            f'{path}:192:10: error: conflicting conditions for field "F1"',
            f"{path}:196:19: info: condition 0 (F1 -> F2): F1 < 80",
            f"{path}:194:19: info: condition 1 (F1 -> Final): F1 > 50",
            f'{path}:204:10: error: unreachable field "F2" in "RFLX_Invalid::M9"',
            f'{path}:219:18: error: fixed size field "F1" with size aspect',
            f"{path}:229:30: error: illegal first aspect on initial link",
            f'{path}:238:19: error: undefined variable "F1"',
            f"{path}:238:19: info: on path F1",
            f'{path}:247:29: error: negative size for field "F2" (F1 -> F2)',
            f'{path}:247:29: error: size of opaque field "F2" not multiple of 8 bit',
            f"{path}:247:29: help: sizes are expressed in bits, not bytes",
            f'{path}:247:29: help: did you mean "(F1 - 2 ** 33) * 8"?',
            f'{path}:242:9: error: negative start for field "Final" (F1 -> F2 -> Final)',
            f"{path}:245:18: info: unsatisfied \"F2'Last = (F1'Last + 1 + (F1 - 2 ** 33)) - 1\"",
            f"{path}:244:10: info: unsatisfied \"F1'Last = (Message'First + 32) - 1\"",
            f'{path}:187:9: info: unsatisfied "F1 <= 4294967295"',
            f"{path}:248:10: info: unsatisfied \"F2'Last + 1 >= Message'First\"",
            f'{path}:254:10: error: unconstrained field "F1" without size aspect',
            f'{path}:260:9: error: field "F3" not congruent with overlaid field "F2"',
            f"{path}:263:10: info: unsatisfied \"F2'Last = (F1'Last + 1 + 8) - 1\"",
            f"{path}:262:18: info: unsatisfied \"F2'First = F1'Last + 1\"",
            f"{path}:264:13: info: unsatisfied \"(F2'First + 16) - 1 = F2'Last\"",
            f'{path}:276:29: error: size aspect for final field in "RFLX_Invalid::M17"',
            f"{path}:279:9: error: unnecessary always-valid aspect"
            ' on "Unnecessary_Always_Valid_Enum"',
            f'{path}:290:30: error: undefined variable "Undef_Var"',
            f'{path}:28:9: error: name conflict for type "RFLX_Invalid::R"',
            f'{path}:8:9: info: previous occurrence of "RFLX_Invalid::R"',
            f'{path}:31:9: error: illegal redefinition of built-in type "Boolean"',
            f"{path}:44:9: error: conflicting literals: False, True",
            '__BUILTINS__:1:1: info: previous occurrence of "False"',
            '__BUILTINS__:1:1: info: previous occurrence of "True"',
            f"{path}:34:9: error: conflicting literals: Foo",
            f'{path}:31:21: info: previous occurrence of "Foo"',
            f"{path}:47:9: error: conflicting literals: Bar, Foo",
            f'{path}:31:26: info: previous occurrence of "Bar"',
            f'{path}:31:21: info: previous occurrence of "Foo"',
            f"{path}:47:9: error: conflicting literals: Foo",
            f'{path}:34:30: info: previous occurrence of "Foo"',
            f'{path}:47:21: error: literal "E4" conflicts with type declaration',
            f'{path}:44:9: info: conflicting type "RFLX_Invalid::E4"',
        ],
    )


def test_parse_error_aggregate_non_number() -> None:
    path = IDE_DIR / "parse_error_aggregate_non_number.rflx"
    assert_error(
        path,
        [
            f"{path}:10:39: error: Expected ';', got 'First'",
        ],
    )


def test_parse_error_invalid_location() -> None:
    path = IDE_DIR / "parse_error_invalid_location.rflx"
    assert_error(
        path,
        [
            f'{path}:7:21: error: invalid aspect "Invalid"',
            f"{path}:3:9: error: message size must be multiple of 8 bit",
            f'{path}:5:10: note: on path "Y"',
        ],
    )
