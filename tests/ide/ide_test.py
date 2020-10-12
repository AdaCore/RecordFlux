"""Ensure that the test specifications contain the expected errors."""

import pathlib

import pytest

from rflx import parser
from rflx.error import RecordFluxError
from tests.const import IDE_DIR


def assert_error(filename: pathlib.Path, regex: str) -> None:
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
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


def test_multiple_errors() -> None:
    path = IDE_DIR / "multiple_errors.rflx"
    assert_error(
        path,
        r"^"
        rf'{path}:6:9: parser: error: illegal prefix "RFLX" in package identifier "RFLX_Invalid"\n'
        rf'{path}:296:5: parser: error: inconsistent package identifier "Inconsistent"\n'
        rf'{path}:6:9: parser: info: previous identifier was "RFLX_Invalid"\n'
        rf'{path}:6:9: parser: error: file name does not match unit name "RFLX_Invalid",'
        r' should be "rflx_invalid.rflx"\n'
        rf'{path}:40:4: model: error: illegal redefinition of built-in type "Boolean"\n'
        rf'{path}:2:6: parser: error: dependency cycle when including "Multiple_Errors"\n'
        rf'{path}:13:4: model: error: first of "Range2" contains variable\n'
        rf'{path}:13:4: model: error: last of "Range2" contains variable\n'
        rf'{path}:16:4: model: error: range of "Range3" negative\n'
        rf'{path}:19:4: model: error: size of "Range4" contains variable\n'
        rf'{path}:22:4: model: error: size of "Range5" too small\n'
        rf'{path}:25:24: parser: error: undefined element type "RFLX_Invalid::Undef"\n'
        rf'{path}:28:18: model: error: modulus of "S" exceeds limit \(2\*\*64\)\n'
        rf'{path}:31:4: model: error: modulus of "T" not power of two\n'
        rf'{path}:34:4: model: error: modulus of "Mod3" contains variable\n'
        rf'{path}:37:4: parser: error: duplicate type "RFLX_Invalid::R"\n'
        rf'{path}:8:4: parser: info: previous occurrence of "RFLX_Invalid::R"\n'
        rf'{path}:47:28: model: error: duplicate literal "E2_2"\n'
        rf"{path}:47:22: model: info: previous occurrence\n"
        rf'{path}:47:4: model: error: size of "E2" too small\n'
        rf'{path}:50:46: model: error: duplicate enumeration value "2" in "E3"\n'
        rf"{path}:50:35: model: info: previous occurrence\n"
        rf'{path}:73:10: model: error: unreachable field "Z" in "RFLX_Invalid::M1"\n'
        rf'{path}:73:10: model: error: duplicate link from "Z" to "Final"\n'
        rf"{path}:73:16: model: info: duplicate link\n"
        rf"{path}:73:26: model: info: duplicate link\n"
        rf'{path}:81:18: parser: error: undefined field "X"\n'
        rf'{path}:79:10: model: error: missing type for field "Z" in "RFLX_Invalid::M2"\n'
        rf'{path}:86:4: model: error: unsupported element type size of array "A3"\n'
        rf'{path}:85:4: model: info: type "E6" has size 4, must be multiple of 8\n'
        rf'{path}:93:30: model: error: invalid First for field "Final"\n'
        rf'{path}:103:26: model: error: length of opaque field "Data" not multiple of 8 bit'
        r" \(Length -> Data\)\n"
        rf'{path}:106:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement\n'
        rf'{path}:109:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement\n'
        rf'{path}:112:4: parser: error: undefined type "RFLX_Invalid::Undef_PDU" in refinement\n'
        rf'{path}:115:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement\n'
        rf'{path}:118:4: parser: error: undefined type "RFLX_Invalid::PDU1" in refinement\n'
        rf'{path}:124:4: parser: error: undefined base message "RFLX_Invalid::PDU1"'
        r" in derived message\n"
        rf'{path}:125:4: parser: error: undefined base message "RFLX_Invalid::PDU1"'
        r" in derived message\n"
        rf'{path}:128:4: parser: error: undefined base message "RFLX_Invalid::Undef_Type"'
        r" in derived message\n"
        rf'{path}:131:4: parser: error: illegal derivation "RFLX_Invalid::Ref3"\n'
        rf'{path}:8:4: parser: info: invalid base message type "RFLX_Invalid::R"\n'
        rf'{path}:134:4: parser: error: undefined base message "RFLX_Invalid::Ref1"'
        r" in derived message\n"
        rf'{path}:142:19: model: error: undefined variable "Undef_Var"\n'
        rf'{path}:144:23: model: error: undefined variable "F1"\n'
        rf"{path}:142:19: model: info: on path Length -> Data\n"
        rf'{path}:148:19: model: error: undefined variable "F1"\n'
        rf"{path}:148:25: model: error: expected integer type\n"
        rf"{path}:148:25: model: info: found aggregate with element type universal integer"
        r" \(5 .. 20\)\n"
        rf"{path}:148:19: model: info: on path Length -> Data -> F1\n"
        rf'{path}:156:24: model: error: expected integer type "RFLX_Invalid::R" \(5 .. 23\)\n'
        rf"{path}:156:24: model: info: found aggregate with element type universal integer"
        r" \(1 .. 100\)\n"
        rf"{path}:156:19: model: info: on path Length -> Data -> F1 -> F2 -> Final\n"
        rf'{path}:152:19: model: error: invalid use of length attribute for "F1"\n'
        rf'{path}:168:10: model: error: name conflict for "F2_F1" in "RFLX_Invalid::M5"\n'
        rf'{path}:159:4: model: info: when merging message "RFLX_Invalid::M4"\n'
        rf'{path}:166:10: model: info: into field "F2"\n'
        rf'{path}:180:27: model: error: expected array type "RFLX_Invalid::R_Array"'
        r' with element integer type "RFLX_Invalid::R" \(5 .. 23\)\n'
        rf"{path}:180:27: model: info: found aggregate with element type universal integer"
        r" \(1 .. 1000\)\n"
        rf"{path}:180:19: model: info: on path Length -> Data1 -> Data2\n"
        rf'{path}:184:27: model: error: expected array type "Opaque" with element integer type'
        r' "Byte" \(0 .. 255\)\n'
        rf"{path}:184:27: model: info: found aggregate with element type universal integer"
        r" \(1 .. 1000\)\n"
        rf"{path}:184:19: model: info: on path Length -> Data1 -> Data2 -> Final\n"
        rf'{path}:187:30: parser: error: undefined element type "RFLX_Invalid::M5"\n'
        rf'{path}:191:18: parser: error: undefined field "Data"\n'
        rf'{path}:193:10: model: error: missing type for field "Data" in "RFLX_Invalid::M7"\n'
        rf'{path}:204:10: model: error: conflicting conditions for field "F1"\n'
        rf"{path}:208:19: model: info: condition 0 \(F1 -> F2\): F1 < 80\n"
        rf"{path}:206:19: model: info: condition 1 \(F1 -> Final\): F1 > 50\n"
        rf'{path}:216:10: model: error: unreachable field "F2" in "RFLX_Invalid::M9"\n'
        rf'{path}:231:18: model: error: fixed size field "F1" with length expression\n'
        rf"{path}:241:30: parser: error: invalid first expression\n"
        rf'{path}:250:19: model: error: undefined variable "F1"\n'
        rf"{path}:250:19: model: info: on path F1\n"
        rf'{path}:257:18: model: error: negative length for field "F2" \(F1 -> F2\)\n'
        rf'{path}:266:10: model: error: unconstrained field "F1" without length expression\n'
        rf'{path}:271:9: model: error: field "F3" not congruent with overlaid field "F1"\n'
        rf'{path}:271:4: model: info: unsatisfied "F1\'First = Message\'First"\n'
        rf'{path}:273:10: model: info: unsatisfied "F1\'Last = \(Message\'First \+ 8\) - 1"\n'
        rf'{path}:275:13: model: info: unsatisfied "\(F1\'First \+ 16\) - 1 = F1\'Last"\n'
        rf'{path}:287:31: model: error: length attribute for final field in "RFLX_Invalid::M17"\n'
        rf"{path}:290:4: model: error: unnecessary always-valid aspect"
        r' on "Unnecessary_Always_Valid_Enum"\n'
        rf"{path}:53:4: model: error: conflicting literals: False, True\n"
        rf'__BUILTINS__:0:0: model: info: previous occurrence of "False"\n'
        rf'__BUILTINS__:0:0: model: info: previous occurrence of "True"\n'
        rf"{path}:43:4: model: error: conflicting literals: Foo\n"
        rf'{path}:40:21: model: info: previous occurrence of "Foo"\n'
        rf"{path}:56:4: model: error: conflicting literals: Bar, Foo\n"
        rf'{path}:40:26: model: info: previous occurrence of "Bar"\n'
        rf'{path}:40:21: model: info: previous occurrence of "Foo"\n'
        rf"{path}:56:4: model: error: conflicting literals: Foo\n"
        rf'{path}:43:30: model: info: previous occurrence of "Foo"\n'
        rf'{path}:56:21: model: error: literal conflicts with type "E4"\n'
        rf"{path}:53:4: model: info: conflicting type declaration"
        r"$",
    )


def test_parse_error_aggregate_non_number() -> None:
    path = IDE_DIR / "parse_error_aggregate_non_number.rflx"
    assert_error(
        path,
        rf'^{path}:10:26: parser: error: unexpected expression type "Variable"$',
    )


def test_parse_error_invalid_location() -> None:
    path = IDE_DIR / "parse_error_invalid_location.rflx"
    assert_error(
        path,
        rf"^{path}:7:21: parser: error: Expected ",
    )
