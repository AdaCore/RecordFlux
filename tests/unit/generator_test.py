# pylint: disable = too-many-lines

from __future__ import annotations

import textwrap
from collections import abc
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from pathlib import Path

import pkg_resources
import pytest
from _pytest.capture import CaptureFixture
from _pytest.monkeypatch import MonkeyPatch

from rflx import ada, const as constants, expression as expr, tac, typing_ as rty
from rflx.common import file_name
from rflx.error import BaseError, FatalError, Location, RecordFluxError
from rflx.generator import Generator, common, const
from rflx.generator.allocator import AllocatorGenerator
from rflx.generator.common import Debug
from rflx.generator.message import create_structure
from rflx.generator.session import EvaluatedDeclaration, ExceptionHandler, SessionGenerator
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import (
    BUILTIN_TYPES,
    Model,
    Session,
    State,
    Transition,
    declaration as decl,
    statement as stmt,
    type_ as mty,
)
from rflx.model.message import FINAL, INITIAL, Field, Link, Message
from tests.const import GENERATED_DIR
from tests.data import models
from tests.utils import assert_equal, assert_equal_code

MODELS = [
    models.DERIVATION_MODEL,
    models.ENUMERATION_MODEL,
    models.ETHERNET_MODEL,
    models.EXPRESSION_MODEL,
    models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
    models.NULL_MODEL,
    models.SEQUENCE_MODEL,
    models.TLV_MODEL,
    Model(models.FIXED_SIZE_SIMPLE_MESSAGE.dependencies),
]


def test_invalid_prefix() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier "A::::B"$'):
        Generator("A..B")


def test_unsupported_checksum(tmp_path: Path) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^generator: error: unsupported checksum"
            r" \(consider --ignore-unsupported-checksum option\)$"
        ),
    ):
        Generator().generate(models.TLV_WITH_CHECKSUM_MODEL, Integration(), tmp_path)


def test_ignore_unsupported_checksum(capsys: CaptureFixture[str], tmp_path: Path) -> None:
    Generator(ignore_unsupported_checksum=True).generate(
        models.TLV_WITH_CHECKSUM_MODEL, Integration(), tmp_path
    )
    captured = capsys.readouterr()
    assert "generator: warning: unsupported checksum ignored" in captured.out


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type(tmp_path: Path) -> None:
    class TestType(mty.Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator().generate(Model([TestType("P::T")]), Integration(), tmp_path)


@pytest.mark.parametrize(
    "debug, debug_expected",
    [
        (Debug.NONE, set()),
        (Debug.BUILTIN, set()),
        (Debug.EXTERNAL, {"rflx_debug.ads"}),
    ],
)
@pytest.mark.parametrize(
    "prefix, library_files, top_level_package, expected",
    [
        ("RFLX", True, True, {"rflx.ads"} | {f"rflx-{f}" for f in const.LIBRARY_FILES}),
        ("RFLX", True, False, {f"rflx-{f}" for f in const.LIBRARY_FILES}),
        ("RFLX", False, True, {"rflx.ads"}),
        ("RFLX", False, False, set()),
        ("", True, True, set(const.LIBRARY_FILES)),
        ("", True, False, set(const.LIBRARY_FILES)),
        ("", False, True, set()),
        ("", False, False, set()),
    ],
)
# pylint: disable-next = too-many-arguments
def test_generate(  # noqa: PLR0913
    debug: Debug,
    debug_expected: set[str],
    prefix: str,
    library_files: bool,
    top_level_package: bool,
    expected: set[str],
    tmp_path: Path,
) -> None:
    Generator(prefix, reproducible=True, debug=debug).generate(
        Model(), Integration(), tmp_path, library_files, top_level_package
    )
    present = {f.name for f in tmp_path.glob("*.ad?")}
    assert present == expected | (
        {(f"{file_name(prefix)}-{f}" if prefix else f) for f in debug_expected}
        if library_files
        else set()
    )

    if prefix and debug == Debug.NONE:
        for f in present:
            assert (tmp_path / f).read_text() == (GENERATED_DIR / f).read_text(), f"Error in {f}"


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_generate_missing_template_directory(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: "non-existent directory")
    with pytest.raises(AssertionError, match="^template directory not found"):
        Generator().generate(Model(), Integration(), tmp_path)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_generate_missing_template_files(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: tmp_path)
    with pytest.raises(AssertionError, match="^template file not found"):
        Generator().generate(Model(), Integration(), tmp_path)


def test_generate_partial_update(tmp_path: Path) -> None:
    Generator().generate(models.TLV_MODEL, Integration(), tmp_path)
    Generator().generate(models.TLV_MODEL, Integration(), tmp_path)
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "generator: error: partial update of generated files\n"
            "generator: info: files not generated in the current run could lead to unexpected"
            " behavior: tlv-message.adb, tlv-message.ads, tlv.ads\n"
            "generator: info: remove the affected files or choose another directory and retry"
            "$"
        ),
    ):
        Generator().generate(models.ETHERNET_MODEL, Integration(), tmp_path)


@pytest.mark.parametrize("model", MODELS)
def test_equality(model: Model, tmp_path: Path) -> None:
    assert_equal_code(model, Integration(), GENERATED_DIR, tmp_path, accept_extra_files=True)


@pytest.mark.parametrize("embedded", [True, False])
@pytest.mark.parametrize(
    "left,right",
    [
        (expr.Variable("Value"), expr.Aggregate(expr.Number(1), expr.Number(2))),
        (expr.Aggregate(expr.Number(1), expr.Number(2)), expr.Variable("Value")),
    ],
)
@pytest.mark.parametrize("relation", [expr.Equal, expr.NotEqual])
def test_substitution_relation_aggregate(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    left: expr.Expr,
    right: expr.Expr,
    embedded: bool,
) -> None:
    aggregate = expr.Aggregate(
        expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(1)),
        expr.Val(expr.Variable(const.TYPES * "Byte"), expr.Number(2)),
    )
    expected: expr.Expr
    if embedded:
        expected = relation(
            expr.Indexed(
                expr.Variable(ID("Buffer") * "all"),
                expr.ValueRange(
                    expr.Call(
                        const.TYPES_TO_INDEX,
                        [
                            expr.Selected(
                                expr.Indexed(
                                    expr.Variable("Cursors"),
                                    expr.Variable("F_Value"),
                                ),
                                "First",
                            )
                        ],
                    ),
                    expr.Call(
                        const.TYPES_TO_INDEX,
                        [
                            expr.Selected(
                                expr.Indexed(
                                    expr.Variable("Cursors"),
                                    expr.Variable("F_Value"),
                                ),
                                "Last",
                            )
                        ],
                    ),
                ),
            ),
            aggregate,
        )
    else:
        equal_call = expr.Call(
            "Equal",
            [
                expr.Variable("Ctx"),
                expr.Variable("F_Value"),
                aggregate,
            ],
        )
        expected = equal_call if relation == expr.Equal else expr.Not(equal_call)

    assert (
        relation(left, right).substituted(common.substitution(models.TLV_MESSAGE, "", embedded))
        == expected
    )


@pytest.mark.parametrize(
    "left,right,expected_left,expected_right",
    [
        (
            expr.Variable("Value"),
            expr.TRUE,
            expr.Call("RFLX_Types::Base_Integer", [expr.Variable("Value")]),
            expr.Call("RFLX_Types::Base_Integer", [expr.Call("To_Base_Integer", [expr.TRUE])]),
        ),
        (
            expr.FALSE,
            expr.Variable("Value"),
            expr.Call("RFLX_Types::Base_Integer", [expr.Variable("Value")]),
            expr.Call("RFLX_Types::Base_Integer", [expr.Call("To_Base_Integer", [expr.FALSE])]),
        ),
    ],
)
@pytest.mark.parametrize("relation", [expr.Equal, expr.NotEqual])
def test_substitution_relation_boolean_literal(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    left: expr.Expr,
    right: expr.Expr,
    expected_left: expr.Expr,
    expected_right: expr.Expr,
) -> None:
    assert relation(left, right).substituted(
        common.substitution(models.TLV_MESSAGE, "")
    ) == relation(expected_left, expected_right)


@pytest.mark.parametrize(
    "expressions,expected",
    [
        (
            (expr.Variable("Length"), expr.Number(1)),
            (expr.Call("Get_Length", [expr.Variable("Ctx")]), expr.Number(1)),
        ),
        (
            (expr.Number(1), expr.Variable("Length")),
            (expr.Number(1), expr.Call("Get_Length", [expr.Variable("Ctx")])),
        ),
        ((expr.Number(1), expr.Variable("Unknown")), (expr.Number(1), expr.Variable("Unknown"))),
    ],
)
@pytest.mark.parametrize(
    "relation",
    [expr.Less, expr.LessEqual, expr.Equal, expr.GreaterEqual, expr.Greater, expr.NotEqual],
)
def test_substitution_relation_scalar(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    expressions: tuple[expr.Expr, expr.Expr],
    expected: tuple[expr.Expr, expr.Expr],
) -> None:
    assert_equal(
        relation(*expressions).substituted(
            common.substitution(models.TLV_MESSAGE, "", public=True)
        ),
        relation(*expected),
    )


def test_prefixed_type_identifier() -> None:
    assert common.prefixed_type_identifier(ID("Integer"), "P") == ID("P.Integer")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_identifier(ID(t), "P") == t.name


DUMMY_SESSION = Session(
    identifier="P::S",
    states=[State("State", [Transition("null")])],
    declarations=[],
    parameters=[],
    types=[*models.UNIVERSAL_MODEL.types],
)


@pytest.mark.parametrize(
    "parameter, expected",
    [
        (
            decl.FunctionDeclaration("F", [], "T", type_=rty.BOOLEAN),
            [
                ada.SubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.InOutParameter(["Ctx"], "Context"),
                            ada.OutParameter(["RFLX_Result"], "Boolean"),
                        ],
                    ),
                    abstract=True,
                ),
            ],
        ),
        (
            decl.FunctionDeclaration(
                "F",
                [
                    decl.Argument("P1", "Boolean", type_=rty.BOOLEAN),
                    decl.Argument("P2", "T2", type_=rty.OPAQUE),
                    decl.Argument(
                        "P3", "T3", type_=rty.Enumeration("T4", [ID("E1")], always_valid=True)
                    ),
                    decl.Argument("P4", "T4", type_=rty.Integer("T2")),
                    decl.Argument("P5", "T5", type_=rty.Message("T5", is_definite=True)),
                ],
                "T",
                type_=rty.Message("T", is_definite=True),
            ),
            [
                ada.SubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.InOutParameter(["Ctx"], "Context"),
                            ada.Parameter(["P1"], "Boolean"),
                            ada.Parameter(["P2"], const.TYPES_BYTES),
                            ada.Parameter(["P3"], "T3"),
                            ada.Parameter(["P4"], "T4"),
                            ada.Parameter(["P5"], "T5.Structure"),
                            ada.OutParameter(["RFLX_Result"], "T.Structure"),
                        ],
                    ),
                    abstract=True,
                ),
            ],
        ),
    ],
)
def test_session_create_abstract_function(
    parameter: decl.FunctionDeclaration, expected: Sequence[ada.SubprogramDeclaration]
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )
    # pylint: disable = protected-access
    assert session_generator._create_abstract_function(parameter) == expected


class UnknownDeclaration(decl.FormalDeclaration, decl.BasicDeclaration):
    def __str__(self) -> str:
        raise NotImplementedError

    @property
    def type_(self) -> rty.Type:
        raise NotImplementedError


@pytest.mark.parametrize(
    "parameter, error_type, error_msg",
    [
        (
            decl.FunctionDeclaration("F", [], "T", location=Location((10, 20))),
            FatalError,
            r'return type of function "F" is undefined',
        ),
        (
            decl.FunctionDeclaration("F", [], "T", type_=rty.OPAQUE, location=Location((10, 20))),
            FatalError,
            r'Opaque as return type of function "F" not allowed',
        ),
        (
            decl.FunctionDeclaration(
                "F", [], "T", type_=rty.Sequence("A", rty.Integer("B")), location=Location((10, 20))
            ),
            RecordFluxError,
            r'sequence as return type of function "F" not yet supported',
        ),
        (
            decl.FunctionDeclaration(
                "F", [], "T", type_=rty.Message("A", is_definite=False), location=Location((10, 20))
            ),
            FatalError,
            r'non-definite message in return type of function "F" not allowed',
        ),
        (
            decl.FunctionDeclaration(
                "F",
                [],
                "T",
                type_=rty.Message(
                    "M", {("F",)}, {ID("F"): rty.Sequence("A", rty.Integer("B"))}, is_definite=True
                ),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'message containing sequence fields in return type of function "F" not yet supported',
        ),
        (
            decl.FunctionDeclaration(
                "F",
                [decl.Argument("P", "T", type_=rty.Sequence("A", rty.Integer("B")))],
                "T",
                type_=rty.BOOLEAN,
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'sequence as parameter of function "F" not yet supported',
        ),
        (
            UnknownDeclaration("X", location=Location((10, 20))),
            FatalError,
            r'unexpected formal parameter "X"',
        ),
    ],
)
def test_session_create_abstract_functions_error(
    parameter: decl.FormalDeclaration, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._create_abstract_functions([parameter])


@pytest.mark.parametrize(
    "declaration, session_global, expected",
    [
        (
            decl.VariableDeclaration("X", "Boolean", type_=rty.BOOLEAN),
            False,
            EvaluatedDeclaration(global_declarations=[ada.ObjectDeclaration("X", "Boolean")]),
        ),
        (
            decl.VariableDeclaration("X", "T", expr.Number(1), type_=rty.Integer("T")),
            False,
            EvaluatedDeclaration(
                global_declarations=[ada.ObjectDeclaration("X", "P.T", ada.Number(1))]
            ),
        ),
        (
            decl.VariableDeclaration("X", "T", expr.Number(1), type_=rty.Integer("T")),
            True,
            EvaluatedDeclaration(
                global_declarations=[ada.ObjectDeclaration("X", "P.T", ada.Number(1))],
                initialization=[
                    ada.CallStatement(
                        "P.S_Allocator.Initialize",
                        [ada.Variable("Ctx.P.Slots"), ada.Variable("Ctx.P.Memory")],
                    ),
                    ada.Assignment("Ctx.P.X", ada.Conversion("T", ada.Number(1))),
                ],
                finalization=[
                    ada.CallStatement("P.S_Allocator.Finalize", [ada.Variable("Ctx.P.Slots")]),
                ],
            ),
        ),
        (
            decl.VariableDeclaration(
                "X", "T", type_=rty.Message("T"), location=Location(start=(1, 1))
            ),
            False,
            EvaluatedDeclaration(
                global_declarations=[
                    ada.ObjectDeclaration(["X_Ctx"], "P.T.Context"),
                ],
                initialization_declarations=[
                    ada.ObjectDeclaration(["X_Buffer"], const.TYPES_BYTES_PTR),
                ],
                initialization=[
                    ada.Assignment(
                        "X_Buffer",
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                    ),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String("unused assignment"),
                        ],
                    ),
                    ada.Assignment(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null")),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String("unused assignment"),
                        ],
                    ),
                    ada.CallStatement(
                        "P.T.Initialize", [ada.Variable("X_Ctx"), ada.Variable("X_Buffer")]
                    ),
                ],
                finalization=[
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String(
                                '"X_Ctx" is set by "Take_Buffer" but not used after the call'
                            ),
                        ],
                    ),
                    ada.CallStatement(
                        "P.T.Take_Buffer",
                        [
                            ada.Variable("X_Ctx"),
                            ada.Variable("X_Buffer"),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String(
                                '"X_Ctx" is set by "Take_Buffer" but not used after the call'
                            ),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.Equal(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null"))],
                    ),
                    ada.PragmaStatement(
                        "Assert", [ada.NotEqual(ada.Variable("X_Buffer"), ada.Variable("null"))]
                    ),
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("X_Buffer")
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [
                            ada.NotEqual(
                                ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null")
                            )
                        ],
                    ),
                ],
            ),
        ),
        (
            decl.VariableDeclaration(
                "X", "T", type_=rty.Message("T"), location=Location(start=(1, 1))
            ),
            True,
            EvaluatedDeclaration(
                global_declarations=[
                    ada.ObjectDeclaration(["X_Ctx"], "P.T.Context"),
                ],
                initialization_declarations=[
                    ada.ObjectDeclaration(["X_Buffer"], const.TYPES_BYTES_PTR),
                ],
                initialization=[
                    ada.CallStatement(
                        "P.S_Allocator.Initialize",
                        [ada.Variable("Ctx.P.Slots"), ada.Variable("Ctx.P.Memory")],
                    ),
                    ada.Assignment(
                        "X_Buffer",
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                    ),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String("unused assignment"),
                        ],
                    ),
                    ada.Assignment(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null")),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String("unused assignment"),
                        ],
                    ),
                    ada.CallStatement(
                        "P.T.Initialize", [ada.Variable("Ctx.P.X_Ctx"), ada.Variable("X_Buffer")]
                    ),
                ],
                finalization=[
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String(
                                '"Ctx.P.X_Ctx" is set by "Take_Buffer" but not used after the call'
                            ),
                        ],
                    ),
                    ada.CallStatement(
                        "P.T.Take_Buffer",
                        [
                            ada.Variable("Ctx.P.X_Ctx"),
                            ada.Variable("X_Buffer"),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String(
                                '"Ctx.P.X_Ctx" is set by "Take_Buffer" but not used after the call'
                            ),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.Equal(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null"))],
                    ),
                    ada.PragmaStatement(
                        "Assert", [ada.NotEqual(ada.Variable("X_Buffer"), ada.Variable("null"))]
                    ),
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("X_Buffer")
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [
                            ada.NotEqual(
                                ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null")
                            )
                        ],
                    ),
                    ada.CallStatement("P.S_Allocator.Finalize", [ada.Variable("Ctx.P.Slots")]),
                ],
            ),
        ),
    ],
)
def test_session_evaluate_declarations(
    declaration: decl.BasicDeclaration, session_global: bool, expected: EvaluatedDeclaration
) -> None:
    allocator = AllocatorGenerator(DUMMY_SESSION, Integration())
    # pylint: disable = protected-access
    allocator._allocation_slots[Location(start=(1, 1))] = 1
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=Debug.BUILTIN)
    assert (
        session_generator._evaluate_declarations(
            [declaration], is_global=lambda x: False, session_global=session_global
        )
        == expected
    )


@pytest.mark.parametrize(
    "declaration, error_type, error_msg",
    [
        (
            decl.RenamingDeclaration(
                "X",
                "T",
                expr.Selected(expr.Variable("M"), "F"),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'renaming declaration "X" not yet supported',
        ),
        (
            UnknownDeclaration("X", location=Location((10, 20))),
            FatalError,
            r'unexpected declaration "X"',
        ),
    ],
)
def test_session_evaluate_declarations_error(
    declaration: decl.BasicDeclaration, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._evaluate_declarations(  # pragma: no branch
            [declaration], is_global=lambda x: False
        )


@dataclass
class EvaluatedDeclarationStr:
    global_declarations: str = ""
    initialization_declarations: str = ""
    initialization: str = ""
    finalization: str = ""


@pytest.mark.parametrize(
    "type_, expression, constant, session_global, expected",
    [
        (
            rty.Integer("T"),
            expr.Number(1),
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.T := 1;",
            ),
        ),
        (
            rty.Integer("T"),
            expr.Number(1),
            False,
            True,
            EvaluatedDeclarationStr(
                global_declarations="X : P.T := 1;",
                initialization="X := T (1);",
            ),
        ),
        (
            rty.Integer("T"),
            expr.Number(1),
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.T := 1;",
            ),
        ),
        (
            rty.Integer("T"),
            None,
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.T;",
            ),
        ),
        (
            rty.Message("T"),
            None,
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations=("X_Ctx : P.T.Context;"),
                initialization_declarations=("X_Buffer : RFLX_Types.Bytes_Ptr;"),
                initialization=(
                    "X_Buffer := Ctx.P.Slots.Slot_Ptr_1;\n"
                    'pragma Warnings (Off, "unused assignment");\n'
                    "Ctx.P.Slots.Slot_Ptr_1 := null;\n"
                    'pragma Warnings (On, "unused assignment");\n'
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);\n"
                    "pragma Assert (X_Buffer /= null);\n"
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;\n"
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);"
                ),
            ),
        ),
        (
            rty.Message("T"),
            None,
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations=("X_Ctx : P.T.Context;"),
                initialization_declarations=("X_Buffer : RFLX_Types.Bytes_Ptr;"),
                initialization=(
                    "X_Buffer := Ctx.P.Slots.Slot_Ptr_1;\n"
                    'pragma Warnings (Off, "unused assignment");\n'
                    "Ctx.P.Slots.Slot_Ptr_1 := null;\n"
                    'pragma Warnings (On, "unused assignment");\n'
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);\n"
                    "pragma Assert (X_Buffer /= null);\n"
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;\n"
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);"
                ),
            ),
        ),
        (
            rty.Message("T"),
            None,
            False,
            True,
            EvaluatedDeclarationStr(
                global_declarations=("X_Ctx : P.T.Context;"),
                initialization_declarations=("X_Buffer : RFLX_Types.Bytes_Ptr;"),
                initialization=(
                    "X_Buffer := Ctx.P.Slots.Slot_Ptr_1;\n"
                    'pragma Warnings (Off, "unused assignment");\n'
                    "Ctx.P.Slots.Slot_Ptr_1 := null;\n"
                    'pragma Warnings (On, "unused assignment");\n'
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);\n"
                    "pragma Assert (X_Buffer /= null);\n"
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;\n"
                    "pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(),
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : RFLX_Types.Bytes (RFLX_Types.Index'Last .. RFLX_Types.Index'First);"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(),
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : RFLX_Types.Bytes (RFLX_Types.Index'Last .. RFLX_Types.Index'First);"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(expr.Number(1)),
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : RFLX_Types.Bytes := (RFLX_Types.Index'First => RFLX_Types.Byte'Val (1))"
                    " with\n  Size =>\n    1 * RFLX_Types.Byte'Size;"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(expr.Number(1)),
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : constant RFLX_Types.Bytes :="
                    " (RFLX_Types.Index'First => RFLX_Types.Byte'Val (1))"
                    " with\n  Size =>\n    1 * RFLX_Types.Byte'Size;"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(expr.Number(1), expr.Number(2)),
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : RFLX_Types.Bytes := (RFLX_Types.Byte'Val (1), RFLX_Types.Byte'Val (2))"
                    " with\n  Size =>\n    2 * RFLX_Types.Byte'Size;"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(expr.Number(1), expr.Number(2)),
            False,
            True,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : RFLX_Types.Bytes := (RFLX_Types.Byte'Val (1), RFLX_Types.Byte'Val (2))"
                    " with\n  Size =>\n    2 * RFLX_Types.Byte'Size;"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            expr.Aggregate(expr.Number(1), expr.Number(2)),
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations=(
                    "X : constant RFLX_Types.Bytes :="
                    " (RFLX_Types.Byte'Val (1), RFLX_Types.Byte'Val (2))"
                    " with\n  Size =>\n    2 * RFLX_Types.Byte'Size;"
                ),
            ),
        ),
        (
            rty.OPAQUE,
            None,
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations=("X : RFLX_Types.Bytes;"),
            ),
        ),
        (
            rty.OPAQUE,
            None,
            False,
            True,
            EvaluatedDeclarationStr(
                global_declarations=("X : RFLX_Types.Bytes;"),
            ),
        ),
        (
            rty.OPAQUE,
            None,
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations=("X : RFLX_Types.Bytes;"),
            ),
        ),
    ],
)
def test_session_declare(
    type_: rty.Type,
    expression: expr.Expr,
    constant: bool,
    session_global: bool,
    expected: EvaluatedDeclarationStr,
) -> None:
    loc: Location = Location(start=(1, 1))
    allocator = AllocatorGenerator(DUMMY_SESSION, Integration())
    # pylint: disable=protected-access
    allocator._allocation_slots[loc] = 1
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=Debug.BUILTIN)
    # pylint: disable = protected-access
    result = session_generator._declare(
        ID("X"), type_, lambda x: False, loc, expression, constant, session_global
    )
    assert "\n".join(str(d) for d in result.global_declarations) == expected.global_declarations
    assert (
        "\n".join(str(d) for d in result.initialization_declarations)
        == expected.initialization_declarations
    )
    assert "\n".join(str(s) for s in result.initialization) == expected.initialization
    assert "\n".join(str(s) for s in result.finalization) == expected.finalization


@pytest.mark.parametrize(
    "type_, expression, error_type, error_msg",
    [
        (
            rty.Integer("T"),
            expr.Call("F", location=Location((10, 20))),
            RecordFluxError,
            r"initialization using function call not yet supported",
        ),
        (
            rty.Message("T"),
            expr.Variable("True", location=Location((10, 20))),
            RecordFluxError,
            r'initialization for message type "T" not yet supported',
        ),
        (
            rty.Undefined(),
            None,
            FatalError,
            r"unexpected variable declaration for undefined type",
        ),
    ],
)
def test_session_declare_error(
    type_: rty.Type, expression: expr.Expr, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._declare(  # pragma: no branch
            ID("X", location=Location((10, 20))),
            type_,
            lambda x: False,
            expression=expression,
            alloc_id=None,
        )


class UnknownStatement(stmt.Statement):
    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[expr.Expr], expr.Expr]
    ) -> RecordFluxError:
        raise NotImplementedError

    def variables(self) -> Sequence[expr.Variable]:
        raise NotImplementedError

    def to_tac(self, variable_id: abc.Generator[ID, None, None]) -> list[tac.Stmt]:
        raise NotImplementedError


@pytest.mark.parametrize(
    "action, expected",
    [
        (
            # Eng/RecordFlux/RecordFlux#1069
            # Replace this test case by an integration test.
            #
            # X := Universal::Message'(Message_Type => Universal::MT_Data, Length => 0, Data => [])
            stmt.VariableAssignment(
                "X",
                expr.MessageAggregate(
                    "Universal::Message",
                    {
                        "Message_Type": expr.Literal(
                            "Universal::MT_Data",
                            type_=rty.Enumeration(
                                "Universal::Message_Type", [ID("Universal::MT_Data")]
                            ),
                        ),
                        "Length": expr.Number(0),
                        "Data": expr.Aggregate(),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration(
                                "Universal::Message_Type", [ID("Universal::MT_Data")]
                            ),
                            ID("Length"): rty.Integer("Universal::Length"),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                ),
                location=Location(start=(1, 1)),
            ),
            ""  # https://github.com/PyCQA/pylint/issues/3368
            + """\
-- <stdin>:1:1
Universal.Message.Reset (X_Ctx);
if Universal.Message.Available_Space (X_Ctx, Universal.Message.F_Message_Type) < 24 then
   Ada.Text_IO.Put_Line ("Error: insufficient space in ""X_Ctx"" for creating message");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
pragma Assert (Universal.Message.Sufficient_Space (X_Ctx, Universal.Message.F_Message_Type));
Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);
pragma Assert (Universal.Message.Sufficient_Space (X_Ctx, Universal.Message.F_Length));
Universal.Message.Set_Length (X_Ctx, Universal.Length (0));
if Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (0 * RFLX_Types.Byte'Size)) then
   Universal.Message.Set_Data_Empty (X_Ctx);
else
   Ada.Text_IO.Put_Line ("Error: invalid message field size for ""[]""\");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;\
""",
        ),
        (
            stmt.VariableAssignment(
                "X",
                expr.Call(
                    "F",
                    [
                        expr.Variable("A", type_=rty.Message("Universal::Message")),
                    ],
                    type_=rty.BOOLEAN,
                    argument_types=[
                        rty.Message("Universal::Message"),
                    ],
                ),
                location=Location(start=(1, 1)),
            ),
            """\
-- <stdin>:1:1
declare
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx, A, X);
end;\
""",
        ),
        (
            stmt.VariableAssignment(
                "X",
                expr.Call(
                    "F",
                    [
                        expr.Variable("A", type_=rty.Message("Universal::Message")),
                    ],
                    type_=rty.Message("Universal::Option"),
                    argument_types=[
                        rty.Message("Universal::Message"),
                    ],
                ),
                location=Location(start=(1, 1)),
            ),
            """\
-- <stdin>:1:1
declare
   X : Universal.Option.Structure;
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx, A, X);
   if Universal.Option.Valid_Structure (X) then
      if Universal.Option.Sufficient_Buffer_Length (X_Ctx, X) then
         Universal.Option.To_Context (X, X_Ctx);
      else
         Ada.Text_IO.Put_Line ("Error: insufficient space for converting message ""X""\");
         Ctx.P.Next_State := S_E;
         pragma Finalization;
         goto Finalize_S;
      end if;
   else
      Ada.Text_IO.Put_Line ("Error: ""F"" returned an invalid message");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
end;\
""",
        ),
        (
            stmt.VariableAssignment(
                "X",
                expr.And(
                    expr.Variable("A", type_=rty.BOOLEAN),
                    expr.Variable("B", type_=rty.BOOLEAN),
                ),
                type_=rty.BOOLEAN,
                location=Location(start=(1, 1)),
            ),
            "-- <stdin>:1:1\nX := A\nand then B;",
        ),
        (
            stmt.Reset(
                "X",
                type_=rty.Message("P::M"),
                location=Location(start=(1, 1)),
            ),
            "-- <stdin>:1:1\nP.M.Reset (X_Ctx);",
        ),
        (
            stmt.Reset(
                "X",
                type_=rty.Sequence("P::S", rty.Integer("A")),
                location=Location(start=(1, 1)),
            ),
            "-- <stdin>:1:1\nP.S.Reset (X_Ctx);",
        ),
        (
            stmt.Read(
                "X",
                expr.Variable("Y", type_=rty.Message("P::M")),
                location=Location(start=(1, 1)),
            ),
            "-- <stdin>:1:1\nP.M.Verify_Message (Y_Ctx);",
        ),
        (
            stmt.Write(
                "X",
                expr.Variable("Y", type_=rty.Message("P::M")),
                location=Location(start=(1, 1)),
            ),
            "-- <stdin>:1:1",
        ),
    ],
)
def test_session_state_action(action: stmt.Statement, expected: str) -> None:
    allocator = AllocatorGenerator(DUMMY_SESSION, Integration())
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=Debug.BUILTIN)
    # pylint: disable = protected-access
    allocator._allocation_slots[Location(start=(1, 1))] = 1
    assert (
        "\n".join(
            str(s)
            for s in session_generator._state_action(
                ID("S"),
                action,
                ExceptionHandler(
                    set(),
                    State("S", exception_transition=Transition("E")),
                    [ada.PragmaStatement("Finalization", [])],
                ),
                lambda x: False,
            )
        )
        == expected
    )


@pytest.mark.parametrize(
    "action, error_type, error_msg",
    [
        (
            stmt.Extend("L", expr.Variable("E"), location=Location((10, 20))),
            RecordFluxError,
            r"Extend statement not yet supported",
        ),
        (
            stmt.Write(
                "X",
                expr.MessageAggregate(
                    "Universal::Message",
                    {
                        "Message_Type": expr.Literal(
                            "Universal::MT_Data",
                            type_=rty.Enumeration(
                                "Universal::Message_Type", [ID("Universal::MT_Data")]
                            ),
                        ),
                        "Length": expr.Number(1),
                        "Data": expr.Aggregate(expr.Number(2)),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration(
                                "Universal::Message_Type", [ID("Universal::MT_Data")]
                            ),
                            ID("Length"): rty.Integer("Universal::Length"),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                    location=Location(start=(10, 20)),
                ),
            ),
            RecordFluxError,
            r'MessageAggregate with message type "Universal::Message" in Write statement'
            " not yet supported",
        ),
        (
            UnknownStatement("X", location=Location((10, 20))),
            FatalError,
            r'unexpected statement "UnknownStatement"',
        ),
    ],
)
def test_session_state_action_error(
    action: stmt.Statement, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._state_action(  # pragma: no branch
            ID("S"),
            action,
            ExceptionHandler(set(), State("S"), []),
            lambda x: False,
        )


@pytest.mark.parametrize(
    "type_, expression, error_type, error_msg",
    [
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Selected(
                expr.Variable("Z", type_=rty.Message("C")),
                "Z",
                type_=rty.Sequence("A", rty.Integer("B")),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r"copying of sequence not yet supported",
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Selected(
                expr.Call(
                    "F",
                    [expr.Variable("Y")],
                    type_=rty.Message("C"),
                    location=Location((10, 20)),
                ),
                "Z",
                type_=rty.Sequence("A", rty.Integer("B")),
            ),
            RecordFluxError,
            r'accessing field of expression "Call" not yet supported',
        ),
        (
            rty.Aggregate(rty.Integer("A")),
            expr.Selected(
                expr.Variable("Z", type_=rty.Message("B")),
                "Z",
                type_=rty.Aggregate(rty.AnyInteger()),
                location=Location((10, 20)),
            ),
            FatalError,
            r'unexpected type \(aggregate with element integer type\) for "Z.Z"'
            r' in assignment of "X"',
        ),
        (
            rty.Message("A"),
            expr.MessageAggregate(
                "Universal::Message",
                {
                    "Message_Type": expr.Literal(
                        "Universal::MT_Data",
                        type_=rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                    ),
                    "Length": expr.Number(1),
                    "Data": expr.Variable(
                        "Z", type_=rty.Message("Universal::Option"), location=Location((10, 20))
                    ),
                },
                type_=rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                        ID("Length"): rty.Integer("Universal::Length"),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r'Variable with message type "Universal::Option" in message aggregate'
            r" not yet supported",
        ),
        (
            rty.Message("A"),
            expr.MessageAggregate(
                "Universal::Message",
                {
                    "Message_Type": expr.Literal(
                        "Universal::MT_Data",
                        type_=rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                    ),
                    "Length": expr.Last(
                        expr.Variable(
                            "Z",
                            type_=rty.Message("Universal::Option"),
                            location=Location((10, 20)),
                        )
                    ),
                    "Data": expr.Variable(
                        "Z",
                        type_=rty.Message("Universal::Option"),
                        location=Location((10, 20)),
                    ),
                },
                type_=rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                        ID("Length"): rty.Integer("Universal::Length"),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r"Last with type universal integer \(undefined\) as value of message field"
            r" not yet supported",
        ),
        (
            rty.Message("A"),
            expr.MessageAggregate(
                "Universal::Message",
                {
                    "Message_Type": expr.Literal(
                        "Universal::MT_Data",
                        type_=rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                    ),
                    "Length": expr.Number(1),
                    "Data": expr.Head(
                        expr.Variable(
                            "Z",
                            type_=rty.Sequence(
                                "Universal::Options", rty.Message("Universal::Option")
                            ),
                            location=Location((10, 20)),
                        ),
                        type_=rty.Message("Universal::Option"),
                    ),
                },
                type_=rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type", [ID("Universal::MT_Data")]
                        ),
                        ID("Length"): rty.Integer("Universal::Length"),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r'Head with message type "Universal::Option" in expression not yet supported',
        ),
        (
            rty.Integer("A"),
            expr.Head(
                expr.Call(
                    "F",
                    [expr.Variable("Y")],
                    type_=rty.Sequence("B", rty.Integer("A", rty.Bounds(1, 100))),
                    location=Location((10, 20)),
                ),
                type_=rty.Integer("A", rty.Bounds(1, 100)),
            ),
            RecordFluxError,
            r'Call with sequence type "B" with element integer type "A" \(1 .. 100\)'
            r" in Head attribute not yet supported",
        ),
        (
            models.SEQUENCE_INTEGER_VECTOR.type_,
            expr.Head(
                expr.Call(
                    "F",
                    [expr.Variable("Y")],
                    type_=rty.Sequence("S", models.SEQUENCE_INTEGER_VECTOR.type_),
                    location=Location((10, 20)),
                ),
            ),
            FatalError,
            r'unexpected sequence element type \(sequence type "Sequence::Integer_Vector" with '
            r'element integer type "Sequence::Integer" \(1 .. 100\)\) '
            r'for "F \(Y\)\'Head" in assignment of "X"',
        ),
        (
            rty.Sequence("A", rty.Message("B")),
            expr.Comprehension(
                "E",
                expr.Variable(
                    "L",
                    type_=rty.Sequence("A", rty.Message("B")),
                ),
                expr.Call(
                    "F",
                    [expr.Variable("E")],
                    type_=rty.Message("B"),
                    location=Location((10, 20)),
                ),
                expr.Greater(
                    expr.Selected(expr.Variable("E", type_=rty.Message("B")), "Z"),
                    expr.Number(0),
                ),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Message("B"),
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Selected(
                        expr.Variable("Y", type_=rty.Message("M")),
                        "Z",
                        type_=rty.Sequence("A", rty.Message("C")),
                    ),
                    expr.Selected(
                        expr.Variable("E"),
                        "Z",
                        type_=rty.Message("B"),
                        location=Location((10, 20)),
                    ),
                    expr.Greater(
                        expr.Selected(expr.Variable("E", type_=rty.Message("B")), "Z"),
                        expr.Number(0),
                    ),
                ),
                type_=rty.Message("B"),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Message("B"),
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Variable(
                        "L",
                        type_=rty.Sequence("A", rty.Message("B")),
                    ),
                    expr.Call(
                        "F",
                        [expr.Variable("E")],
                        type_=rty.Message("B"),
                        location=Location((10, 20)),
                    ),
                    expr.Greater(
                        expr.Selected(expr.Variable("E", type_=rty.Message("B")), "Z"),
                        expr.Number(0),
                    ),
                ),
                type_=rty.Message("B"),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Comprehension(
                "E",
                expr.Selected(
                    expr.Call(
                        "L",
                        [expr.Variable("Z")],
                        location=Location((10, 20)),
                    ),
                    "Z",
                    type_=rty.Sequence("A", rty.Message("C")),
                ),
                expr.Selected(expr.Variable("E"), "Z", type_=rty.Integer("B")),
                expr.Greater(expr.Selected(expr.Variable("E"), "Z"), expr.Number(0)),
            ),
            RecordFluxError,
            r"Call with undefined type as prefix of Selected in list comprehension"
            r" not yet supported",
        ),
        (
            rty.Message("B"),
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Selected(
                        expr.Call(
                            "L",
                            [expr.Variable("Z")],
                            location=Location((10, 20)),
                        ),
                        "Z",
                        type_=rty.Sequence("A", rty.Message("C")),
                    ),
                    expr.Selected(expr.Variable("E"), "Z", type_=rty.Message("B")),
                    expr.Greater(expr.Selected(expr.Variable("E"), "Z"), expr.Number(0)),
                ),
                type_=rty.Message("B"),
            ),
            RecordFluxError,
            r"Call with undefined type as prefix of Selected in list comprehension"
            r" not yet supported",
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Comprehension(
                "E",
                expr.Variable(
                    "L", type_=rty.Sequence("A", rty.AnyInteger()), location=Location((10, 20))
                ),
                expr.Variable("E", type_=rty.Integer("B")),
                expr.Greater(expr.Variable("E"), expr.Number(0)),
            ),
            RecordFluxError,
            r"iterating over sequence of integer type in list comprehension not yet supported",
        ),
        (
            rty.Integer("B"),
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Variable(
                        "L", type_=rty.Sequence("A", rty.AnyInteger()), location=Location((10, 20))
                    ),
                    expr.Variable("E", type_=rty.Integer("B")),
                    expr.Greater(expr.Variable("E"), expr.Number(0)),
                ),
                type_=rty.Integer("B"),
            ),
            RecordFluxError,
            r"iterating over sequence of integer type in list comprehension not yet supported",
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Comprehension(
                "E",
                expr.Call(
                    "L",
                    [expr.Variable("Z")],
                    type_=rty.Sequence("A", rty.Message("C")),
                    location=Location((10, 20)),
                ),
                expr.Selected(
                    expr.Variable("E", type_=rty.Message("C")), "Z", type_=rty.Integer("B")
                ),
                expr.Greater(expr.Selected(expr.Variable("E"), "Z"), expr.Number(0)),
            ),
            RecordFluxError,
            r'Call with sequence type "A" with element message type "C"'
            r" as sequence in list comprehension not yet supported",
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Call(
                "F",
                [
                    expr.Call(
                        "G",
                        [
                            expr.Variable(
                                "Z",
                            )
                        ],
                        location=Location((10, 20)),
                    ),
                ],
                argument_types=[rty.Integer("C")],
            ),
            RecordFluxError,
            r"Call with undefined type as function argument not yet supported",
        ),
        (
            rty.Message("A"),
            expr.Conversion(
                "T",
                expr.Selected(
                    expr.Variable("Z", type_=rty.Message("B")),
                    "Z",
                    type_=rty.OPAQUE,
                ),
                type_=rty.Message("A"),
                location=Location((10, 20)),
            ),
            FatalError,
            r'no refinement for field "Z" of message "B" leads to "A"',
        ),
        (
            rty.Message("A"),
            expr.Variable(
                "X",
                type_=rty.Message("A"),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'referencing assignment target "X" of type message in expression not yet supported',
        ),
        (
            rty.Message("A"),
            expr.Variable(
                "Y",
                type_=rty.Message("A"),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'Variable with message type "A" in assignment not yet supported',
        ),
        (
            rty.Undefined(),
            expr.UndefinedExpr(location=Location((10, 20))),
            FatalError,
            r"unexpected UndefinedExpr with undefined type in assignment",
        ),
    ],
)
def test_session_assign_error(
    type_: rty.Type,
    expression: expr.Expr,
    error_type: type[BaseError],
    error_msg: str,
) -> None:
    allocator = AllocatorGenerator(DUMMY_SESSION, Integration())
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=Debug.BUILTIN)
    alloc_id = Location(start=(1, 1))
    # pylint: disable = protected-access
    allocator._allocation_slots[alloc_id] = 1

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._assign(
            ID("X", location=Location((10, 20))),
            type_,
            expression,
            ExceptionHandler(set(), State("S", exception_transition=Transition("E")), []),
            lambda x: False,
            ID("State"),
            alloc_id=alloc_id,
        )


@pytest.mark.parametrize(
    "append, error_type, error_msg",
    [
        (
            stmt.Append(
                "L",
                expr.Variable("X", type_=rty.Message("A"), location=Location((10, 20))),
                type_=rty.Sequence("B", rty.Message("A")),
            ),
            RecordFluxError,
            r'Variable with message type "A" in Append statement not yet supported',
        ),
        (
            stmt.Append(
                "L",
                expr.Variable("X", location=Location((10, 20))),
                type_=rty.Sequence("B", rty.Undefined()),
            ),
            FatalError,
            r"unexpected element type undefined type in Append statement",
        ),
        (
            stmt.Append(
                "L",
                expr.Call("X", type_=rty.Integer("A"), location=Location((10, 20))),
                type_=rty.Sequence("B", rty.Integer("A")),
            ),
            RecordFluxError,
            r'Call with integer type "A" \(undefined\) in Append statement not yet supported',
        ),
    ],
)
def test_session_append_error(
    append: stmt.Append, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._append(
            append,
            ExceptionHandler(set(), State("S", exception_transition=Transition("E")), []),
            lambda x: False,
            ID("State"),
        )


@pytest.mark.parametrize(
    "read, error_type, error_msg",
    [
        (
            stmt.Read(
                "L",
                expr.Call("X", type_=rty.Message("A"), location=Location((10, 20))),
                type_=rty.Message("A"),
            ),
            RecordFluxError,
            r'Call with message type "A" in Read statement not yet supported',
        ),
    ],
)
def test_session_read_error(read: stmt.Read, error_type: type[BaseError], error_msg: str) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._read(  # pragma: no branch
            read,
            lambda x: False,
        )


@pytest.mark.parametrize(
    "write, error_type, error_msg",
    [
        (
            stmt.Write(
                "L",
                expr.Call("X", type_=rty.Message("A"), location=Location((10, 20))),
                type_=rty.Message("A"),
            ),
            RecordFluxError,
            r'Call with message type "A" in Write statement not yet supported',
        ),
    ],
)
def test_session_write_error(
    write: stmt.Write, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._write(write)


@pytest.mark.parametrize(
    "expression, expected",
    [
        (
            expr.And(expr.Variable("X"), expr.Variable("Y")),
            expr.AndThen(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.Or(expr.Variable("X"), expr.Variable("Y")),
            expr.OrElse(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.Selected(expr.Variable("X", type_=rty.Message("P::M")), "Y"),
            expr.Call("P::M::Get_Y", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Valid(expr.Variable("X", type_=rty.Message("P::M"))),
            expr.Call("P::M::Well_Formed_Message", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Valid(expr.Variable("X", type_=rty.Sequence("P::S", rty.Message("P::M")))),
            expr.Call("P.S.Valid", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Valid(
                expr.Selected(
                    expr.Variable("X", type_=rty.Message("P::M")), "Y", type_=rty.Integer("A")
                )
            ),
            expr.Call("P::M::Valid", [expr.Variable("X_Ctx"), expr.Variable("P::M::F_Y")]),
        ),
        (
            expr.Valid(
                expr.Selected(expr.Variable("X", type_=rty.Message("P::M")), "Y", type_=rty.OPAQUE)
            ),
            expr.Call("P::M::Well_Formed", [expr.Variable("X_Ctx"), expr.Variable("P::M::F_Y")]),
        ),
        (
            expr.Present(expr.Selected(expr.Variable("X", type_=rty.Message("P::M")), "Y")),
            expr.Call("P::M::Present", [expr.Variable("X_Ctx"), expr.Variable("P::M::F_Y")]),
        ),
        (
            expr.Aggregate(expr.Number(1)),
            expr.NamedAggregate(
                (
                    str(expr.First(const.TYPES_INDEX)),
                    expr.Val(const.TYPES_BYTE, expr.Number(1)),
                )
            ),
        ),
        (
            expr.Aggregate(expr.Number(1), expr.Number(2)),
            expr.Aggregate(
                expr.Val(const.TYPES_BYTE, expr.Number(1)),
                expr.Val(const.TYPES_BYTE, expr.Number(2)),
            ),
        ),
        (
            expr.Equal(expr.Variable("X"), expr.Variable("Y")),
            expr.Equal(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.NotEqual(expr.Variable("X"), expr.Variable("Y")),
            expr.NotEqual(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.Equal(
                expr.Variable("X", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
                expr.Variable("Y", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            ),
            expr.Equal(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.NotEqual(
                expr.Variable("X", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
                expr.Variable("Y", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            ),
            expr.NotEqual(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.Size(expr.Variable("X", type_=rty.Integer("P::I"))),
            expr.Size(expr.Variable("X")),
        ),
        (
            expr.Size(expr.Variable("X", type_=rty.Aggregate(rty.Integer("P::I")))),
            expr.Size(expr.Variable("X")),
        ),
        (
            expr.Size(expr.Variable("X", type_=rty.Message("P::M"))),
            expr.Call("P::M::Size", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Size(expr.Variable("X", type_=rty.Sequence("P::S", rty.Message("P::M")))),
            expr.Call("P::S::Size", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Size(expr.Selected(expr.Variable("X", type_=rty.Message("P::M")), "Y")),
            expr.Call("P::M::Field_Size", [expr.Variable("X_Ctx"), expr.Variable("P::M::F_Y")]),
        ),
        (
            expr.HasData(expr.Variable("X", type_=rty.Message("P::M"))),
            expr.Greater(expr.Call("P::M::Byte_Size", [expr.Variable("X_Ctx")]), expr.Number(0)),
        ),
        (
            expr.Opaque(expr.Variable("X", type_=rty.Message("P::M"))),
            expr.Call("P::M::Data", [expr.Variable("X_Ctx")]),
        ),
        (
            expr.Selected(expr.Variable("M", type_=rty.Message("P::M")), "F"),
            expr.Call("P::M::Get_F", [expr.Variable("M_Ctx")]),
        ),
        (
            expr.Selected(expr.Variable("M", type_=rty.Structure("P::M")), "F"),
            expr.Selected(expr.Variable("M"), "F"),
        ),
    ],
)
def test_session_substitution(expression: expr.Expr, expected: expr.Expr) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )
    # pylint: disable = protected-access
    assert expression.substituted(session_generator._substitution(lambda x: False)) == expected


@pytest.mark.parametrize(
    "expression, error_type, error_msg",
    [
        (
            expr.Size(expr.Variable("X", location=Location((10, 20)))),
            FatalError,
            r"unexpected Variable with undefined type in Size attribute",
        ),
        (
            expr.Size(expr.Call("X", type_=rty.AnyInteger(), location=Location((10, 20)))),
            RecordFluxError,
            r"Call with integer type in Size attribute not yet supported",
        ),
        (
            expr.Opaque(expr.Call("X", type_=rty.AnyInteger(), location=Location((10, 20)))),
            RecordFluxError,
            r"Call with integer type in Opaque attribute not yet supported",
        ),
        (
            expr.Opaque(expr.Variable("X", type_=rty.AnyInteger(), location=Location((10, 20)))),
            RecordFluxError,
            r"Variable with integer type in Opaque attribute not yet supported",
        ),
    ],
)
def test_session_substitution_error(
    expression: expr.Expr, error_type: type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )
    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        expression.substituted(  # pragma: no branch
            session_generator._substitution(lambda x: False)
        )


@pytest.mark.parametrize(
    "relation, left, right, expected",
    [
        (expr.Equal, expr.Variable("X"), expr.TRUE, expr.Variable("X")),
        (expr.Equal, expr.Variable("X"), expr.FALSE, expr.Not(expr.Variable("X"))),
        (expr.NotEqual, expr.Variable("X"), expr.TRUE, expr.Not(expr.Variable("X"))),
        (expr.NotEqual, expr.Variable("X"), expr.FALSE, expr.Variable("X")),
        (
            expr.Equal,
            expr.Selected(
                expr.Variable("X", type_=rty.Message("P::M")),
                "Y",
                type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True),
            ),
            expr.Variable("Z", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            expr.AndThen(
                expr.Selected(expr.Call("P::M::Get_Y", [expr.Variable("X_Ctx")]), "Known"),
                expr.Equal(
                    expr.Selected(expr.Call("P::M::Get_Y", [expr.Variable("X_Ctx")]), "Enum"),
                    expr.Variable("Z"),
                ),
            ),
        ),
        (
            expr.NotEqual,
            expr.Selected(
                expr.Variable("X", type_=rty.Message("P::M")),
                "Y",
                type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True),
            ),
            expr.Variable("Z", type_=rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            expr.AndThen(
                expr.Selected(expr.Call("P::M::Get_Y", [expr.Variable("X_Ctx")]), "Known"),
                expr.NotEqual(
                    expr.Selected(expr.Call("P::M::Get_Y", [expr.Variable("X_Ctx")]), "Enum"),
                    expr.Variable("Z"),
                ),
            ),
        ),
    ],
)
def test_session_substitution_equality(
    relation: Callable[[expr.Expr, expr.Expr], expr.Expr],
    left: expr.Expr,
    right: expr.Expr,
    expected: expr.Expr,
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=Debug.BUILTIN
    )

    # pylint: disable = protected-access
    assert (
        relation(left, right).substituted(session_generator._substitution(lambda x: False))
        == expected
    )
    assert (
        relation(right, left).substituted(session_generator._substitution(lambda x: False))
        == expected
    )


def test_generate_unused_valid_function_parameter(tmp_path: Path) -> None:
    types = [
        mty.Integer(
            "P::T",
            first=expr.Number(0),
            last=expr.Sub(
                expr.Pow(expr.Number(2), expr.Number(constants.MAX_SCALAR_SIZE)), expr.Number(1)
            ),
            size=expr.Number(constants.MAX_SCALAR_SIZE),
        )
    ]
    Generator(reproducible=True).generate(
        Model(types), Integration(), tmp_path, library_files=False, top_level_package=False
    )
    assert (tmp_path / "p.ads").exists()
    assert (tmp_path / "p.ads").read_text() == textwrap.dedent(
        '''\
        pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
        pragma Warnings (Off, "redundant conversion");
        with RFLX_Types;

        package P with
          SPARK_Mode
        is

           type T is range 0 .. 2**63 - 1 with
             Size =>
               63;

           pragma Warnings (Off, "unused variable ""Val""");

           pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

           function Valid_T (Val : RFLX_Types.Base_Integer) return Boolean is
             (True);

           pragma Warnings (On, "formal parameter ""Val"" is not referenced");

           pragma Warnings (On, "unused variable ""Val""");

           function To_Base_Integer (Val : P.T) return RFLX_Types.Base_Integer is
             (RFLX_Types.Base_Integer (Val));

           function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
             (P.T (Val))
            with
             Pre =>
               Valid_T (Val);

        end P;
        '''
    )


@pytest.mark.parametrize(
    "always_valid, expected",
    [
        (
            False,
            """\
            pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
            pragma Warnings (Off, "redundant conversion");
            with RFLX_Types;

            package P with
              SPARK_Mode
            is

               type T is (E1) with
                 Size =>
                   63;
               for T use (E1 => 1);

               use type RFLX_Types.Base_Integer;

               function Valid_T (Val : RFLX_Types.Base_Integer) return Boolean is
                 (Val in 1);

               function To_Base_Integer (Enum : P.T) return RFLX_Types.Base_Integer is
                 ((case Enum is
                      when E1 =>
                         1));

               pragma Warnings (Off, "unreachable branch");

               function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
                 ((case Val is
                      when 1 =>
                         E1,
                      when others =>
                         P.T'Last))
                with
                 Pre =>
                   Valid_T (Val);

               pragma Warnings (On, "unreachable branch");

            end P;
            """,
        ),
        (
            True,
            """\
            pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
            pragma Warnings (Off, "redundant conversion");
            with RFLX_Types;

            package P with
              SPARK_Mode
            is

               type T_Enum is (E1) with
                 Size =>
                   63;
               for T_Enum use (E1 => 1);

               type T (Known : Boolean := False) is
                  record
                     case Known is
                        when True =>
                           Enum : T_Enum;
                        when False =>
                           Raw : RFLX_Types.Base_Integer;
                     end case;
                  end record;

               function Valid_T (Unused_Val : RFLX_Types.Base_Integer) return Boolean is
                 (True);

               function Valid_T (Val : T) return Boolean is
                 ((if Val.Known then True else Valid_T (Val.Raw) and Val.Raw not in 1));

               function To_Base_Integer (Enum : P.T_Enum) return RFLX_Types.Base_Integer is
                 ((case Enum is
                      when E1 =>
                         1));

               function To_Actual (Enum : T_Enum) return P.T is
                 ((True, Enum));

               function To_Actual (Val : RFLX_Types.Base_Integer) return P.T is
                 ((case Val is
                      when 1 =>
                         (True, E1),
                      when others =>
                         (False, Val)))
                with
                 Pre =>
                   Valid_T (Val);

               function To_Base_Integer (Val : P.T) return RFLX_Types.Base_Integer is
                 ((if Val.Known then To_Base_Integer (Val.Enum) else Val.Raw));

            end P;
            """,
        ),
    ],
)
def test_generate_enumeration_base_type_use(
    always_valid: bool, expected: str, tmp_path: Path
) -> None:
    types = [
        mty.Enumeration(
            "P::T",
            literals=[("E1", expr.Number(1))],
            size=expr.Number(constants.MAX_SCALAR_SIZE),
            always_valid=always_valid,
        )
    ]
    Generator(reproducible=True).generate(
        Model(types), Integration(), tmp_path, library_files=False, top_level_package=False
    )
    assert (tmp_path / "p.ads").exists()
    assert (tmp_path / "p.ads").read_text() == textwrap.dedent(expected)


def test_generate_field_size_optimization() -> None:
    message = Message(
        "P::Message",
        [
            Link(INITIAL, Field("Length")),
            Link(
                Field("Length"),
                Field("Data"),
                size=expr.Add(expr.Size(expr.Variable("Length")), expr.Number(8)),
            ),
            Link(
                Field("Data"),
                FINAL,
            ),
        ],
        {
            Field("Length"): models.UNIVERSAL_LENGTH,
            Field("Data"): mty.OPAQUE,
        },
    )
    structure = create_structure("", message)
    assert (
        ada.ExpressionFunctionDeclaration(
            specification=ada.FunctionSpecification(
                identifier=ID("Field_Size_Length"),
                parameters=[
                    ada.Parameter(identifiers=[ID("Struct")], type_identifier=ID("Structure"))
                ],
                return_type=ID("RFLX_Types::Bit_Length"),
            ),
            expression=ada.Number(value=16),
        )
        in structure.private
    )


def test_generate_string_substitution() -> None:
    subst = common.substitution(models.DEFINITE_MESSAGE, "")
    assert subst(expr.String("abc")) == expr.Aggregate(
        expr.Number(97), expr.Number(98), expr.Number(99)
    )
