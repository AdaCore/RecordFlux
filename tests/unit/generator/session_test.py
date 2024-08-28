from __future__ import annotations

import typing
from dataclasses import dataclass
from functools import lru_cache
from typing import Callable, Sequence

import pytest
import z3
from attr import define

from rflx import ada, ir, typing_ as rty
from rflx.error import FatalError
from rflx.generator import const
from rflx.generator.allocator import AllocatorGenerator
from rflx.generator.common import Debug
from rflx.generator.session import (
    EvaluatedDeclaration,
    ExceptionHandler,
    FSMGenerator,
    SessionGenerator,
)
from rflx.identifier import ID, id_generator
from rflx.integration import Integration
from rflx.rapidflux import Location, RecordFluxError, ty
from tests.data import models

INT_TY = rty.Integer("I", ty.Bounds(1, 100))
MSG_TY = rty.Message(ID("M", Location((1, 1))))
SEQ_TY = rty.Sequence("S", rty.Message(ID("M", Location((1, 1)))))


@lru_cache
def dummy_session() -> ir.Session:
    return ir.Session(
        identifier=ID("P::S"),
        states=[
            ir.State(
                "State",
                [ir.Transition("Final", ir.ComplexExpr([], ir.BoolVal(value=True)), None, None)],
                None,
                [],
                None,
                None,
            ),
        ],
        declarations=[],
        parameters=[],
        types={t.identifier: t for t in models.universal_model().types},
        location=None,
        variable_id=id_generator(),
    )


@pytest.mark.parametrize(
    ("parameter", "expected"),
    [
        (
            ir.FuncDecl("F", [], "T", type_=rty.BOOLEAN, location=None),
            [
                ada.SubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.InOutParameter(["State"], "P.S_Environment.State"),
                            ada.OutParameter(["RFLX_Result"], "Boolean"),
                        ],
                    ),
                ),
            ],
        ),
        (
            ir.FuncDecl(
                "F",
                [
                    ir.Argument("P1", "Boolean", type_=rty.BOOLEAN),
                    ir.Argument("P2", "T2", type_=rty.OPAQUE),
                    ir.Argument(
                        "P3",
                        "T3",
                        type_=rty.Enumeration("T4", [ID("E1")], always_valid=True),
                    ),
                    ir.Argument("P4", "T4", type_=INT_TY),
                    ir.Argument("P5", "T5", type_=rty.Message("T5", is_definite=True)),
                ],
                "T",
                type_=rty.Message("T", is_definite=True),
                location=None,
            ),
            [
                ada.SubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.InOutParameter(["State"], "P.S_Environment.State"),
                            ada.Parameter(["P1"], "Boolean"),
                            ada.Parameter(["P2"], const.TYPES_BYTES),
                            ada.Parameter(["P3"], "T3"),
                            ada.Parameter(["P4"], "T4"),
                            ada.Parameter(["P5"], "T5.Structure"),
                            ada.OutParameter(["RFLX_Result"], "T.Structure"),
                        ],
                    ),
                ),
            ],
        ),
    ],
)
def test_session_create_function(
    parameter: ir.FuncDecl,
    expected: Sequence[ada.SubprogramDeclaration],
) -> None:
    session_generator = SessionGenerator(
        dummy_session(),
        AllocatorGenerator(dummy_session(), Integration()),
    )

    assert session_generator._create_function(parameter) == expected  # noqa: SLF001


class UnknownDeclaration(ir.FormalDecl):
    def __str__(self) -> str:
        raise NotImplementedError

    @property
    def type_(self) -> rty.Type:
        raise NotImplementedError


@pytest.mark.parametrize(
    ("parameter", "error_type", "error_msg"),
    [
        (
            UnknownDeclaration("X", Location((10, 20))),
            FatalError,
            r'unexpected formal parameter "X"',
        ),
    ],
)
def test_session_verify_formal_parameters(
    parameter: ir.FuncDecl,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    session_generator = SessionGenerator(
        dummy_session(),
        AllocatorGenerator(dummy_session(), Integration()),
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        session_generator._verify_formal_parameters([parameter])  # noqa: SLF001


@pytest.mark.parametrize(
    ("parameter", "error_type", "error_msg"),
    [
        (
            ir.FuncDecl(
                "F",
                [],
                "T",
                rty.Undefined(),
                Location((10, 20)),
            ),
            FatalError,
            r'return type of function "F" is undefined',
        ),
        (
            ir.FuncDecl(
                "F",
                [],
                "T",
                rty.OPAQUE,
                Location((10, 20)),
            ),
            FatalError,
            r'Opaque as return type of function "F" not allowed',
        ),
        (
            ir.FuncDecl(
                "F",
                [],
                "T",
                rty.Sequence("A", INT_TY),
                Location((10, 20)),
            ),
            RecordFluxError,
            r'sequence as return type of function "F" not yet supported',
        ),
        (
            ir.FuncDecl(
                "F",
                [],
                "T",
                rty.Message("A", is_definite=False),
                Location((10, 20)),
            ),
            FatalError,
            r'non-definite message in return type of function "F" not allowed',
        ),
        (
            ir.FuncDecl(
                "F",
                [],
                "T",
                rty.Message(
                    "M",
                    {("F",)},
                    {ID("F"): rty.Sequence("A", INT_TY)},
                    is_definite=True,
                ),
                Location((10, 20)),
            ),
            RecordFluxError,
            r'message containing sequence fields in return type of function "F" not yet supported',
        ),
        (
            ir.FuncDecl(
                "F",
                [ir.Argument("P", "T", rty.Sequence("A", INT_TY))],
                "T",
                rty.BOOLEAN,
                Location((10, 20)),
            ),
            RecordFluxError,
            r'sequence as parameter of function "F" not yet supported',
        ),
    ],
)
def test_session_create_functions_error(
    parameter: ir.FuncDecl,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    session_generator = SessionGenerator(
        dummy_session(),
        AllocatorGenerator(dummy_session(), Integration()),
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        session_generator._create_functions([parameter])  # noqa: SLF001


@pytest.mark.parametrize(
    ("declaration", "session_global", "expected"),
    [
        (
            ir.VarDecl("X", rty.BOOLEAN),
            False,
            EvaluatedDeclaration(global_declarations=[ada.ObjectDeclaration("X", "Boolean")]),
        ),
        (
            ir.VarDecl("X", INT_TY, ir.ComplexIntExpr([], ir.IntVal(1))),
            False,
            EvaluatedDeclaration(
                global_declarations=[ada.ObjectDeclaration("X", "P.I", ada.Number(1))],
            ),
        ),
        (
            ir.VarDecl("X", INT_TY, ir.ComplexIntExpr([], ir.IntVal(1))),
            True,
            EvaluatedDeclaration(
                global_declarations=[ada.ObjectDeclaration("X", "P.I", ada.Number(1))],
                initialization=[
                    ada.CallStatement(
                        "P.S.FSM_Allocator.Initialize",
                        [ada.Variable("Ctx.P.Slots"), ada.Variable("Ctx.P.Memory")],
                    ),
                    ada.Assignment("Ctx.P.X", ada.Number(1)),
                ],
                finalization=[
                    ada.CallStatement("P.S.FSM_Allocator.Finalize", [ada.Variable("Ctx.P.Slots")]),
                ],
            ),
        ),
        (
            ir.VarDecl(
                "X",
                rty.Message("T"),
                origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
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
                        "P.T.Initialize",
                        [ada.Variable("X_Ctx"), ada.Variable("X_Buffer")],
                    ),
                ],
                finalization=[
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String(
                                '"X_Ctx" is set by "Take_Buffer" but not used after the call',
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
                                '"X_Ctx" is set by "Take_Buffer" but not used after the call',
                            ),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.Equal(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null"))],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.NotEqual(ada.Variable("X_Buffer"), ada.Variable("null"))],
                    ),
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                        ada.Variable("X_Buffer"),
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [
                            ada.NotEqual(
                                ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                                ada.Variable("null"),
                            ),
                        ],
                    ),
                ],
            ),
        ),
        (
            ir.VarDecl(
                "X",
                rty.Message("T"),
                origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
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
                        "P.S.FSM_Allocator.Initialize",
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
                        "P.T.Initialize",
                        [ada.Variable("Ctx.P.X_Ctx"), ada.Variable("X_Buffer")],
                    ),
                ],
                finalization=[
                    ada.PragmaStatement(
                        "Warnings",
                        [
                            ada.Variable("Off"),
                            ada.String(
                                '"Ctx.P.X_Ctx" is set by "Take_Buffer" but not used after the call',
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
                                '"Ctx.P.X_Ctx" is set by "Take_Buffer" but not used after the call',
                            ),
                        ],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.Equal(ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("null"))],
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [ada.NotEqual(ada.Variable("X_Buffer"), ada.Variable("null"))],
                    ),
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                        ada.Variable("X_Buffer"),
                    ),
                    ada.PragmaStatement(
                        "Assert",
                        [
                            ada.NotEqual(
                                ada.Variable("Ctx.P.Slots.Slot_Ptr_1"),
                                ada.Variable("null"),
                            ),
                        ],
                    ),
                    ada.CallStatement("P.S.FSM_Allocator.Finalize", [ada.Variable("Ctx.P.Slots")]),
                ],
            ),
        ),
    ],
)
def test_session_evaluate_declarations(
    declaration: ir.VarDecl,
    session_global: bool,
    expected: EvaluatedDeclaration,
) -> None:
    allocator = AllocatorGenerator(dummy_session(), Integration())

    allocator._allocation_slots[Location((1, 1))] = 1  # noqa: SLF001
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        allocator,
        debug=Debug.BUILTIN,
    )
    assert (
        fsm_generator._evaluate_declarations(  # noqa: SLF001
            [declaration],
            is_global=lambda _: False,
            session_global=session_global,
        )
        == expected
    )


@dataclass
class EvaluatedDeclarationStr:
    global_declarations: str = ""
    initialization_declarations: str = ""
    initialization: str = ""
    finalization: str = ""


@pytest.mark.parametrize(
    ("type_", "expression", "constant", "session_global", "expected"),
    [
        (
            INT_TY,
            ir.ComplexExpr([], ir.IntVal(1)),
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.I := 1;",
            ),
        ),
        (
            INT_TY,
            ir.ComplexExpr([], ir.IntVal(1)),
            False,
            True,
            EvaluatedDeclarationStr(
                global_declarations="X : P.I := 1;",
                initialization="X := 1;",
            ),
        ),
        (
            INT_TY,
            ir.ComplexExpr([], ir.IntVal(1)),
            True,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.I := 1;",
            ),
        ),
        (
            INT_TY,
            None,
            False,
            False,
            EvaluatedDeclarationStr(
                global_declarations="X : P.I;",
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
            ir.ComplexExpr([], ir.Agg([])),
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
            ir.ComplexExpr([], ir.Agg([])),
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
            ir.ComplexExpr([], ir.Agg([ir.IntVal(1)])),
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
            ir.ComplexExpr([], ir.Agg([ir.IntVal(1)])),
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
            ir.ComplexExpr([], ir.Agg([ir.IntVal(1), ir.IntVal(2)])),
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
            ir.ComplexExpr([], ir.Agg([ir.IntVal(1), ir.IntVal(2)])),
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
            ir.ComplexExpr([], ir.Agg([ir.IntVal(1), ir.IntVal(2)])),
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
    expression: ir.ComplexExpr | None,
    constant: bool,
    session_global: bool,
    expected: EvaluatedDeclarationStr,
) -> None:
    loc: Location = Location((1, 1))
    allocator = AllocatorGenerator(dummy_session(), Integration())

    allocator._allocation_slots[loc] = 1  # noqa: SLF001
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        allocator,
        debug=Debug.BUILTIN,
    )

    result = fsm_generator._declare(  # noqa: SLF001
        ID("X"),
        type_,
        lambda _: False,
        loc,
        expression,
        constant,
        session_global,
    )
    assert "\n".join(str(d) for d in result.global_declarations) == expected.global_declarations
    assert (
        "\n".join(str(d) for d in result.initialization_declarations)
        == expected.initialization_declarations
    )
    assert "\n".join(str(s) for s in result.initialization) == expected.initialization
    assert "\n".join(str(s) for s in result.finalization) == expected.finalization


@pytest.mark.parametrize(
    ("type_", "expression", "error_type", "error_msg"),
    [
        (
            INT_TY,
            ir.ComplexExpr(
                [],
                ir.IntCall(
                    "F",
                    [],
                    [],
                    INT_TY,
                    origin=ir.ConstructedOrigin("F", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r"initialization using function call not yet supported",
        ),
        (
            rty.OPAQUE,
            ir.ComplexExpr(
                [ir.Assign("X", ir.IntVal(0), INT_TY)],
                ir.IntVar(
                    "X",
                    INT_TY,
                    origin=ir.ConstructedOrigin("X", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r"initialization not yet supported",
        ),
        (
            rty.Message("T"),
            ir.ComplexExpr(
                [],
                ir.EnumLit(
                    "True",
                    rty.BOOLEAN,
                    origin=ir.ConstructedOrigin("True", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r'initialization for message type "T" not yet supported',
        ),
        (
            INT_TY,
            ir.ComplexExpr(
                [ir.Assign("X", ir.IntVal(0), INT_TY)],
                ir.IntVar(
                    "X",
                    INT_TY,
                    origin=ir.ConstructedOrigin("X", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r"initialization with complex expression not yet supported",
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
    type_: rty.Type,
    expression: ir.ComplexExpr | None,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._declare(  # pragma: no branch # noqa: SLF001
            ID("X", Location((10, 20))),
            type_,
            lambda _: False,
            expression=expression,
            alloc_id=None,
        )


@define
class UnknownStatement(ir.Stmt):
    @property
    def accessed_vars(self) -> list[ID]:
        raise NotImplementedError

    def preconditions(self, variable_id: typing.Generator[ID, None, None]) -> list[ir.Cond]:
        raise NotImplementedError

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        raise NotImplementedError


@pytest.mark.parametrize(
    ("action", "expected"),
    [
        (
            # Eng/RecordFlux/RecordFlux#1069
            # Replace this test case by an integration test.
            #
            # X := Universal::Message'(Message_Type => Universal::MT_Data, Length => 0, Data => [])
            ir.Assign(
                "X",
                ir.MsgAgg(
                    "Universal::Message",
                    {
                        ID("Message_Type"): ir.EnumLit(
                            ID("Universal::MT_Data"),
                            rty.Enumeration("Universal::Message_Type", [ID("Universal::MT_Data")]),
                        ),
                        ID("Length"): ir.IntVal(0),
                        ID("Data"): ir.Agg([]),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration(
                                "Universal::Message_Type",
                                [ID("Universal::MT_Data")],
                            ),
                            ID("Length"): rty.Integer("Universal::Length", ty.Bounds(0, 100)),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                ),
                MSG_TY,
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            '''\
-- <stdin>:1:1
Universal.Message.Reset (X_Ctx);
if not Universal.Message.Sufficient_Space (X_Ctx, Universal.Message.F_Message_Type) then
   Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx"" to set field ""Message_Type"" to ""Universal::MT_Data""");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);
if not Universal.Message.Sufficient_Space (X_Ctx, Universal.Message.F_Length) then
   Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx"" to set field ""Length"" to ""0""");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
Universal.Message.Set_Length (X_Ctx, 0);
if not Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (0 * RFLX_Types.Byte'Size)) then
   Ada.Text_IO.Put_Line ("Error: invalid message field size for ""[]""\");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
Universal.Message.Set_Data_Empty (X_Ctx);\
''',  # noqa: E501
        ),
        (
            ir.Assign(
                "X",
                ir.BoolCall(
                    "F",
                    [
                        ir.ObjVar("A", rty.Message("Universal::Message")),
                    ],
                    [
                        rty.Message("Universal::Message"),
                    ],
                ),
                rty.BOOLEAN,
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            """\
-- <stdin>:1:1
declare
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx.E, A, X);
end;\
""",
        ),
        (
            ir.Assign(
                "X",
                ir.ObjCall(
                    "F",
                    [
                        ir.ObjVar("A", rty.Message("Universal::Message")),
                    ],
                    [
                        rty.Message("Universal::Message"),
                    ],
                    rty.Message("Universal::Option"),
                ),
                rty.Message("Universal::Option"),
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            """\
-- <stdin>:1:1
declare
   X : Universal.Option.Structure;
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx.E, A, X);
   if not Universal.Option.Valid_Structure (X) then
      Ada.Text_IO.Put_Line ("Error: ""F"" returned an invalid message");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
   if not Universal.Option.Sufficient_Buffer_Length (X_Ctx, X) then
      Ada.Text_IO.Put_Line ("Error: insufficient space for converting message ""X""\");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
   Universal.Option.To_Context (X, X_Ctx);
end;\
""",
        ),
        (
            ir.Assign(
                "X",
                ir.And(
                    ir.BoolVar("A"),
                    ir.BoolVar("B"),
                ),
                rty.BOOLEAN,
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            "-- <stdin>:1:1\nX := A\nand then B;",
        ),
        (
            ir.Reset(
                "X",
                {},
                rty.Message("P::M"),
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            "-- <stdin>:1:1\nP.M.Reset (X_Ctx);",
        ),
        (
            ir.Reset(
                "X",
                {},
                rty.Sequence("P::S", INT_TY),
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            "-- <stdin>:1:1\nP.S.Reset (X_Ctx);",
        ),
        (
            ir.Read(
                "X",
                ir.ObjVar("Y", rty.Message("P::M")),
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            "-- <stdin>:1:1\nP.M.Verify_Message (Y_Ctx);",
        ),
        (
            ir.Write(
                "X",
                ir.ObjVar("Y", rty.Message("P::M")),
                origin=ir.ConstructedOrigin("", Location((1, 1))),
            ),
            "-- <stdin>:1:1",
        ),
    ],
)
def test_session_state_action(action: ir.Stmt, expected: str) -> None:
    allocator = AllocatorGenerator(dummy_session(), Integration())
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        allocator,
        debug=Debug.BUILTIN,
    )

    allocator._allocation_slots[Location((1, 1))] = 1  # noqa: SLF001
    assert (
        "\n".join(
            str(s)
            for s in fsm_generator._state_action(  # noqa: SLF001
                ID("S"),
                action,
                ExceptionHandler(
                    ir.State(
                        "S",
                        [],
                        ir.Transition("E", ir.ComplexExpr([], ir.BoolVal(value=True)), None, None),
                        [],
                        None,
                        None,
                    ),
                    [ada.PragmaStatement("Finalization", [])],
                    lambda: None,
                ),
                lambda _: False,
            )
        )
        == expected
    )


@pytest.mark.parametrize(
    ("action", "error_type", "error_msg"),
    [
        (
            ir.Extend(
                "L",
                ir.ObjVar("E", MSG_TY),
                SEQ_TY,
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            RecordFluxError,
            r"Extend statement not yet supported",
        ),
        (
            UnknownStatement(ir.ConstructedOrigin("", Location((10, 20)))),
            FatalError,
            r'unexpected statement "UnknownStatement"',
        ),
    ],
)
def test_session_state_action_error(
    action: ir.Stmt,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._state_action(  # pragma: no branch # noqa: SLF001
            ID("S"),
            action,
            ExceptionHandler(
                ir.State("State", [], None, [], None, None),
                [],
                lambda: None,
            ),
            lambda _: False,
        )


@define
class UnknownExpr(ir.Expr):
    @property
    def type_(self) -> rty.Any:
        return rty.Message("T")

    @property
    def accessed_vars(self) -> list[ID]:
        raise NotImplementedError

    def preconditions(self, variable_id: typing.Generator[ID, None, None]) -> list[ir.Cond]:
        raise NotImplementedError

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        raise NotImplementedError


@pytest.mark.parametrize(
    ("type_", "expression", "error_type", "error_msg"),
    [
        (
            rty.Sequence("A", INT_TY),
            ir.ObjFieldAccess(
                "Z",
                "Z",
                rty.Message("C", {("Z",)}, {}, {ID("Z"): rty.Sequence("A", INT_TY)}),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            RecordFluxError,
            r"copying of sequence not yet supported",
        ),
        (
            rty.Aggregate(INT_TY),
            ir.ObjFieldAccess(
                "Z",
                "Z",
                rty.Message("B", {("Z",)}, {}, {ID("Z"): rty.Aggregate(INT_TY)}),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            FatalError,
            r'unexpected type \(aggregate with element integer type "I" \(1 \.\. 100\)\) for "Z.Z"'
            r' in assignment of "X"',
        ),
        (
            rty.Message("A"),
            ir.MsgAgg(
                "Universal::Message",
                {
                    ID("Message_Type"): ir.EnumLit(
                        "Universal::MT_Data",
                        rty.Enumeration("Universal::Message_Type", [ID("Universal::MT_Data")]),
                    ),
                    ID("Length"): ir.IntVal(1),
                    ID("Data"): ir.ObjVar(
                        "Z",
                        rty.Message("Universal::Option"),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                },
                rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type",
                            [ID("Universal::MT_Data")],
                        ),
                        ID("Length"): rty.Integer("Universal::Length", ty.Bounds(0, 100)),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r'ObjVar with message type "Universal::Option" in message aggregate'
            r" not yet supported",
        ),
        (
            rty.Message("A"),
            ir.MsgAgg(
                "Universal::Message",
                {
                    ID("Message_Type"): ir.EnumLit(
                        "Universal::MT_Data",
                        rty.Enumeration("Universal::Message_Type", [ID("Universal::MT_Data")]),
                    ),
                    ID("Length"): ir.Last(
                        "Z",
                        rty.Message("Universal::Option"),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                    ID("Data"): ir.ObjVar(
                        "Z",
                        rty.Message("Universal::Option"),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                },
                rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type",
                            [ID("Universal::MT_Data")],
                        ),
                        ID("Length"): rty.Integer("Universal::Length", ty.Bounds(0, 100)),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r"Last with type universal integer \(0 .. 2\*\*63 - 1\) as value of message"
            r" field not yet supported",
        ),
        (
            rty.Message("A"),
            ir.MsgAgg(
                "Universal::Message",
                {
                    ID("Message_Type"): ir.EnumLit(
                        "Universal::MT_Data",
                        rty.Enumeration("Universal::Message_Type", [ID("Universal::MT_Data")]),
                    ),
                    ID("Length"): ir.IntVal(1),
                    ID("Data"): ir.Head(
                        "Z",
                        rty.Sequence("Universal::Options", rty.Message("Universal::Option")),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                },
                rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration(
                            "Universal::Message_Type",
                            [ID("Universal::MT_Data")],
                        ),
                        ID("Length"): rty.Integer("Universal::Length", ty.Bounds(0, 100)),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r'Head with message type "Universal::Option" in expression not yet supported',
        ),
        (
            rty.Sequence("A", rty.Message("B")),
            ir.Comprehension(
                "E",
                ir.ObjVar(
                    "L",
                    rty.Sequence("A", rty.Message("B")),
                ),
                ir.ComplexExpr(
                    [],
                    ir.ObjCall(
                        "F",
                        [],
                        [],
                        rty.Message("B"),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                ),
                ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Message("B"),
            ir.Find(
                "E",
                ir.ObjFieldAccess(
                    "Y",
                    "Z",
                    rty.Message("C", {("Z",)}, {}, {ID("Z"): rty.Sequence("D", rty.Message("B"))}),
                ),
                ir.ComplexExpr(
                    [],
                    ir.ObjFieldAccess(
                        "E",
                        "Z",
                        rty.Message("B", {("Z",)}, {}, {ID("Z"): rty.Message("B")}),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                ),
                ir.ComplexBoolExpr(
                    [
                        ir.VarDecl("T_0", INT_TY),
                        ir.Assign(
                            "T_0",
                            ir.IntFieldAccess(
                                "E",
                                "Z",
                                rty.Message("B", {("Z",)}, {}, {ID("Z"): INT_TY}),
                            ),
                            INT_TY,
                            origin=ir.ConstructedOrigin("", Location((20, 30))),
                        ),
                    ],
                    ir.Greater(
                        ir.IntVar("T_0", INT_TY),
                        ir.IntVal(0),
                    ),
                ),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Message("B"),
            ir.Find(
                "E",
                ir.ObjVar(
                    "L",
                    rty.Sequence("A", rty.Message("B")),
                ),
                ir.ComplexExpr(
                    [],
                    ir.ObjCall(
                        "F",
                        [],
                        [],
                        rty.Message("B"),
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                ),
                ir.ComplexBoolExpr(
                    [
                        ir.VarDecl("T_0", INT_TY),
                        ir.Assign(
                            "T_0",
                            ir.IntFieldAccess(
                                "E",
                                "Z",
                                rty.Message("B", {("Z",)}, {}, {ID("Z"): INT_TY}),
                            ),
                            INT_TY,
                            origin=ir.ConstructedOrigin("", Location((20, 30))),
                        ),
                    ],
                    ir.Greater(
                        ir.IntVar("T_0", INT_TY),
                        ir.IntVal(0),
                    ),
                ),
            ),
            RecordFluxError,
            "expressions other than variables not yet supported as selector for message types",
        ),
        (
            rty.Sequence("A", INT_TY),
            ir.Comprehension(
                "E",
                ir.ObjVar(
                    "L",
                    rty.Sequence("A", INT_TY),
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
                ir.ComplexExpr([], ir.ObjVar("E", INT_TY)),
                ir.ComplexBoolExpr([], ir.Greater(ir.IntVar("E", INT_TY), ir.IntVal(0))),
            ),
            RecordFluxError,
            r'iterating over sequence of integer type "I" \(1 \.\. 100\) in list comprehension'
            r" not yet supported",
        ),
        (
            INT_TY,
            ir.Find(
                "E",
                ir.ObjVar(
                    "L",
                    rty.Sequence("A", INT_TY),
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
                ir.ComplexExpr([], ir.ObjVar("E", INT_TY)),
                ir.ComplexBoolExpr(
                    [],
                    ir.Greater(ir.IntVar("E", INT_TY), ir.IntVal(0)),
                ),
            ),
            RecordFluxError,
            r'iterating over sequence of integer type "I" \(1 \.\. 100\) in list comprehension'
            r" not yet supported",
        ),
        (
            rty.Sequence("A", INT_TY),
            ir.BoolCall(
                "F",
                [
                    ir.IntCall(
                        "G",
                        [],
                        [],
                        INT_TY,
                        origin=ir.ConstructedOrigin("", Location((10, 20))),
                    ),
                ],
                [INT_TY],
            ),
            RecordFluxError,
            r'IntCall with integer type "I" \(1 \.\. 100\) as function argument not yet supported',
        ),
        (
            rty.Message("A"),
            ir.Conversion(
                rty.Message("A"),
                ir.ObjFieldAccess("Z", "Z", rty.Message("B", {("Z",)}, {}, {ID("Z"): rty.OPAQUE})),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            FatalError,
            r'no refinement for field "Z" of message "B" leads to "A"',
        ),
        (
            rty.Message("A"),
            ir.ObjVar(
                "X",
                rty.Message("A"),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            RecordFluxError,
            r'referencing assignment target "X" of type message in expression not yet supported',
        ),
        (
            rty.Message("A"),
            ir.ObjVar(
                "Y",
                rty.Message("A"),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            RecordFluxError,
            r'ObjVar with message type "A" in assignment not yet supported',
        ),
        (
            rty.Message("A"),
            UnknownExpr(
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            FatalError,
            r'unexpected expression "UnknownExpr" with message type "T" in assignment',
        ),
        (
            rty.Message("A"),
            ir.Head(
                "X",
                rty.Sequence("B", rty.OPAQUE),
                origin=ir.ConstructedOrigin("", Location((10, 20))),
            ),
            FatalError,
            r'unexpected sequence element type sequence type "__INTERNAL__::Opaque" with element'
            r' integer type "__INTERNAL__::Byte" \(0 .. 255\) for "X\'Head" in assignment of "X"',
        ),
    ],
)
def test_session_assign_error(
    type_: rty.Type,
    expression: ir.Expr,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    allocator = AllocatorGenerator(dummy_session(), Integration())
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        allocator,
        debug=Debug.BUILTIN,
    )
    alloc_id = Location((1, 1))

    allocator._allocation_slots[alloc_id] = 1  # noqa: SLF001

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._assign(  # noqa: SLF001
            ID("X", Location((10, 20))),
            type_,
            expression,
            ExceptionHandler(
                ir.State(
                    "S",
                    [],
                    ir.Transition("E", ir.ComplexExpr([], ir.BoolVal(value=True)), None, None),
                    [],
                    None,
                    None,
                ),
                [],
                lambda: None,
            ),
            lambda _: False,
            ID("State"),
            alloc_id=alloc_id,
        )


@pytest.mark.parametrize(
    ("append", "error_type", "error_msg"),
    [
        (
            ir.Append(
                "L",
                ir.ObjVar(
                    "X",
                    rty.Message("A"),
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
                rty.Sequence("B", rty.Message("A")),
            ),
            RecordFluxError,
            r'ObjVar with message type "A" in Append statement not yet supported',
        ),
        (
            ir.Append(
                "L",
                ir.ObjVar("X", MSG_TY, origin=ir.ConstructedOrigin("", Location((10, 20)))),
                rty.Sequence("B", rty.Undefined()),
            ),
            FatalError,
            r"unexpected element type undefined type in Append statement",
        ),
        (
            ir.Append(
                "L",
                ir.IntCall(
                    "X",
                    [],
                    [],
                    INT_TY,
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
                rty.Sequence("B", INT_TY),
            ),
            RecordFluxError,
            r'IntCall with integer type "I" \(1 \.\. 100\) in Append statement not yet supported',
        ),
    ],
)
def test_session_append_error(
    append: ir.Append,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._append(  # noqa: SLF001
            append,
            ExceptionHandler(
                ir.State(
                    "S",
                    [],
                    ir.Transition("E", ir.ComplexExpr([], ir.BoolVal(value=True)), None, None),
                    [],
                    None,
                    None,
                ),
                [],
                lambda: None,
            ),
            lambda _: False,
        )


@pytest.mark.parametrize(
    ("read", "error_type", "error_msg"),
    [
        (
            ir.Read(
                "L",
                ir.EnumLit(
                    "E",
                    rty.Enumeration("A", [ID("E")]),
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r'EnumLit with enumeration type "A" in Read statement not yet supported',
        ),
    ],
)
def test_session_read_error(
    read: ir.Read,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._read(  # pragma: no branch # noqa: SLF001
            read,
            lambda _: False,
        )


@pytest.mark.parametrize(
    ("write", "error_type", "error_msg"),
    [
        (
            ir.Write(
                "L",
                ir.EnumLit(
                    "E",
                    rty.Enumeration("A", [ID("E")]),
                    origin=ir.ConstructedOrigin("", Location((10, 20))),
                ),
            ),
            RecordFluxError,
            r'EnumLit with enumeration type "A" in Write statement not yet supported',
        ),
    ],
)
def test_session_write_error(
    write: ir.Write,
    error_type: type[RecordFluxError],
    error_msg: str,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: error: {error_msg}$"):
        fsm_generator._write(write, [], lambda _: False)  # pragma: no branch # noqa: SLF001


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        (
            ir.First("X", INT_TY),
            ada.First(ada.Variable("P.X")),
        ),
        (
            ir.Last("X", INT_TY),
            ada.Last(ada.Variable("P.X")),
        ),
        (
            ir.IntIfExpr(
                ir.BoolVar("X"),
                ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
                ir.ComplexIntExpr([], ir.IntVal(1)),
                INT_TY,
            ),
            ada.If([(ada.Variable("X"), ada.Variable("Y"))], ada.Number(1)),
        ),
        (
            ir.NamedAgg([("X", ir.IntVal(1)), ("Y", ir.BoolVal(value=False))]),
            ada.NamedAggregate(("X", ada.Number(1)), ("Y", ada.Literal("False"))),
        ),
        (
            ir.And(ir.BoolVar("X"), ir.BoolVar("Y")),
            ada.AndThen(ada.Variable("X"), ada.Variable("Y")),
        ),
        (
            ir.Or(ir.BoolVar("X"), ir.BoolVar("Y")),
            ada.OrElse(ada.Variable("X"), ada.Variable("Y")),
        ),
        (
            ir.Not(ir.BoolVar("X")),
            ada.Not(ada.Variable("X")),
        ),
        (
            ir.Equal(
                ir.ObjVar("X", rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
                ir.EnumLit("P::E1", rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            ),
            ada.Equal(
                ada.Variable("X"),
                ada.NamedAggregate(("Known", ada.Literal("True")), ("Enum", ada.Literal("P::E1"))),
            ),
        ),
        (
            ir.NotEqual(
                ir.EnumLit("P::E1", rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
                ir.ObjVar("X", rty.Enumeration("P::E", [ID("P::E1")], always_valid=True)),
            ),
            ada.NotEqual(
                ada.NamedAggregate(("Known", ada.Literal("True")), ("Enum", ada.Literal("P::E1"))),
                ada.Variable("X"),
            ),
        ),
    ],
)
def test_session_to_ada_expr(expression: ir.Expr, expected: ada.Expr) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    assert fsm_generator._to_ada_expr(expression, lambda _: False) == expected  # noqa: SLF001


@pytest.mark.parametrize(
    ("relation", "left", "right", "expected"),
    [
        (ir.Equal, ir.BoolVar("X"), ir.BoolVal(value=True), ada.Variable("X")),
        (ir.Equal, ir.BoolVar("X"), ir.BoolVal(value=False), ada.Not(ada.Variable("X"))),
        (ir.NotEqual, ir.BoolVar("X"), ir.BoolVal(value=True), ada.Not(ada.Variable("X"))),
        (ir.NotEqual, ir.BoolVar("X"), ir.BoolVal(value=False), ada.Variable("X")),
    ],
)
def test_session_to_ada_expr_equality(
    relation: Callable[[ir.Expr, ir.Expr], ir.Expr],
    left: ir.Expr,
    right: ir.Expr,
    expected: ada.Expr,
) -> None:
    fsm_generator = FSMGenerator(
        dummy_session(),
        Integration(),
        AllocatorGenerator(dummy_session(), Integration()),
        debug=Debug.BUILTIN,
    )

    assert (
        fsm_generator._to_ada_expr(relation(left, right), lambda _: False)  # noqa: SLF001
        == expected
    )
    assert (
        fsm_generator._to_ada_expr(relation(right, left), lambda _: False)  # noqa: SLF001
        == expected
    )
