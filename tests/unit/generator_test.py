# pylint: disable = too-many-lines

from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Mapping, Optional, Sequence, Tuple, Type

import pkg_resources
import pytest
from _pytest.capture import CaptureFixture
from _pytest.monkeypatch import MonkeyPatch

from rflx import ada, expression as expr, typing_ as rty
from rflx.error import BaseError, FatalError, Location, RecordFluxError
from rflx.generator import Generator, common, const
from rflx.generator.allocator import AllocatorGenerator
from rflx.generator.session import EvaluatedDeclaration, ExceptionHandler, SessionGenerator
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    Field,
    Link,
    Message,
    Model,
    Session,
    State,
    Transition,
    declaration as decl,
    statement as stmt,
    type_ as mty,
)
from tests.const import GENERATED_DIR
from tests.data import models
from tests.utils import (
    assert_compilable_code,
    assert_equal,
    assert_executable_code,
    assert_provable_code,
)


def units(generator: Generator) -> Mapping[str, str]:
    result = {}
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.name.startswith("rflx-p"):
            result[f"{unit.name}.ads"] = unit.ads
            result[f"{unit.name}.adb"] = unit.adb
    return result


def assert_specification(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        assert unit.ads == (GENERATED_DIR / f"{unit.name}.ads").read_text(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.adb:
            with open(f"{GENERATED_DIR}/{unit.name}.adb", "r", encoding="utf-8") as f:
                assert unit.adb == f.read(), unit.name


def generate(model: Model) -> Generator:
    generator = Generator(model, Integration(), "RFLX", reproducible=True)
    return generator


def test_invalid_prefix() -> None:
    with pytest.raises(FatalError, match=r'^id: error: empty part in identifier "A..B"$'):
        Generator(Model(), Integration(), "A..B")


def test_unsupported_checksum() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^generator: error: unsupported checksum"
            r" \(consider --ignore-unsupported-checksum option\)$"
        ),
    ):
        Generator(models.TLV_WITH_CHECKSUM_MODEL, Integration())


def test_ignore_unsupported_checksum(capsys: CaptureFixture[str]) -> None:
    Generator(models.TLV_WITH_CHECKSUM_MODEL, Integration(), ignore_unsupported_checksum=True)
    captured = capsys.readouterr()
    assert "generator: warning: unsupported checksum ignored" in captured.out


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type() -> None:
    class TestType(mty.Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator(Model([TestType("P::T")]), Integration())


def test_library_files(tmp_path: Path) -> None:
    generator = Generator(Model(), Integration(), "RFLX", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in [f"rflx-{f}" for f in const.LIBRARY_FILES]:
        assert (tmp_path / filename).read_text() == (GENERATED_DIR / filename).read_text(), filename


def test_library_files_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), Integration(), "", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in const.LIBRARY_FILES:
        assert (tmp_path / filename).exists()


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_directory(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: "non-existent directory")
    with pytest.raises(AssertionError, match="^template directory not found"):
        generator = Generator(Model(), Integration())
        generator.write_library_files(tmp_path)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_files(monkeypatch: MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: tmp_path)
    with pytest.raises(AssertionError, match="^template file not found"):
        generator = Generator(Model(), Integration())
        generator.write_library_files(tmp_path)


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator(Model(), Integration(), "RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        assert (
            created_file.read_text() == (GENERATED_DIR / created_file.name).read_text()
        ), created_file.name


def test_top_level_package_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), Integration(), "", reproducible=True)
    generator.write_top_level_package(tmp_path)
    assert list(tmp_path.glob("*")) == []


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
            expr.Call("RFLX_Types::U64", [expr.Variable("Value")]),
            expr.Call("RFLX_Types::U64", [expr.Call("To_Base", [expr.TRUE])]),
        ),
        (
            expr.FALSE,
            expr.Variable("Value"),
            expr.Call("RFLX_Types::U64", [expr.Variable("Value")]),
            expr.Call("RFLX_Types::U64", [expr.Call("To_Base", [expr.FALSE])]),
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
    expressions: Tuple[expr.Expr, expr.Expr],
    expected: Tuple[expr.Expr, expr.Expr],
) -> None:
    assert_equal(
        relation(*expressions).substituted(
            common.substitution(models.TLV_MESSAGE, "", public=True)
        ),
        relation(*expected),
    )


def test_prefixed_type_identifier() -> None:
    assert common.prefixed_type_identifier(ada.ID("Modular"), "P") == ada.ID("P.Modular")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_identifier(ada.ID(t), "P") == t.name


def test_base_type_name() -> None:
    assert common.base_type_name(models.MODULAR_INTEGER) == ada.ID("Modular")
    assert common.base_type_name(models.RANGE_INTEGER) == ada.ID("Range_Integer_Base")


def test_full_base_type_name() -> None:
    assert common.full_base_type_name(models.MODULAR_INTEGER) == ada.ID("P.Modular")
    assert common.full_base_type_name(models.RANGE_INTEGER) == ada.ID("P.Range_Integer_Base")


DUMMY_SESSION = Session(
    identifier="P::S",
    initial=ID("State"),
    final=ID("State"),
    states=[State("State")],
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
                    aspects=[
                        ada.ClassPrecondition(
                            ada.And(
                                ada.Call("Initialized", [ada.Variable("Ctx")]),
                            )
                        ),
                        ada.ClassPostcondition(ada.Call("Initialized", [ada.Variable("Ctx")])),
                    ],
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
                    decl.Argument("P3", "T3", type_=rty.Enumeration("T4", always_valid=True)),
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
                    aspects=[
                        ada.ClassPrecondition(
                            ada.And(
                                ada.Call("Initialized", [ada.Variable("Ctx")]),
                            )
                        ),
                        ada.ClassPostcondition(ada.Call("Initialized", [ada.Variable("Ctx")])),
                    ],
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
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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
            decl.TypeDeclaration(
                mty.ModularInteger("T", expr.Number(8), location=Location((10, 20)))
            ),
            RecordFluxError,
            r'type declaration "T" not yet supported',
        ),
        (
            UnknownDeclaration("X", location=Location((10, 20))),
            FatalError,
            r'unexpected formal parameter "X"',
        ),
    ],
)
def test_session_create_abstract_functions_error(
    parameter: decl.FormalDeclaration, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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
                initialization=[ada.Assignment("Ctx.P.X", ada.Conversion("T", ada.Number(1)))],
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
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("X_Buffer")
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
                    ada.Assignment(
                        ada.Variable("Ctx.P.Slots.Slot_Ptr_1"), ada.Variable("X_Buffer")
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
    # pylint: disable=protected-access
    allocator._allocation_slots[Location(start=(1, 1))] = 1
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=True)
    # pylint: disable = protected-access
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
    declaration: decl.BasicDeclaration, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._evaluate_declarations([declaration], is_global=lambda x: False)


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
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;"
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
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;"
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
                    "Ctx.P.Slots.Slot_Ptr_1 := X_Buffer;"
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
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=True)
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
    type_: rty.Type, expression: expr.Expr, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._declare(
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


@pytest.mark.parametrize(
    "action, expected",
    [
        (
            stmt.Assignment(
                "X",
                expr.Binding(
                    expr.MessageAggregate(
                        "Universal::Message",
                        {
                            "Message_Type": expr.Variable(
                                "A", type_=rty.Enumeration("Universal::Message_Type")
                            ),
                            "Length": expr.Variable("B", type_=rty.UniversalInteger()),
                            "Data": expr.Opaque(
                                expr.Variable(
                                    "C",
                                    type_=rty.Message("Universal::Option"),
                                )
                            ),
                        },
                        type_=rty.Message(
                            "Universal::Message",
                            field_types={
                                ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                                ID("Length"): rty.Integer("Universal::Length"),
                                ID("Data"): rty.OPAQUE,
                            },
                        ),
                    ),
                    {
                        "A": expr.Variable(
                            "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
                        ),
                        "B": expr.Number(0),
                        "C": expr.MessageAggregate(
                            "Universal::Option",
                            {
                                "Option_Type": expr.Variable(
                                    "Universal::OT_Null",
                                    type_=rty.Enumeration("Universal::Option_Type"),
                                ),
                            },
                            type_=rty.Message(
                                "Universal::Option",
                                field_types={
                                    ID("Option_Type"): rty.Enumeration("Universal::Option_Type"),
                                },
                            ),
                        ),
                    },
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
declare
   A : Universal.Message_Type;
begin
   A := Universal.MT_Data;
   declare
      B : Universal.Length;
   begin
      B := Universal.Length (0);
      declare
         C_Ctx : Universal.Option.Context;
         C_Buffer : RFLX_Types.Bytes_Ptr;
      begin
         C_Buffer := Ctx.P.Slots.Slot_Ptr_1;
         pragma Warnings (Off, "unused assignment");
         Ctx.P.Slots.Slot_Ptr_1 := null;
         pragma Warnings (On, "unused assignment");
         Universal.Option.Initialize (C_Ctx, C_Buffer);
         if RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_First) + 1 >= 8 then
            Universal.Option.Reset (C_Ctx, RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_First) + 8 - 1);
            Universal.Option.Set_Option_Type (C_Ctx, Universal.OT_Null);
         else
            Ada.Text_IO.Put_Line ("Error: insufficient space in message ""C_Ctx""\");
            RFLX_Exception := True;
         end if;
         if RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + 1 >= RFLX_Types.Bit_Length (B) * 8 + 24 then
            Universal.Message.Reset (X_Ctx, RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + (RFLX_Types.Bit_Length (B) * 8 + 24) - 1);
            Universal.Message.Set_Message_Type (X_Ctx, A);
            Universal.Message.Set_Length (X_Ctx, B);
            if Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (Universal.Option.Size (C_Ctx))) then
               declare
                  function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
                    (Universal.Option.Has_Buffer (C_Ctx)
                     and then Universal.Option.Structural_Valid_Message (C_Ctx)
                     and then Length >= Universal.Option.Byte_Size (C_Ctx));
                  procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
                    Pre =>
                      RFLX_Process_Data_Pre (Data'Length)
                  is
                  begin
                     Universal.Option.Message_Data (C_Ctx, Data);
                  end RFLX_Process_Data;
                  procedure RFLX_Universal_Message_Set_Data is new Universal.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
               begin
                  RFLX_Universal_Message_Set_Data (X_Ctx, Universal.Option.Byte_Size (C_Ctx));
               end;
            else
               Ada.Text_IO.Put_Line ("Error: invalid message field size for ""C'Opaque""\");
               RFLX_Exception := True;
            end if;
         else
            Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""\");
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, ""\"C_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option.Take_Buffer (C_Ctx, C_Buffer);
         pragma Warnings (On, ""\"C_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_1 := C_Buffer;
      end;
   end;
   if RFLX_Exception then
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
end;
if RFLX_Exception then
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ],
            # pylint: enable = line-too-long
        ),
        (
            stmt.Assignment(
                "X",
                expr.Binding(
                    expr.Call(
                        "F",
                        [
                            expr.Variable("A", type_=rty.Enumeration("Universal::Message_Type")),
                            expr.Variable("B", type_=rty.UniversalInteger()),
                            expr.Variable(
                                "C", type_=rty.Aggregate(rty.UniversalInteger(rty.Bounds(0, 255)))
                            ),
                        ],
                        type_=rty.Message("Universal::Message"),
                        argument_types=[
                            rty.Enumeration("Universal::Message_Type"),
                            rty.Integer("Universal::Length"),
                            rty.OPAQUE,
                        ],
                    ),
                    {
                        "A": expr.Variable(
                            "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
                        ),
                        "B": expr.Number(0),
                        "C": expr.Aggregate(),
                    },
                ),
            ),
            """
declare
   A : Universal.Message_Type;
begin
   A := Universal.MT_Data;
   declare
      B : Universal.Length;
   begin
      B := Universal.Length (0);
      declare
         C : RFLX_Types.Bytes (RFLX_Types.Index'Last .. RFLX_Types.Index'First);
      begin
         declare
            X : Universal.Message.Structure;
         begin
            F (Ctx, A, B, C, X);
            Universal.Message.To_Context (X, X_Ctx);
         end;
      end;
   end;
end;
"""[
                1:-1
            ],
        ),
        (
            stmt.Assignment(
                "X",
                expr.Binding(
                    expr.Conversion(
                        "Universal::Option",
                        expr.Selected(
                            expr.Variable(
                                "A",
                                type_=rty.Message(
                                    "Universal::Message",
                                    refinements=[
                                        rty.Refinement(
                                            ID("Data"),
                                            rty.Message("Universal::Option"),
                                            "Universal",
                                        ),
                                    ],
                                ),
                            ),
                            "Data",
                            type_=rty.OPAQUE,
                        ),
                        type_=rty.Message("Universal::Option"),
                    ),
                    {
                        "A": expr.MessageAggregate(
                            "Universal::Message",
                            {
                                "Message_Type": expr.Variable(
                                    "Universal::MT_Data",
                                    type_=rty.Enumeration("Universal::Message_Type"),
                                ),
                                "Length": expr.Number(2),
                                "Data": expr.Aggregate(expr.Number(3), expr.Number(4)),
                            },
                            type_=rty.Message(
                                "Universal::Message",
                                field_types={
                                    ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                                    ID("Length"): rty.Integer("Universal::Length"),
                                    ID("Data"): rty.OPAQUE,
                                },
                            ),
                        ),
                    },
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
declare
   A_Ctx : Universal.Message.Context;
   A_Buffer : RFLX_Types.Bytes_Ptr;
begin
   A_Buffer := Ctx.P.Slots.Slot_Ptr_1;
   pragma Warnings (Off, "unused assignment");
   Ctx.P.Slots.Slot_Ptr_1 := null;
   pragma Warnings (On, "unused assignment");
   Universal.Message.Initialize (A_Ctx, A_Buffer);
   if RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + 1 >= 40 then
      Universal.Message.Reset (A_Ctx, RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + 40 - 1);
      Universal.Message.Set_Message_Type (A_Ctx, Universal.MT_Data);
      Universal.Message.Set_Length (A_Ctx, Universal.Length (2));
      if Universal.Message.Valid_Length (A_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
         Universal.Message.Set_Data (A_Ctx, (RFLX_Types.Byte'Val (3), RFLX_Types.Byte'Val (4)));
      else
         Ada.Text_IO.Put_Line ("Error: invalid message field size for ""[3, 4]""\");
         RFLX_Exception := True;
      end if;
   else
      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""A_Ctx""\");
      RFLX_Exception := True;
   end if;
   if Universal.Contains.Option_In_Message_Data (A_Ctx) then
      Universal.Contains.Copy_Data (A_Ctx, X_Ctx);
      Universal.Option.Verify_Message (X_Ctx);
   else
      Ada.Text_IO.Put_Line ("Error: invalid conversion ""Universal::Option (A.Data)""\");
      RFLX_Exception := True;
   end if;
   pragma Warnings (Off, ""\"A_Ctx"" is set by ""Take_Buffer"" but not used after the call");
   Universal.Message.Take_Buffer (A_Ctx, A_Buffer);
   pragma Warnings (On, ""\"A_Ctx"" is set by ""Take_Buffer"" but not used after the call");
   Ctx.P.Slots.Slot_Ptr_1 := A_Buffer;
end;
if RFLX_Exception then
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ]
            # pylint: enable = line-too-long
        ),
        (
            stmt.Assignment(
                "X",
                expr.Binding(
                    expr.Selected(
                        expr.Variable(
                            "A",
                            type_=rty.Message("Universal::Message"),
                        ),
                        "Message_Type",
                        type_=rty.Enumeration("Universal::Message_Type"),
                    ),
                    {
                        "A": expr.MessageAggregate(
                            "Universal::Message",
                            {
                                "Message_Type": expr.Variable(
                                    "Universal::MT_Null",
                                    type_=rty.Enumeration("Universal::Message_Type"),
                                ),
                            },
                            type_=rty.Message(
                                "Universal::Message",
                                field_types={
                                    ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                                },
                            ),
                        ),
                    },
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
declare
   A_Ctx : Universal.Message.Context;
   A_Buffer : RFLX_Types.Bytes_Ptr;
begin
   A_Buffer := Ctx.P.Slots.Slot_Ptr_1;
   pragma Warnings (Off, "unused assignment");
   Ctx.P.Slots.Slot_Ptr_1 := null;
   pragma Warnings (On, "unused assignment");
   Universal.Message.Initialize (A_Ctx, A_Buffer);
   if RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + 1 >= 8 then
      Universal.Message.Reset (A_Ctx, RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + 8 - 1);
      Universal.Message.Set_Message_Type (A_Ctx, Universal.MT_Null);
   else
      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""A_Ctx""\");
      RFLX_Exception := True;
   end if;
   if Universal.Message.Valid (A_Ctx, Universal.Message.F_Message_Type) then
      X := Universal.Message.Get_Message_Type (A_Ctx);
   else
      Ada.Text_IO.Put_Line ("Error: access to invalid field ""Message_Type"" of ""A_Ctx""\");
      RFLX_Exception := True;
   end if;
   pragma Warnings (Off, ""\"A_Ctx"" is set by ""Take_Buffer"" but not used after the call");
   Universal.Message.Take_Buffer (A_Ctx, A_Buffer);
   pragma Warnings (On, ""\"A_Ctx"" is set by ""Take_Buffer"" but not used after the call");
   Ctx.P.Slots.Slot_Ptr_1 := A_Buffer;
end;
if RFLX_Exception then
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ]
            # pylint: enable = line-too-long
        ),
        # ISSUE: Componolit/RecordFlux#577
        # Copying of sequences is not yet supported.
        # (
        #     stmt.Assignment(
        #         "X",
        #         expr.Binding(
        #             expr.Head(
        #                 expr.Variable(
        #                     "A",
        #                     type_=rty.Sequence(
        #                         "Universal::Values", rty.Integer("Universal::Value")
        #                     ),
        #                 )
        #             ),
        #             {
        #                 "A": expr.Selected(
        #                     expr.Variable(
        #                         "Y",
        #                         type_=rty.Message(
        #                             "Universal::Message",
        #                             field_types={
        #                                 ID("Message_Type"): rty.Enumeration(
        #                                     "Universal::Message_Type"
        #                                 ),
        #                                 ID("Length"): rty.Integer("Universal::Length"),
        #                                 ID("Values"): rty.Sequence(
        #                                     "Universal::Values", rty.Integer("Universal::Value")
        #                                 ),
        #                             },
        #                         ),
        #                     ),
        #                     "Values",
        #                     type_=rty.Sequence(
        #                         "Universal::Values", rty.Integer("Universal::Value")
        #                     ),
        #                 ),
        #             },
        #         ),
        #     ),
        #     "",
        # ),
        # ISSUE: Componolit/RecordFlux#577
        # Copying of sequences is not yet supported.
        # (
        #     stmt.Assignment(
        #         "X",
        #         expr.Binding(
        #             expr.Head(
        #                 expr.Variable(
        #                     "A",
        #                     type_=rty.Sequence(
        #                         "Universal::Values", rty.Integer("Universal::Value")
        #                     ),
        #                 )
        #             ),
        #             {
        #                 "A": expr.Variable(
        #                     "Y",
        #                     type_=rty.Sequence(
        #                         "Universal::Values", rty.Integer("Universal::Value")
        #                     ),
        #                 ),
        #             },
        #         ),
        #     ),
        #     "",
        # ),
        (
            stmt.Assignment(
                "X",
                expr.MessageAggregate(
                    "Universal::Message",
                    {
                        "Message_Type": expr.Variable(
                            "Universal::MT_Data",
                            type_=rty.Enumeration("Universal::Message_Type"),
                        ),
                        "Length": expr.Number(0),
                        "Data": expr.Aggregate(),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                            ID("Length"): rty.Integer("Universal::Length"),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
if RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + 1 >= 24 then
   Universal.Message.Reset (X_Ctx, RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + 24 - 1);
   Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);
   Universal.Message.Set_Length (X_Ctx, Universal.Length (0));
   if Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (0 * RFLX_Types.Byte'Size)) then
      Universal.Message.Set_Data_Empty (X_Ctx);
   else
      Ada.Text_IO.Put_Line ("Error: invalid message field size for ""[]""\");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
else
   Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""\");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ]
            # pylint: enable = line-too-long
        ),
        (
            stmt.Assignment(
                "X",
                expr.MessageAggregate(
                    "Universal::Message",
                    {
                        "Message_Type": expr.Variable(
                            "Universal::MT_Data",
                            type_=rty.Enumeration("Universal::Message_Type"),
                        ),
                        "Length": expr.Div(
                            expr.Size(expr.Variable("Y", type_=rty.Message("Universal::Option"))),
                            expr.Number(8),
                        ),
                        "Data": expr.Opaque(
                            expr.Variable("Y", type_=rty.Message("Universal::Option"))
                        ),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                            ID("Length"): rty.Integer("Universal::Length"),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
if
   Universal.Option.Size (Y_Ctx) <= 32768
   and then Universal.Option.Size (Y_Ctx) mod RFLX_Types.Byte'Size = 0
then
   if RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + 1 >= (Universal.Option.Size (Y_Ctx) / 8) * 8 + 24 then
      Universal.Message.Reset (X_Ctx, RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + ((Universal.Option.Size (Y_Ctx) / 8) * 8 + 24) - 1);
      Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);
      Universal.Message.Set_Length (X_Ctx, Universal.Option.Size (Y_Ctx) / 8);
      if Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (Universal.Option.Size (Y_Ctx))) then
         declare
            function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
              (Universal.Option.Has_Buffer (Y_Ctx)
               and then Universal.Option.Structural_Valid_Message (Y_Ctx)
               and then Length >= Universal.Option.Byte_Size (Y_Ctx));
            procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
              Pre =>
                RFLX_Process_Data_Pre (Data'Length)
            is
            begin
               Universal.Option.Message_Data (Y_Ctx, Data);
            end RFLX_Process_Data;
            procedure RFLX_Universal_Message_Set_Data is new Universal.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
         begin
            RFLX_Universal_Message_Set_Data (X_Ctx, Universal.Option.Byte_Size (Y_Ctx));
         end;
      else
         Ada.Text_IO.Put_Line ("Error: invalid message field size for ""Y'Opaque""\");
         Ctx.P.Next_State := S_E;
         pragma Finalization;
         goto Finalize_S;
      end if;
   else
      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""\");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
else
   Ada.Text_IO.Put_Line ("Error: unexpected size");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ]
            # pylint: enable = line-too-long
        ),
        (
            stmt.Assignment(
                "X",
                expr.MessageAggregate(
                    "Universal::Message",
                    {
                        "Message_Type": expr.Selected(
                            expr.Variable(
                                "Y",
                                type_=rty.Message(
                                    "Universal::Message",
                                    field_types={
                                        ID("Message_Type"): rty.Enumeration(
                                            "Universal::Message_Type"
                                        ),
                                        ID("Length"): rty.Integer("Universal::Length"),
                                        ID("Data"): rty.OPAQUE,
                                    },
                                ),
                            ),
                            "Message_Type",
                            type_=rty.Enumeration("Universal::Message_Type"),
                        ),
                        "Length": expr.Selected(
                            expr.Variable(
                                "Y",
                                type_=rty.Message(
                                    "Universal::Message",
                                    field_types={
                                        ID("Message_Type"): rty.Enumeration(
                                            "Universal::Message_Type"
                                        ),
                                        ID("Length"): rty.Integer("Universal::Length"),
                                        ID("Data"): rty.OPAQUE,
                                    },
                                ),
                            ),
                            "Length",
                        ),
                        "Data": expr.Selected(
                            expr.Variable(
                                "Y",
                                type_=rty.Message(
                                    "Universal::Message",
                                    field_types={
                                        ID("Message_Type"): rty.Enumeration(
                                            "Universal::Message_Type"
                                        ),
                                        ID("Length"): rty.Integer("Universal::Length"),
                                        ID("Data"): rty.OPAQUE,
                                    },
                                ),
                            ),
                            "Data",
                            type_=rty.OPAQUE,
                        ),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                            ID("Length"): rty.Integer("Universal::Length"),
                            ID("Data"): rty.OPAQUE,
                        },
                    ),
                ),
                location=Location(start=(1, 1)),
            ),
            # pylint: disable = line-too-long
            """
if
   Universal.Message.Size (Y_Ctx) <= 32768
   and then Universal.Message.Size (Y_Ctx) mod RFLX_Types.Byte'Size = 0
then
   if RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + 1 >= RFLX_Types.Bit_Length (Universal.Message.Get_Length (Y_Ctx)) * 8 + 24 then
      Universal.Message.Reset (X_Ctx, RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + (RFLX_Types.Bit_Length (Universal.Message.Get_Length (Y_Ctx)) * 8 + 24) - 1);
      if Universal.Message.Valid (Y_Ctx, Universal.Message.F_Message_Type) then
         Universal.Message.Set_Message_Type (X_Ctx, Universal.Message.Get_Message_Type (Y_Ctx));
         if Universal.Message.Valid (Y_Ctx, Universal.Message.F_Length) then
            Universal.Message.Set_Length (X_Ctx, Universal.Length (Universal.Message.Get_Length (Y_Ctx)));
            if Universal.Message.Valid_Next (Y_Ctx, Universal.Message.F_Data) then
               if Universal.Message.Valid_Length (X_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (Universal.Message.Field_Size (Y_Ctx, Universal.Message.F_Data))) then
                  if Universal.Message.Structural_Valid (Y_Ctx, Universal.Message.F_Data) then
                     declare
                        pragma Warnings (Off, "is not modified, could be declared constant");
                        RFLX_Y_Ctx_Tmp : Universal.Message.Context := Y_Ctx;
                        pragma Warnings (On, "is not modified, could be declared constant");
                        function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
                          (Universal.Message.Has_Buffer (RFLX_Y_Ctx_Tmp)
                           and then Universal.Message.Structural_Valid (RFLX_Y_Ctx_Tmp, Universal.Message.F_Data)
                           and then Length >= RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Y_Ctx_Tmp, Universal.Message.F_Data)));
                        procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
                          Pre =>
                            RFLX_Process_Data_Pre (Data'Length)
                        is
                        begin
                           Universal.Message.Get_Data (RFLX_Y_Ctx_Tmp, Data);
                        end RFLX_Process_Data;
                        procedure RFLX_Universal_Message_Set_Data is new Universal.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
                     begin
                        RFLX_Universal_Message_Set_Data (X_Ctx, RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Y_Ctx_Tmp, Universal.Message.F_Data)));
                        Y_Ctx := RFLX_Y_Ctx_Tmp;
                     end;
                  else
                     Ada.Text_IO.Put_Line ("Error: access to invalid message field in ""Y.Data""\");
                     Ctx.P.Next_State := S_E;
                     pragma Finalization;
                     goto Finalize_S;
                  end if;
               else
                  Ada.Text_IO.Put_Line ("Error: invalid message field size for ""Y.Data""\");
                  Ctx.P.Next_State := S_E;
                  pragma Finalization;
                  goto Finalize_S;
               end if;
            else
               Ada.Text_IO.Put_Line ("Error: access to invalid next message field for ""Y.Data""\");
               Ctx.P.Next_State := S_E;
               pragma Finalization;
               goto Finalize_S;
            end if;
         else
            Ada.Text_IO.Put_Line ("Error: access to invalid message field in ""Y.Length""\");
            Ctx.P.Next_State := S_E;
            pragma Finalization;
            goto Finalize_S;
         end if;
      else
         Ada.Text_IO.Put_Line ("Error: access to invalid message field in ""Y.Message_Type""\");
         Ctx.P.Next_State := S_E;
         pragma Finalization;
         goto Finalize_S;
      end if;
   else
      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""\");
      Ctx.P.Next_State := S_E;
      pragma Finalization;
      goto Finalize_S;
   end if;
else
   Ada.Text_IO.Put_Line ("Error: unexpected size");
   Ctx.P.Next_State := S_E;
   pragma Finalization;
   goto Finalize_S;
end if;
"""[
                1:-1
            ]
            # pylint: enable = line-too-long
        ),
        (
            stmt.Assignment(
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
            ),
            """
declare
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx, A, X);
end;
"""[
                1:-1
            ],
        ),
        (
            stmt.Assignment(
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
            ),
            """
declare
   X : Universal.Option.Structure;
   A : Universal.Message.Structure;
begin
   Universal.Message.To_Structure (A_Ctx, A);
   F (Ctx, A, X);
   Universal.Option.To_Context (X, X_Ctx);
end;
"""[
                1:-1
            ],
        ),
        (
            stmt.Reset("X", type_=rty.Message("P::M")),
            "P.M.Reset (X_Ctx);",
        ),
        (
            stmt.Reset("X", type_=rty.Sequence("P::S", rty.Integer("A"))),
            "P.S.Reset (X_Ctx);",
        ),
        (
            stmt.Read("X", expr.Variable("Y", type_=rty.Message("P::M"))),
            "P.M.Verify_Message (Y_Ctx);",
        ),
        (
            stmt.Write("X", expr.Variable("Y", type_=rty.Message("P::M"))),
            "",
        ),
    ],
)
def test_session_state_action(action: stmt.Statement, expected: str) -> None:
    allocator = AllocatorGenerator(DUMMY_SESSION, Integration())
    session_generator = SessionGenerator(DUMMY_SESSION, allocator, debug=True)
    # pylint: disable=protected-access
    allocator._allocation_slots[Location(start=(1, 1))] = 1
    # pylint: disable = protected-access
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
                        "Message_Type": expr.Variable(
                            "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
                        ),
                        "Length": expr.Number(1),
                        "Data": expr.Aggregate(expr.Number(2)),
                    },
                    type_=rty.Message(
                        "Universal::Message",
                        field_types={
                            ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
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
    action: stmt.Statement, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._state_action(
            ID("S"),
            action,
            ExceptionHandler(set(), State("S"), []),
            lambda x: False,
        )


@pytest.mark.parametrize(
    "type_, expression, error_type, error_msg",
    [
        (
            rty.Message("A"),
            expr.Binding(
                expr.MessageAggregate(
                    "P::M",
                    {"F": expr.Variable("Y")},
                    type_=rty.Message("A"),
                ),
                {"Z": expr.Number(1)},
                location=Location((10, 20)),
            ),
            FatalError,
            r'"Z" must be value of message aggregate',
        ),
        (
            rty.Integer("A"),
            expr.Binding(
                expr.Call("F", [expr.Variable("Y")]),
                {"Z": expr.Number(1)},
                location=Location((10, 20)),
            ),
            FatalError,
            r'"Z" must be argument of call',
        ),
        (
            rty.Integer("A"),
            expr.Binding(
                expr.ValueRange(
                    expr.Variable("Y"), expr.Variable("Z"), location=Location((10, 20))
                ),
                {"Z": expr.Number(1)},
            ),
            RecordFluxError,
            r'binding for expression "ValueRange" not yet supported',
        ),
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
                    "Message_Type": expr.Variable(
                        "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
                    ),
                    "Length": expr.Number(1),
                    "Data": expr.Variable(
                        "Z", type_=rty.Message("Universal::Option"), location=Location((10, 20))
                    ),
                },
                type_=rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
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
                    "Message_Type": expr.Variable(
                        "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
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
                        ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
                        ID("Length"): rty.Integer("Universal::Length"),
                        ID("Data"): rty.OPAQUE,
                    },
                ),
            ),
            RecordFluxError,
            r"Last with type universal integer \(undefined\) in message aggregate"
            r" not yet supported",
        ),
        (
            rty.Message("A"),
            expr.MessageAggregate(
                "Universal::Message",
                {
                    "Message_Type": expr.Variable(
                        "Universal::MT_Data", type_=rty.Enumeration("Universal::Message_Type")
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
                    ),
                },
                type_=rty.Message(
                    "Universal::Message",
                    field_types={
                        ID("Message_Type"): rty.Enumeration("Universal::Message_Type"),
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
            ),
            RecordFluxError,
            r'Call with sequence type "B" with element integer type "A" \(1 .. 100\)'
            r" in Head attribute not yet supported",
        ),
        (
            rty.Private("T"),
            expr.Head(
                expr.Call(
                    "F",
                    [expr.Variable("Y")],
                    type_=rty.Sequence("B", rty.Private("T")),
                    location=Location((10, 20)),
                ),
            ),
            FatalError,
            r'unexpected sequence element type \(private type "T"\) for "F \(Y\)\'Head"'
            r' in assignment of "X"',
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
    error_type: Type[BaseError],
    error_msg: str,
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._assign(
            ID("X", location=Location((10, 20))),
            type_,
            expression,
            ExceptionHandler(set(), State("S", exception_transition=Transition("E")), []),
            lambda x: False,
            ID("State"),
            alloc_id=None,
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
    ],
)
def test_session_append_error(
    append: stmt.Append, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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
def test_session_read_error(read: stmt.Read, error_type: Type[BaseError], error_msg: str) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._read(
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
    write: stmt.Write, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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
            expr.Call("P::M::Structural_Valid_Message", [expr.Variable("X_Ctx")]),
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
            expr.Call(
                "P::M::Structural_Valid", [expr.Variable("X_Ctx"), expr.Variable("P::M::F_Y")]
            ),
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
                expr.Variable("X", type_=rty.Enumeration("P::E", always_valid=True)),
                expr.Variable("Y", type_=rty.Enumeration("P::E", always_valid=True)),
            ),
            expr.Equal(expr.Variable("X"), expr.Variable("Y")),
        ),
        (
            expr.NotEqual(
                expr.Variable("X", type_=rty.Enumeration("P::E", always_valid=True)),
                expr.Variable("Y", type_=rty.Enumeration("P::E", always_valid=True)),
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
            expr.Call("P::M::Message_Data", [expr.Variable("X_Ctx")]),
        ),
    ],
)
def test_session_substitution(expression: expr.Expr, expected: expr.Expr) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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
    ],
)
def test_session_substitution_error(
    expression: expr.Expr, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
    )
    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        expression.substituted(session_generator._substitution(lambda x: False))


@pytest.mark.parametrize(
    "relation, left, right, expected",
    [
        (expr.Equal, expr.Variable("X"), expr.Variable("True"), expr.Variable("X")),
        (expr.Equal, expr.Variable("X"), expr.Variable("False"), expr.Not(expr.Variable("X"))),
        (expr.NotEqual, expr.Variable("X"), expr.Variable("True"), expr.Not(expr.Variable("X"))),
        (expr.NotEqual, expr.Variable("X"), expr.Variable("False"), expr.Variable("X")),
        (
            expr.Equal,
            expr.Selected(
                expr.Variable("X", type_=rty.Message("P::M")),
                "Y",
                type_=rty.Enumeration("P::E", always_valid=True),
            ),
            expr.Variable("Z", type_=rty.Enumeration("P::E", always_valid=True)),
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
                type_=rty.Enumeration("P::E", always_valid=True),
            ),
            expr.Variable("Z", type_=rty.Enumeration("P::E", always_valid=True)),
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
        DUMMY_SESSION, AllocatorGenerator(DUMMY_SESSION, Integration()), debug=True
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


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.SEQUENCE_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_specification(model: Model) -> None:
    generator = generate(model)
    assert_specification(generator)


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.SEQUENCE_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_body(model: Model) -> None:
    generator = generate(model)
    assert_body(generator)


TLV_TAGS_MESSAGE = Message(
    "TLV::Tags_Message",
    [
        Link(INITIAL, Field("Length")),
        Link(
            Field("Length"),
            Field("Tags"),
            size=expr.Mul(expr.Variable("Length"), expr.Number(8)),
        ),
        Link(Field("Tags"), FINAL),
    ],
    {
        Field("Length"): models.TLV_LENGTH,
        Field("Tags"): models.TLV_TAGS,
    },
    skip_proof=True,
)


@dataclass
class GeneratorTestCase:
    model: Model
    expected_code: Optional[Mapping[str, str]] = None
    complement: Optional[Mapping[str, str]] = None
    expected_output: Optional[str] = None
    spark_units: Optional[Sequence[str]] = None


TEST_CASES = {
    "empty_model": GeneratorTestCase(
        Model([], []),
        {},
    ),
    "scalar_type": GeneratorTestCase(
        Model(
            [models.RANGE_INTEGER],
            [],
        ),
        expected_code={
            "rflx-p.adb": "",
            "rflx-p.ads": """pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

   type Range_Integer_Base is mod 2**8 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Range_Integer is range 1 .. 100 with
     Size =>
       8;

   function Valid (Val : RFLX.P.Range_Integer_Base) return Boolean is
     (Val >= 1
      and Val <= 100);

   function To_Base (Val : RFLX.P.Range_Integer) return RFLX.P.Range_Integer_Base is
     (RFLX.P.Range_Integer_Base (Val));

   function To_Actual (Val : RFLX.P.Range_Integer_Base) return RFLX.P.Range_Integer is
     (RFLX.P.Range_Integer (Val))
    with
     Pre =>
       Valid (Val);

end RFLX.P;
""",
        },
        spark_units=["rflx-p"],
    ),
}


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_equality(test_case: str) -> None:
    expected_code = TEST_CASES[test_case].expected_code
    if not expected_code:
        pytest.skip()
    generator = generate(TEST_CASES[test_case].model)
    result = units(generator)
    assert set(result) == set(expected_code), "unexpected or missing units"
    for filename, content in expected_code.items():
        assert result[filename] == content, f"mismatch in {filename}"


@pytest.mark.compilation
@pytest.mark.parametrize("test_case", TEST_CASES)
def test_compilability(test_case: str, tmp_path: Path) -> None:
    if TEST_CASES[test_case].complement:
        pytest.skip()
    assert_compilable_code(TEST_CASES[test_case].model, Integration(), tmp_path)


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_executability(test_case: str, tmp_path: Path) -> None:
    complement = TEST_CASES[test_case].complement
    if complement is None:
        pytest.skip()
    main = "main.adb"
    assert main in complement
    for filename, content in complement.items():
        (tmp_path / filename).write_text(content)
    output = assert_executable_code(TEST_CASES[test_case].model, Integration(), tmp_path, main=main)
    assert output == TEST_CASES[test_case].expected_output


@pytest.mark.verification
@pytest.mark.parametrize("test_case", TEST_CASES)
def test_provability(test_case: str, tmp_path: Path) -> None:
    spark_units = TEST_CASES[test_case].spark_units
    if spark_units is None:
        pytest.skip()
    complement = TEST_CASES[test_case].complement
    main = "main.adb" if complement else None
    if main and complement:
        assert main in complement
        for filename, content in complement.items():
            (tmp_path / filename).write_text(content)
    assert_provable_code(
        TEST_CASES[test_case].model,
        Integration(),
        tmp_path,
        main=main,
        units=spark_units,
    )


@pytest.mark.compilation
def test_session_with_only_null_state(tmp_path: Path) -> None:
    state = State("St", transitions=[])
    session = Session(
        "P::S", initial="St", final="St", states=[state], declarations=[], parameters=[], types=[]
    )
    model = Model(types=[], sessions=[session])
    assert_compilable_code(model, Integration(), tmp_path)
