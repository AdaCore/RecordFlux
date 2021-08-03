# pylint: disable = too-many-lines

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, List, Mapping, Optional, Sequence, Tuple, Type

import pkg_resources
import pytest

import rflx.model.type_ as mty
from rflx import ada, declaration as decl, expression as expr, statement as stmt, typing_ as rty
from rflx.error import BaseError, FatalError, Location, RecordFluxError
from rflx.generator import Generator, common, const
from rflx.generator.session import EvaluatedDeclaration, ExceptionHandler, SessionGenerator
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    OPAQUE,
    Field,
    Link,
    Message,
    Model,
    Session,
    State,
    Transition,
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
        with open(f"{GENERATED_DIR}/{unit.name}.ads", "r") as f:
            assert unit.ads == f.read(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.adb:
            with open(f"{GENERATED_DIR}/{unit.name}.adb", "r") as f:
                assert unit.adb == f.read(), unit.name


def generate(model: Model) -> Generator:
    generator = Generator(model, "RFLX", reproducible=True)
    return generator


def test_invalid_prefix() -> None:
    with pytest.raises(RecordFluxError, match=r'^id: error: empty part in identifier "A..B"$'):
        Generator(Model(), "A..B")


def test_unsupported_checksum() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^generator: error: unsupported checksum"
            r" \(consider --ignore-unsupported-checksum option\)$"
        ),
    ):
        Generator(models.TLV_WITH_CHECKSUM_MODEL)


def test_ignore_unsupported_checksum(capsys: Any) -> None:
    Generator(models.TLV_WITH_CHECKSUM_MODEL, ignore_unsupported_checksum=True)
    captured = capsys.readouterr()
    assert "generator: warning: unsupported checksum ignored" in captured.out


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type() -> None:
    class TestType(mty.Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator(Model([TestType("P::T")]))


def test_library_files(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in [f"rflx-{f}" for f in const.LIBRARY_FILES]:
        with open(tmp_path / filename) as library_file:
            with open(GENERATED_DIR / filename) as expected_file:
                assert library_file.read() == expected_file.read(), filename


def test_library_files_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in const.LIBRARY_FILES:
        assert (tmp_path / filename).exists()


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_directory(monkeypatch: Any, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: "non-existent directory")
    with pytest.raises(AssertionError, match="^template directory not found"):
        generator = Generator(Model())
        generator.write_library_files(tmp_path)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_missing_template_files(monkeypatch: Any, tmp_path: Path) -> None:
    monkeypatch.setattr(pkg_resources, "resource_filename", lambda *x: tmp_path)
    with pytest.raises(AssertionError, match="^template file not found"):
        generator = Generator(Model())
        generator.write_library_files(tmp_path)


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        with open(created_file) as library_file:
            with open(GENERATED_DIR / created_file.name) as expected_file:
                assert library_file.read() == expected_file.read(), created_file.name


def test_top_level_package_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
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
                expr.Variable(expr.ID("Buffer") * "all"),
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
            decl.ChannelDeclaration("Channel", readable=True, writable=False),  # §S-P-C-R
            [
                ada.FormalSubprogramDeclaration(
                    specification=ada.FunctionSpecification(
                        identifier="Channel_Has_Data", parameters=[], return_type="Boolean"
                    )
                ),
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="Channel_Read",
                        parameters=[
                            ada.OutParameter(
                                identifiers=["Buffer"],
                                type_identifier=const.TYPES_BYTES,
                            ),
                            ada.OutParameter(
                                identifiers=["Length"],
                                type_identifier=const.TYPES_LENGTH,
                            ),
                        ],
                    )
                ),
            ],
        ),
        (
            decl.ChannelDeclaration("Channel", readable=False, writable=True),  # §S-P-C-W
            [
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="Channel_Write",
                        parameters=[
                            ada.Parameter(
                                identifiers=["Buffer"],
                                type_identifier=const.TYPES_BYTES,
                            )
                        ],
                    )
                ),
            ],
        ),
        (
            decl.ChannelDeclaration("Channel", readable=True, writable=True),  # §S-P-C-RW
            [
                ada.FormalSubprogramDeclaration(
                    specification=ada.FunctionSpecification(
                        identifier="Channel_Has_Data", parameters=[], return_type="Boolean"
                    )
                ),
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="Channel_Read",
                        parameters=[
                            ada.OutParameter(
                                identifiers=["Buffer"],
                                type_identifier=const.TYPES_BYTES,
                            ),
                            ada.OutParameter(
                                identifiers=["Length"],
                                type_identifier=const.TYPES_LENGTH,
                            ),
                        ],
                    )
                ),
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="Channel_Write",
                        parameters=[
                            ada.Parameter(
                                identifiers=["Buffer"],
                                type_identifier=const.TYPES_BYTES,
                            )
                        ],
                    )
                ),
            ],
        ),
        (
            decl.FunctionDeclaration("F", [], "T", type_=rty.BOOLEAN),
            [
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.OutParameter(["F"], "T"),
                        ],
                    )
                ),
            ],
        ),
        (
            decl.FunctionDeclaration(
                "F",
                [
                    decl.Argument("P1", "T1", type_=rty.BOOLEAN),
                    decl.Argument("P2", "T2", type_=rty.OPAQUE),
                    decl.Argument("P3", "T3", type_=rty.Enumeration("T4", always_valid=True)),
                    decl.Argument("P4", "T4", type_=rty.Integer("T2")),
                    decl.Argument("P5", "T5", type_=rty.Message("T5", is_definite=True)),
                ],
                "T",
                type_=rty.Message("T", is_definite=True),
            ),
            [
                ada.FormalSubprogramDeclaration(
                    specification=ada.ProcedureSpecification(
                        identifier="F",
                        parameters=[
                            ada.OutParameter(["F"], "T.Structure"),
                            ada.Parameter(["P1"], "T1"),
                            ada.Parameter(["P2"], const.TYPES_BYTES),
                            ada.Parameter(["P3"], "T3"),
                            ada.Parameter(["P4"], "T4"),
                            ada.Parameter(["P5"], "T5.Structure"),
                        ],
                    )
                ),
            ],
        ),
    ],
)
def test_session_create_formal_parameters(
    parameter: decl.FormalDeclaration, expected: Sequence[ada.FormalDeclaration]
) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    assert session_generator._create_formal_parameters([parameter]) == expected


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
def test_session_create_formal_parameters_error(
    parameter: decl.FormalDeclaration, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._create_formal_parameters([parameter])


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
                global_declarations=[ada.ObjectDeclaration("X", "P.T", ada.Number(1))]
            ),
        ),
        (
            decl.VariableDeclaration("X", "T", type_=rty.Message("T")),
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
                        ada.New(
                            ada.QualifiedExpr(
                                const.TYPES_BYTES,
                                ada.NamedAggregate(
                                    (
                                        ada.ValueRange(
                                            ada.First(const.TYPES_INDEX),
                                            ada.Add(ada.First(const.TYPES_INDEX), ada.Number(4095)),
                                        ),
                                        ada.First(const.TYPES_BYTE),
                                    )
                                ),
                            )
                        ),
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
                            ada.String('unused assignment to "X_Ctx"'),
                        ],
                    ),
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
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String('unused assignment to "X_Ctx"'),
                        ],
                    ),
                    ada.CallStatement(const.TYPES * "Free", [ada.Variable("X_Buffer")]),
                ],
            ),
        ),
        (
            decl.VariableDeclaration("X", "T", type_=rty.Message("T")),
            True,
            EvaluatedDeclaration(
                global_declarations=[
                    ada.ObjectDeclaration(["X_Ctx"], "P.T.Context"),
                ],
                initialization_declarations=[
                    ada.ObjectDeclaration(["X_Buffer"], const.TYPES_BYTES_PTR),
                ],
                initialization=[
                    ada.IfStatement(
                        [
                            (
                                ada.Call("P.T.Has_Buffer", [ada.Variable("X_Ctx")]),
                                [
                                    ada.PragmaStatement(
                                        "Warnings",
                                        [
                                            ada.Variable("Off"),
                                            ada.String('unused assignment to "X_Ctx"'),
                                        ],
                                    ),
                                    ada.PragmaStatement(
                                        "Warnings",
                                        [
                                            ada.Variable("Off"),
                                            ada.String(
                                                '"X_Ctx" is set by "Take_Buffer"'
                                                " but not used after the call"
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
                                                '"X_Ctx" is set by "Take_Buffer"'
                                                " but not used after the call"
                                            ),
                                        ],
                                    ),
                                    ada.PragmaStatement(
                                        "Warnings",
                                        [
                                            ada.Variable("On"),
                                            ada.String('unused assignment to "X_Ctx"'),
                                        ],
                                    ),
                                    ada.CallStatement(
                                        const.TYPES * "Free", [ada.Variable("X_Buffer")]
                                    ),
                                ],
                            )
                        ]
                    ),
                    ada.Assignment(
                        "X_Buffer",
                        ada.New(
                            ada.QualifiedExpr(
                                const.TYPES_BYTES,
                                ada.NamedAggregate(
                                    (
                                        ada.ValueRange(
                                            ada.First(const.TYPES_INDEX),
                                            ada.Add(ada.First(const.TYPES_INDEX), ada.Number(4095)),
                                        ),
                                        ada.First(const.TYPES_BYTE),
                                    )
                                ),
                            )
                        ),
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
                            ada.String('unused assignment to "X_Ctx"'),
                        ],
                    ),
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
                        "Warnings",
                        [
                            ada.Variable("On"),
                            ada.String('unused assignment to "X_Ctx"'),
                        ],
                    ),
                    ada.CallStatement(const.TYPES * "Free", [ada.Variable("X_Buffer")]),
                ],
            ),
        ),
    ],
)
def test_session_evaluate_declarations(
    declaration: decl.BasicDeclaration, session_global: bool, expected: EvaluatedDeclaration
) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    assert session_generator._evaluate_declarations([declaration], session_global) == expected


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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._evaluate_declarations([declaration])


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
                    "X_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .."
                    " RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);\n"
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, "unused assignment to ""X_Ctx""");\n'
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    'pragma Warnings (On, "unused assignment to ""X_Ctx""");\n'
                    "RFLX_Types.Free (X_Buffer);"
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
                    "X_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .."
                    " RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);\n"
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, "unused assignment to ""X_Ctx""");\n'
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    'pragma Warnings (On, "unused assignment to ""X_Ctx""");\n'
                    "RFLX_Types.Free (X_Buffer);"
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
                    "if P.T.Has_Buffer (X_Ctx) then\n"
                    '   pragma Warnings (Off, "unused assignment to ""X_Ctx""");\n'
                    '   pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used'
                    ' after the call");\n'
                    "   P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    '   pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used'
                    ' after the call");\n'
                    '   pragma Warnings (On, "unused assignment to ""X_Ctx""");\n'
                    "   RFLX_Types.Free (X_Buffer);\n"
                    "end if;\n"
                    "X_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .."
                    " RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);\n"
                    "P.T.Initialize (X_Ctx, X_Buffer);"
                ),
                finalization=(
                    'pragma Warnings (Off, "unused assignment to ""X_Ctx""");\n'
                    'pragma Warnings (Off, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    "P.T.Take_Buffer (X_Ctx, X_Buffer);\n"
                    'pragma Warnings (On, """X_Ctx"" is set by ""Take_Buffer"" but not used after'
                    ' the call");\n'
                    'pragma Warnings (On, "unused assignment to ""X_Ctx""");\n'
                    "RFLX_Types.Free (X_Buffer);"
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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    result = session_generator._declare(ID("X"), type_, expression, constant, session_global)
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
            rty.Message("T"),
            expr.BooleanTrue(location=Location((10, 20))),
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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._declare(
            ID("X", location=Location((10, 20))), type_, expression=expression
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
            ),
            "declare\n"
            "   A : Universal.Message_Type;\n"
            "begin\n"
            "   A := Universal.MT_Data;\n"
            "   declare\n"
            "      B : Universal.Length;\n"
            "   begin\n"
            "      B := 0;\n"
            "      declare\n"
            "         C_Ctx : Universal.Option.Context;\n"
            "         C_Buffer : RFLX_Types.Bytes_Ptr;\n"
            "      begin\n"
            "         C_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First"
            " .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);\n"
            "         Universal.Option.Initialize (C_Ctx, C_Buffer);\n"
            "         if C_Ctx.Last - C_Ctx.First + 1 >= RFLX_Types.Bit_Length (8) then\n"
            "            Universal.Option.Reset (C_Ctx,"
            " RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (C_Ctx.Buffer_First)"
            " + RFLX_Types.Bit_Length (8) - 1);\n"
            "            Universal.Option.Set_Option_Type (C_Ctx, Universal.OT_Null);\n"
            "         else\n"
            '            Ada.Text_IO.Put_Line ("Error: insufficient space in message ""C_Ctx""");\n'
            "            RFLX_Exception := True;\n"
            "         end if;\n"
            "         if X_Ctx.Last - X_Ctx.First + 1 >= RFLX_Types.Bit_Length (B * 8 + 24) then\n"
            "            Universal.Message.Reset (X_Ctx,"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First)"
            " + RFLX_Types.Bit_Length (B * 8 + 24) - 1);\n"
            "            Universal.Message.Set_Message_Type (X_Ctx, A);\n"
            "            Universal.Message.Set_Length (X_Ctx, B);\n"
            "            Universal.Message.Set_Data (X_Ctx,"
            " Universal.Option.Message_Data (C_Ctx));\n"
            "         else\n"
            '            Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""");\n'
            "            RFLX_Exception := True;\n"
            "         end if;\n"
            '         pragma Warnings (Off, "unused assignment to ""C_Ctx""");\n'
            '         pragma Warnings (Off, """C_Ctx"" is set by ""Take_Buffer""'
            ' but not used after the call");\n'
            "         Universal.Option.Take_Buffer (C_Ctx, C_Buffer);\n"
            '         pragma Warnings (On, """C_Ctx"" is set by ""Take_Buffer""'
            ' but not used after the call");\n'
            '         pragma Warnings (On, "unused assignment to ""C_Ctx""");\n'
            "         RFLX_Types.Free (C_Buffer);\n"
            "      end;\n"
            "      if RFLX_Exception then\n"
            "         State := S_E;\n"
            "         pragma Finalization;\n"
            "         return;\n"
            "      end if;\n"
            "   end;\n"
            "   if RFLX_Exception then\n"
            "      State := S_E;\n"
            "      pragma Finalization;\n"
            "      return;\n"
            "   end if;\n"
            "end;\n"
            "if RFLX_Exception then\n"
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            "declare\n"
            "   A : Universal.Message_Type;\n"
            "begin\n"
            "   A := Universal.MT_Data;\n"
            "   declare\n"
            "      B : Universal.Length;\n"
            "   begin\n"
            "      B := 0;\n"
            "      declare\n"
            "         C : RFLX_Types.Bytes (RFLX_Types.Index'Last .. RFLX_Types.Index'First);\n"
            "      begin\n"
            "         declare\n"
            "            X : Universal.Message.Structure;\n"
            "         begin\n"
            "            F (X, A, B, C);\n"
            "            Universal.Message.To_Context (X, X_Ctx);\n"
            "         end;\n"
            "      end;\n"
            "      if RFLX_Exception then\n"
            "         State := S_E;\n"
            "         pragma Finalization;\n"
            "         return;\n"
            "      end if;\n"
            "   end;\n"
            "   if RFLX_Exception then\n"
            "      State := S_E;\n"
            "      pragma Finalization;\n"
            "      return;\n"
            "   end if;\n"
            "end;\n"
            "if RFLX_Exception then\n"
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            ),
            "declare\n"
            "   A_Ctx : Universal.Message.Context;\n"
            "   A_Buffer : RFLX_Types.Bytes_Ptr;\n"
            "begin\n"
            "   A_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First"
            " .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);\n"
            "   Universal.Message.Initialize (A_Ctx, A_Buffer);\n"
            "   if A_Ctx.Last - A_Ctx.First + 1 >= RFLX_Types.Bit_Length (40) then\n"
            "      Universal.Message.Reset (A_Ctx,"
            " RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + RFLX_Types.Bit_Length (40)"
            " - 1);\n"
            "      Universal.Message.Set_Message_Type (A_Ctx, Universal.MT_Data);\n"
            "      Universal.Message.Set_Length (A_Ctx, Universal.Length (2));\n"
            "      Universal.Message.Set_Data (A_Ctx, (RFLX_Types.Byte'Val (3),"
            " RFLX_Types.Byte'Val (4)));\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""A_Ctx""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            "   if Universal.Contains.Option_In_Message_Data (A_Ctx) then\n"
            "      Universal.Contains.Copy_Data (A_Ctx, X_Ctx);\n"
            "      Universal.Option.Verify_Message (X_Ctx);\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: invalid conversion'
            ' ""Universal::Option (A.Data)""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            '   pragma Warnings (Off, "unused assignment to ""A_Ctx""");\n'
            '   pragma Warnings (Off, """A_Ctx"" is set by ""Take_Buffer"" but not used after the'
            ' call");\n'
            "   Universal.Message.Take_Buffer (A_Ctx, A_Buffer);\n"
            '   pragma Warnings (On, """A_Ctx"" is set by ""Take_Buffer"" but not used after the'
            ' call");\n'
            '   pragma Warnings (On, "unused assignment to ""A_Ctx""");\n'
            "   RFLX_Types.Free (A_Buffer);\n"
            "end;\n"
            "if RFLX_Exception then\n"
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            ),
            "declare\n"
            "   A_Ctx : Universal.Message.Context;\n"
            "   A_Buffer : RFLX_Types.Bytes_Ptr;\n"
            "begin\n"
            "   A_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First"
            " + 4095 => RFLX_Types.Byte'First);\n"
            "   Universal.Message.Initialize (A_Ctx, A_Buffer);\n"
            "   if A_Ctx.Last - A_Ctx.First + 1 >= RFLX_Types.Bit_Length (8) then\n"
            "      Universal.Message.Reset (A_Ctx,"
            " RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (A_Ctx.Buffer_First) + RFLX_Types.Bit_Length (8)"
            " - 1);\n"
            "      Universal.Message.Set_Message_Type (A_Ctx, Universal.MT_Null);\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""A_Ctx""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            "   if Universal.Message.Valid (A_Ctx, Universal.Message.F_Message_Type) then\n"
            "      X := Universal.Message.Get_Message_Type (A_Ctx);\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: access to invalid field ""Message_Type""'
            ' of ""A_Ctx""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            '   pragma Warnings (Off, "unused assignment to ""A_Ctx""");\n'
            '   pragma Warnings (Off, """A_Ctx"" is set by ""Take_Buffer"" but not used after the'
            ' call");\n'
            "   Universal.Message.Take_Buffer (A_Ctx, A_Buffer);\n"
            '   pragma Warnings (On, """A_Ctx"" is set by ""Take_Buffer"" but not used after the'
            ' call");\n'
            '   pragma Warnings (On, "unused assignment to ""A_Ctx""");\n'
            "   RFLX_Types.Free (A_Buffer);\n"
            "end;\n"
            "if RFLX_Exception then\n"
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            ),
            "if X_Ctx.Last - X_Ctx.First + 1 >= RFLX_Types.Bit_Length (24) then\n"
            "   Universal.Message.Reset (X_Ctx, RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First) + RFLX_Types.Bit_Length (24)"
            " - 1);\n"
            "   Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);\n"
            "   Universal.Message.Set_Length (X_Ctx, Universal.Length (0));\n"
            "   Universal.Message.Set_Data_Empty (X_Ctx);\n"
            "else\n"
            '   Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""");\n'
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            ),
            "if\n"
            "  Universal.Option.Size (Y_Ctx) <= 32768\n"
            "  and then Universal.Option.Size (Y_Ctx) mod RFLX_Types.Byte'Size = 0\n"
            "then\n"
            "   if X_Ctx.Last - X_Ctx.First + 1"
            " >= RFLX_Types.Bit_Length ((Universal.Option.Size (Y_Ctx) / 8) * 8 + 24) then\n"
            "      Universal.Message.Reset (X_Ctx,"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First)"
            " + RFLX_Types.Bit_Length ((Universal.Option.Size (Y_Ctx) / 8) * 8 + 24) - 1);\n"
            "      Universal.Message.Set_Message_Type (X_Ctx, Universal.MT_Data);\n"
            "      Universal.Message.Set_Length (X_Ctx, Universal.Option.Size (Y_Ctx) / 8);\n"
            "      Universal.Message.Set_Data (X_Ctx, Universal.Option.Message_Data (Y_Ctx));\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""");\n'
            "      State := S_E;\n"
            "      pragma Finalization;\n"
            "      return;\n"
            "   end if;\n"
            "else\n"
            '   Ada.Text_IO.Put_Line ("Error: unexpected size of ""Y""");\n'
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            ),
            "if\n"
            "  Universal.Message.Size (Y_Ctx) <= 32768\n"
            "  and then Universal.Message.Size (Y_Ctx) mod RFLX_Types.Byte'Size = 0\n"
            "then\n"
            "   if X_Ctx.Last - X_Ctx.First + 1 >= RFLX_Types.Bit_Length"
            " (Universal.Message.Get_Length (Y_Ctx) * 8 + 24) then\n"
            "      Universal.Message.Reset (X_Ctx,"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (X_Ctx.Buffer_First)"
            " + RFLX_Types.Bit_Length (Universal.Message.Get_Length (Y_Ctx) * 8 + 24) - 1);\n"
            "      Universal.Message.Set_Message_Type (X_Ctx,"
            " Universal.Message.Get_Message_Type (Y_Ctx));\n"
            "      Universal.Message.Set_Length (X_Ctx,"
            " Universal.Length (Universal.Message.Get_Length (Y_Ctx)));\n"
            "      Universal.Message.Set_Data (X_Ctx, Universal.Message.Get_Data (Y_Ctx));\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: insufficient space in message ""X_Ctx""");\n'
            "      State := S_E;\n"
            "      pragma Finalization;\n"
            "      return;\n"
            "   end if;\n"
            "else\n"
            '   Ada.Text_IO.Put_Line ("Error: unexpected size of ""Y""");\n'
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
            "declare\n"
            "   A : Universal.Message.Structure;\n"
            "begin\n"
            "   Universal.Message.To_Structure (A_Ctx, A);\n"
            "   F (X, A);\n"
            "end;",
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
            "declare\n"
            "   X : Universal.Option.Structure;\n"
            "   A : Universal.Message.Structure;\n"
            "begin\n"
            "   Universal.Message.To_Structure (A_Ctx, A);\n"
            "   F (X, A);\n"
            "   Universal.Option.To_Context (X, X_Ctx);\n"
            "end;",
        ),
        (
            stmt.Reset("X", type_=rty.Message("P::M")),
            "P.M.Reset (X_Ctx);",
        ),
        (
            stmt.Read("X", expr.Variable("Y", type_=rty.Message("P::M"))),
            "declare\n"
            "   procedure P_M_Write is new P.M.Write (X_Read);\n"
            "begin\n"
            "   P_M_Write (Y_Ctx);\n"
            "end;\n"
            "P.M.Verify_Message (Y_Ctx);",
        ),
        (
            stmt.Write("X", expr.Variable("Y", type_=rty.Message("P::M"))),
            "if P.M.Structural_Valid_Message (Y_Ctx) then\n"
            "   declare\n"
            "      procedure P_M_Read is new P.M.Read (X_Write);\n"
            "   begin\n"
            "      P_M_Read (Y_Ctx);\n"
            "   end;\n"
            "else\n"
            '   Ada.Text_IO.Put_Line ("Error: invalid message ""Y_Ctx""");\n'
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
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
                ),
            ),
            "declare\n"
            "   RFLX_Message_Ctx : Universal.Message.Context;\n"
            "   RFLX_Message_Buffer : RFLX_Types.Bytes_Ptr;\n"
            "begin\n"
            "   RFLX_Message_Buffer :="
            " new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095"
            " => RFLX_Types.Byte'First);\n"
            "   Universal.Message.Initialize (RFLX_Message_Ctx, RFLX_Message_Buffer);\n"
            "   if RFLX_Message_Ctx.Last - RFLX_Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (32)"
            " then\n"
            "      Universal.Message.Reset (RFLX_Message_Ctx,"
            " RFLX_Types.To_First_Bit_Index (RFLX_Message_Ctx.Buffer_First),"
            " RFLX_Types.To_First_Bit_Index (RFLX_Message_Ctx.Buffer_First)"
            " + RFLX_Types.Bit_Length (32) - 1);\n"
            "      Universal.Message.Set_Message_Type (RFLX_Message_Ctx, Universal.MT_Data);\n"
            "      Universal.Message.Set_Length (RFLX_Message_Ctx, Universal.Length (1));\n"
            "      Universal.Message.Set_Data (RFLX_Message_Ctx,"
            " (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: insufficient space in message'
            ' ""RFLX_Message_Ctx""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            "   if Universal.Message.Structural_Valid_Message (RFLX_Message_Ctx) then\n"
            "      declare\n"
            "         procedure Universal_Message_Read is new Universal.Message.Read (X_Write);\n"
            "      begin\n"
            "         Universal_Message_Read (RFLX_Message_Ctx);\n"
            "      end;\n"
            "   else\n"
            '      Ada.Text_IO.Put_Line ("Error: invalid message ""RFLX_Message_Ctx""");\n'
            "      RFLX_Exception := True;\n"
            "   end if;\n"
            '   pragma Warnings (Off, "unused assignment to ""RFLX_Message_Ctx""");\n'
            '   pragma Warnings (Off, """RFLX_Message_Ctx"" is set by ""Take_Buffer"" but not used'
            ' after the call");\n'
            "   Universal.Message.Take_Buffer (RFLX_Message_Ctx, RFLX_Message_Buffer);\n"
            '   pragma Warnings (On, """RFLX_Message_Ctx"" is set by ""Take_Buffer"" but not used'
            ' after the call");\n'
            '   pragma Warnings (On, "unused assignment to ""RFLX_Message_Ctx""");\n'
            "   RFLX_Types.Free (RFLX_Message_Buffer);\n"
            "end;\n"
            "if RFLX_Exception then\n"
            "   State := S_E;\n"
            "   pragma Finalization;\n"
            "   return;\n"
            "end if;",
        ),
    ],
)
def test_session_state_action(action: stmt.Statement, expected: str) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    assert (
        "\n".join(
            str(s)
            for s in session_generator._state_action(
                action,
                ExceptionHandler(
                    State("S", exception_transition=Transition("E")),
                    [ada.PragmaStatement("Finalization", [])],
                ),
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
            UnknownStatement("X", location=Location((10, 20))),
            FatalError,
            r'unexpected statement "UnknownStatement"',
        ),
    ],
)
def test_session_state_action_error(
    action: stmt.Statement, error_type: Type[BaseError], error_msg: str
) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._state_action(action, ExceptionHandler(State("S"), []))


@pytest.mark.parametrize(
    "type_, expression, error_type, error_msg",
    [
        (
            rty.Message("A"),
            expr.Binding(
                expr.MessageAggregate(
                    "P::M",
                    {"F": expr.Variable("X")},
                    type_=rty.Message("A"),
                ),
                {"Y": expr.Number(1)},
                location=Location((10, 20)),
            ),
            FatalError,
            r'"Y" must be value of message aggregate',
        ),
        (
            rty.Integer("A"),
            expr.Binding(
                expr.Call("F", [expr.Variable("X")]),
                {"Y": expr.Number(1)},
                location=Location((10, 20)),
            ),
            FatalError,
            r'"Y" must be argument of call',
        ),
        (
            rty.Integer("A"),
            expr.Binding(
                expr.ValueRange(
                    expr.Variable("X"), expr.Variable("Y"), location=Location((10, 20))
                ),
                {"Y": expr.Number(1)},
            ),
            RecordFluxError,
            r'binding for expression "ValueRange" not yet supported',
        ),
        (
            rty.Sequence("A", rty.Integer("B")),
            expr.Selected(
                expr.Variable("Y", type_=rty.Message("C")),
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
                    [expr.Variable("X")],
                    type_=rty.Message("C"),
                    location=Location((10, 20)),
                ),
                "Y",
                type_=rty.Sequence("A", rty.Integer("B")),
            ),
            RecordFluxError,
            r'accessing field of expression "Call" not yet supported',
        ),
        (
            rty.Aggregate(rty.Integer("A")),
            expr.Selected(
                expr.Variable("Y", type_=rty.Message("B")),
                "Z",
                type_=rty.Aggregate(rty.AnyInteger()),
                location=Location((10, 20)),
            ),
            FatalError,
            r'unexpected type \(aggregate with element integer type\) for "Y.Z"'
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
                        "Y", type_=rty.Message("Universal::Option"), location=Location((10, 20))
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
                    "Length": expr.Number(1),
                    "Data": expr.Head(
                        expr.Variable(
                            "Y",
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
            r'Head with message type "Universal::Option" in message aggregate not yet supported',
        ),
        (
            rty.Integer("A"),
            expr.Head(
                expr.Call(
                    "F",
                    [expr.Variable("X")],
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
                    [expr.Variable("X")],
                    type_=rty.Sequence("B", rty.Private("T")),
                    location=Location((10, 20)),
                ),
            ),
            FatalError,
            r'unexpected sequence element type \(private type "T"\) for "F \(X\)\'Head"'
            r' in assignment of "X"',
        ),
        (
            rty.Sequence("A", rty.Message("B")),
            expr.Comprehension(
                "E",
                expr.Variable("L"),
                expr.Selected(expr.Variable("E"), "Y", type_=rty.Message("B")),
                expr.Greater(expr.Selected(expr.Variable("E"), "Z"), expr.Number(0)),
                location=Location((10, 20)),
            ),
            RecordFluxError,
            r'creating sequence with element message type "B" using list comprehension'
            r" not yet supported",
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
                    "Y",
                    type_=rty.Sequence("A", rty.Message("C")),
                ),
                expr.Selected(expr.Variable("E"), "Y", type_=rty.Integer("B")),
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
                    expr.Variable("E", type_=rty.Message("C")), "Y", type_=rty.Integer("B")
                ),
                expr.Greater(expr.Selected(expr.Variable("E"), "Y"), expr.Number(0)),
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
                                "Y",
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
                    expr.Variable("Y", type_=rty.Message("B")),
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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._assign(
            ID("X", location=Location((10, 20))),
            type_,
            expression,
            ExceptionHandler(State("S", exception_transition=Transition("E")), []),
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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._append(
            append, ExceptionHandler(State("S", exception_transition=Transition("E")), [])
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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._read(read)


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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)

    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        session_generator._write(
            write, ExceptionHandler(State("S", exception_transition=Transition("E")), [])
        )


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
            expr.HasData(expr.Variable("X", type_=rty.Channel(writable=True, readable=True))),
            expr.Call("X_Has_Data"),
        ),
        (
            expr.Opaque(expr.Variable("X", type_=rty.Message("P::M"))),
            expr.Call("P::M::Message_Data", [expr.Variable("X_Ctx")]),
        ),
    ],
)
def test_session_substitution(expression: expr.Expr, expected: expr.Expr) -> None:
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    assert expression.substituted(session_generator._substitution()) == expected


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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    with pytest.raises(error_type, match=rf"^<stdin>:10:20: generator: error: {error_msg}$"):
        # pylint: disable = protected-access
        expression.substituted(session_generator._substitution())


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
    session_generator = SessionGenerator(DUMMY_SESSION, debug=True)
    # pylint: disable = protected-access
    assert relation(left, right).substituted(session_generator._substitution()) == expected
    assert relation(right, left).substituted(session_generator._substitution()) == expected


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


def session_main(
    messages: Sequence[Tuple[int, ...]] = None,
    read: bool = True,
    write: bool = True,
    context: Sequence[ada.ContextItem] = None,
    subprograms: Sequence[ada.SubprogramBody] = None,
) -> Mapping[str, str]:
    assert (messages and write) or not (messages and write)

    context = context or []
    subprograms = subprograms or []
    io_functions_decl: List[ada.Declaration] = []
    io_functions_body: List[ada.Declaration] = []
    parameters: List[ada.StrID] = []

    if write and messages:
        has_data_func_spec = ada.FunctionSpecification("Has_Data", "Boolean")
        io_functions_decl.append(ada.SubprogramDeclaration(has_data_func_spec))
        io_functions_body.append(
            ada.ExpressionFunctionDeclaration(
                has_data_func_spec,
                ada.Less(ada.Variable("Written_Messages"), ada.Number(len(messages)))
                if write
                else ada.FALSE,
            )
        )
        parameters.append("Lib.Has_Data")

        write_proc_spec = ada.ProcedureSpecification(
            "Write",
            [
                ada.OutParameter(["Buffer"], "RFLX" * const.TYPES_BYTES),
                ada.OutParameter(["Length"], "RFLX" * const.TYPES_LENGTH),
            ],
        )

        io_functions_decl.extend(
            [
                ada.UseTypeClause("RFLX" * const.TYPES_LENGTH),
                ada.SubprogramDeclaration(
                    write_proc_spec,
                    aspects=[
                        ada.Postcondition(
                            ada.LessEqual(ada.Variable("Length"), ada.Length("Buffer")),
                        )
                    ],
                ),
            ]
        )
        io_functions_body.append(
            ada.SubprogramBody(
                write_proc_spec,
                [
                    ada.UseTypeClause("RFLX" * const.TYPES_INDEX),
                    ada.ObjectDeclaration(
                        ["None"],
                        ada.Slice(
                            ada.Variable("RFLX" * const.TYPES_BYTES),
                            ada.Last("RFLX" * const.TYPES_INDEX),
                            ada.First("RFLX" * const.TYPES_INDEX),
                        ),
                        ada.NamedAggregate(("others", ada.First("RFLX" * const.TYPES_BYTE))),
                        constant=True,
                    ),
                    ada.ObjectDeclaration(
                        ["M"],
                        "RFLX" * const.TYPES_BYTES,
                        ada.If(
                            [
                                (
                                    ada.Equal(ada.Variable("Written_Messages"), ada.Number(i)),
                                    ada.Aggregate(*[ada.Number(b) for b in message])
                                    if len(message) > 1
                                    else ada.NamedAggregate(
                                        *[
                                            (
                                                ada.First("RFLX" * const.TYPES_INDEX),
                                                ada.Number(message[0]),
                                            )
                                        ]
                                    ),
                                )
                                for i, message in enumerate(messages)
                            ],
                            ada.Variable("None"),
                        ),
                        constant=True,
                    ),
                ],
                [
                    ada.Assignment("Buffer", ada.NamedAggregate(("others", ada.Number(0)))),
                    ada.IfStatement(
                        [
                            (
                                ada.AndThen(
                                    ada.LessEqual(
                                        ada.First("Buffer"),
                                        ada.Sub(
                                            ada.Last("RFLX" * const.TYPES_INDEX),
                                            ada.Length("M"),
                                        ),
                                    ),
                                    ada.LessEqual(ada.Length("M"), ada.Length("Buffer")),
                                ),
                                [
                                    ada.CallStatement(
                                        "Print", [ada.String("Write"), ada.Variable("M")]
                                    ),
                                    ada.Assignment(
                                        ada.Indexed(
                                            ada.Variable("Buffer"),
                                            ada.ValueRange(
                                                ada.First("Buffer"),
                                                ada.Add(
                                                    ada.First("Buffer"),
                                                    ada.Length("M"),
                                                    ada.Number(-1),
                                                ),
                                            ),
                                        ),
                                        ada.Variable("M"),
                                    ),
                                    ada.Assignment(
                                        ada.Indexed(
                                            ada.Variable("Buffer"),
                                            ada.ValueRange(
                                                ada.Add(ada.First("Buffer"), ada.Length("M")),
                                                ada.Last("Buffer"),
                                            ),
                                        ),
                                        ada.NamedAggregate(("others", ada.Number(0))),
                                    ),
                                    ada.Assignment("Length", ada.Length("M")),
                                    ada.IfStatement(
                                        [
                                            (
                                                ada.Less(
                                                    ada.Variable("Written_Messages"),
                                                    ada.Last("Natural"),
                                                ),
                                                [
                                                    ada.Assignment(
                                                        "Written_Messages",
                                                        ada.Add(
                                                            ada.Variable("Written_Messages"),
                                                            ada.Number(1),
                                                        ),
                                                    )
                                                ],
                                            )
                                        ]
                                    ),
                                ],
                            ),
                        ],
                        [
                            ada.CallStatement(
                                "Ada.Text_IO.Put_Line", [ada.String("Target buffer too small")]
                            ),
                            ada.Assignment("Length", ada.Number(0)),
                        ],
                    ),
                ],
            )
        )
        parameters.append("Lib.Write")

    if read:
        read_proc_spec = ada.ProcedureSpecification(
            "Read", [ada.Parameter(["Buffer"], "RFLX" * const.TYPES_BYTES)]
        )

        io_functions_decl.append(ada.SubprogramDeclaration(read_proc_spec))
        io_functions_body.append(
            ada.SubprogramBody(
                read_proc_spec,
                [],
                [ada.CallStatement("Print", [ada.String("Read"), ada.Variable("Buffer")])],
            )
        )
        parameters.append("Lib.Read")

    print_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Print",
            [
                ada.Parameter(["Prefix"], "String"),
                ada.Parameter(["Buffer"], "RFLX" * const.TYPES_BYTES),
            ],
        ),
        [],
        [
            ada.CallStatement(
                "Ada.Text_IO.Put", [ada.Concatenation(ada.Variable("Prefix"), ada.String(":"))]
            ),
            ada.ForOf(
                "B",
                ada.Variable("Buffer"),
                [
                    ada.CallStatement("Ada.Text_IO.Put", [ada.Variable("B'Image")]),
                ],
            ),
            ada.CallStatement("Ada.Text_IO.New_Line"),
        ],
    )

    parameters.extend(s.specification.identifier for s in subprograms)

    lib_unit = ada.PackageUnit(
        [
            *const.CONFIGURATION_PRAGMAS,
            ada.WithClause("RFLX" * const.TYPES),
            ada.WithClause("RFLX.P.S"),
            *context,
        ],
        ada.PackageDeclaration(
            "Lib",
            [
                *io_functions_decl,
                *[
                    ada.SubprogramDeclaration(s.specification, aspects=s.aspects)
                    for s in subprograms
                ],
                ada.GenericPackageInstantiation("Session", "RFLX.P.S", parameters),
            ],
            aspects=[ada.SparkMode()],
        ),
        [
            *const.CONFIGURATION_PRAGMAS,
            ada.WithClause("Ada.Text_IO"),
        ],
        ada.PackageBody(
            "Lib",
            [
                *(
                    [ada.ObjectDeclaration(["Written_Messages"], "Natural", ada.Number(0))]
                    if write
                    else []
                ),
                print_procedure,
                *io_functions_body,
                *[
                    ada.SubprogramBody(s.specification, s.declarations, s.statements)
                    for s in subprograms
                ],
            ],
            aspects=[ada.SparkMode()],
        ),
    )

    return {
        f"{lib_unit.name}.ads": lib_unit.ads,
        f"{lib_unit.name}.adb": lib_unit.adb,
        "main.adb": """with Lib;

procedure Main with
  SPARK_Mode
is
begin
   Lib.Session.Run;
end Main;
""",
    }


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
    "messages": GeneratorTestCase(
        models.UNIVERSAL_MODEL,
        {},
    ),
    "session_minimal": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="Universal::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
    ),
    "session_simple": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Equal(
                                            expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                            expr.TRUE,  # §S-S-T-L
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Message_Type"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Variable("Universal::MT_Data"),  # §S-S-T-L
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Length"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Number(1),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Assignment(
                                    "Message",
                                    expr.MessageAggregate(
                                        "Universal::Message",
                                        {
                                            "Message_Type": expr.Variable("Universal::MT_Data"),
                                            "Length": expr.Number(1),
                                            "Data": expr.Aggregate(
                                                expr.Number(2)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-A-MA
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(1, 0, 1, 0)]),
        expected_output="State: Start\nWrite: 1 0 1 0\nState: Reply\nRead: 1 0 1 2\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_channels": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.HasData("Channel"),  # §S-S-T-HDAT, §S-E-AT-HD-V
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("Start"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(1, 0, 1, 0)]),
        expected_output="State: Start\nState: Reply\nWrite: 1 0 1 0\nRead: 1 0 1 0\nState: Start\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_variable_initialization": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.Less(
                                        expr.Variable("Local"), expr.Variable("Global")
                                    ),  # §S-S-T-V
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Message", "Universal::Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                                decl.VariableDeclaration(
                                    "Local", "Universal::Value", expr.Number(2)
                                ),  # §S-S-D-V-T-SC, §S-S-D-V-E-L
                            ],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                                stmt.Assignment(
                                    "Local",
                                    expr.Add(
                                        expr.Variable("Local"),
                                        expr.Selected(
                                            expr.Variable("Message"), "Value"
                                        ),  # §S-E-S-V
                                    ),
                                ),  # §S-S-A-A-ME, §S-S-A-A-L, §S-S-A-A-S, §S-S-A-A-V
                                stmt.Assignment(
                                    "Global", expr.Add(expr.Variable("Local"), expr.Number(20))
                                ),  # §S-S-A-A-ME, §S-S-A-A-L, §S-S-A-A-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Message", "Universal::Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Message",
                                    expr.MessageAggregate(
                                        "Universal::Message",
                                        {
                                            "Message_Type": expr.Variable("Universal::MT_Value"),
                                            "Length": expr.Div(
                                                expr.Size("Universal::Value"), expr.Number(8)
                                            ),
                                            "Value": expr.Variable("Global"),
                                        },
                                    ),
                                ),  # §S-S-A-A-MA
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Global", "Universal::Value", expr.Number(11)
                        ),  # §S-D-V-T-SC, §S-D-V-E-L
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(2, 0, 1, 20)]),
        expected_output="State: Start\nWrite: 2 0 1 20\nState: Reply\nRead: 2 0 1 42\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_sequence_append_head": GeneratorTestCase(
        Model(
            [*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Equal(
                                            expr.Variable("Message_Tag"),  # §S-S-T-V
                                            expr.Variable("TLV::Msg_Data"),  # §S-S-T-L
                                        ),
                                        expr.Equal(
                                            expr.Variable("Tag"),  # §S-S-T-V
                                            expr.Variable("TLV::Msg_Error"),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Message", "TLV::Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                                decl.VariableDeclaration(
                                    "Message_Tag", "TLV::Tag"
                                ),  # §S-S-D-V-T-SC, §S-S-D-V-E-N
                                decl.VariableDeclaration(
                                    "Tag", "TLV::Tag"
                                ),  # §S-S-D-V-T-SC, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Append(
                                    "Messages",
                                    expr.MessageAggregate(
                                        "TLV::Message",
                                        {
                                            "Tag": expr.Variable("TLV::Msg_Data"),
                                            "Length": expr.Number(1),
                                            "Value": expr.Aggregate(
                                                expr.Number(2)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Append(
                                    "Tags",
                                    expr.Variable("TLV::Msg_Error"),
                                ),  # §S-S-A-AP-L
                                stmt.Assignment(
                                    "Message",
                                    expr.Head("Messages"),  # §S-E-AT-H-V, §S-E-AT-H-MS
                                ),  # §S-S-A-A-HAT
                                stmt.Assignment(
                                    "Message_Tag",
                                    expr.Selected(expr.Variable("Message"), "Tag"),  # §S-E-S-V
                                ),  # §S-S-A-A-S
                                stmt.Assignment(
                                    "Tag",
                                    expr.Head("Tags"),  # §S-E-AT-H-V, §S-E-AT-H-SS
                                ),  # §S-S-A-A-HAT
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Message", "TLV::Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Message",
                                    expr.Head("Messages"),  # §S-E-AT-H-V, §S-E-AT-H-MS
                                ),  # §S-S-A-A-HAT
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Messages", "TLV::Messages"
                        ),  # §S-D-V-T-MS, §S-D-V-E-N
                        decl.VariableDeclaration("Tags", "TLV::Tags"),  # §S-D-V-T-SS, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=False, writable=True
                        ),  # §S-P-C-W
                    ],
                    types=[*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
                )
            ],
        ),
        complement=session_main(write=False),
        expected_output="State: Start\nState: Reply\nRead: 1 0 1 2\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_comprehension_on_sequence": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Data"),
                                            "Length": expr.Number(1),
                                            "Data": expr.Aggregate(
                                                expr.Number(2)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Null"),
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Data"),
                                            "Length": expr.Number(2),
                                            "Data": expr.Aggregate(
                                                expr.Number(2), expr.Number(3)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Option_Types", "Universal::Option_Types"
                                ),  # §S-S-D-V-T-SS, §S-S-D-V-E-N
                                decl.VariableDeclaration(
                                    "Message", "Universal::Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Option_Types",
                                    expr.Comprehension(
                                        "E",
                                        expr.Variable("Options"),
                                        expr.Selected(
                                            expr.Variable("E"), "Option_Type"
                                        ),  # §S-E-S-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("E"), "Option_Type"
                                            ),  # §S-E-S-V
                                            expr.Variable("Universal::OT_Data"),
                                        ),
                                    ),  # §S-E-LC-V, §S-E-LC-SMS, §S-E-LC-TSS, §S-E-LC-CS, §S-E-LC-A
                                ),  # §S-S-A-A-LC
                                stmt.Assignment(
                                    "Message",
                                    expr.MessageAggregate(
                                        "Universal::Message",
                                        {
                                            "Message_Type": expr.Variable(
                                                "Universal::MT_Option_Types"
                                            ),
                                            "Length": expr.Div(
                                                expr.Size("Option_Types"), expr.Number(8)
                                            ),
                                            "Option_Types": expr.Variable("Option_Types"),
                                        },
                                    ),
                                ),  # §S-S-A-A-MA
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Options", "Universal::Options"
                        ),  # §S-D-V-T-MS, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=False, writable=True
                        ),  # §S-P-C-W
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main(write=False),
        expected_output="State: Start\nState: Reply\nRead: 4 0 2 1 1\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_comprehension_on_message_field": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Message_Type"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Variable("Universal::MT_Options"),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Option_Types", "Universal::Option_Types"
                                ),  # §S-S-D-V-T-SS, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Option_Types",
                                    expr.Comprehension(
                                        "E",
                                        expr.Selected(
                                            expr.Variable("Message"), "Options"
                                        ),  # §S-E-S-V
                                        expr.Selected(
                                            expr.Variable("E"), "Option_Type"
                                        ),  # §S-E-S-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("E"), "Option_Type"
                                            ),  # §S-E-S-V
                                            expr.Variable("Universal::OT_Data"),
                                        ),
                                    ),  # §S-E-LC-S, §S-E-LC-SMS, §S-E-LC-TSS, §S-E-LC-CS, §S-E-LC-A
                                ),  # §S-S-A-A-LC
                                stmt.Assignment(
                                    "Message",
                                    expr.MessageAggregate(
                                        "Universal::Message",
                                        {
                                            "Message_Type": expr.Variable(
                                                "Universal::MT_Option_Types"
                                            ),
                                            "Length": expr.Div(
                                                expr.Size("Option_Types"), expr.Number(8)
                                            ),
                                            "Option_Types": expr.Variable("Option_Types"),
                                        },
                                    ),
                                ),  # §S-S-A-A-MA
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(5, 0, 10, 1, 0, 1, 2, 0, 1, 0, 2, 3, 4)]),
        expected_output=(
            "State: Start\nWrite: 5 0 10 1 0 1 2 0 1 0 2 3 4\nState: Reply\nRead: 4 0 2 1 1\n"
        ),
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_append_unconstrained": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Options", "Universal::Options"
                                ),  # §S-S-D-V-T-MS, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Data"),
                                            "Length": expr.Number(1),
                                            "Data": expr.Aggregate(
                                                expr.Number(1)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Data"),
                                            "Length": expr.Number(2),
                                            "Data": expr.Aggregate(
                                                expr.Number(2), expr.Number(3)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Append(
                                    "Options",
                                    expr.MessageAggregate(
                                        "Universal::Option",
                                        {
                                            "Option_Type": expr.Variable("Universal::OT_Null"),
                                        },
                                    ),
                                ),  # §S-S-A-AP-MA
                                stmt.Assignment(
                                    "Message",
                                    expr.MessageAggregate(
                                        "Universal::Message",
                                        {
                                            "Message_Type": expr.Variable(
                                                "Universal::MT_Unconstrained_Options"
                                            ),
                                            "Options": expr.Variable("Options"),
                                        },
                                    ),
                                ),  # §S-S-A-A-MA
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=False, writable=True
                        ),  # §S-P-C-W
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main(write=False),
        expected_output="State: Start\nRead: 7 1 0 1 1 1 0 2 2 3 0\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_functions": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types, models.FIXED_SIZE_SIMPLE_MESSAGE, OPAQUE],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Message_Type"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Variable("Universal::MT_Data"),  # §S-S-T-L
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Length"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Number(3),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Message_Type", "Universal::Option_Type"
                                ),  # §S-S-D-V-T-SC, §S-S-D-V-E-N
                                decl.VariableDeclaration(
                                    "Fixed_Size_Message", "Fixed_Size::Simple_Message"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Message_Type",
                                    expr.Call("Get_Message_Type", []),  # §S-E-CL-N
                                ),  # §S-S-A-A-CL
                                stmt.Assignment(
                                    "Fixed_Size_Message",
                                    expr.Call(
                                        "Create_Message",
                                        [
                                            expr.Variable("Message_Type"),
                                            expr.Selected(
                                                expr.Variable("Message"), "Data"
                                            ),  # §S-E-S-V
                                        ],
                                    ),  # §S-E-CL-V, §S-E-CL-S
                                ),  # §S-S-A-A-CL
                                stmt.Write(
                                    "Channel", expr.Variable("Fixed_Size_Message")
                                ),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                        decl.FunctionDeclaration(
                            "Get_Message_Type", [], "Universal::Option_Type"
                        ),  # §S-P-F-R-S
                        decl.FunctionDeclaration(
                            "Create_Message",
                            [
                                decl.Argument("Message_Type", "Universal::Option_Type"),
                                decl.Argument("Data", OPAQUE.identifier),
                            ],
                            "Fixed_Size::Simple_Message",
                        ),  # §S-P-F-P-S, §S-P-F-P-O, §S-P-F-R-M
                    ],
                    types=[*models.UNIVERSAL_MODEL.types, models.FIXED_SIZE_SIMPLE_MESSAGE, OPAQUE],
                )
            ],
        ),
        complement=session_main(
            [(1, 0, 3, 0, 1, 2)],
            context=[
                ada.WithClause("RFLX.Universal"),
                ada.WithClause("RFLX.Fixed_Size.Simple_Message"),
            ],
            subprograms=[
                ada.SubprogramBody(
                    ada.ProcedureSpecification(
                        "Get_Message_Type",
                        [ada.OutParameter(["Result"], "RFLX.Universal.Option_Type")],
                    ),
                    [],
                    [
                        ada.Assignment(
                            "Result",
                            ada.NamedAggregate(
                                ("Known", ada.TRUE),
                                ("Enum", ada.Variable("RFLX.Universal.OT_Data")),
                            ),
                        ),
                    ],
                    aspects=[
                        ada.Precondition(
                            ada.Not(ada.Constrained("Result")),
                        )
                    ],
                ),
                ada.SubprogramBody(
                    ada.ProcedureSpecification(
                        "Create_Message",
                        [
                            ada.OutParameter(
                                ["Result"], "RFLX.Fixed_Size.Simple_Message.Structure"
                            ),
                            ada.Parameter(["Message_Type"], "RFLX.Universal.Option_Type"),
                            ada.Parameter(["Data"], "RFLX" * const.TYPES_BYTES),
                        ],
                    ),
                    [],
                    [
                        ada.Assignment("Result.Message_Type", ada.Variable("Message_Type")),
                        ada.IfStatement(
                            [
                                (
                                    ada.Equal(ada.Length("Result.Data"), ada.Length("Data")),
                                    [ada.Assignment("Result.Data", ada.Variable("Data"))],
                                )
                            ],
                            [
                                ada.Assignment(
                                    "Result.Data", ada.NamedAggregate(("others", ada.Number(0)))
                                )
                            ],
                        ),
                    ],
                ),
            ],
        ),
        expected_output="State: Start\nWrite: 1 0 3 0 1 2\nState: Reply\nRead: 1 0 1 2\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_conversion": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Message_Type"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Variable("Universal::MT_Data"),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[
                                decl.VariableDeclaration(
                                    "Inner_Message", "Universal::Option"
                                ),  # §S-S-D-V-T-M, §S-S-D-V-E-N
                            ],
                            actions=[
                                stmt.Assignment(
                                    "Inner_Message",
                                    expr.Conversion(
                                        "Universal::Option",
                                        expr.Selected(expr.Variable("Message"), "Data"),  # §S-E-S-V
                                    ),  # §S-E-CV-V
                                ),  # §S-S-A-A-CV
                                stmt.Write(
                                    "Channel", expr.Variable("Inner_Message")
                                ),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(1, 0, 5, 1, 0, 2, 4, 2)]),
        expected_output="State: Start\nWrite: 1 0 5 1 0 2 4 2\nState: Reply\nRead: 1 0 2 4 2\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_binding": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Message_Type"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Variable("Universal::MT_Data"),  # §S-S-T-L
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Variable("Message"), "Length"
                                            ),  # §S-S-T-S, §S-E-S-V
                                            expr.Number(1),  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Assignment(
                                    "Message",
                                    expr.Binding(
                                        expr.MessageAggregate(
                                            "Universal::Message",
                                            {
                                                "Message_Type": expr.Variable("MT"),
                                                "Length": expr.Variable("Length"),
                                                "Data": expr.Variable("Opaque"),
                                            },
                                        ),
                                        {
                                            "MT": expr.Variable("Universal::MT_Data"),
                                            "Length": expr.Number(1),
                                            "Opaque": expr.Aggregate(
                                                expr.Number(2)
                                            ),  # §S-E-A-T-SC, §S-E-A-E-L
                                        },
                                    ),  # §S-E-B-MA
                                ),  # §S-S-A-A-B
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(1, 0, 1, 0)]),
        expected_output="State: Start\nWrite: 1 0 1 0\nState: Reply\nRead: 1 0 1 2\n",
        spark_units=["main", "lib", "rflx-p"],
    ),
    "session_reuse_of_message": GeneratorTestCase(
        Model(
            [*models.UNIVERSAL_MODEL.types],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("Reply"),
                                    condition=expr.And(
                                        expr.Equal(
                                            expr.Valid("Message"),  # §S-S-T-VAT, §S-E-AT-V-V
                                            expr.TRUE,  # §S-S-T-L
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            declarations=[],
                            actions=[
                                stmt.Read("Channel", expr.Variable("Message")),  # §S-S-A-RD-V
                            ],
                        ),
                        State(
                            "Reply",
                            transitions=[
                                Transition(
                                    target=ID("Start"),
                                    condition=expr.HasData("Channel"),  # §S-S-T-HDAT
                                ),
                                Transition(
                                    target=ID("End"),
                                ),  # §S-S-T-N
                            ],
                            exception_transition=Transition(target=ID("End")),  # §S-S-E
                            declarations=[],
                            actions=[
                                stmt.Write("Channel", expr.Variable("Message")),  # §S-S-A-WR-V
                            ],
                        ),
                        State("End"),  # §S-S-N
                    ],
                    declarations=[
                        decl.VariableDeclaration(
                            "Message", "Universal::Message"
                        ),  # §S-D-V-T-M, §S-D-V-E-N
                    ],
                    parameters=[
                        decl.ChannelDeclaration(
                            "Channel", readable=True, writable=True
                        ),  # §S-P-C-RW
                    ],
                    types=[*models.UNIVERSAL_MODEL.types],
                )
            ],
        ),
        complement=session_main([(0,), (1, 0, 1, 0), (1, 0, 2, 0, 3)]),
        expected_output=(
            "State: Start\nWrite: 0\nState: Reply\nRead: 0\n"
            "State: Start\nWrite: 1 0 1 0\nState: Reply\nRead: 1 0 1 0\n"
            "State: Start\nWrite: 1 0 2 0 3\nState: Reply\nRead: 1 0 2 0 3\n"
        ),
        spark_units=["main", "lib", "rflx-p"],
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


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_compilability(test_case: str, tmp_path: Path) -> None:
    if TEST_CASES[test_case].complement:
        pytest.skip()
    assert_compilable_code(TEST_CASES[test_case].model, tmp_path)


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_executability(test_case: str, tmp_path: Path) -> None:
    complement = TEST_CASES[test_case].complement
    if complement is None:
        pytest.skip()
    main = "main.adb"
    assert main in complement
    for filename, content in complement.items():
        with open(tmp_path / filename, "x") as f:
            f.write(content)
    output = assert_executable_code(TEST_CASES[test_case].model, tmp_path, main=main)
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
            with open(tmp_path / filename, "x") as f:
                f.write(content)
    assert_provable_code(
        TEST_CASES[test_case].model,
        tmp_path,
        main=main,
        units=spark_units,
    )
