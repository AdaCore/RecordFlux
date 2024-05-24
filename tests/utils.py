from __future__ import annotations

import os
import pathlib
import re
import shutil
import subprocess
import textwrap
from collections.abc import Iterable, Mapping, Sequence
from typing import Optional, Union

import pytest

from rflx import ada, lang
from rflx.common import STDIN
from rflx.expression import Expr
from rflx.generator import Debug, Generator, const
from rflx.identifier import ID, StrID
from rflx.integration import Integration
from rflx.model import Field, Link, Message, Model, Session, State, TypeDecl, declaration as decl
from rflx.rapidflux import Location, RecordFluxError
from rflx.specification import Parser
from rflx.specification.parser import (
    create_bool_expression,
    create_expression,
    create_math_expression,
    diagnostics_to_error,
)
from tests.const import MAIN, SPEC_DIR


def check_regex(regex: str) -> None:
    if not regex.startswith("^"):
        raise AssertionError("regex must start with anchor (^)")
    if not regex.endswith("$"):
        raise AssertionError("regex must end with anchor ($)")


def assert_equal(left: object, right: object) -> None:
    assert left == right


def assert_message_model_error(
    structure: Sequence[Link],
    types: Mapping[Field, TypeDecl],
    regex: str,
    checksums: Optional[Mapping[ID, Sequence[Expr]]] = None,
    location: Optional[Location] = None,
) -> None:
    location = location or Location((1, 1))
    check_regex(regex)
    with pytest.raises(RecordFluxError, match=regex):
        Message(
            ID("P::M", location),
            structure,
            types,
            checksums=checksums,
            location=location,
        )


def assert_session_model_error(
    states: Sequence[State],
    declarations: Sequence[decl.BasicDeclaration],
    parameters: Sequence[decl.FormalDeclaration],
    types: Sequence[TypeDecl],
    regex: str,
) -> None:
    check_regex(regex)
    with pytest.raises(RecordFluxError, match=regex):
        Session(
            "P::S",
            states,
            declarations,
            parameters,
            types,
            location=Location((1, 1)),
        )


def assert_equal_code_specs(
    spec_files: Iterable[Union[str, pathlib.Path]],
    expected_dir: pathlib.Path,
    tmp_path: pathlib.Path,
    accept_extra_files: bool = False,
) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(pathlib.Path(spec_file))

    assert_equal_code(
        parser.create_model(),
        parser.get_integration(),
        expected_dir,
        tmp_path,
        accept_extra_files,
    )


def assert_equal_code(
    model: Model,
    integration: Integration,
    expected_dir: pathlib.Path,
    tmp_path: pathlib.Path,
    accept_extra_files: bool = False,
) -> None:
    Generator(
        "RFLX",
        reproducible=True,
        ignore_unsupported_checksum=True,
    ).generate(model, integration, tmp_path)

    generated_files = list(tmp_path.glob("*"))
    generated_files.sort(key=lambda x: x.name)

    expected_files = list(expected_dir.glob("*"))
    expected_files.sort(key=lambda x: x.name)

    if accept_extra_files:
        assert {f.name for f in generated_files} <= {
            f.name for f in expected_files
        }, "missing files"
    else:
        assert [f.name for f in generated_files] == [
            f.name for f in expected_files
        ], "unexpected or missing files"
    for generated in generated_files:
        assert (
            generated.read_text() == (expected_dir / generated.name).read_text()
        ), f"mismatch in {generated.name}"


def assert_compilable_code_specs(
    spec_files: Iterable[Union[str, pathlib.Path]],
    tmp_path: pathlib.Path,
    prefix: Optional[str] = None,
) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(pathlib.Path(spec_file))

    assert_compilable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix)


def assert_compilable_code_string(
    specification: str,
    tmp_path: pathlib.Path,
    prefix: Optional[str] = None,
) -> None:
    parser = Parser()
    parser.parse_string(specification)

    assert_compilable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix)


def assert_compilable_code(  # noqa: PLR0913
    model: Model,
    integration: Integration,
    tmp_path: pathlib.Path,
    main: Optional[str] = None,
    prefix: Optional[str] = None,
    debug: Debug = Debug.BUILTIN,
    mode: str = "strict",
) -> None:
    _create_files(tmp_path, model, integration, main, prefix, debug)

    p = subprocess.run(
        ["gprbuild", "-Ptest", f"-Xmode={mode}", f"-Xgnat={os.getenv('GNAT', '')}"],
        cwd=tmp_path,
        check=False,
        stderr=subprocess.PIPE,
    )
    if p.returncode:
        raise AssertionError(
            f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
        )


def assert_executable_code(
    model: Model,
    integration: Integration,
    tmp_path: pathlib.Path,
    main: str = MAIN,
    prefix: Optional[str] = None,
    debug: Debug = Debug.BUILTIN,
) -> str:
    assert_compilable_code(
        model,
        integration,
        tmp_path,
        main,
        prefix,
        debug,
        mode="asserts_enabled",
    )

    p = subprocess.run(
        ["obj/" + main.split(".")[0]],
        cwd=tmp_path,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        timeout=10,
    )
    if p.returncode:
        raise AssertionError(
            f"non-zero exit status {p.returncode}\n{p.stdout.decode('utf-8')}",
        )
    return p.stdout.decode("utf-8")


def assert_provable_code_string(
    specification: str,
    tmp_path: pathlib.Path,
    prefix: Optional[str] = None,
    units: Optional[Sequence[str]] = None,
) -> None:
    parser = Parser()
    parser.parse_string(specification)

    assert_provable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix, units=units)


def assert_provable_code(
    model: Model,
    integration: Integration,
    tmp_path: pathlib.Path,
    main: Optional[str] = None,
    prefix: Optional[str] = None,
    units: Optional[Sequence[str]] = None,
) -> None:
    _create_files(tmp_path, model, integration, main, prefix)

    def run(command: Sequence[str]) -> None:
        p = subprocess.run(
            command,
            cwd=tmp_path,
            check=False,
            stderr=subprocess.PIPE,
        )
        if p.returncode:
            raise AssertionError(
                f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
            )

    gnatprove = [str(pathlib.Path(__file__).parent.parent / "tools/gnatprove"), "-Ptest"]

    if units:
        args = [arg for unit in units for arg in ["-u", unit]]
        run([*gnatprove, *args])
    else:
        run(gnatprove)


def _create_files(
    tmp_path: pathlib.Path,
    model: Model,
    integration: Integration,
    main: Optional[str] = None,
    prefix: Optional[str] = None,
    debug: Debug = Debug.BUILTIN,
) -> None:
    shutil.copy("defaults.gpr", tmp_path)
    shutil.copy("defaults.adc", tmp_path)
    shutil.copy("defaults_backward_compatible.adc", tmp_path)
    main = f'"{main}"' if main else ""
    (tmp_path / "test.gpr").write_text(
        textwrap.dedent(
            f"""\
            with "defaults";

            project Test is
               type Build_Mode is ("strict", "asserts_enabled");
               Mode : Build_Mode := external ("mode", "strict");

               for Source_Dirs use ("src", "generated");
               for Object_Dir use "obj";
               for Create_Missing_Dirs use "True";
               for Main use ({main});

               package Builder is
                  for Default_Switches ("Ada") use Defaults.Builder_Switches;
                  case Mode is
                     when "strict" =>
                        for Global_Configuration_Pragmas use
                           Defaults.Global_Configuration_Pragmas;
                     when others =>
                        null;
                  end case;
               end Builder;

               package Compiler is
                  for Default_Switches ("Ada") use Defaults.Compiler_Switches;
               end Compiler;

               package Prove is
                  for Proof_Switches ("Ada") use
                     Defaults.Proof_Switches & ("--steps=0", "--timeout=150");
               end Prove;
            end Test;""",
        ),
    )

    src_dir = tmp_path / "src"
    src_dir.mkdir(exist_ok=True)

    generated_dir = tmp_path / "generated"
    generated_dir.mkdir()

    Generator(
        prefix if prefix is not None else "RFLX",
        debug=debug,
        ignore_unsupported_checksum=True,
    ).generate(model, integration, generated_dir)


def session_main(
    input_channels: Optional[dict[str, Sequence[tuple[int, ...]]]] = None,
    output_channels: Optional[Sequence[str]] = None,
    context: Optional[Sequence[ada.ContextItem]] = None,
    subprograms: Optional[Sequence[ada.SubprogramBody]] = None,
    session_package: StrID = "RFLX.Test.Session",
) -> Mapping[str, str]:
    input_channels = input_channels or {}
    output_channels = output_channels or []
    context = context or []
    subprograms = subprograms or []
    session_package = ID(session_package)

    run_procedure_spec = ada.ProcedureSpecification("Run")
    run_procedure_decl = ada.SubprogramDeclaration(run_procedure_spec)
    run_procedure_body = ada.SubprogramBody(
        run_procedure_spec,
        [
            ada.ObjectDeclaration(["Ctx"], "Session.Context"),
        ],
        [
            ada.CallStatement(session_package * "Initialize", [ada.Variable("Ctx")]),
            ada.While(
                ada.Call(session_package * "Active", [ada.Variable("Ctx")]),
                [
                    ada.PragmaStatement(
                        "Loop_Invariant",
                        [ada.Call(session_package * "Initialized", [ada.Variable("Ctx")])],
                    ),
                    *(
                        [
                            ada.ForIn(
                                "C",
                                ada.Range(session_package * "Channel"),
                                [
                                    ada.PragmaStatement(
                                        "Loop_Invariant",
                                        [
                                            ada.Call(
                                                session_package * "Initialized",
                                                [ada.Variable("Ctx")],
                                            ),
                                        ],
                                    ),
                                    *(
                                        [
                                            ada.IfStatement(
                                                [
                                                    (
                                                        ada.Call(
                                                            session_package * "Has_Data",
                                                            [
                                                                ada.Variable("Ctx"),
                                                                ada.Variable("C"),
                                                            ],
                                                        ),
                                                        [
                                                            ada.CallStatement(
                                                                "Read",
                                                                [
                                                                    ada.Variable("Ctx"),
                                                                    ada.Variable("C"),
                                                                ],
                                                            ),
                                                        ],
                                                    ),
                                                ],
                                            ),
                                        ]
                                        if output_channels
                                        else []
                                    ),
                                    *(
                                        [
                                            ada.IfStatement(
                                                [
                                                    (
                                                        ada.Call(
                                                            session_package * "Needs_Data",
                                                            [
                                                                ada.Variable("Ctx"),
                                                                ada.Variable("C"),
                                                            ],
                                                        ),
                                                        [
                                                            ada.CallStatement(
                                                                "Write",
                                                                [
                                                                    ada.Variable("Ctx"),
                                                                    ada.Variable("C"),
                                                                ],
                                                            ),
                                                        ],
                                                    ),
                                                ],
                                            ),
                                        ]
                                        if input_channels
                                        else []
                                    ),
                                ],
                            ),
                        ]
                        if input_channels or output_channels
                        else []
                    ),
                    ada.CallStatement(session_package * "Run", [ada.Variable("Ctx")]),
                ],
            ),
            ada.PragmaStatement(
                "Warnings",
                [ada.Variable("Off"), ada.String("statement has no effect")],
            ),
            ada.PragmaStatement(
                "Warnings",
                [
                    ada.Variable("Off"),
                    ada.String('"Ctx" is set by "Finalize" but not used after the call'),
                ],
            ),
            ada.CallStatement(session_package * "Finalize", [ada.Variable("Ctx")]),
            ada.PragmaStatement(
                "Warnings",
                [ada.Variable("On"), ada.String("statement has no effect")],
            ),
            ada.PragmaStatement(
                "Warnings",
                [
                    ada.Variable("On"),
                    ada.String('"Ctx" is set by "Finalize" but not used after the call'),
                ],
            ),
        ],
    )

    print_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Print",
            [
                ada.Parameter(["Prefix"], "String"),
                ada.Parameter(["Chan"], session_package * "Channel"),
                ada.Parameter(["Buffer"], "RFLX" * const.TYPES_BYTES),
            ],
        ),
        [],
        [
            ada.CallStatement(
                "Ada.Text_IO.Put",
                [
                    ada.Concatenation(
                        ada.Variable("Prefix"),
                        ada.String(" "),
                        ada.Case(
                            ada.Variable("Chan"),
                            [
                                (
                                    ada.Variable(session_package * f"C_{channel}"),
                                    ada.String(channel),
                                )
                                for channel in sorted({*input_channels.keys(), *output_channels})
                            ],
                        ),
                        ada.String(":"),
                    ),
                ],
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
        aspects=[
            ada.Precondition(
                ada.AndThen(
                    ada.Equal(ada.First("Prefix"), ada.Number(1)),
                    ada.LessEqual(ada.Length("Prefix"), ada.Number(1000)),
                ),
            ),
        ],
    )

    read_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Read",
            [
                ada.Parameter(["Ctx"], "Session.Context"),
                ada.Parameter(["Chan"], session_package * "Channel"),
            ],
        ),
        [
            ada.UseTypeClause("RFLX" * const.TYPES_INDEX),
            ada.UseTypeClause("RFLX" * const.TYPES_LENGTH),
            ada.ObjectDeclaration(
                ["Buffer"],
                ada.Slice(
                    ada.Variable("RFLX" * const.TYPES_BYTES),
                    ada.First("RFLX" * const.TYPES_INDEX),
                    ada.Add(ada.First("RFLX" * const.TYPES_INDEX), ada.Number(4095)),
                ),
                ada.NamedAggregate(("others", ada.Number(0))),
            ),
            ada.ObjectDeclaration(
                ["Size"],
                "RFLX" * const.TYPES_LENGTH,
                ada.Call(
                    session_package * "Read_Buffer_Size",
                    [
                        ada.Variable("Ctx"),
                        ada.Variable("Chan"),
                    ],
                ),
                constant=True,
            ),
        ],
        [
            ada.IfStatement(
                [
                    (
                        ada.Less(ada.Length("Buffer"), ada.Variable("Size")),
                        [
                            ada.CallStatement(
                                "Ada.Text_IO.Put_Line",
                                [
                                    ada.Concatenation(
                                        ada.String("Read "),
                                        ada.Image("Chan"),
                                        ada.String(": read buffer size too small"),
                                    ),
                                ],
                            ),
                            ada.ReturnStatement(),
                        ],
                    ),
                ],
            ),
            ada.CallStatement(
                session_package * "Read",
                [
                    ada.Variable("Ctx"),
                    ada.Variable("Chan"),
                    ada.Slice(
                        ada.Variable("Buffer"),
                        ada.First("Buffer"),
                        ada.Add(
                            ada.First("Buffer"),
                            -ada.Number(2),
                            ada.Call(
                                "RFLX" * const.TYPES_INDEX,
                                [ada.Add(ada.Variable("Size"), ada.Number(1))],
                            ),
                        ),
                    ),
                ],
            ),
            ada.CallStatement(
                "Print",
                [
                    ada.String("Read"),
                    ada.Variable("Chan"),
                    ada.Slice(
                        ada.Variable("Buffer"),
                        ada.First("Buffer"),
                        ada.Add(
                            ada.First("Buffer"),
                            -ada.Number(2),
                            ada.Call(
                                "RFLX" * const.TYPES_INDEX,
                                [ada.Add(ada.Variable("Size"), ada.Number(1))],
                            ),
                        ),
                    ),
                ],
            ),
        ],
        aspects=[
            ada.Precondition(
                ada.AndThen(
                    ada.Call(session_package * "Initialized", [ada.Variable("Ctx")]),
                    ada.Call(
                        session_package * "Has_Data",
                        [ada.Variable("Ctx"), ada.Variable("Chan")],
                    ),
                ),
            ),
            ada.Postcondition(ada.Call(session_package * "Initialized", [ada.Variable("Ctx")])),
        ],
    )

    write_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Write",
            [
                ada.InOutParameter(["Ctx"], "Session.Context"),
                ada.Parameter(["Chan"], session_package * "Channel"),
            ],
        ),
        [
            ada.UseTypeClause("RFLX" * const.TYPES_LENGTH),
            *([ada.UseTypeClause(session_package * "Channel")] if len(input_channels) > 1 else []),
            ada.ObjectDeclaration(
                ["None"],
                ada.Slice(
                    ada.Variable("RFLX" * const.TYPES_BYTES),
                    ada.Number(1),
                    ada.Number(0),
                ),
                ada.NamedAggregate(("others", ada.Number(0))),
                constant=True,
            ),
            ada.ObjectDeclaration(
                ["Message"],
                "RFLX" * const.TYPES_BYTES,
                ada.If(
                    [
                        (
                            ada.And(
                                *(
                                    [
                                        ada.Equal(
                                            ada.Variable("Chan"),
                                            ada.Variable(session_package * f"C_{channel}"),
                                        ),
                                    ]
                                    if len(input_channels) > 1
                                    else []
                                ),
                                ada.Equal(
                                    ada.Call("Written_Messages", [ada.Variable("Chan")]),
                                    ada.Number(i),
                                ),
                            ),
                            (
                                ada.Aggregate(*[ada.Number(b) for b in message])
                                if len(message) > 1
                                else ada.NamedAggregate(
                                    *[
                                        (
                                            ada.First("RFLX" * const.TYPES_INDEX),
                                            ada.Number(message[0]),
                                        ),
                                    ],
                                )
                            ),
                        )
                        for channel, messages in input_channels.items()
                        for i, message in enumerate(messages)
                    ],
                    ada.Variable("None"),
                ),
                constant=True,
            ),
        ],
        [
            ada.IfStatement(
                [
                    (
                        ada.And(
                            ada.Greater(
                                ada.Length("Message"),
                                ada.Number(0),
                            ),
                            ada.LessEqual(
                                ada.Length("Message"),
                                ada.Call(
                                    session_package * "Write_Buffer_Size",
                                    [ada.Variable("Ctx"), ada.Variable("Chan")],
                                ),
                            ),
                        ),
                        [
                            ada.CallStatement(
                                "Print",
                                [
                                    ada.String("Write"),
                                    ada.Variable("Chan"),
                                    ada.Variable("Message"),
                                ],
                            ),
                            ada.CallStatement(
                                session_package * "Write",
                                [
                                    ada.Variable("Ctx"),
                                    ada.Variable("Chan"),
                                    ada.Variable("Message"),
                                ],
                            ),
                            ada.IfStatement(
                                [
                                    (
                                        ada.Less(
                                            ada.Call("Written_Messages", [ada.Variable("Chan")]),
                                            ada.Last("Natural"),
                                        ),
                                        [
                                            ada.Assignment(
                                                ada.Call(
                                                    "Written_Messages",
                                                    [ada.Variable("Chan")],
                                                ),
                                                ada.Add(
                                                    ada.Call(
                                                        "Written_Messages",
                                                        [ada.Variable("Chan")],
                                                    ),
                                                    ada.Number(1),
                                                ),
                                            ),
                                        ],
                                    ),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        ],
        aspects=[
            ada.Precondition(
                ada.AndThen(
                    ada.Call(session_package * "Initialized", [ada.Variable("Ctx")]),
                    ada.Call(
                        session_package * "Needs_Data",
                        [ada.Variable("Ctx"), ada.Variable("Chan")],
                    ),
                ),
            ),
            ada.Postcondition(ada.Call(session_package * "Initialized", [ada.Variable("Ctx")])),
        ],
    )

    lib_unit = ada.PackageUnit(
        [
            *const.CONFIGURATION_PRAGMAS,
            *context,
        ],
        ada.PackageDeclaration(
            "Lib",
            [
                run_procedure_decl,
            ],
            aspects=[ada.SparkMode()],
        ),
        [
            *const.CONFIGURATION_PRAGMAS,
            *(
                [
                    ada.WithClause("Ada.Text_IO"),
                    ada.WithClause("RFLX" * const.TYPES),
                ]
                if input_channels or output_channels
                else []
            ),
            ada.WithClause(session_package),
            ada.WithClause("Session"),
        ],
        ada.PackageBody(
            "Lib",
            [
                *([print_procedure] if input_channels or output_channels else []),
                *([read_procedure] if output_channels else []),
                *(
                    [
                        ada.ArrayType("Number_Per_Channel", session_package * "Channel", "Natural"),
                        ada.ObjectDeclaration(
                            ["Written_Messages"],
                            "Number_Per_Channel",
                            ada.NamedAggregate(("others", ada.Number(0))),
                        ),
                        write_procedure,
                    ]
                    if input_channels
                    else []
                ),
                run_procedure_body,
                *[
                    ada.SubprogramBody(s.specification, s.declarations, s.statements)
                    for s in subprograms
                ],
            ],
            aspects=[ada.SparkMode()],
        ),
    )

    session_unit = ada.PackageUnit(
        [
            *const.CONFIGURATION_PRAGMAS,
            ada.WithClause(session_package),
        ],
        ada.PackageDeclaration(
            "Session",
            [
                ada.DerivedType("Context", session_package * "Context", []),
            ],
            aspects=[
                ada.SparkMode(),
            ],
        ),
        [],
        ada.PackageBody("Session"),
    )

    return {
        f"{session_unit.name}.ads": session_unit.ads,
        f"{lib_unit.name}.ads": lib_unit.ads,
        f"{lib_unit.name}.adb": lib_unit.adb,
        MAIN: """with Lib;

procedure Main with
   SPARK_Mode
is
begin
   Lib.Run;
end Main;
""",
    }


def parse(
    data: str,
    rule: str,
) -> tuple[lang.RFLXNode, pathlib.Path]:
    unit = lang.AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error, STDIN):
        error.propagate()
    assert isinstance(unit.root, lang.RFLXNode)
    return (unit.root, STDIN)


def parse_math_expression(data: str, extended: bool) -> Expr:
    rule = (
        lang.GrammarRule.extended_expression_rule if extended else lang.GrammarRule.expression_rule
    )
    parser_expression, filename = parse(data, rule)
    assert isinstance(parser_expression, lang.Expr)
    error = RecordFluxError()
    expression = create_math_expression(error, parser_expression, filename)
    error.propagate()
    assert isinstance(expression, Expr)
    return expression


def parse_bool_expression(data: str, extended: bool) -> Expr:
    rule = (
        lang.GrammarRule.extended_expression_rule if extended else lang.GrammarRule.expression_rule
    )
    parser_expression, filename = parse(data, rule)
    assert isinstance(parser_expression, lang.Expr)
    error = RecordFluxError()
    expression = create_bool_expression(error, parser_expression, filename)
    error.propagate()
    assert isinstance(expression, Expr)
    return expression


def parse_expression(data: str, rule: str = lang.GrammarRule.extended_expression_rule) -> Expr:
    parser_expression, filename = parse(data, rule)
    assert isinstance(parser_expression, lang.Expr)
    error = RecordFluxError()
    expression = create_expression(error, parser_expression, filename)
    error.propagate()
    assert isinstance(expression, Expr)
    return expression


def get_test_model(name: str) -> Model:
    parser = Parser()
    parser.parse(SPEC_DIR / f"{name}.rflx")
    return parser.create_model()


def spark_version() -> str:
    p = subprocess.run(["gnatprove", "--version"], stdout=subprocess.PIPE, check=False)
    m = re.fullmatch(
        r"SPARK Pro ([0-9]+)\.[0-9]w? \([0-9]+\)",
        p.stdout.decode("utf-8").split("\n")[0],
    )
    return m.group(1) if m else "unknown"


def assert_stderr_regex(
    expected_regex: str,
    capfd: pytest.CaptureFixture[str],
) -> None:
    check_regex(expected_regex)
    capture = capfd.readouterr()
    assert re.match(expected_regex, capture.err) is not None
