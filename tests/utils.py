from __future__ import annotations

import os
import pathlib
import shutil
import subprocess
from typing import Iterable, Mapping, Sequence, Tuple, Union

import librflxlang as lang
import pytest

from rflx import ada
from rflx.error import Location, RecordFluxError
from rflx.expression import Expr
from rflx.generator import Generator, const
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import Field, Link, Message, Model, Session, State, Type, declaration as decl
from rflx.specification import Parser
from rflx.specification.parser import (
    STDIN,
    create_bool_expression,
    create_expression,
    create_math_expression,
    diagnostics_to_error,
)


def assert_equal(left: object, right: object) -> None:
    assert left == right


def assert_message_model_error(
    structure: Sequence[Link],
    types: Mapping[Field, Type],
    regex: str,
    checksums: Mapping[ID, Sequence[Expr]] = None,
    location: Location = None,
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Message("P::M", structure, types, checksums=checksums, location=location)


def assert_session_model_error(
    states: Sequence[State],
    declarations: Sequence[decl.BasicDeclaration],
    parameters: Sequence[decl.FormalDeclaration],
    types: Sequence[Type],
    regex: str,
    location: Location = Location((1, 1)),
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Session(
            "P::S",
            ID("Start", location=Location((1, 2))),
            ID("End", location=Location((1, 3))),
            states,
            declarations,
            parameters,
            types,
            location=location,
        )


def assert_type_error(instance: Type, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        instance.error.propagate()


def assert_compilable_code_specs(
    spec_files: Iterable[Union[str, pathlib.Path]], tmp_path: pathlib.Path, prefix: str = None
) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(pathlib.Path(spec_file))

    assert_compilable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix)


def assert_compilable_code_string(
    specification: str, tmp_path: pathlib.Path, prefix: str = None
) -> None:
    parser = Parser()
    parser.parse_string(specification)

    assert_compilable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix)


def assert_compilable_code(
    model: Model,
    integration: Integration,
    tmp_path: pathlib.Path,
    main: str = None,
    prefix: str = None,
    mode: str = "strict",
) -> None:
    _create_files(tmp_path, model, integration, main, prefix)

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
    model: Model, integration: Integration, tmp_path: pathlib.Path, main: str, prefix: str = None
) -> str:
    assert_compilable_code(model, integration, tmp_path, main, prefix, mode="asserts_enabled")

    p = subprocess.run(
        ["./" + main.split(".")[0]],
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
    specification: str, tmp_path: pathlib.Path, prefix: str = None, units: Sequence[str] = None
) -> None:
    parser = Parser()
    parser.parse_string(specification)

    assert_provable_code(parser.create_model(), Integration(), tmp_path, prefix=prefix, units=units)


def assert_provable_code(
    model: Model,
    integration: Integration,
    tmp_path: pathlib.Path,
    main: str = None,
    prefix: str = None,
    units: Sequence[str] = None,
) -> None:
    _create_files(tmp_path, model, integration, main, prefix)

    def run(command: Sequence[str]) -> None:
        p = subprocess.run(command, cwd=tmp_path, check=False, stderr=subprocess.PIPE)
        if p.returncode:
            raise AssertionError(
                f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
            )

    gnatprove = ["gnatprove", "-Ptest", "--memcached-server=localhost:11211"]

    if units:
        for unit in units:
            run([*gnatprove, "-u", unit])
    else:
        run(gnatprove)


def _create_files(
    tmp_path: pathlib.Path,
    model: Model,
    integration: Integration,
    main: str = None,
    prefix: str = None,
) -> None:
    shutil.copy("defaults.gpr", tmp_path)
    shutil.copy("defaults.adc", tmp_path)
    main = f'"{main}"' if main else ""
    (tmp_path / "test.gpr").write_text(
        multilinestr(
            f"""with "defaults";

               project Test is
                  type Build_Mode is ("strict", "asserts_enabled");
                  Mode : Build_Mode := external ("mode", "strict");

                  for Source_Dirs use (".");
                  for Main use ({main});

                  package Builder is
                     for Default_Switches ("Ada") use Defaults.Builder_Switches;
                     case Mode is
                        when "strict" =>
                           for Global_Configuration_Pragmas use "defaults.adc";
                        when others =>
                           null;
                     end case;
                  end Builder;

                  package Compiler is
                     for Default_Switches ("Ada") use Defaults.Compiler_Switches;
                  end Compiler;

                  package Prove is
                     for Proof_Switches ("Ada") use
                        Defaults.Proof_Switches & ("--steps=0", "--timeout=90");
                  end Prove;
               end Test;"""
        )
    )

    generator = Generator(
        model,
        integration,
        prefix if prefix is not None else "RFLX",
        debug=True,
        ignore_unsupported_checksum=True,
    )
    generator.write_units(tmp_path)
    generator.write_library_files(tmp_path)
    generator.write_top_level_package(tmp_path)


def session_main(
    input_channels: dict[str, Sequence[tuple[int, ...]]] = None,
    output_channels: Sequence[str] = None,
    context: Sequence[ada.ContextItem] = None,
    subprograms: Sequence[ada.SubprogramBody] = None,
    session_package: ada.StrID = "RFLX.Test.Session",
) -> Mapping[str, str]:
    input_channels = input_channels or {}
    output_channels = output_channels or []
    context = context or []
    subprograms = subprograms or []
    session_package = ada.ID(session_package)

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
                    ada.ForIn(
                        "C",
                        ada.Range(session_package * "Channel"),
                        [
                            ada.PragmaStatement(
                                "Loop_Invariant",
                                [ada.Call(session_package * "Initialized", [ada.Variable("Ctx")])],
                            ),
                            *(
                                [
                                    ada.IfStatement(
                                        [
                                            (
                                                ada.Call(
                                                    session_package * "Has_Data",
                                                    [ada.Variable("Ctx"), ada.Variable("C")],
                                                ),
                                                [
                                                    ada.CallStatement(
                                                        "Read",
                                                        [ada.Variable("Ctx"), ada.Variable("C")],
                                                    ),
                                                ],
                                            )
                                        ]
                                    )
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
                                                    [ada.Variable("Ctx"), ada.Variable("C")],
                                                ),
                                                [
                                                    ada.CallStatement(
                                                        "Write",
                                                        [ada.Variable("Ctx"), ada.Variable("C")],
                                                    ),
                                                ],
                                            )
                                        ]
                                    )
                                ]
                                if input_channels
                                else []
                            ),
                        ],
                    ),
                    ada.CallStatement(session_package * "Run", [ada.Variable("Ctx")]),
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
                    )
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
                        ada.Equal(ada.Variable("Size"), ada.Number(0)),
                        [
                            ada.CallStatement(
                                "Ada.Text_IO.Put_Line",
                                [
                                    ada.Concatenation(
                                        ada.String("Read "),
                                        ada.Image("Chan"),
                                        ada.String(": read buffer size is 0"),
                                    )
                                ],
                            ),
                            ada.ReturnStatement(),
                        ],
                    ),
                ]
            ),
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
                                    )
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
                        session_package * "Has_Data", [ada.Variable("Ctx"), ada.Variable("Chan")]
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
                                        )
                                    ]
                                    if len(input_channels) > 1
                                    else []
                                ),
                                ada.Equal(
                                    ada.Call("Written_Messages", [ada.Variable("Chan")]),
                                    ada.Number(i),
                                ),
                            ),
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
                                                    "Written_Messages", [ada.Variable("Chan")]
                                                ),
                                                ada.Add(
                                                    ada.Call(
                                                        "Written_Messages", [ada.Variable("Chan")]
                                                    ),
                                                    ada.Number(1),
                                                ),
                                            )
                                        ],
                                    )
                                ]
                            ),
                        ],
                    )
                ],
            )
        ],
        aspects=[
            ada.Precondition(
                ada.AndThen(
                    ada.Call(session_package * "Initialized", [ada.Variable("Ctx")]),
                    ada.Call(
                        session_package * "Needs_Data", [ada.Variable("Ctx"), ada.Variable("Chan")]
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
            ada.WithClause("Ada.Text_IO"),
            ada.WithClause("RFLX" * const.TYPES),
            ada.WithClause(session_package),
            ada.WithClause("Session"),
        ],
        ada.PackageBody(
            "Lib",
            [
                print_procedure,
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
        "main.adb": """with Lib;

procedure Main with
   SPARK_Mode
is
begin
   Lib.Run;
end Main;
""",
    }


def multilinestr(string: str) -> str:
    correct_indentation = [not l or l.startswith(15 * " ") for l in string.split("\n")[1:]]
    assert all(
        correct_indentation
    ), f"invalid indentation of line {correct_indentation.index(False) + 2}"
    return string.replace(15 * " ", "")


def parse(
    data: str,
    rule: str,
) -> Tuple[lang.RFLXNode, pathlib.Path]:
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
    expression = create_math_expression(parser_expression, filename)
    assert isinstance(expression, Expr)
    return expression


def parse_bool_expression(data: str, extended: bool) -> Expr:
    rule = (
        lang.GrammarRule.extended_expression_rule if extended else lang.GrammarRule.expression_rule
    )
    parser_expression, filename = parse(data, rule)
    assert isinstance(parser_expression, lang.Expr)
    expression = create_bool_expression(parser_expression, filename)
    assert isinstance(expression, Expr)
    return expression


def parse_expression(data: str, rule: str = lang.GrammarRule.extended_expression_rule) -> Expr:
    parser_expression, filename = parse(data, rule)
    assert isinstance(parser_expression, lang.Expr)
    expression = create_expression(parser_expression, filename)
    assert isinstance(expression, Expr)
    return expression
