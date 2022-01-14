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
) -> None:
    _create_files(tmp_path, model, integration, main, prefix)

    p = subprocess.run(
        ["gprbuild", "-Ptest", f"-Xgnat={os.getenv('GNAT', '')}"],
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
    assert_compilable_code(model, integration, tmp_path, main, prefix)

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
    main = f'"{main}"' if main else ""
    (tmp_path / "test.gpr").write_text(
        multilinestr(
            f"""with "defaults";

               project Test is
                  for Source_Dirs use (".");
                  for Main use ({main});

                  package Builder is
                     for Default_Switches ("Ada") use Defaults.Builder_Switches;
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
    session_package: str = "RFLX.P.S",
    session_parameters: Sequence[ada.StrID] = None,
) -> Mapping[str, str]:
    input_channels = input_channels or {}
    output_channels = output_channels or []
    context = context or []
    subprograms = subprograms or []
    session_parameters = session_parameters or []

    run_procedure_spec = ada.ProcedureSpecification("Run")
    run_procedure_decl = ada.SubprogramDeclaration(
        run_procedure_spec,
        aspects=[
            ada.Precondition(ada.Variable("Session.Uninitialized")),
            ada.Postcondition(ada.Variable("Session.Uninitialized")),
        ],
    )
    run_procedure_body = ada.SubprogramBody(
        run_procedure_spec,
        [],
        [
            ada.CallStatement("Session.Initialize"),
            ada.While(
                ada.Variable("Session.Active"),
                [
                    ada.PragmaStatement("Loop_Invariant", [ada.Variable("Session.Initialized")]),
                    ada.ForIn(
                        "C",
                        ada.Range("Session.Channel"),
                        [
                            ada.PragmaStatement(
                                "Loop_Invariant", [ada.Variable("Session.Initialized")]
                            ),
                            *(
                                [
                                    ada.IfStatement(
                                        [
                                            (
                                                ada.Call("Session.Has_Data", [ada.Variable("C")]),
                                                [
                                                    ada.CallStatement("Read", [ada.Variable("C")]),
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
                                                ada.Call("Session.Needs_Data", [ada.Variable("C")]),
                                                [
                                                    ada.CallStatement("Write", [ada.Variable("C")]),
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
                    ada.CallStatement("Session.Run"),
                ],
            ),
            ada.CallStatement("Session.Finalize"),
        ],
    )

    image_function = ada.ExpressionFunctionDeclaration(
        ada.FunctionSpecification("Image", "String", [ada.Parameter(["Chan"], "Session.Channel")]),
        ada.Case(
            ada.Variable("Chan"),
            [
                (ada.Variable(f"Session.C_{channel}"), ada.String(channel))
                for channel in sorted({*input_channels.keys(), *output_channels})
            ],
        ),
    )

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

    next_message_function = ada.SubprogramBody(
        ada.FunctionSpecification(
            "Next_Message",
            "RFLX" * const.TYPES_BYTES,
            [
                ada.Parameter(["Chan"], "Session.Channel"),
            ],
        ),
        [
            *(
                [ada.Pragma("Unreferenced", [ada.Variable("Chan")])]
                if sum(len(message) for message in input_channels.values()) == 0
                else []
            ),
            *([ada.UseTypeClause("Session.Channel")] if len(input_channels) > 1 else []),
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
                                            ada.Variable(f"Session.C_{channel}"),
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
            ada.ReturnStatement(ada.Variable("Message")),
        ],
    )

    read_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Read",
            [
                ada.Parameter(["Chan"], "Session.Channel"),
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
        ],
        [
            ada.IfStatement(
                [
                    (
                        ada.GreaterEqual(
                            ada.Length("Buffer"),
                            ada.Call("Session.Read_Buffer_Size", [ada.Variable("Chan")]),
                        ),
                        [
                            ada.CallStatement(
                                "Session.Read",
                                [
                                    ada.Variable("Chan"),
                                    ada.Slice(
                                        ada.Variable("Buffer"),
                                        ada.First("Buffer"),
                                        ada.Add(
                                            ada.First("Buffer"),
                                            -ada.Number(2),
                                            ada.Call(
                                                "RFLX" * const.TYPES_INDEX,
                                                [
                                                    ada.Add(
                                                        ada.Call(
                                                            "Session.Read_Buffer_Size",
                                                            [ada.Variable("Chan")],
                                                        ),
                                                        ada.Number(1),
                                                    )
                                                ],
                                            ),
                                        ),
                                    ),
                                ],
                            ),
                            ada.CallStatement(
                                "Print",
                                [
                                    ada.Concatenation(
                                        ada.String("Read "),
                                        ada.Call("Image", [ada.Variable("Chan")]),
                                    ),
                                    ada.Slice(
                                        ada.Variable("Buffer"),
                                        ada.First("Buffer"),
                                        ada.Add(
                                            ada.First("Buffer"),
                                            -ada.Number(2),
                                            ada.Call(
                                                "RFLX" * const.TYPES_INDEX,
                                                [
                                                    ada.Add(
                                                        ada.Call(
                                                            "Session.Read_Buffer_Size",
                                                            [ada.Variable("Chan")],
                                                        ),
                                                        ada.Number(1),
                                                    )
                                                ],
                                            ),
                                        ),
                                    ),
                                ],
                            ),
                        ],
                    )
                ],
                [
                    ada.CallStatement(
                        "Ada.Text_IO.Put_Line",
                        [
                            ada.Concatenation(
                                ada.String("Read "),
                                ada.Image("Chan"),
                                ada.String(": buffer too small"),
                            )
                        ],
                    ),
                ],
            )
        ],
        aspects=[
            ada.Precondition(
                ada.AndThen(
                    ada.Variable("Session.Initialized"),
                    ada.Call("Session.Has_Data", [ada.Variable("Chan")]),
                ),
            ),
            ada.Postcondition(ada.Variable("Session.Initialized")),
        ],
    )

    write_procedure = ada.SubprogramBody(
        ada.ProcedureSpecification(
            "Write",
            [
                ada.Parameter(["Chan"], "Session.Channel"),
            ],
        ),
        [
            ada.UseTypeClause("RFLX" * const.TYPES_LENGTH),
            ada.ObjectDeclaration(
                ["Message"],
                ada.Variable("RFLX" * const.TYPES_BYTES),
                ada.Call("Next_Message", [ada.Variable("Chan")]),
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
                                ada.Call("Session.Write_Buffer_Size", [ada.Variable("Chan")]),
                            ),
                        ),
                        [
                            ada.CallStatement(
                                "Print",
                                [
                                    ada.Concatenation(
                                        ada.String("Write "),
                                        ada.Call("Image", [ada.Variable("Chan")]),
                                    ),
                                    ada.Variable("Message"),
                                ],
                            ),
                            ada.CallStatement(
                                "Session.Write",
                                [
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
                    ada.Variable("Session.Initialized"),
                    ada.Call("Session.Needs_Data", [ada.Variable("Chan")]),
                ),
            ),
            ada.Postcondition(ada.Variable("Session.Initialized")),
        ],
    )

    lib_unit = ada.PackageUnit(
        [
            *const.CONFIGURATION_PRAGMAS,
            ada.WithClause(session_package),
            *context,
        ],
        ada.PackageDeclaration(
            "Lib",
            [
                ada.GenericPackageInstantiation("Session", session_package, session_parameters),
                run_procedure_decl,
            ],
            aspects=[ada.SparkMode(), ada.InitialCondition(ada.Call("Session.Uninitialized"))],
        ),
        [
            *const.CONFIGURATION_PRAGMAS,
            ada.WithClause("Ada.Text_IO"),
            ada.WithClause("RFLX" * const.TYPES),
        ],
        ada.PackageBody(
            "Lib",
            [
                image_function,
                print_procedure,
                *([read_procedure] if output_channels else []),
                *(
                    [
                        ada.ArrayType("Number_Per_Channel", "Session.Channel", "Natural"),
                        ada.ObjectDeclaration(
                            ["Written_Messages"],
                            "Number_Per_Channel",
                            ada.NamedAggregate(("others", ada.Number(0))),
                        ),
                        next_message_function,
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

    return {
        f"{lib_unit.name}.ads": lib_unit.ads,
        f"{lib_unit.name}.adb": lib_unit.adb,
        "main.adb": """with Lib;
pragma Elaborate (Lib);

procedure Main with
   SPARK_Mode,
   Pre => Lib.Session.Uninitialized
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
