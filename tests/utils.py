import os
import pathlib
import shutil
import subprocess
from distutils.dir_util import copy_tree
from typing import Any, Callable, Iterable, List, Mapping, Sequence, Tuple, Union

import pytest

from rflx import ada
from rflx.error import Location, RecordFluxError
from rflx.expression import Expr
from rflx.generator import Generator, const
from rflx.identifier import ID
from rflx.model import Field, Link, Message, Model, Session, State, Type, declaration as decl
from rflx.specification import Parser
from rflx.specification.parser import (
    STDIN,
    AnalysisContext,
    GrammarRule,
    create_bool_expression,
    create_expression,
    create_math_expression,
    diagnostics_to_error,
)


def assert_equal(left: Any, right: Any) -> None:
    assert left == right


def assert_message_model_error(
    structure: Sequence[Link],
    types: Mapping[Field, Type],
    regex: str,
    aspects: Mapping[ID, Mapping[ID, Sequence[Expr]]] = None,
    location: Location = None,
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Message("P::M", structure, types, aspects=aspects, location=location)


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

    assert_compilable_code(parser.create_model(), tmp_path, prefix)


def assert_compilable_code_string(
    specification: str, tmp_path: pathlib.Path, prefix: str = None
) -> None:
    parser = Parser()
    parser.parse_string(specification)

    assert_compilable_code(parser.create_model(), tmp_path, prefix)


def assert_compilable_code(
    model: Model, tmp_path: pathlib.Path, main: str = None, prefix: str = None
) -> None:
    _create_files(tmp_path, model, main, prefix)

    p = subprocess.run(["gprbuild", "-Ptest"], cwd=tmp_path, check=False, stderr=subprocess.PIPE)
    if p.returncode:
        raise AssertionError(
            f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
        )


def assert_executable_code(
    model: Model, tmp_path: pathlib.Path, main: str, prefix: str = None
) -> str:
    assert_compilable_code(model, tmp_path, main, prefix)

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

    assert_provable_code(parser.create_model(), tmp_path, prefix=prefix, units=units)


def assert_provable_code(
    model: Model,
    tmp_path: pathlib.Path,
    main: str = None,
    prefix: str = None,
    units: Sequence[str] = None,
    cache_proof_results: bool = True,
) -> None:
    proof_dir = (
        pathlib.Path.cwd()
        / pathlib.Path("tests/spark/proof")
        / os.environ.get("PYTEST_CURRENT_TEST", ".").split()[0].replace("/", "-")
    )

    if cache_proof_results and proof_dir.exists():
        copy_tree(str(proof_dir), str(tmp_path / "proof"))

    _create_files(tmp_path, model, main, prefix)

    def run(command: Sequence[str]) -> None:
        p = subprocess.run(command, cwd=tmp_path, check=False, stderr=subprocess.PIPE)
        if p.returncode:
            raise AssertionError(
                f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
            )

    if units:
        for unit in units:
            run(["gnatprove", "-Ptest", "-u", unit])
    else:
        run(["gnatprove", "-Ptest"])

    if cache_proof_results:
        copy_tree(str(tmp_path / "proof"), str(proof_dir))


def _create_files(
    tmp_path: pathlib.Path,
    model: Model,
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
                     for Default_Switches ("Ada") use
                        Defaults.Builder_Switches & Defaults.Compiler_Switches;
                  end Builder;

                  package Prove is
                     for Proof_Dir use "proof";
                     for Proof_Switches ("Ada") use
                        Defaults.Proof_Switches & ("--steps=0", "--timeout=90");
                  end Prove;
               end Test;"""
        )
    )

    generator = Generator(
        model, prefix if prefix else "RFLX", debug=True, ignore_unsupported_checksum=True
    )
    generator.write_units(tmp_path)
    generator.write_library_files(tmp_path)
    generator.write_top_level_package(tmp_path)


def session_main(
    messages: Sequence[Tuple[int, ...]] = None,
    read: bool = True,
    write: bool = True,
    context: Sequence[ada.ContextItem] = None,
    subprograms: Sequence[ada.SubprogramBody] = None,
    session_package: str = "RFLX.P.S",
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
            ada.WithClause(session_package),
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
                ada.GenericPackageInstantiation("Session", session_package, parameters),
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


def multilinestr(string: str) -> str:
    correct_indentation = [not l or l.startswith(15 * " ") for l in string.split("\n")[1:]]
    assert all(
        correct_indentation
    ), f"invalid indentation of line {correct_indentation.index(False) + 2}"
    return string.replace(15 * " ", "")


def parse(data: str, rule: GrammarRule, convert: Callable[..., Any]) -> Any:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    error = RecordFluxError()
    if diagnostics_to_error(unit.diagnostics, error, STDIN):
        error.propagate()
    return convert(unit.root, STDIN)


def parse_math_expression(data: str, extended: bool) -> Expr:
    rule = GrammarRule.extended_expression_rule if extended else GrammarRule.expression_rule
    return parse(data, rule, create_math_expression)


def parse_bool_expression(data: str, extended: bool) -> Expr:
    rule = GrammarRule.extended_expression_rule if extended else GrammarRule.expression_rule
    return parse(data, rule, create_bool_expression)


def parse_expression(data: str, rule: GrammarRule = GrammarRule.extended_expression_rule) -> Expr:
    return parse(data, rule, create_expression)
