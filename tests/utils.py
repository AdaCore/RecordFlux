import pathlib
import shutil
import subprocess
from typing import Any, Mapping, Sequence

import pytest

from rflx import declaration as decl
from rflx.error import Location, RecordFluxError
from rflx.expression import Expr
from rflx.generator import Generator
from rflx.identifier import ID
from rflx.model import Field, Link, Message, Model, Session, State, Type


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
    declarations: Sequence[decl.Declaration],
    parameters: Sequence[decl.Declaration],
    types: Sequence[Type],
    regex: str,
    location: Location = None,
) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Session(
            "P::S",
            ID("Start"),
            ID("End"),
            states,
            declarations,
            parameters,
            types,
            location=location,
        )


def assert_type_model_error(instance: Type, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        instance.error.propagate()


def assert_compilable_code(model: Model, tmp_path: pathlib.Path, prefix: str = None) -> None:
    _create_files(tmp_path, model, prefix)

    p = subprocess.run(["gprbuild", "-Ptest"], cwd=tmp_path, check=False, stderr=subprocess.PIPE)
    if p.returncode:
        raise AssertionError(
            f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
        )


def assert_provable_code(model: Model, tmp_path: pathlib.Path, prefix: str = None) -> None:
    _create_files(tmp_path, model, prefix)

    p = subprocess.run(["gnatprove", "-Ptest"], cwd=tmp_path, check=False, stderr=subprocess.PIPE)
    if p.returncode:
        raise AssertionError(
            f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
        )


def _create_files(tmp_path: pathlib.Path, model: Model, prefix: str = None) -> None:
    shutil.copy("defaults.gpr", tmp_path)
    with open(tmp_path / "test.gpr", "x") as f:
        f.write(
            """
            with "defaults";

            project Test is
                for Source_Dirs use (".");

                package Builder is
                   for Default_Switches ("Ada") use
                      Defaults.Builder_Switches & Defaults.Compiler_Switches;
                end Builder;

                package Prove is
                   for Proof_Switches ("Ada") use
                      Defaults.Proof_Switches & ("--steps=0", "--timeout=90");
                end Prove;
            end Test;
            """
        )

    generator = Generator(prefix if prefix else "RFLX")
    generator.generate(model)
    generator.write_units(tmp_path)
    generator.write_library_files(tmp_path)
    generator.write_top_level_package(tmp_path)


def multilinestr(string: str) -> str:
    correct_indentation = [not l or l.startswith(15 * " ") for l in string.split("\n")[1:]]
    assert all(
        correct_indentation
    ), f"invalid indentation of line {correct_indentation.index(False) + 2}"
    return string.replace(15 * " ", "")
