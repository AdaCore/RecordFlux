from __future__ import annotations

import typing as ty
from collections.abc import Sequence
from dataclasses import dataclass, field as dataclass_field
from pathlib import Path
from shutil import copytree
from typing import Optional

import pytest
from pydantic import BaseModel, validator
from ruamel.yaml.main import YAML

from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.utils import (
    assert_compilable_code,
    assert_equal_code,
    assert_executable_code,
    assert_provable_code,
    session_main,
)

MAIN = "main.adb"
FEATURES = [f for f in Path(__file__).parent.glob("*") if f.is_dir() and f.name != "__pycache__"]
INCOMPLETE_FEATURE_TESTS = [f for f in FEATURES if not (f / "test.rflx").is_file()]

assert not INCOMPLETE_FEATURE_TESTS


class ConfigFile(BaseModel):
    input: Optional[ty.Mapping[str, Optional[ty.Sequence[str]]]]  # noqa: PEA001
    output: Optional[ty.Sequence[str]]  # noqa: PEA001
    sequence: Optional[str]
    prove: Optional[ty.Sequence[str]]  # noqa: PEA001

    @validator("input")  # pylint: disable-next = no-self-argument
    def initialize_input_if_present(
        cls, value: Optional[ty.Mapping[str, ty.Sequence[str]]]  # noqa: PEA001
    ) -> ty.Mapping[str, ty.Sequence[str]]:  # noqa: PEA001
        return value if value is not None else {}

    @validator("output")  # pylint: disable-next = no-self-argument
    def initialize_output_if_present(
        cls, value: Optional[ty.Sequence[str]]  # noqa: PEA001
    ) -> ty.Sequence[str]:  # noqa: PEA001
        return value if value is not None else []

    @validator("prove")  # pylint: disable-next = no-self-argument
    def initialize_prove_if_present(
        cls, value: Optional[ty.Sequence[str]]  # noqa: PEA001
    ) -> ty.Sequence[str]:  # noqa: PEA001
        return value if value is not None else []


@dataclass(frozen=True)
class Config:
    inp: dict[str, Sequence[tuple[int, ...]]] = dataclass_field(default_factory=dict)
    out: Sequence[str] = dataclass_field(default_factory=list)
    sequence: str = dataclass_field(default="")
    prove: Optional[Sequence[str]] = dataclass_field(default=None)


def get_config(feature: str) -> Config:
    config_file = Path(__file__).parent / feature / "config.yml"

    if config_file.is_file():
        yaml = YAML(typ="safe")
        cfg = ConfigFile.parse_obj(yaml.load(config_file))
        return Config(
            {
                str(c): [tuple(int(e) for e in str(m).split()) for m in i]
                for c, i in cfg.input.items()
                if i is not None
            }
            if cfg.input is not None
            else {},
            cfg.output if cfg.output is not None else [],
            cfg.sequence if cfg.sequence else "",
            cfg.prove,
        )

    return Config()


def create_model(feature: str) -> tuple[Model, Integration]:
    parser = Parser()
    parser.parse(Path("tests/integration") / feature / "test.rflx")
    return parser.create_model(), parser.get_integration()


def create_complement(config: Config, feature: str, tmp_path: Path) -> None:
    complement = session_main(
        config.inp,
        config.out,
        session_package="RFLX.Test.Session",
    )

    assert MAIN in complement

    for filename, content in complement.items():
        (tmp_path / filename).write_text(content)

    src_dir = Path(__file__).parent / feature / "src"
    if src_dir.is_dir():
        copytree(str(src_dir), str(tmp_path), dirs_exist_ok=True)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_parsability_and_model_creation(feature: str) -> None:
    create_model(feature)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_equality(feature: str, tmp_path: Path) -> None:
    generated_dir = Path(__file__).parent / feature / "generated"

    if not generated_dir.is_dir():
        pytest.skip()

    model, integration = create_model(feature)
    assert_equal_code(model, integration, generated_dir, tmp_path)


@pytest.mark.compilation
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_compilability(feature: str, tmp_path: Path) -> None:
    model, integration = create_model(feature)
    assert_compilable_code(model, integration, tmp_path)


@pytest.mark.compilation
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if not config.sequence:
        pytest.skip()
    model, integration = create_model(feature)
    create_complement(config, feature, tmp_path)
    assert assert_executable_code(model, integration, tmp_path, main=MAIN) == config.sequence


@pytest.mark.verification
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.prove is None:
        pytest.skip()
    model, integration = create_model(feature)
    units = []
    if model.sessions:
        assert len(model.sessions) == 1
        assert model.sessions[0].identifier == ID("Test::Session")
        units = ["main", "lib", "rflx-test-session"]
        create_complement(config, feature, tmp_path)
    assert_provable_code(model, integration, tmp_path, main=MAIN, units=[*units, *config.prove])
