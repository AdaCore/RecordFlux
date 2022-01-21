from __future__ import annotations

from dataclasses import dataclass, field as dataclass_field
from distutils.dir_util import copy_tree
from pathlib import Path
from typing import Optional, Sequence, Tuple

import pytest
from ruamel.yaml.main import YAML

from rflx.generator import Generator
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.utils import (
    assert_compilable_code,
    assert_executable_code,
    assert_provable_code,
    session_main,
)

MAIN = "main.adb"
FEATURES = [
    f for f in Path(__file__).parent.glob("*") if f.is_dir() and (f / "test.rflx").is_file()
]


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
        cfg = yaml.load(config_file)
        return Config(
            {
                str(c): [tuple(int(e) for e in str(m).split()) for m in i]
                for c, i in cfg["input"].items()
            }
            if "input" in cfg and isinstance(cfg["input"], dict)
            else {},
            cfg["output"] if "output" in cfg and cfg["output"] else [],
            cfg["sequence"] if "sequence" in cfg else "",
            (cfg["prove"] if cfg["prove"] else []) if "prove" in cfg else None,
        )

    return Config()


def create_model(feature: str) -> Tuple[Model, Integration]:
    parser = Parser()
    parser.parse(Path(__file__).parent / feature / "test.rflx")
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
        copy_tree(str(src_dir), str(tmp_path))


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_parsability_and_model_creation(feature: str) -> None:
    create_model(feature)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_equality(feature: str, tmp_path: Path) -> None:
    generated_dir = Path(__file__).parent / feature / "generated"

    if not generated_dir.is_dir():
        pytest.skip()

    model, integration = create_model(feature)
    generator = Generator(
        model, integration, "RFLX", reproducible=True, ignore_unsupported_checksum=True
    )
    generator.write_top_level_package(tmp_path)
    generator.write_library_files(tmp_path)
    generator.write_units(tmp_path)
    generated_files = list(tmp_path.glob("*"))
    expected_files = list(generated_dir.glob("*"))

    generated_files.sort(key=lambda x: x.name)
    expected_files.sort(key=lambda x: x.name)

    assert [f.name for f in generated_files] == [
        f.name for f in expected_files
    ], "unexpected or missing units"
    for generated, expected in zip(generated_files, expected_files):
        assert generated.read_text() == expected.read_text(), f"mismatch in {generated.name}"


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
