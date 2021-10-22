from dataclasses import dataclass, field as dataclass_field
from distutils.dir_util import copy_tree
from pathlib import Path
from typing import Optional, Sequence, Tuple

import pytest
from ruamel.yaml.main import YAML

from rflx import ada
from rflx.generator import Generator
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
    functions: Sequence[str] = dataclass_field(default_factory=list)
    inp: Sequence[Tuple[int, ...]] = dataclass_field(default_factory=list)
    out: str = dataclass_field(default="")
    prove: Optional[Sequence[str]] = dataclass_field(default=None)


def get_config(feature: str) -> Config:
    config_file = Path(__file__).parent / feature / "config.yml"

    if config_file.is_file():
        yaml = YAML(typ="safe")
        cfg = yaml.load(config_file)
        return Config(
            cfg["functions"] if "functions" in cfg and cfg["functions"] else [],
            [tuple(int(e) for e in str(m).split()) for m in cfg["input"]]
            if "input" in cfg and cfg["input"]
            else [],
            cfg["output"] if "output" in cfg else "",
            (cfg["prove"] if cfg["prove"] else []) if "prove" in cfg else None,
        )

    return Config()


def create_model(feature: str) -> Model:
    parser = Parser()
    parser.parse(Path(__file__).parent / feature / "test.rflx")
    return parser.create_model()


def create_complement(config: Config, feature: str, tmp_path: Path) -> None:
    context = [ada.WithClause(f.split(".")[0]) for f in config.functions]
    complement = session_main(
        config.inp,
        write=bool(config.inp),
        context=context,
        session_package="RFLX.Test.Session",
        session_parameters=config.functions,
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

    model = create_model(feature)
    generator = Generator(model, "RFLX", reproducible=True, ignore_unsupported_checksum=True)
    generator.write_top_level_package(tmp_path)
    generator.write_library_files(tmp_path)
    generator.write_units(tmp_path)
    generated_files = list(tmp_path.glob("*"))
    expected_files = list(generated_dir.glob("*"))

    assert [f.name for f in generated_files] == [
        f.name for f in expected_files
    ], "unexpected or missing units"
    for generated, expected in zip(generated_files, expected_files):
        assert generated.read_text() == expected.read_text(), f"mismatch in {generated.name}"


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_compilability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.out:
        pytest.skip()
    model = create_model(feature)
    assert_compilable_code(model, tmp_path)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if not config.out:
        pytest.skip()
    model = create_model(feature)
    create_complement(config, feature, tmp_path)
    assert assert_executable_code(model, tmp_path, main=MAIN) == config.out


@pytest.mark.verification
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.prove is None:
        pytest.skip()
    model = create_model(feature)
    create_complement(config, feature, tmp_path)
    assert_provable_code(model, tmp_path, main=MAIN, units=["main", "lib", *config.prove])
