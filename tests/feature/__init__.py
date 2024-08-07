from __future__ import annotations

import typing as ty
from collections.abc import Sequence
from dataclasses import dataclass, field as dataclass_field
from pathlib import Path
from shutil import copytree
from typing import Optional

from pydantic import BaseModel, field_validator
from ruamel.yaml.main import YAML

from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.const import FEATURE_DIR, MAIN
from tests.utils import session_main

FEATURES = [f for f in FEATURE_DIR.glob("*") if f.is_dir() and f.name != "__pycache__"]

# TODO(eng/recordflux/RecordFlux#1359): Replace ty.* by collections.abc.*
# Sequence and Mapping are imported from collections.abc as importing them
# from typing is deprecated. However pydantic does not support the imported
# version from collections.abc. To fix that typing is imported as ty and the
# typing versions of Sequence and Mapping are used in classes that derive
# from pydantic.BaseModel.
# This is only relevant for Python 3.8.


class ConfigFile(BaseModel):  # type: ignore[misc]
    input: Optional[ty.Mapping[str, Optional[ty.Sequence[ty.Union[int, str]]]]] = None
    output: Optional[ty.Sequence[str]] = None
    sequence: Optional[str] = None
    prove: Optional[ty.Sequence[str]] = None
    external_io_buffers: Optional[int] = None

    @field_validator("input")
    def initialize_input_if_present(
        cls,  # noqa: N805
        value: Optional[ty.Mapping[str, ty.Sequence[str]]],
    ) -> ty.Mapping[str, ty.Sequence[str]]:
        return value if value is not None else {}

    @field_validator("output")
    def initialize_output_if_present(
        cls,  # noqa: N805
        value: Optional[ty.Sequence[str]],
    ) -> ty.Sequence[str]:
        return value if value is not None else []

    @field_validator("prove")
    def initialize_prove_if_present(
        cls,  # noqa: N805
        value: Optional[ty.Sequence[str]],
    ) -> ty.Sequence[str]:
        return value if value is not None else []

    @field_validator("external_io_buffers")
    def initialize_external_io_buffers_if_present(
        cls,  # noqa: N805
        value: Optional[int],
    ) -> int:
        return value if value is not None else 0


@dataclass
class Config:
    inp: dict[str, Sequence[tuple[int, ...]]] = dataclass_field(default_factory=dict)
    out: Sequence[str] = dataclass_field(default_factory=list)
    sequence: str = dataclass_field(default="")
    prove: Optional[Sequence[str]] = dataclass_field(default=None)
    external_io_buffers: int = dataclass_field(default=0)


def get_config(feature: str) -> Config:
    config_file = FEATURE_DIR / feature / "config.yml"

    if config_file.is_file():
        yaml = YAML(typ="safe")
        cfg = ConfigFile.model_validate(yaml.load(config_file))
        return Config(
            (
                {
                    str(c): [tuple(int(e) for e in str(m).split()) for m in i]
                    for c, i in cfg.input.items()
                    if i is not None
                }
                if cfg.input is not None
                else {}
            ),
            cfg.output if cfg.output is not None else [],
            cfg.sequence if cfg.sequence else "",
            cfg.prove,
            cfg.external_io_buffers if cfg.external_io_buffers is not None else 0,
        )

    return Config()


def create_model(feature: str) -> tuple[Model, Integration]:
    parser = Parser()
    parser.parse(FEATURE_DIR / feature / "test.rflx")
    return parser.create_model(), parser.get_integration()


def create_complement(config: Config, feature: str, tmp_path: Path) -> None:
    complement = session_main(
        config.inp,
        config.out,
        external_io_buffers=config.external_io_buffers,
    )

    assert MAIN in complement

    target_dir = tmp_path / "src"
    target_dir.mkdir()

    for filename, content in complement.items():
        (target_dir / filename).write_text(content)

    copy_src(feature, tmp_path)


def copy_src(feature: str, tmp_path: Path) -> None:
    src_dir = FEATURE_DIR / feature / "src"
    if src_dir.is_dir():
        target_dir = tmp_path / "src"
        copytree(str(src_dir), str(target_dir), dirs_exist_ok=True)
