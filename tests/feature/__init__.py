from __future__ import annotations

import typing as ty
from pathlib import Path
from shutil import copytree

from pydantic import BaseModel, ConfigDict, Field, field_validator
from ruamel.yaml.main import YAML

from rflx.integration import Integration
from rflx.model import Model
from rflx.specification import Parser
from tests.const import FEATURE_DIR, MAIN
from tests.utils import state_machine_main

FEATURES = [f for f in FEATURE_DIR.glob("*") if f.is_dir() and f.name != "__pycache__"]

# TODO(eng/recordflux/RecordFlux#1359): Replace ty.* by collections.abc.*
# Sequence and Mapping are imported from collections.abc as importing them
# from typing is deprecated. However pydantic does not support the imported
# version from collections.abc. To fix that typing is imported as ty and the
# typing versions of Sequence and Mapping are used in classes that derive
# from pydantic.BaseModel.
# This is only relevant for Python 3.8.


# TODO(eng/recordflux/RecordFlux#1424): Replace remaining use of Optional
# and Union. Pydantic has issues with PEP604 type annotations in Python
# 3.8 and 3.9.


class ProofConfig(BaseModel):  # type: ignore[misc]
    enabled: bool = Field(default=True)
    timeout: int = Field(default=60)
    memlimit: int = Field(default=1500)
    units: ty.Sequence[str] = Field(default_factory=list)

    model_config = ConfigDict(extra="forbid")


class Config(BaseModel):  # type: ignore[misc]
    input: ty.Mapping[str, ty.Optional[ty.Sequence[ty.Union[int, str]]]] = Field(  # noqa: UP007
        default_factory=dict,
    )
    output: ty.Sequence[str] = Field(default_factory=list)
    sequence: str = Field(default="")
    proof: ty.Optional[ProofConfig] = Field(default=None)  # noqa: UP007
    external_io_buffers: int = Field(default=0)

    model_config = ConfigDict(extra="forbid")

    @field_validator("proof")
    def initialize_proof_if_present(
        cls,  # noqa: N805
        value: ty.Optional[ProofConfig],  # noqa: UP007
    ) -> ProofConfig:
        return value if value is not None else ProofConfig()

    @property
    def inp(self) -> dict[str, ty.Sequence[tuple[int, ...]]]:
        return (
            {
                str(c): [tuple(int(e) for e in str(m).split()) for m in i]
                for c, i in self.input.items()
                if i is not None
            }
            if self.input is not None
            else {}
        )

    @property
    def out(self) -> ty.Sequence[str]:
        return self.output


def get_config(feature: str) -> Config:
    config_file = FEATURE_DIR / feature / "config.yml"

    if config_file.is_file():
        yaml = YAML(typ="safe")
        return Config.model_validate(yaml.load(config_file))

    return Config()


def create_model(feature: str) -> tuple[Model, Integration]:
    parser = Parser()
    parser.parse(FEATURE_DIR / feature / "test.rflx")
    return parser.create_model(), parser.get_integration()


def create_complement(config: Config, feature: str, tmp_path: Path) -> list[str]:
    complement = state_machine_main(
        config.inp,
        config.out,
        external_io_buffers=config.external_io_buffers,
    )

    assert MAIN in complement

    target_dir = tmp_path / "src"
    target_dir.mkdir()

    for filename, content in complement.items():
        (target_dir / filename).write_text(content)

    src_files = copy_src(feature, tmp_path)

    return [*complement, *src_files]


def copy_src(feature: str, tmp_path: Path) -> list[str]:
    src_dir = FEATURE_DIR / feature / "src"
    if src_dir.is_dir():
        target_dir = tmp_path / "src"
        copytree(str(src_dir), str(target_dir), dirs_exist_ok=True)
    return [f.name for f in src_dir.glob("**/*.ad?")]
