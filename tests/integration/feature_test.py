from distutils.dir_util import copy_tree
from pathlib import Path
from typing import List, Tuple

import pytest

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


def input_file(feature: str) -> Path:
    return Path(__file__).parent / feature / "INPUT"


def output_file(feature: str) -> Path:
    return Path(__file__).parent / feature / "OUTPUT"


def prove_file(feature: str) -> Path:
    return Path(__file__).parent / feature / "PROVE"


def functions_file(feature: str) -> Path:
    return Path(__file__).parent / feature / "FUNCTIONS"


def read_input(feature: str) -> List[Tuple[int, ...]]:
    if not input_file(feature).is_file():
        return []
    return [
        tuple(int(e) for e in l.split()) for l in input_file(feature).read_text().split("\n") if l
    ]


def read_output(feature: str) -> str:
    if not output_file(feature).is_file():
        return ""
    return output_file(feature).read_text()


def read_prove(feature: str) -> List[str]:
    if not prove_file(feature).is_file():
        return []
    return [l for l in prove_file(feature).read_text().split("\n") if l]


def read_functions(feature: str) -> List[str]:
    if not functions_file(feature).is_file():
        return []
    return [l for l in functions_file(feature).read_text().split("\n") if l]


def create_model(feature: str) -> Model:
    parser = Parser()
    parser.parse(Path(__file__).parent / feature / "test.rflx")
    return parser.create_model()


def create_complement(feature: str, tmp_path: Path) -> None:
    functions = read_functions(feature)
    context = [ada.WithClause(f.split(".")[0]) for f in functions]
    complement = session_main(
        read_input(feature),
        write=bool(read_input(feature)),
        context=context,
        session_package="RFLX.Test.Session",
        session_parameters=functions,
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
def test_equality(feature: str) -> None:
    generated_dir = Path(__file__).parent / feature / "generated"
    if not generated_dir.is_dir():
        pytest.skip()
    model = create_model(feature)
    expected_files = list(generated_dir.glob("rflx-test*"))
    generator = Generator(model, "RFLX", reproducible=True, ignore_unsupported_checksum=True)
    result = {}
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.name.startswith("rflx-test"):
            result[f"{unit.name}.ads"] = unit.ads
            if unit.adb:
                result[f"{unit.name}.adb"] = unit.adb
    assert set(result) == set(f.name for f in expected_files), "unexpected or missing units"
    for f in expected_files:
        assert result[f.name] == f.read_text(), f"mismatch in {f}"


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_compilability(feature: str, tmp_path: Path) -> None:
    if input_file(feature).is_file() and output_file(feature).is_file():
        pytest.skip()
    model = create_model(feature)
    assert_compilable_code(model, tmp_path)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability(feature: str, tmp_path: Path) -> None:
    if not input_file(feature).is_file() or not output_file(feature).is_file():
        pytest.skip()
    model = create_model(feature)
    create_complement(feature, tmp_path)
    assert assert_executable_code(model, tmp_path, main=MAIN) == read_output(feature)


@pytest.mark.verification
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    if not prove_file(feature).is_file():
        pytest.skip()
    model = create_model(feature)
    create_complement(feature, tmp_path)
    assert_provable_code(model, tmp_path, main=MAIN, units=["main", "lib", *read_prove(feature)])
