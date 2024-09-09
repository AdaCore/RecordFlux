from __future__ import annotations

import re
from collections.abc import Sequence
from pathlib import Path

import pytest
from ruamel.yaml.main import YAML

from rflx.identifier import ID
from rflx.integration import Integration
from rflx.rapidflux import RecordFluxError


@pytest.mark.parametrize(
    ("rfi_content", "match_error"),
    [
        ("garbage", "Input should be a valid dictionary"),
        ("Machine: garbage", "Machine.*Input should be a valid dictionary"),
        ("{}", "Machine.*Field required"),
        (
            """Machine:
                State_Machine: 1
          """,
            "Machine.State_Machine.*Input should be a valid dictionary",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: -1024
          """,
            "Input should be greater than 0",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: Hello
                        Global:
                            Msg: 2048
          """,
            "Machine.State_Machine.Buffer_Size.Default.*Input should be a valid integer",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: Hello
          """,
            "Machine.State_Machine.Buffer_Size.Global.Msg.*Input should be a valid integer",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: -10
          """,
            "Machine.State_Machine.Buffer_Size.Global.Msg.*Input should be greater than 0",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size: 2
          """,
            "Machine.State_Machine.Buffer_Size.*Input should be a valid dictionary",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Next:
                                Msg2: -10
          """,
            "Machine.State_Machine.Buffer_Size.Local.Next.Msg2.*Input should be greater than 0",
        ),
        (
            """Machine:
                State_Machine:
                    Buffer_Size:
                        Default: 1024
                    Other: 1
          """,
            "Machine.State_Machine.Other.*Extra inputs are not permitted",
        ),
    ],
)
def test_rfi_add_integration(rfi_content: str, match_error: str) -> None:
    # pydantic messages end with the type of the error in parentheses.
    regex = re.compile(
        (f"^test.rfi:1:1: error: 1 validation error for IntegrationFile.*{match_error}.*$"),
        re.DOTALL,
    )
    yaml = YAML()
    content = yaml.load(rfi_content)
    error = RecordFluxError()
    integration = Integration()
    integration._add_integration_object(Path("test.rfi"), content, error)  # noqa: SLF001
    with pytest.raises(RecordFluxError, match=regex):
        error.propagate()


def test_rfi_get_size() -> None:
    integration = Integration()
    error = RecordFluxError()
    integration._add_integration_object(  # noqa: SLF001
        Path("p.rfi"),
        {
            "Machine": {
                "S": {},
            },
        },
        error,
    )
    error.propagate()

    assert integration.get_size(ID("P::S"), None, None) == 4096
    assert integration.get_size(ID("P::S"), ID("X"), ID("S")) == 4096

    integration = Integration()
    error = RecordFluxError()
    integration._add_integration_object(  # noqa: SLF001
        Path("p.rfi"),
        {
            "Machine": {
                "S": {
                    "Buffer_Size": {
                        "Default": 1024,
                        "Global": {
                            "Y": 2048,
                            "Z": 512,
                        },
                        "Local": {
                            "S": {
                                "Y": 8192,
                            },
                        },
                    },
                },
            },
        },
        error,
    )
    error.propagate()

    assert integration.get_size(ID("P::S"), None, None) == 1024
    assert integration.get_size(ID("P::S"), ID("X"), ID("S")) == 1024
    assert integration.get_size(ID("P::S"), ID("X"), ID("S")) == 1024
    assert integration.get_size(ID("P::S"), ID("X"), None) == 1024
    assert integration.get_size(ID("P::S2"), ID("X"), None) == 4096
    assert integration.get_size(ID("P::S"), ID("Y"), None) == 2048
    assert integration.get_size(ID("P::S"), ID("Y"), ID("S")) == 8192
    assert integration.get_size(ID("P::S"), ID("Z"), None) == 512
    assert integration.get_size(ID("P::S"), ID("Z"), ID("S")) == 512


@pytest.mark.parametrize(
    ("state_machine_object", "result"),
    [
        (
            {
                "Machine": {},
            },
            False,
        ),
        (
            {
                "Machine": {
                    "S": {},
                },
            },
            False,
        ),
        (
            {
                "Machine": {
                    "S": {
                        "External_IO_Buffers": False,
                    },
                },
            },
            False,
        ),
        (
            {
                "Machine": {
                    "S": {
                        "External_IO_Buffers": True,
                    },
                },
            },
            True,
        ),
    ],
)
def test_rfi_use_external_io_buffers(
    state_machine_object: dict[object, object],
    result: bool,
) -> None:
    integration = Integration()

    assert not integration.use_external_io_buffers(ID("P::S"))

    error = RecordFluxError()
    integration._add_integration_object(  # noqa: SLF001
        Path("p.rfi"),
        state_machine_object,
        error,
    )
    error.propagate()

    assert integration.use_external_io_buffers(ID("P::S")) == result


@pytest.mark.parametrize(
    ("content", "error_msg", "line", "column"),
    [
        ('"', ["while scanning a quoted scalar", "unexpected end of stream"], 1, 2),
        ("Machine: 1, Machine : 1", ["mapping values are not allowed here"], 1, 21),
        (
            "Machine: 1\nMachine : 1",
            ["while constructing a mapping", 'found duplicate key "Machine" with value "1"'],
            2,
            1,
        ),
    ],
)
def test_load_integration_file(
    tmp_path: Path,
    content: str,
    error_msg: Sequence[str],
    line: int,
    column: int,
) -> None:
    test_rfi = tmp_path / "test.rfi"
    test_rfi.write_text(content)
    integration = Integration()
    error = RecordFluxError()
    regex = rf"^{test_rfi}:{line}:{column}: error: "
    for elt in error_msg:
        regex += elt
        regex += rf'.*in "{test_rfi}", line [0-9]+, column [0-9]+.*'
    regex += "$"
    compiled_regex = re.compile(regex, re.DOTALL)
    integration.load_integration_file(test_rfi, error)
    with pytest.raises(RecordFluxError, match=compiled_regex):
        error.propagate()


def test_load_integration_path(tmp_path: Path) -> None:
    subfolder = tmp_path / "sub"
    subfolder.mkdir()
    test_rfi = subfolder / "test.rfi"
    test_rfi.write_text("{ Machine: { State_Machine : { Buffer_Size : 0 }}}")
    integration = Integration(integration_files_dir=subfolder)
    error = RecordFluxError()
    regex = re.compile(
        (
            r"^"
            r"test.rfi:1:1: error: 1 validation error for IntegrationFile.*"
            r"Input should be a valid dictionary.*"
            r"$"
        ),
        re.DOTALL,
    )
    integration.load_integration_file(tmp_path / "test.rflx", error)
    with pytest.raises(RecordFluxError, match=regex):
        error.propagate()
