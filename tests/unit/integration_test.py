import re
from pathlib import Path

import pytest
from ruamel.yaml.main import YAML

from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.integration import Integration


@pytest.mark.parametrize(
    "rfi_content,match_error",
    [
        ("garbage", "expected dict not str"),
        ("Session: garbage", "Session.*value is not a valid dict"),
        ("{}", "Session.*field required"),
        (
            """Session:
                Session: 1
          """,
            "value is not a valid dict",
        ),
        (
            """Session:
                Session: {}
          """,
            "Buffer_Size.*field required",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: -1024
          """,
            "ensure this value is greater than 0",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: Hello
                        Global:
                            Msg: 2048
          """,
            "Default.*value is not a valid integer",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: Hello
          """,
            "Msg.*value is not a valid integer",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: -10
          """,
            "ensure this value is greater than 0",
        ),
        (
            """Session:
                Session:
                    Buffer_Size: 2
          """,
            "Buffer_Size.*value is not a valid dict",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Next:
                                Msg2: -10
          """,
            "ensure this value is greater than 0",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                    Other: 1
          """,
            "Other.*extra fields not permitted",
        ),
    ],
)
def test_rfi_add_integration(rfi_content: str, match_error: str) -> None:
    # pydantic messages end with the type of the error in parentheses.
    regex = re.compile(
        (
            "^test.rfi:0:0: parser: error: 1 validation error for "
            fr"IntegrationFile.*{match_error} \([^()]*\)$"
        ),
        re.DOTALL,
    )
    yaml = YAML()
    content = yaml.load(rfi_content)
    error = RecordFluxError()
    integration = Integration()
    with pytest.raises(RecordFluxError, match=regex):
        integration.add_integration_file(Path("test.rfi"), content, error)
        error.propagate()


def test_rfi_get_size() -> None:
    integration = Integration()
    session_object = {
        "Session": {
            "S": {
                "Buffer_Size": {"Default": 1024, "Global": {"y": 2048}, "Local": {"S": {"y": 8192}}}
            }
        }
    }
    error = RecordFluxError()
    integration.add_integration_file(Path("p.rfi"), session_object, error)
    error.propagate()
    assert integration.get_size(ID("P::S"), ID("x"), ID("S")) == 1024
    assert integration.get_size(ID("P::S"), ID("x"), ID("S")) == 1024
    assert integration.get_size(ID("P::S"), ID("x"), None) == 1024
    assert integration.get_size(ID("P::S2"), ID("x"), None) == 4096
    assert integration.get_size(ID("P::S"), ID("y"), None) == 2048
    assert integration.get_size(ID("P::S"), ID("y"), ID("S")) == 8192
