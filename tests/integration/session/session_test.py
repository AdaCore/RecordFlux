from pathlib import Path

import pytest

from rflx.specification.parser import Parser
from tests import utils

TESTDIR = "tests/integration/session"


@pytest.mark.parametrize("spec", ["tls_record_session"])
def test_session(spec: str, tmp_path: Path) -> None:
    p = Parser()
    p.parse(Path(f"{TESTDIR}/{spec}.rflx"))
    model = p.create_model()
    assert len(model.sessions) == 1
    utils.assert_compilable_code(model, tmp_path)
