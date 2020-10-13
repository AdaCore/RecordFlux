from pathlib import Path

import pytest

from rflx.specification.parser import Parser

TESTDIR = "tests/integration/session"


@pytest.mark.parametrize("spec", ["tls_handshake_session", "tls_record_session"])
def test_session(spec: str) -> None:
    p = Parser()
    p.parse(Path(f"{TESTDIR}/{spec}.rflx"))
    model = p.create_model()
    assert len(model.sessions) == 1
