from __future__ import annotations

from collections.abc import Mapping, Sequence

from pydantic import BaseModel


class ProofAttempt(BaseModel):  # type: ignore[misc]
    result: str
    steps: int
    time: float


class CheckTreeElement(BaseModel):  # type: ignore[misc]
    proof_attempts: Mapping[str, ProofAttempt]


class Proof(BaseModel):  # type: ignore[misc]
    file: str
    line: int
    col: int
    rule: str
    check_tree: Sequence[CheckTreeElement]


class SPARKFile(BaseModel):  # type: ignore[misc]
    proof: Sequence[Proof]
