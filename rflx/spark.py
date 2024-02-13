from __future__ import annotations

import typing as ty

from pydantic import BaseModel


class ProofAttempt(BaseModel):
    result: str
    steps: int
    time: int


class CheckTreeElement(BaseModel):
    proof_attempts: ty.Mapping[str, ProofAttempt]


class Proof(BaseModel):
    file: str
    line: int
    col: int
    rule: str
    check_tree: ty.Sequence[CheckTreeElement]


class SPARKFile(BaseModel):
    proof: ty.Sequence[Proof]
