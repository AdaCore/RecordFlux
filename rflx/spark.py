from __future__ import annotations

import typing as ty

from pydantic import BaseModel

# TODO(eng/recordflux/RecordFlux#1359): Replace ty.* by collections.abc.*
# Sequence and Mapping are imported from collections.abc as importing them
# from typing is deprecated. However pydantic does not support the imported
# version from collections.abc. To fix that typing is imported as ty and the
# typing versions of Sequence and Mapping are used in classes that derive
# from pydantic.BaseModel.
# This is only relevant for Python 3.8.


class ProofAttempt(BaseModel):  # type: ignore[misc]
    result: str
    steps: int
    time: float


class CheckTreeElement(BaseModel):  # type: ignore[misc]
    proof_attempts: ty.Mapping[str, ProofAttempt]


class Proof(BaseModel):  # type: ignore[misc]
    file: str
    line: int
    col: int
    rule: str
    check_tree: ty.Sequence[CheckTreeElement]


class SPARKFile(BaseModel):  # type: ignore[misc]
    proof: ty.Sequence[Proof]
