from __future__ import annotations

from collections.abc import Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass
from enum import Enum
from typing import Final, Optional

import z3

from rflx.const import MP_CONTEXT
from rflx.expression import And, Expr, Or
from rflx.rapidflux import ErrorEntry, Location, RecordFluxError, Severity

PROVER_TIMEOUT: Final = 1800000


class ProofResult(Enum):
    SAT = z3.sat
    UNSAT = z3.unsat
    UNKNOWN = z3.unknown


class Proof:
    def __init__(self, expr: Expr, facts: Optional[Sequence[Expr]] = None, logic: str = "QF_NIA"):
        self._expr = expr
        self._facts = facts or []
        self._result = ProofResult.UNSAT
        self._logic = logic
        self._unknown_reason: Optional[str] = None

        solver = z3.SolverFor(self._logic)
        solver.set("timeout", PROVER_TIMEOUT)
        solver.add(self._expr.z3expr())
        for f in self._facts:
            solver.add(f.z3expr())

        self._result = ProofResult(solver.check())
        if self._result == ProofResult.UNKNOWN:
            self._unknown_reason = solver.reason_unknown()

    @property
    def result(self) -> ProofResult:
        return self._result

    @property
    def error(self) -> list[tuple[str, Optional[Location]]]:
        assert self._result != ProofResult.SAT

        if self._result == ProofResult.UNKNOWN:
            assert self._unknown_reason is not None
            return [(self._unknown_reason, None)]

        solver = z3.SolverFor(self._logic)
        solver.set(unsat_core=True)

        facts = {f"H{index}": fact for index, fact in enumerate(self._facts)}

        # Track facts for proof goals in disjunctive normal form
        if isinstance(self._expr, Or):
            for term in self._expr.terms:
                index_start = len(facts)
                if isinstance(term, And):
                    facts.update(
                        {
                            f"H{index}": fact
                            for index, fact in enumerate(term.terms, start=index_start)
                        },
                    )
                else:
                    facts.update({f"H{index_start}": term})
        else:
            solver.assert_and_track(self._expr.z3expr(), "goal")

        for name, fact in facts.items():
            solver.assert_and_track(fact.z3expr(), name)

        facts["goal"] = self._expr
        result = solver.check()
        assert result == z3.unsat, f"result should be unsat (is {result})"
        return [
            (" ".join(str(facts[str(fact)]).replace("\n", " ").split()), facts[fact].location)
            for fact in sorted([str(h) for h in solver.unsat_core()])
        ]


@dataclass
class ProofJob:
    goal: Expr
    facts: Sequence[Expr]
    results: Mapping[ProofResult, Sequence[ErrorEntry]]
    add_unsat: bool


class ParallelProofs:
    def __init__(self, workers: int) -> None:
        self._proofs: list[ProofJob] = []
        self._workers = workers

    def add(  # noqa: PLR0913
        self,
        goal: Expr,
        facts: Sequence[Expr],
        unsat_error: Sequence[ErrorEntry] | None = None,
        unknown_error: Sequence[ErrorEntry] | None = None,
        sat_error: Sequence[ErrorEntry] | None = None,
        add_unsat: bool = False,
    ) -> None:
        """
        Add a proof goal and facts to a parallel proof.

        Errors can be set separately for each possible proof result. When running check_proof
        the sat_error, unsat_error or unknown_error will be returned when the proof result is
        ProofResult.SAT, ProofResult.UNSAT or ProofResult.UNKNOWN respectively.

        When add_unsat is set to True, unsatisfied facts will added as extra info messages.
        This option should only be set to True if ProofResult.UNSAT is considered an error.
        """
        self._proofs.append(
            ProofJob(
                goal,
                facts,
                {
                    ProofResult.SAT: sat_error if sat_error is not None else [],
                    ProofResult.UNSAT: unsat_error if unsat_error is not None else [],
                    ProofResult.UNKNOWN: unknown_error if unknown_error is not None else [],
                },
                add_unsat,
            ),
        )

    @staticmethod
    def check_proof(job: ProofJob) -> list[ErrorEntry]:
        result: list[ErrorEntry] = []
        proof = Proof(job.goal, job.facts)
        result.extend(job.results[proof.result])
        if (
            job.add_unsat and proof.result == ProofResult.UNSAT
        ) or proof.result == ProofResult.UNKNOWN:
            result.extend(
                [
                    ErrorEntry(
                        (
                            f'unsatisfied "{m}"'
                            if proof.result == ProofResult.UNSAT
                            else f"reason: {m}"
                        ),
                        Severity.INFO,
                        locn,
                    )
                    for m, locn in proof.error
                ],
            )
        return result

    def check(self, error: RecordFluxError) -> None:
        with ProcessPoolExecutor(max_workers=self._workers, mp_context=MP_CONTEXT) as executor:
            for e in executor.map(ParallelProofs.check_proof, self._proofs):
                error.extend(e)
