from __future__ import annotations

import operator
from collections.abc import Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass
from enum import Enum
from functools import singledispatch
from typing import Final, Union

import z3

from rflx import expr, ty
from rflx.const import MP_CONTEXT
from rflx.identifier import ID
from rflx.rapidflux import Annotation, ErrorEntry, Location, RecordFluxError, Severity

PROVER_TIMEOUT: Final = 1800000


# TODO(eng/recordflux/RecordFlux#1424): Replace with PEP604 union
ArithBoolRef = Union[z3.ArithRef, z3.BoolRef]


class ProofResult(Enum):
    SAT = z3.sat
    UNSAT = z3.unsat
    UNKNOWN = z3.unknown


class Proof:
    def __init__(
        self,
        expr: expr.Expr,
        facts: Sequence[expr.Expr] | None = None,
        logic: str = "QF_NIA",
    ):
        self._expr = expr
        self._facts = facts or []
        self._result = ProofResult.UNSAT
        self._logic = logic
        self._unknown_reason: str | None = None

        solver = z3.SolverFor(self._logic)
        solver.set("timeout", PROVER_TIMEOUT)
        solver.add(_to_z3(self._expr))
        for f in self._facts:
            solver.add(_to_z3(f))

        self._result = ProofResult(solver.check())
        if self._result == ProofResult.UNKNOWN:
            self._unknown_reason = solver.reason_unknown()

    @property
    def result(self) -> ProofResult:
        return self._result

    @property
    def error(self) -> list[tuple[str, Location]]:
        assert self._result != ProofResult.SAT

        if self._result == ProofResult.UNKNOWN:
            assert self._unknown_reason is not None
            return [(self._unknown_reason, self._expr.location)]

        solver = z3.SolverFor(self._logic)
        solver.set(unsat_core=True)

        facts = {f"H{index}": fact for index, fact in enumerate(self._facts)}

        # Track facts for proof goals in disjunctive normal form
        if isinstance(self._expr, expr.Or):
            for term in self._expr.terms:
                index_start = len(facts)
                if isinstance(term, expr.And):
                    facts.update(
                        {
                            f"H{index}": fact
                            for index, fact in enumerate(term.terms, start=index_start)
                        },
                    )
                else:
                    facts.update({f"H{index_start}": term})
        else:
            solver.assert_and_track(_to_z3(self._expr), "goal")

        for name, fact in facts.items():
            solver.assert_and_track(_to_z3(fact), name)

        facts["goal"] = self._expr
        result = solver.check()
        assert result == z3.unsat, f"result should be unsat (is {result})"
        return [
            (" ".join(str(facts[str(fact)]).replace("\n", " ").split()), facts[fact].location)
            for fact in sorted([str(h) for h in solver.unsat_core()])
        ]


@dataclass
class ProofJob:
    goal: expr.Expr
    facts: Sequence[expr.Expr]
    results: Mapping[ProofResult, Sequence[ErrorEntry]]
    add_unsat: bool


class ParallelProofs:
    def __init__(self, workers: int) -> None:
        self._proofs: list[ProofJob] = []
        self._workers = workers

    def add(  # noqa: PLR0913
        self,
        goal: expr.Expr,
        facts: Sequence[expr.Expr],
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
    def check_proof(job: ProofJob) -> tuple[Sequence[ErrorEntry], Sequence[Annotation]]:
        result: list[Annotation] = []
        proof = Proof(job.goal, job.facts)

        if (
            job.add_unsat and proof.result == ProofResult.UNSAT
        ) or proof.result == ProofResult.UNKNOWN:
            result.extend(
                [
                    Annotation(
                        (
                            f'unsatisfied "{m}"'
                            if proof.result == ProofResult.UNSAT
                            else f"reason: {m}"
                        ),
                        Severity.NOTE,
                        l,
                    )
                    for m, l in proof.error
                ],
            )
        return job.results[proof.result], result

    def check(self, error: RecordFluxError) -> None:
        with ProcessPoolExecutor(max_workers=self._workers, mp_context=MP_CONTEXT) as executor:
            for entries, unsatcore_annotations in executor.map(
                ParallelProofs.check_proof,
                self._proofs,
            ):
                assert (entries and unsatcore_annotations) or not unsatcore_annotations
                if unsatcore_annotations:
                    entries[0].extend(unsatcore_annotations)
                error.extend(entries)


class Z3TypeError(TypeError):
    pass


@singledispatch
def _to_z3(expression: expr.Expr) -> z3.ExprRef:  # noqa: ARG001
    raise NotImplementedError


@_to_z3.register
def _(expression: expr.Not) -> z3.BoolRef:
    z3expr = _to_z3(expression.expr)
    if not isinstance(z3expr, z3.BoolRef):
        raise Z3TypeError("negating non-boolean term")
    return z3.Not(z3expr)


@_to_z3.register
def _(expression: expr.And) -> z3.BoolRef:
    z3exprs = [_to_z3(t) for t in expression.terms]
    boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
    if len(z3exprs) != len(boolexprs):
        raise Z3TypeError("conjunction of non-boolean terms")
    return z3.And(*boolexprs)


@_to_z3.register
def _(expression: expr.Or) -> z3.BoolRef:
    z3exprs = [_to_z3(t) for t in expression.terms]
    boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
    if len(z3exprs) != len(boolexprs):
        raise Z3TypeError("disjunction of non-boolean terms")
    return z3.Or(*boolexprs)


@_to_z3.register
def _(expression: expr.Number) -> z3.ArithRef:
    return z3.IntVal(expression.value)


@_to_z3.register
def _(expression: expr.Neg) -> z3.ArithRef:
    z3expr = _to_z3(expression.expr)
    if not isinstance(z3expr, z3.ArithRef):
        raise Z3TypeError("negating non-arithmetic term")
    return -z3expr


@_to_z3.register
def _(expression: expr.Add) -> z3.ArithRef:
    terms = [t for t in (_to_z3(e) for e in expression.terms) if isinstance(t, z3.ArithRef)]
    if len(terms) != len(expression.terms):
        raise Z3TypeError("adding non-arithmetic terms")
    return z3.Sum(*terms)


@_to_z3.register
def _(expression: expr.Mul) -> z3.ArithRef:
    terms = [t for t in (_to_z3(e) for e in expression.terms) if isinstance(t, z3.ArithRef)]
    if len(terms) != len(expression.terms):
        raise Z3TypeError("multiplying non-arithmetic terms")
    return z3.Product(*terms)


@_to_z3.register
def _(expression: expr.Sub) -> z3.ArithRef:
    left = _to_z3(expression.left)
    right = _to_z3(expression.right)
    if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
        raise Z3TypeError("subtracting non-arithmetic terms")
    return left - right


@_to_z3.register
def _(expression: expr.Div) -> z3.ArithRef:
    left = _to_z3(expression.left)
    right = _to_z3(expression.right)
    if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
        raise Z3TypeError("dividing non-arithmetic terms")
    return left / right


@_to_z3.register
def _(expression: expr.Pow) -> z3.ArithRef:
    left = _to_z3(expression.left)
    right = _to_z3(expression.right)
    if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
        raise Z3TypeError("exponentiating non-arithmetic terms")
    return left**right


@_to_z3.register
def _(expression: expr.Mod) -> z3.ArithRef:
    left = _to_z3(expression.left.simplified())
    right = _to_z3(expression.right)
    if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
        raise Z3TypeError("modulo operation on non-arithmetic terms")
    if not left.is_int():
        raise Z3TypeError(f'modulo operation on non-integer term "{left}"')
    return left % right


@_to_z3.register
def _(expression: expr.Literal) -> z3.ExprRef:
    if expression.identifier == ID("True"):
        return z3.BoolVal(val=True)
    if expression.identifier == ID("False"):
        return z3.BoolVal(val=False)
    return z3.Int(expression.name)


@_to_z3.register
def _(expression: expr.Variable) -> z3.ExprRef:
    if expression.type_ == ty.BOOLEAN:
        return z3.Bool(expression.name)
    return z3.Int(expression.name)


@_to_z3.register
def _(expression: expr.Attribute) -> z3.ExprRef:
    if not isinstance(
        expression.prefix,
        (expr.Variable, expr.Literal, expr.TypeName, expr.Selected),
    ):
        raise Z3TypeError("illegal prefix of attribute")
    return z3.Int(expression.representation)


@_to_z3.register
def _(expression: expr.ValidChecksum) -> z3.BoolRef:
    return z3.Bool(expression.representation)


@_to_z3.register
def _(_: expr.Val) -> z3.ExprRef:
    raise NotImplementedError


@_to_z3.register
def _(expression: expr.Selected) -> z3.ExprRef:
    return z3.Int(expression.representation)


@_to_z3.register
def _(expression: expr.Aggregate) -> z3.ExprRef:
    return z3.Int(str(expression))


@_to_z3.register
def _(expression: expr.Relation) -> z3.BoolRef:
    left = _to_z3(expression.left)
    right = _to_z3(expression.right)
    if not (isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef)) and not (
        isinstance(left, z3.BoolRef) and isinstance(right, z3.BoolRef)
    ):
        raise Z3TypeError(
            f'invalid relation between "{type(left).__name__}" and "{type(right).__name__}"'
            f" in {expression}",
        )
    result = _relation_operator(expression, left, right)
    assert isinstance(result, z3.BoolRef)
    return result


@singledispatch
def _relation_operator(
    _: expr.Relation,
    left: ArithBoolRef,  # noqa: ARG001
    right: ArithBoolRef,  # noqa: ARG001
) -> object:
    raise NotImplementedError


@_relation_operator.register
def _(
    _: expr.Less,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.lt(left, right)


@_relation_operator.register
def _(
    _: expr.LessEqual,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.le(left, right)


@_relation_operator.register
def _(
    _: expr.Equal,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.eq(left, right)


@_relation_operator.register
def _(
    _: expr.GreaterEqual,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.ge(left, right)


@_relation_operator.register
def _(
    _: expr.Greater,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.gt(left, right)


@_relation_operator.register
def _(
    _: expr.NotEqual,
    left: ArithBoolRef,
    right: ArithBoolRef,
) -> object:
    return operator.ne(left, right)


@_to_z3.register
def _(self: expr.IfExpr) -> z3.ExprRef:
    if len(self.condition_expressions) != 1:
        raise Z3TypeError("more than one condition")
    if self.else_expression is None:
        raise Z3TypeError("missing else expression")

    condition = _to_z3(self.condition_expressions[0][0])

    if not isinstance(condition, z3.BoolRef):
        raise Z3TypeError("non-boolean condition")

    return z3.If(
        condition,
        _to_z3(self.condition_expressions[0][1]),
        _to_z3(self.else_expression),
    )


def max_value(target: expr.Expr, facts: Sequence[expr.Expr]) -> expr.Number:
    opt = z3.Optimize()
    opt.add(*[_to_z3(e) for e in facts])
    value = opt.maximize(_to_z3(target))
    result = opt.check()
    assert result == z3.sat
    upper = value.upper()
    assert isinstance(upper, z3.IntNumRef)
    return expr.Number(upper.as_long())
