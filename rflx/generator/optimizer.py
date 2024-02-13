from __future__ import annotations

import json
import logging
import re
import shutil
import tempfile
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path
from subprocess import PIPE, STDOUT, run
from tempfile import TemporaryDirectory

from rflx.error import Subsystem, fail
from rflx.spark import SPARKFile

log = logging.getLogger(__name__)


@dataclass
class Check:
    begin: int
    assertion: int
    end: int

    def valid_begin(self) -> bool:
        return self.begin > 0

    def valid(self) -> bool:
        return 0 < self.begin < self.assertion < self.end


def optimize(generated_dir: Path, workers: int = 0, timeout: int = 1) -> None:
    """Remove unnecessary checks in generated state machine code."""

    if not gnatprove_found():
        fail("GNATprove is required for code optimization", Subsystem.GENERATOR)

    with TemporaryDirectory() as tmp_dir_name:
        tmp_dir = Path(tmp_dir_name)
        shutil.copytree(generated_dir, tmp_dir, dirs_exist_ok=True)

        checks: dict[Path, dict[int, Check]] = {}

        for f in tmp_dir.glob("*.adb"):
            cs = instrument(f)
            if cs:
                checks[f] = cs

        checks_to_remove: dict[Path, dict[int, Check]] = {}

        for i, (f, cs) in enumerate(checks.items(), start=1):
            log.info("Analyzing %s (%d/%d)", f.name, i, len(checks))
            checks_to_remove[f] = analyze(f, cs, workers, timeout)
            if checks_to_remove[f]:
                remove(f, checks_to_remove[f], checks[f])
                shutil.copy(f, generated_dir)

        log.info(
            "Optimization completed: %d/%d checks removed",
            sum(1 for cs in checks_to_remove.values() for c in cs),
            sum(1 for cs in checks.values() for c in cs),
        )


def gnatprove_found() -> bool:
    return bool(shutil.which("gnatprove"))


def instrument(file: Path) -> dict[int, Check]:
    """Add an always false assertion before each goto statement."""
    checks: dict[int, Check] = {}
    content = file.read_text()
    instrumented_content = ""
    check = Check(0, 0, 0)

    for i, l in enumerate(content.split("\n"), start=1):
        if l.strip().startswith("if"):
            check.begin = i + len(checks)
        elif l.strip().startswith("goto"):
            match = re.match(r" *", l)
            assert match
            indentation = match.group()
            check.assertion = i + len(checks)
            if check.valid_begin():
                instrumented_content += f"{indentation}pragma Assert (False);\n"
        elif l.strip().startswith("else"):
            check.begin = 0
        elif l.strip().startswith("end if"):
            check.end = i + len(checks) + 1
            if check.valid():
                checks[check.assertion] = check
                check = Check(0, 0, 0)

        instrumented_content += f"{l}\n"

    file.write_text(instrumented_content.strip())

    return checks


def analyze(
    file: Path,
    checks: dict[int, Check],
    workers: int = 0,
    timeout: int = 1,
) -> dict[int, Check]:
    """Analyze file and return removable checks."""

    result: dict[int, Check] = {}

    if not checks:
        return result

    proof_results = prove(file, workers, timeout)

    for line in checks:
        if proof_results[line]:
            result[line] = checks[line]

    return result


def prove(file: Path, workers: int = 0, timeout: int = 1) -> dict[int, bool]:
    """Prove file and return results for all assertions."""

    with tempfile.TemporaryDirectory() as tmp_dir_name:
        tmp_dir = Path(tmp_dir_name)
        project_file = tmp_dir / "optimize.gpr"
        project_file.write_text(
            f'project Optimize is\n   for Source_Dirs use ("{file.parent}");\nend Optimize;\n',
        )

        p = run(
            [
                "gnatprove",
                "-P",
                str(project_file),
                "-u",
                file.name,
                "-j",
                str(workers),
                "--prover=z3,cvc5",
                "--timeout",
                str(timeout),
            ],
            cwd=file.parent,
            stdout=PIPE,
            stderr=STDOUT,
        )

        if p.returncode != 0:
            fail(
                f"gnatprove terminated with exit code {p.returncode}"
                + ("\n" + p.stdout.decode("utf-8") if p.stdout is not None else ""),
                Subsystem.GENERATOR,
            )

        return get_proof_results_for_asserts(
            project_file.parent / "gnatprove" / file.with_suffix(".spark").name,
        )


def get_proof_results_for_asserts(spark_file: Path) -> dict[int, bool]:
    result = {}

    for proof in SPARKFile(**json.loads(spark_file.read_text())).proof:
        if proof.rule != "VC_ASSERT":
            continue

        for attempt in proof.check_tree[0].proof_attempts.values():
            if attempt.result == "Valid":
                result[proof.line] = True
                break
        else:
            result[proof.line] = False

    return result


def remove(file: Path, checks: dict[int, Check], assertions: Iterable[int]) -> None:
    """Remove checks and always false assertions."""

    lines = {i for c in checks.values() for i in range(c.begin, c.end + 1)} | set(assertions)
    content = file.read_text()
    optimized_content = ""

    for i, l in enumerate(content.split("\n"), start=1):
        if i not in lines:
            optimized_content += f"{l}\n"

    assert "pragma Assert (False)" not in optimized_content
    file.write_text(optimized_content.strip() + "\n")
