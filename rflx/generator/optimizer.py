from __future__ import annotations

import json
import re
import shutil
import tempfile
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path
from subprocess import PIPE, STDOUT, CompletedProcess, run

from rflx.common import file_name
from rflx.error import fail
from rflx.rapidflux import logging
from rflx.spark import SPARKFile

from . import const


@dataclass
class Check:
    begin: int
    assertion: int
    end: int

    def valid_begin(self) -> bool:
        return self.begin > 0

    def valid(self) -> bool:
        return 0 < self.begin < self.assertion < self.end


def optimize(project_file: Path) -> None:
    """Remove unnecessary checks in generated state machine code."""

    if not gnatprove_found():
        fail("GNATprove is required for code optimization")

    files = [
        f
        for d in get_source_dirs(project_file)
        for f in d.glob(f"*-{file_name(str(const.STATE_MACHINE_PACKAGE))}.adb")
        if is_generated_by_recordflux(f)
    ]

    if not files:
        fail("No optimizable code found")

    checks: dict[Path, dict[int, Check]] = {}

    for f in files:

        cs = instrument(f)
        if cs:
            checks[f] = cs

    checks_to_remove: dict[Path, dict[int, Check]] = {}

    for i, (f, cs) in enumerate(checks.items(), start=1):
        logging.info("Analyzing {name} ({i}/{total})", name=f.name, i=i, total=len(checks))

        backup = f.with_suffix(f"{f.suffix}.bak")
        shutil.copy2(f, backup)

        try:
            checks_to_remove[f] = analyze(f, cs, project_file)
            remove(f, checks_to_remove[f], checks[f])
        except KeyboardInterrupt:
            logging.warning("Optimization aborted by user")
            backup.replace(f)
        except BaseException as e:
            backup.replace(f)
            raise e from e

    completed = sum(1 for cs in checks_to_remove.values() for c in cs)
    total = sum(1 for cs in checks.values() for c in cs)
    logging.info(
        "Optimization completed: {completed}/{total} checks removed",
        completed=completed,
        total=total,
    )


def gnatprove_found() -> bool:
    return bool(shutil.which("gnatprove"))


def gnatprove_supports_limit_lines() -> bool:
    p = run(["gnatprove", "--help"], stdout=PIPE, check=False)
    return p.stdout is not None and "--limit-lines=" in p.stdout.decode("utf-8")


def is_generated_by_recordflux(file: Path) -> bool:
    return const.GENERATED_BY_RECORDFLUX in file.read_text()


def instrument(file: Path) -> dict[int, Check]:
    """Add an always false assertion before each goto statement."""
    checks: dict[int, Check] = {}
    content = file.read_text().strip()
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

    file.write_text(instrumented_content)

    return checks


def analyze(file: Path, checks: dict[int, Check], project_file: Path) -> dict[int, Check]:
    """Analyze file and return removable checks."""

    result: dict[int, Check] = {}

    if not checks:
        return result

    proof_results = prove(file, checks, project_file)

    for line in checks:
        if proof_results[line]:
            result[line] = checks[line]

    return result


def prove(file: Path, lines: Iterable[int], project_file: Path) -> dict[int, bool]:
    """Prove file and return results for all assertions."""

    with tempfile.TemporaryDirectory() as tmp_dir_name:
        project_file = project_file.absolute()

        limit_lines_args = []

        if gnatprove_supports_limit_lines():
            tmp_dir = Path(tmp_dir_name)
            limit_file = tmp_dir / "limit"
            limit_file.write_text("\n".join([f"{file.name}:{l}" for l in lines]))
            limit_lines_args.append(f"--limit-lines={limit_file}")

        run_gnatprove(
            [
                "-P",
                str(project_file),
                "-u",
                file.name,
                "--prover=z3,cvc5",
                *limit_lines_args,
            ],
        )

        return get_proof_results_for_asserts(
            get_object_dir(project_file) / "gnatprove" / file.with_suffix(".spark").name,
        )


def run_gnatprove(args: list[str]) -> None:
    p = run(
        ["gnatprove", "--checks-as-errors=off", *args],
        stdout=PIPE,
        stderr=STDOUT,
        check=False,
    )
    check_exit_code("gnatprove", p)


def get_source_dirs(project_file: Path) -> list[Path]:
    return [
        Path(d)
        for d in json.loads(run_gprinspect(project_file).stdout)["projects"][0]["project"][
            "source-directories"
        ]
    ]


def get_object_dir(project_file: Path) -> Path:
    return Path(
        json.loads(run_gprinspect(project_file).stdout)["projects"][0]["project"][
            "object-directory"
        ],
    )


def run_gprinspect(project_file: Path) -> CompletedProcess[bytes]:
    p = run(
        [
            get_gprinspect_path(),
            "-P",
            str(project_file),
            "--display=json",
        ],
        stdout=PIPE,
        stderr=STDOUT,
        check=False,
    )
    check_exit_code("gprinspect", p)
    return p


def check_exit_code(name: str, process: CompletedProcess[bytes]) -> None:
    if process.returncode != 0:
        fail(
            f"{name} terminated with exit code {process.returncode}"
            + ("\n" + process.stdout.decode("utf-8") if process.stdout is not None else ""),
        )


def get_gprinspect_path() -> Path:
    gnatprove = shutil.which("gnatprove")
    assert gnatprove
    return Path(gnatprove).parents[1] / "libexec/spark/bin/gprinspect"


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
