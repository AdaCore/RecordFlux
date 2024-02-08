from __future__ import annotations

import logging
import re
import shutil
from collections import defaultdict
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path
from subprocess import PIPE, STDOUT, run
from tempfile import TemporaryDirectory

from rflx.error import Subsystem, fail

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


def optimize(generated_dir: Path, workers: int = 0) -> None:
    if not gnatprove_found():
        fail("GNATprove is required for code optimization", Subsystem.GENERATOR)

    with TemporaryDirectory() as tmp_dir_name:
        tmp_dir = Path(tmp_dir_name)
        shutil.copytree(generated_dir, tmp_dir, dirs_exist_ok=True)

        checks: dict[Path, dict[int, Check]] = defaultdict(dict)

        for f in tmp_dir.glob("*.adb"):
            checks[f] = instrument(f)

        i = 1
        total = sum(1 for cs in checks.values() for c in cs)
        checks_to_remove: dict[Path, dict[int, Check]] = defaultdict(dict)

        for f, cs in checks.items():
            for assertion, check in cs.items():
                log.info("Analyzing %s (%d/%d)", f.name, i, total)
                checks_to_remove[f] |= analyze(f, {assertion: check}, workers)
                i += 1

        for f in checks_to_remove:
            remove(f, checks_to_remove[f], checks[f])
            shutil.copy(f, generated_dir)

        log.info(
            "Optimization completed: %d checks removed",
            sum(1 for cs in checks_to_remove.values() for c in cs),
        )


def gnatprove_found() -> bool:
    return bool(shutil.which("gnatprove"))


def instrument(file: Path) -> dict[int, Check]:
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


def analyze(file: Path, checks: dict[int, Check], workers: int = 0) -> dict[int, Check]:
    result: dict[int, Check] = {}

    if not checks:
        return result

    for i in checks:
        if prove(file, i, workers):
            result[i] = checks[i]

    return result


def prove(file: Path, line: int, workers: int = 0) -> bool:
    return (
        run(
            [
                "gnatprove",
                f"--limit-line={file.name}:{line}",
                f"-j{workers}",
                "--prover=all",
                "--timeout=1",
                "--checks-as-errors=on",
                "--quiet",
            ],
            cwd=file.parent,
            stdout=PIPE,
            stderr=STDOUT,
        ).returncode
        == 0
    )


def remove(file: Path, checks: dict[int, Check], assertions: Iterable[int]) -> None:
    lines = {i for c in checks.values() for i in range(c.begin, c.end + 1)} | set(assertions)
    content = file.read_text()
    optimized_content = ""

    for i, l in enumerate(content.split("\n"), start=1):
        if i not in lines:
            optimized_content += f"{l}\n"

    assert "pragma Assert (False)" not in optimized_content
    file.write_text(optimized_content.strip())
