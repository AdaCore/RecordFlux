#!/usr/bin/env python3

"""
Extract job definitions and variables from a .gitlab-ci.yml file.

Run with the "--help" switch to see the available options.

Limitations/design choices:
 * Jobs with parallel specifications (i.e., with "parallel/matrix" attributes)
   are executed sequentially in the order given in the matrix.
 * The "parallel/matrix" attributes are applied only to the current job and are
   ignored in prerequisite jobs.
"""
from __future__ import annotations

import argparse
import logging
import textwrap
from collections.abc import Generator
from dataclasses import dataclass, field
from itertools import product
from pathlib import Path
from typing import Final

from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq

SCRIPT_PREFIX: Final = r"""
    set -euxo pipefail
"""

# Having the CI_PROJECT_DIR variable set is mandatory. The commands should be
# executed from that directory. Additionally, job commands may need to refer to
# this variable.
SET_WORKING_DIR: Final = r"""
    if [ -z "${CI_PROJECT_DIR}" ]; then
        echo "Error: CI_PROJECT_DIR environment variable is not set."
        exit 1
    fi
    cd "${CI_PROJECT_DIR}" || \
        { echo "Error: Could not change directory to ${CI_PROJECT_DIR}"; exit 1; }
"""

INDENT: Final = " " * 4


@dataclass
class JobData:
    extends: str | None = None
    needs: list[str] = field(default_factory=list)
    script: list[str] | None = None
    parallel_matrix: list[dict[str, str]] | None = None


def main() -> None:
    logging.basicConfig(format="%(levelname)s: %(message)s", level=logging.INFO)

    parser = argparse.ArgumentParser(description="Extract GitLab CI jobs.")
    parser.add_argument("input", help="Path to the .gitlab-ci.yml file.")
    parser.add_argument(
        "--job",
        help=(
            "Name of a specific job to extract."
            " If not provided, then all jobs will be extracted."
        ),
    )
    parser.add_argument(
        "--output",
        help="Output directory for the generated scripts. Mandatory, if no '--job' given.",
    )
    parser.add_argument(
        "--no-needs",
        action="store_true",
        help="Do not include scripts of jobs specified in 'needs'.",
    )
    parser.add_argument(
        "--var",
        action="append",
        default=[],
        help="Additional variable to set in the form 'name=value'. Can be repeated.",
    )
    parser.add_argument(
        "--keep-origin-vars",
        action="store_true",
        help=(
            "By default variables with a name ending with '_ORIGIN' are skipped."
            " This setting overrides that behavior."
        ),
    )
    parser.add_argument(
        "--strict-unshallow",
        action="store_true",
        help=(
            "Do not check if the repository is shallow before trying to unshallow it."
            " Results in an error if the repository is already unshallow."
        ),
    )
    parser.add_argument(
        "--pure-script",
        action="store_true",
        help='Only extract the "script" section of the jobs. Do not add any wrapper code.',
    )

    args = parser.parse_args()

    if args.job and not args.output:
        args.output = "-"
    elif not args.job and not args.output:
        parser.error("The --output option is mandatory unless --job is provided.")

    extra_vars = []
    for var in args.var:
        if "=" not in var:
            raise ValueError(f"Invalid variable format: {var}. Expected 'name=value'.")
        name, value = var.split("=", 1)
        extra_vars.append(f"{name}={value!r}")

    _process_ci_file(
        args.input,
        args.output,
        args.job,
        args.no_needs,
        args.keep_origin_vars,
        args.strict_unshallow,
        args.pure_script,
        extra_vars,
    )
    if args.output != "-":
        logging.info("Extraction complete. Scripts and variables saved to: %s", args.output)


def _process_ci_file(  # noqa: PLR0913
    input_path: str,
    output_dir: str | Path,
    job_name: str | None,
    no_needs: bool = False,
    keep_origin_vars: bool = False,
    strict_unshallow: bool = False,
    pure_script: bool = False,
    extra_vars: list[str] | None = None,
) -> None:
    """Parse the GitLab CI YAML file and generate scripts based on its content."""

    yaml = YAML()
    yaml.preserve_quotes = True  # Ensure anchor references are retained
    with Path(input_path).open() as file:
        data = yaml.load(file)

    is_stdout = output_dir == "-"
    if not is_stdout:
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

    variables = _extract_variables(data, keep_origin_vars, extra_vars)
    job_data = _process_jobs(data, strict_unshallow)

    _extract_jobs(job_name, job_data, is_stdout, output_dir, variables, pure_script, no_needs)

    if not is_stdout and variables:
        assert isinstance(output_dir, Path)
        variables_file = output_dir / "variables"
        variables_file.write_text("\n".join(["export " + v for v in variables]) + "\n")


def _extract_variables(
    data: dict[str, CommentedMap],
    keep_origin_vars: bool,
    extra_vars: list[str] | None,
) -> list[str]:
    """Extract variables from the CI file and add extra variables."""

    variables = []
    if "variables" in data and isinstance(data["variables"], dict):
        for var_name, var_value in data["variables"].items():
            if not keep_origin_vars and var_name.endswith("_ORIGIN"):
                continue
            variables.append(f"{var_name}={var_value!r}")
    if extra_vars:
        variables.extend(extra_vars)
    return variables


def _process_jobs(data: CommentedMap, strict_unshallow: bool) -> dict[str, JobData]:
    """Process jobs and reusable scripts from the CI file."""

    job_data: dict[str, JobData] = {}
    for key, value in data.items():
        if key == "variables":
            continue

        if isinstance(value, dict):
            parallel_matrix = _process_parallel(value.get("parallel", None))
            job_data[key] = JobData(
                extends=value.get("extends", None),
                needs=value.get("needs", []),
                script=(
                    _extract_script_field(value["script"], strict_unshallow)
                    if "script" in value
                    else None
                ),
                parallel_matrix=parallel_matrix,
            )

        elif isinstance(value, CommentedSeq):
            job_data[key] = JobData(
                extends=None,
                needs=[],
                script=_extract_script_field(value, strict_unshallow),
                parallel_matrix=None,
            )
    return job_data


def _process_parallel(parallel: CommentedMap | None) -> list[dict[str, str]] | None:
    """Process the parallel matrix with scalar list expansion."""

    if parallel and "matrix" in parallel:
        expanded_matrix: list[dict[str, str]] = []
        for entry in parallel["matrix"]:
            expanded_entry: list[dict[str, str]] = [{}]
            for var, vals in entry.items():
                if isinstance(vals, list):
                    expanded_entry = [{**e, var: val} for e in expanded_entry for val in vals]
                else:
                    for e in expanded_entry:
                        e[var] = vals
            expanded_matrix.extend(expanded_entry)
        return expanded_matrix
    return None


def _extract_jobs(  # noqa: PLR0913
    job_name: str | None,
    job_data: dict[str, JobData],
    is_stdout: bool,
    output_dir: Path | str,
    variables: list[str],
    pure_script: bool,
    no_needs: bool,
) -> None:
    """Extract a given job or all jobs."""

    if job_name:
        if job_name not in job_data:
            raise ValueError(f"Job '{job_name}' not found in the CI file.")
        job_info = job_data[job_name]
        cmds = _get_all_cmds(job_name, no_needs, job_data, job_info.script)

        if is_stdout:
            _output_to_stdout(
                SCRIPT_PREFIX,
                variables,
                SET_WORKING_DIR,
                cmds,
                job_name,
                pure_script,
            )
        else:
            assert isinstance(output_dir, Path)
            _create_script_file(output_dir, job_name, cmds, pure_script=pure_script)
    else:
        assert isinstance(output_dir, Path)
        for job_name, job_info in job_data.items():
            cmds = _get_all_cmds(job_name, no_needs, job_data, job_info.script)
            _create_script_file(output_dir, job_name, cmds, pure_script=pure_script)


def _get_all_cmds(
    job_name: str,
    no_needs: bool,
    job_data: dict[str, JobData],
    job_cmds: list[str] | None,
) -> list[str]:
    """
    Generate a flat list of commands for the given job.

    Args:
    ----
        job_name: The name of the current job.
        no_needs: Whether to exclude prerequisite jobs given in the "needs" attribute.
        job_data: The dictionary containing job definitions and metadata.
        job_cmds: The list of script commands for the current job.

    Returns:
    -------
        The complete list of bash commands including function definitions and calls.

    """

    def collect_needs(job_name: str) -> list[str]:
        """Recursively collect all job names needed, including those from ancestors."""

        job: JobData | None = job_data.get(job_name)
        if not job:
            raise ValueError(f"Data for job '{job_name}' not found.")

        all_needs = []

        if job.extends:
            all_needs.extend(collect_needs(job.extends))

        all_needs.extend(job.needs)

        # Remove duplicates while preserving order
        return list(dict.fromkeys(all_needs))

    def wrap_job_as_function(job_name: str, job_cmds: list[str] | None) -> list[str]:
        """Wrap the commands of a job into a bash function."""

        if not job_cmds:
            return []
        function_name = f"run_{job_name}"
        function_body = INDENT + f"\n{INDENT}".join(job_cmds)
        return [f"function {function_name}() {{", function_body, "}\n"]

    def expand_parallel(
        matrix: dict[str, list[str]] | list[dict[str, str]],
    ) -> Generator[dict[str, str], None, None]:
        """Generate all environment variable combinations from the parallel matrix."""

        # If keys map to lists of values, we need to combine them
        if isinstance(matrix, dict):
            keys, values = zip(*matrix.items())
            for combination in product(*values):
                yield dict(zip(keys, combination))
        # If we already have a list of dicts, iterate directly
        elif isinstance(matrix, list):
            yield from matrix

    def generate_parallel_calls(
        job_name: str,
        parallel_matrix: list[dict[str, str]] | None,
    ) -> list[str]:
        """Generate function calls for each parallel environment combination."""

        parallel_calls = []
        if parallel_matrix:
            for env_vars in parallel_matrix:
                export_lines = [f"export {key}={value}" for key, value in env_vars.items()]
                function_call = f"run_{job_name}"
                parallel_calls.append("\n".join([*export_lines, function_call]))
        return parallel_calls

    # Collect all unique needs in order
    all_needs: list[str] = []
    if not no_needs:
        all_needs = collect_needs(job_name)

    # Create bash functions for all needed jobs
    function_definitions = []
    function_calls = []
    for needed_job in all_needs:
        needed_cmds = job_data[needed_job].script or []
        function_definitions.extend(wrap_job_as_function(needed_job, needed_cmds))
        function_calls.append(f"run_{needed_job}")

    # Process the current job
    current_job = job_data[job_name]
    current_parallel = current_job.parallel_matrix
    current_job_definition = wrap_job_as_function(job_name, job_cmds)
    if current_job_definition:
        function_definitions.extend(current_job_definition)
        if current_parallel:
            # Generate calls for parallel execution if applicable
            function_calls.extend(generate_parallel_calls(job_name, current_parallel))
        else:
            # Single call
            function_calls.append(f"run_{job_name}")

    # Combine all function definitions and function calls
    return function_definitions + function_calls


def _extract_script_field(
    script_field: CommentedSeq | str,
    strict_unshallow: bool,
) -> list[str]:
    """
    Extract the contents of a 'script' field.

    Args:
    ----
        script_field:     The script field from the job definition.
        strict_unshallow: If set, then do not adapt any "git fetch --unshallow" calls.

    Returns:
    -------
        A list of script lines.

    """
    lines: list[str] = []

    if isinstance(script_field, str):
        lines.append(script_field)
    elif isinstance(script_field, CommentedSeq):
        for line in script_field:
            if isinstance(line, str):
                s = line
                if not strict_unshallow and s == "git fetch --unshallow":
                    s = "[ ! -f .git/shallow ] || git fetch --unshallow"
                lines.append(s)
            elif isinstance(line, CommentedSeq):
                # Recursively process nested sequences
                lines.extend(_extract_script_field(line, strict_unshallow))

    return lines


def _create_script_file(
    output_dir: Path,
    job_name: str,
    cmds: list[str],
    pure_script: bool,
) -> None:
    """Generate a bash script for a job or reusable fragment."""
    script_file = output_dir / f"{job_name}.sh"
    if not cmds:
        if script_file.exists():
            script_file.unlink()
        return
    script = "\n".join(cmds)
    if not pure_script:
        script = (
            "#!/bin/bash"
            f"{textwrap.dedent(SCRIPT_PREFIX)}"
            f"{textwrap.dedent(SET_WORKING_DIR)}"
            f"\n{script}\n"
            f'echo Job \\"{job_name}\\" simulation completed successfully.'
        ).lstrip("\n")
    with script_file.open("w") as f:
        f.write(f"{script}\n")
    if not pure_script:
        script_file.chmod(0o770)


def _output_to_stdout(
    script_prefix: str,
    variables: list[str],
    set_working_dir: str,
    cmds: list[str],
    job_name: str,
    pure_script: bool,
) -> None:
    """Output the generated script to stdout."""
    output = [
        textwrap.dedent(script_prefix),
        "\n".join([f"export {v}" for v in variables]) if variables else "",
        textwrap.dedent(set_working_dir),
        "\n".join(cmds),
        f'echo Job "{job_name}" simulation completed successfully.' if not pure_script else "",
    ]
    print("\n".join(filter(None, output)))  # noqa: T201


if __name__ == "__main__":
    main()
