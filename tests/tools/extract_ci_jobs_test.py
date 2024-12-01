import subprocess
import textwrap
from pathlib import Path

import pytest

from tools.extract_ci_jobs import _process_ci_file


@pytest.fixture()
def ci_file(tmp_path: Path) -> Path:
    content = """
    variables:
      VAR1: value1
      VAR2_ORIGIN: value2
    job11:
      script:
        - echo "Job 11"
    job12:
      script:
        - echo "Job 12"
    job13:
      script:
        - echo "Job 13"
    job21:
      needs:
        - job11
        - job12
      script:
        - echo "Job 21"
    job31:
      extends: job21
      needs:
        - job13
      script:
        - echo "Job 31"
        - git fetch --unshallow
    job41:
      parallel:
        matrix:
          - VAR_A:
            - val1
            - val2
      script:
        - echo "Job 41 with VAR_A=$VAR_A"
    job42:
      parallel:
        matrix:
          - VAR_A:
            - valA1
            - valA2
          - VAR_B:
            - valB1
            - valB2
      script:
        - echo "Job 42 with VAR_A=$VAR_A, VAR_B=$VAR_B"
    """
    file_path = tmp_path / ".gitlab-ci.yml"
    file_path.write_text(content)
    return file_path


@pytest.mark.parametrize(
    (
        "job_name",
        "no_needs",
        "pure_script",
        "strict_unshallow",
        "expected_file_name",
        "expected_content",
    ),
    [
        pytest.param(
            "job31",
            True,
            False,
            False,
            "job31.sh",
            r"""
               #!/bin/bash
               set -euxo pipefail

               if [ -z "${CI_PROJECT_DIR}" ]; then
                   echo "Error: CI_PROJECT_DIR environment variable is not set."
                   exit 1
               fi
               cd "${CI_PROJECT_DIR}" || \
                   { echo "Error: Could not change directory to ${CI_PROJECT_DIR}"; exit 1; }

               function run_job31() {
                   echo "Job 31"
                   [ ! -f .git/shallow ] || git fetch --unshallow
               }

               run_job31
               echo Job \"job31\" simulation completed successfully.
            """,
            id="job31_no_needs",
        ),
        pytest.param(
            "job31",
            False,
            False,
            True,
            "job31.sh",
            r"""
               #!/bin/bash
               set -euxo pipefail

               if [ -z "${CI_PROJECT_DIR}" ]; then
                   echo "Error: CI_PROJECT_DIR environment variable is not set."
                   exit 1
               fi
               cd "${CI_PROJECT_DIR}" || \
                   { echo "Error: Could not change directory to ${CI_PROJECT_DIR}"; exit 1; }

               function run_job11() {
                   echo "Job 11"
               }

               function run_job12() {
                   echo "Job 12"
               }

               function run_job13() {
                   echo "Job 13"
               }

               function run_job31() {
                   echo "Job 31"
                   git fetch --unshallow
               }

               run_job11
               run_job12
               run_job13
               run_job31
               echo Job \"job31\" simulation completed successfully.
            """,
            id="job31_no_needs",
        ),
        pytest.param(
            "job41",
            True,
            True,
            False,
            "job41.sh",
            r"""
            function run_job41() {
                echo "Job 41 with VAR_A=$VAR_A"
            }

            export VAR_A=val1
            run_job41
            export VAR_A=val2
            run_job41
            """,
            id="job41",
        ),
        pytest.param(
            "job42",
            False,
            True,
            False,
            "job42.sh",
            r"""
            function run_job42() {
                echo "Job 42 with VAR_A=$VAR_A, VAR_B=$VAR_B"
            }

            export VAR_A=valA1
            run_job42
            export VAR_A=valA2
            run_job42
            export VAR_B=valB1
            run_job42
            export VAR_B=valB2
            run_job42
            """,
            id="job42",
        ),
    ],
)
def test_process_ci_file_single_job(  # noqa: PLR0913
    ci_file: Path,
    tmp_path: Path,
    job_name: str,
    no_needs: bool,
    pure_script: bool,
    strict_unshallow: bool,
    expected_file_name: str,
    expected_content: str,
) -> None:
    output_dir = tmp_path / "output"
    _process_ci_file(
        input_path=str(ci_file),
        output_dir=str(output_dir),
        job_name=job_name,
        no_needs=no_needs,
        strict_unshallow=strict_unshallow,
        pure_script=pure_script,
    )
    script_path = output_dir / expected_file_name
    assert script_path.exists()
    script_content = script_path.read_text()
    expected = textwrap.dedent(expected_content).lstrip()
    assert expected == script_content


def test_process_ci_file_all_jobs(ci_file: Path, tmp_path: Path) -> None:
    output_dir = tmp_path / "output"
    _process_ci_file(
        input_path=str(ci_file),
        output_dir=str(output_dir),
        job_name=None,
        no_needs=False,
        keep_origin_vars=True,
        strict_unshallow=False,
        pure_script=False,
        extra_vars=[],
    )
    for job_name in ["job11", "job12", "job13", "job21", "job31", "job41", "job42"]:
        script_path = output_dir / f"{job_name}.sh"
        assert script_path.exists()
    variables_path = output_dir / "variables"
    assert variables_path.exists()


@pytest.mark.parametrize(
    ("keep_origin_vars", "extra_vars", "expected_content"),
    [
        pytest.param(
            True,
            [],
            r"""
               export VAR1='value1'
               export VAR2_ORIGIN='value2'
            """,
            id="1",
        ),
        pytest.param(
            False,
            [],
            r"""
               export VAR1='value1'
            """,
            id="2",
        ),
        pytest.param(
            True,
            ["X=1", "Y=2"],
            r"""
               export VAR1='value1'
               export VAR2_ORIGIN='value2'
               export X=1
               export Y=2
            """,
            id="3",
        ),
    ],
)
def test_process_ci_file_variables(
    ci_file: Path,
    tmp_path: Path,
    keep_origin_vars: bool,
    extra_vars: list[str],
    expected_content: str,
) -> None:
    output_dir = tmp_path / "output"
    _process_ci_file(
        input_path=str(ci_file),
        output_dir=str(output_dir),
        job_name=None,
        no_needs=False,
        keep_origin_vars=keep_origin_vars,
        strict_unshallow=False,
        pure_script=False,
        extra_vars=extra_vars,
    )
    variables_path = output_dir / "variables"
    assert variables_path.exists()
    variables_content = variables_path.read_text()
    expected = textwrap.dedent(expected_content).lstrip()
    assert expected == variables_content


def test_extract_ci_jobs_current_project(tmp_path: Path) -> None:
    ci_file_path = Path(__file__).parent.parent.parent / ".gitlab-ci.yml"
    script_path = Path(__file__).parent.parent.parent / "tools" / "extract_ci_jobs.py"

    result = subprocess.run(
        [
            "python3",
            str(script_path),
            str(ci_file_path),
            "--output",
            str(tmp_path),
        ],
        capture_output=True,
        text=True,
        check=False,
    )

    expected_output = textwrap.dedent(
        f"""
            INFO: Extraction complete. Scripts and variables saved to: {tmp_path}
         """,
    ).lstrip()
    actual_output = result.stdout + result.stderr

    assert (
        result.returncode == 0
    ), f"Script failed with return code {result.returncode}. Output:\n{actual_output}"

    assert (
        expected_output.strip() in actual_output.strip()
    ), f"Expected: {expected_output.strip()}, but got: {actual_output.strip()}"
