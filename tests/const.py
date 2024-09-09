from pathlib import Path
from typing import Final

TEST_DIR = Path("tests")
DATA_DIR = TEST_DIR / "data"
CAPTURED_DIR = DATA_DIR / "captured"
FIXTURE_DIR = DATA_DIR / "fixtures"
SPEC_DIR = DATA_DIR / "specs"
IDE_DIR = TEST_DIR / "ide"
GENERATED_DIR = TEST_DIR / "spark" / "generated"
FEATURE_DIR = TEST_DIR / "feature"
VALIDATOR_DIR = DATA_DIR / "validator"

EX_SPEC_DIR = Path("examples/specs")
MAIN = "main.adb"
STATE_MACHINE_NAME = "S"

GITHUB_TRACKER_REF_PATTERN: Final = (
    r".*GitHub.*https://github.com/AdaCore/RecordFlux/issues/new\?labels=bug.*"
)
GNAT_TRACKER_REF_PATTERN: Final = r".*GNATtracker.*https://support.adacore.com/csm.*"
