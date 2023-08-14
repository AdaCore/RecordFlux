#!/usr/bin/env -S python3 -O

"""Generate separate test runners for the test project."""

import argparse
import logging
import pathlib
import sys
from collections.abc import Sequence

from rflx import common

logging.basicConfig(level=logging.INFO, format="%(message)s")
logging.disable(logging.NOTSET)

TESTS = [
    "Builtin_Types",
    "Custom_Types",
    "Ethernet",
    "IPv4",
    "In_Ethernet",
    "In_IPv4",
    "TLV",
    "In_TLV",
    "Enumeration",
    "Sequence",
    "Derivation",
    "Expression",
    "Fixed_Size",
]


def main(argv: Sequence[str]) -> int:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument(
        "directory",
        metavar="DIRECTORY",
        help="output directory",
        type=pathlib.Path,
    )
    args = arg_parser.parse_args(argv[1:])

    create_test_runner(TESTS, args.directory)
    for test in TESTS:
        create_test_runner([test], args.directory)

    return 0


def create_test_runner(tests: Sequence[str], directory: pathlib.Path) -> None:
    assert len(tests) > 0
    test = tests[0] if len(tests) == 1 else None
    filename = directory / (
        f"test_{common.file_name(test)}.adb" if test else pathlib.Path("test.adb")
    )
    with_clause = "\n".join(f"with RFLX.{t}_Tests;" for t in tests)
    procedure_name = f"Test_{test}" if test else "Test"
    test_cases = "\n".join(f"      Result.all.Add_Test (new RFLX.{t}_Tests.Test);" for t in tests)

    print(f"Create {filename}")  # noqa: T201

    filename.write_text(
        f"""
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Ada.Command_Line;

{with_clause}

procedure {procedure_name} is
   pragma Warnings (Off, "use of an anonymous access type allocator");

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
{test_cases}
      return Result;
   end Suite;

   pragma Warnings (On, "use of an anonymous access type allocator");

   function Run is new AUnit.Run.Test_Runner_With_Status (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Status : AUnit.Status;

   use type AUnit.Status;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Status := Run (Reporter);
   Ada.Command_Line.Set_Exit_Status
     (if Status = AUnit.Success then Ada.Command_Line.Success else Ada.Command_Line.Failure);
end {procedure_name};
"""[
            1:
        ],
    )


if __name__ == "__main__":
    sys.exit(main(sys.argv))
