import subprocess
import textwrap
from pathlib import Path

from tests.const import DATA_DIR, SPEC_DIR, VALIDATOR_DIR


def test_check() -> None:
    p = subprocess.run(
        ["rflx", "--no-caching", "check", SPEC_DIR / "ethernet.rflx"],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        """\
        info: Parsing tests/data/specs/ethernet.rflx
        info: Processing Ethernet
        info: Verifying __BUILTINS__::Boolean
        info: Verifying __INTERNAL__::Opaque
        info: Verifying Ethernet::Address
        info: Verifying Ethernet::Type_Length
        info: Verifying Ethernet::TPID
        info: Verifying Ethernet::TCI
        info: Verifying Ethernet::Frame
        """,
    )


def test_check_error() -> None:
    p = subprocess.run(
        ["rflx", "--no-caching", "check", SPEC_DIR / "invalid" / "incorrect_name.rflx"],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 1
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        """\
        info: Parsing tests/data/specs/invalid/incorrect_name.rflx
        info: Processing Test
        info: Verifying __BUILTINS__::Boolean
        info: Verifying __INTERNAL__::Opaque
        error: source file name does not match the package name "Test"
         --> tests/data/specs/invalid/incorrect_name.rflx:1:9
          |
        1 | package Test is
          |         ^^^^
          |
        help: either rename the file to "test.rflx" or change the package name to "Incorrect_Name"
         --> tests/data/specs/invalid/incorrect_name.rflx:1:9
          |
        1 | package Test is
          |         ---- help: rename to "Incorrect_Name"
        2 |
        3 | end Test;
          |     ---- help: rename to "Incorrect_Name"
          |
          """,
    )


def test_check_no_verification() -> None:
    p = subprocess.run(
        [
            "rflx",
            "--no-caching",
            "--unsafe",
            "--no-verification",
            "check",
            SPEC_DIR / "ethernet.rflx",
        ],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        """\
        warning: model verification skipped
        info: Parsing tests/data/specs/ethernet.rflx
        info: Processing Ethernet
        """,
    )


def test_generate(tmp_path: Path) -> None:
    p = subprocess.run(
        [
            "rflx",
            "--no-caching",
            "generate",
            "-d",
            tmp_path,
            SPEC_DIR / "ethernet.rflx",
        ],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        f"""\
        info: Parsing tests/data/specs/ethernet.rflx
        info: Processing Ethernet
        info: Verifying __BUILTINS__::Boolean
        info: Verifying __INTERNAL__::Opaque
        info: Verifying Ethernet::Address
        info: Verifying Ethernet::Type_Length
        info: Verifying Ethernet::TPID
        info: Verifying Ethernet::TCI
        info: Verifying Ethernet::Frame
        info: Generating Ethernet::Address
        info: Generating Ethernet::Type_Length
        info: Generating Ethernet::TPID
        info: Generating Ethernet::TCI
        info: Generating Ethernet::Frame
        info: Creating {tmp_path}/rflx-ethernet.ads
        info: Creating {tmp_path}/rflx-ethernet-frame.ads
        info: Creating {tmp_path}/rflx-ethernet-frame.adb
        info: Creating {tmp_path}/rflx-rflx_arithmetic.ads
        info: Creating {tmp_path}/rflx-rflx_builtin_types-conversions.ads
        info: Creating {tmp_path}/rflx-rflx_builtin_types.ads
        info: Creating {tmp_path}/rflx-rflx_generic_types.ads
        info: Creating {tmp_path}/rflx-rflx_generic_types-generic_operators.ads
        info: Creating {tmp_path}/rflx-rflx_generic_types-generic_operations.ads
        info: Creating {tmp_path}/rflx-rflx_message_sequence.ads
        info: Creating {tmp_path}/rflx-rflx_scalar_sequence.ads
        info: Creating {tmp_path}/rflx-rflx_types.ads
        info: Creating {tmp_path}/rflx-rflx_types-operators.ads
        info: Creating {tmp_path}/rflx-rflx_types-operations.ads
        info: Creating {tmp_path}/rflx-rflx_arithmetic.adb
        info: Creating {tmp_path}/rflx-rflx_generic_types-generic_operations.adb
        info: Creating {tmp_path}/rflx-rflx_message_sequence.adb
        info: Creating {tmp_path}/rflx-rflx_scalar_sequence.adb
        info: Creating {tmp_path}/rflx.ads
        """,
    )
    assert (tmp_path / "rflx.ads").is_file()


def test_graph(tmp_path: Path) -> None:
    p = subprocess.run(
        [
            "rflx",
            "--no-caching",
            "graph",
            "-d",
            tmp_path,
            SPEC_DIR / "ethernet.rflx",
        ],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        f"""\
        info: Parsing tests/data/specs/ethernet.rflx
        info: Processing Ethernet
        info: Verifying __BUILTINS__::Boolean
        info: Verifying __INTERNAL__::Opaque
        info: Verifying Ethernet::Address
        info: Verifying Ethernet::Type_Length
        info: Verifying Ethernet::TPID
        info: Verifying Ethernet::TCI
        info: Verifying Ethernet::Frame
        info: Creating {tmp_path}/Ethernet_Frame.svg
        """,
    )
    assert (tmp_path / "Ethernet_Frame.svg").is_file()


def test_validate() -> None:
    p = subprocess.run(
        [
            "rflx",
            "--no-caching",
            "validate",
            "-v",
            VALIDATOR_DIR / "ethernet" / "frame" / "valid",
            SPEC_DIR / "ethernet.rflx",
            "Ethernet::Frame",
        ],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == textwrap.dedent(
        """\
        tests/data/validator/ethernet/frame/valid/802.3-LLC-CDP.raw                      PASSED
        tests/data/validator/ethernet/frame/valid/EII-802.1AD-802.1Q-IPv4.raw            PASSED
        tests/data/validator/ethernet/frame/valid/EII-802.1Q-802.1Q-IPv4-ICMP.raw        PASSED
        tests/data/validator/ethernet/frame/valid/EII-802.1Q-LLC-CDP.raw                 PASSED
        tests/data/validator/ethernet/frame/valid/EII-802.1Q-LLC-STP.raw                 PASSED
        tests/data/validator/ethernet/frame/valid/ethernet_802.3.raw                     PASSED
        tests/data/validator/ethernet/frame/valid/ethernet_double_vlan_tag.raw           PASSED
        tests/data/validator/ethernet/frame/valid/ethernet_ipv4_udp.raw                  PASSED
        tests/data/validator/ethernet/frame/valid/ethernet_vlan_tag.raw                  PASSED
        """,
    )
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        """\
        info: Parsing tests/data/specs/ethernet.rflx
        info: Processing Ethernet
        info: Verifying __BUILTINS__::Boolean
        info: Verifying __INTERNAL__::Opaque
        info: Verifying Ethernet::Address
        info: Verifying Ethernet::Type_Length
        info: Verifying Ethernet::TPID
        info: Verifying Ethernet::TCI
        info: Verifying Ethernet::Frame
        """,
    )


def test_install(tmp_path: Path) -> None:
    p = subprocess.run(
        ["rflx", "install", "gnatstudio", "--gnat-studio-dir", tmp_path],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == textwrap.dedent(
        f"""\
        Installing RecordFlux plugin into "{tmp_path}/plug-ins"
        """,
    )
    assert p.stderr.decode("utf-8") == ""
    assert (tmp_path / "plug-ins" / "recordflux.py").is_file()


def test_convert(tmp_path: Path) -> None:
    p = subprocess.run(
        ["rflx", "convert", "iana", "-d", tmp_path, DATA_DIR / "bootp-dhcp-parameters.xml"],
        capture_output=True,
        check=False,
    )
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == ""
    assert (tmp_path / "bootp_dhcp_parameters.rflx").is_file()


def test_run_ls() -> None:
    p = subprocess.run(["rflx", "run_ls"], capture_output=True, input="", check=False)
    assert p.returncode == 0
    assert p.stdout.decode("utf-8") == ""
    assert p.stderr.decode("utf-8") == textwrap.dedent(
        """\
        Registered builtin feature exit
        Registered builtin feature initialize
        Registered builtin feature initialized
        Registered builtin feature notebookDocument/didChange
        Registered builtin feature notebookDocument/didClose
        Registered builtin feature notebookDocument/didOpen
        Registered builtin feature $/setTrace
        Registered builtin feature shutdown
        Registered builtin feature textDocument/didChange
        Registered builtin feature textDocument/didClose
        Registered builtin feature textDocument/didOpen
        Registered builtin feature window/workDoneProgress/cancel
        Registered builtin feature workspace/didChangeWorkspaceFolders
        Registered builtin feature workspace/executeCommand
        Registered "textDocument/didOpen" with options "None"
        Registered "textDocument/didSave" with options "None"
        Registered "textDocument/didChange" with options "None"
        Registered "textDocument/definition" with options "None"
        Registered "textDocument/semanticTokens/full" with options "SemanticTokensLegend(token_types=['type', 'enum', 'enumMember', 'struct', 'property', 'keyword', 'class', 'namespace', 'event', 'method', 'parameter', 'variable'], token_modifiers=[])"
        Command "showMessageGraph" is successfully registered.
        Registered "textDocument/codeLens" with options "None"
        Starting IO server
        Shutting down the server
        Closing the event loop.
        """,  # noqa: E501
    )
