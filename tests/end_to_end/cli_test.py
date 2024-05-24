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
        Parsing tests/data/specs/ethernet.rflx
        Processing Ethernet
        Verifying __BUILTINS__::Boolean
        Verifying __INTERNAL__::Opaque
        Verifying Ethernet::Address
        Verifying Ethernet::Type_Length
        Verifying Ethernet::TPID
        Verifying Ethernet::TCI
        Verifying Ethernet::Frame
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
        Parsing tests/data/specs/invalid/incorrect_name.rflx
        Processing Test
        Verifying __BUILTINS__::Boolean
        Verifying __INTERNAL__::Opaque
        error: source file name does not match the package name "Test"
         --> tests/data/specs/invalid/incorrect_name.rflx:1:9
          |
        1 | package Test is
          |         ^^^^
          |
        info: either rename the file to "test.rflx"
         --> tests/data/specs/invalid/incorrect_name.rflx:1:9
          |
        1 | package Test is
          |         ----
          |
        info: or change the package name to "Incorrect_Name"
         --> tests/data/specs/invalid/incorrect_name.rflx:1:9
          |
        1 | package Test is
          |         ----
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
        Parsing tests/data/specs/ethernet.rflx
        Processing Ethernet
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
        Parsing tests/data/specs/ethernet.rflx
        Processing Ethernet
        Verifying __BUILTINS__::Boolean
        Verifying __INTERNAL__::Opaque
        Verifying Ethernet::Address
        Verifying Ethernet::Type_Length
        Verifying Ethernet::TPID
        Verifying Ethernet::TCI
        Verifying Ethernet::Frame
        Generating Ethernet::Address
        Generating Ethernet::Type_Length
        Generating Ethernet::TPID
        Generating Ethernet::TCI
        Generating Ethernet::Frame
        Creating {tmp_path}/rflx-ethernet.ads
        Creating {tmp_path}/rflx-ethernet-frame.ads
        Creating {tmp_path}/rflx-ethernet-frame.adb
        Creating {tmp_path}/rflx-rflx_arithmetic.ads
        Creating {tmp_path}/rflx-rflx_builtin_types-conversions.ads
        Creating {tmp_path}/rflx-rflx_builtin_types.ads
        Creating {tmp_path}/rflx-rflx_generic_types.ads
        Creating {tmp_path}/rflx-rflx_generic_types-generic_operators.ads
        Creating {tmp_path}/rflx-rflx_generic_types-generic_operations.ads
        Creating {tmp_path}/rflx-rflx_message_sequence.ads
        Creating {tmp_path}/rflx-rflx_scalar_sequence.ads
        Creating {tmp_path}/rflx-rflx_types.ads
        Creating {tmp_path}/rflx-rflx_types-operators.ads
        Creating {tmp_path}/rflx-rflx_types-operations.ads
        Creating {tmp_path}/rflx-rflx_arithmetic.adb
        Creating {tmp_path}/rflx-rflx_generic_types-generic_operations.adb
        Creating {tmp_path}/rflx-rflx_message_sequence.adb
        Creating {tmp_path}/rflx-rflx_scalar_sequence.adb
        Creating {tmp_path}/rflx.ads
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
        Parsing tests/data/specs/ethernet.rflx
        Processing Ethernet
        Verifying __BUILTINS__::Boolean
        Verifying __INTERNAL__::Opaque
        Verifying Ethernet::Address
        Verifying Ethernet::Type_Length
        Verifying Ethernet::TPID
        Verifying Ethernet::TCI
        Verifying Ethernet::Frame
        Creating {tmp_path}/Ethernet_Frame.svg
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
        Parsing tests/data/specs/ethernet.rflx
        Processing Ethernet
        Verifying __BUILTINS__::Boolean
        Verifying __INTERNAL__::Opaque
        Verifying Ethernet::Address
        Verifying Ethernet::Type_Length
        Verifying Ethernet::TPID
        Verifying Ethernet::TCI
        Verifying Ethernet::Frame
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
        Starting IO server
        Shutting down the server
        Closing the event loop.
        """,
    )
