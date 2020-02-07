from pathlib import Path
from tempfile import TemporaryDirectory
from unittest import TestCase, mock

from rflx import cli
from rflx.model import ModelError


class TestCLI(TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_main_noarg(self) -> None:
        self.assertEqual(cli.main(["rflx"]), 2)

    def test_main_help(self) -> None:
        with self.assertRaises(SystemExit):
            cli.main(["rflx", "-h"])

    def test_main_version(self) -> None:
        self.assertEqual(cli.main(["rflx", "--version"]), 0)

    def test_main_check(self) -> None:
        self.assertEqual(cli.main(["rflx", "check", "specs/tlv.rflx"]), 0)

    def test_main_check_parser_error(self) -> None:
        self.assertRegex(str(cli.main(["rflx", "check", "README.md"])), r"parser error: ")

    def test_main_check_model_error(self) -> None:
        with mock.patch("rflx.cli.check", lambda x: raise_model_error()):
            self.assertRegex(str(cli.main(["rflx", "check", "README.md"])), r"model error: ")

    def test_main_check_non_existent_file(self) -> None:
        self.assertRegex(
            str(cli.main(["rflx", "check", "non-existent file"])),
            r'error: file not found: "non-existent file"$',
        )

    def test_main_generate(self) -> None:
        with TemporaryDirectory() as tmpdir:
            self.assertEqual(cli.main(["rflx", "generate", "-d", tmpdir, "specs/tlv.rflx"]), 0)
            top_level_package = Path(tmpdir) / (cli.DEFAULT_PREFIX.lower() + ".ads")
            self.assertTrue(top_level_package.exists())

    def test_main_generate_prefix(self) -> None:
        with TemporaryDirectory() as tmpdir:
            for prefix in ["", "A", "A.B", "A.B.C"]:
                self.assertEqual(
                    cli.main(["rflx", "generate", "-d", tmpdir, "-p", prefix, "specs/tlv.rflx"]), 0
                )
                top_level_package = Path(tmpdir) / (prefix.replace(".", "-").lower() + ".ads")
                self.assertFalse(top_level_package.exists())

    def test_main_generate_invalid_prefix(self) -> None:
        with TemporaryDirectory() as tmpdir:
            for prefix in [".", "A.B.", ".A.B", "A..B"]:
                self.assertIn(
                    rf'invalid prefix: "{prefix}"',
                    str(
                        cli.main(["rflx", "generate", "-d", tmpdir, "-p", prefix, "specs/tlv.rflx"])
                    ),
                )

    def test_main_generate_no_output_files(self) -> None:
        with TemporaryDirectory() as tmpdir:
            self.assertEqual(cli.main(["rflx", "generate", "-d", tmpdir, "tests/package.rflx"]), 0)

    def test_main_generate_non_existent_directory(self) -> None:
        self.assertRegex(
            str(cli.main(["rflx", "generate", "-d", "non-existent directory", "specs/tlv.rflx"])),
            r'error: directory not found: "non-existent directory"$',
        )

    def test_main_generate_missing_template_directory(self) -> None:
        with TemporaryDirectory() as tmpdir:
            with mock.patch("pkg_resources.resource_filename", lambda *x: "non-existent directory"):
                self.assertRegex(
                    str(cli.main(["rflx", "generate", "-d", tmpdir, "specs/tlv.rflx"])),
                    r"internal error: template directory not found$",
                )

    def test_main_generate_missing_template_files(self) -> None:
        with TemporaryDirectory() as tmpdir:
            with mock.patch("pkg_resources.resource_filename", lambda *x: tmpdir):
                self.assertRegex(
                    str(cli.main(["rflx", "generate", "-d", tmpdir, "specs/tlv.rflx"])),
                    r"internal error: template file not found",
                )

    def test_main_graph(self) -> None:
        with TemporaryDirectory() as tmpdir:
            self.assertEqual(cli.main(["rflx", "graph", "-d", tmpdir, "specs/tlv.rflx"]), 0)

    def test_main_graph_non_existent_file(self) -> None:
        with TemporaryDirectory() as tmpdir:
            self.assertRegex(
                str(cli.main(["rflx", "graph", "-d", tmpdir, "non-existent file"])),
                r'error: file not found: "non-existent file"$',
            )

    def test_main_graph_non_existent_directory(self) -> None:
        self.assertRegex(
            str(cli.main(["rflx", "graph", "-d", "non-existent directory", "specs/tlv.rflx"])),
            r'error: directory not found: "non-existent directory"$',
        )

    def test_main_graph_no_output_files(self) -> None:
        with TemporaryDirectory() as tmpdir:
            self.assertEqual(cli.main(["rflx", "graph", "-d", tmpdir, "tests/package.rflx"]), 0)


def raise_model_error() -> None:
    raise ModelError
