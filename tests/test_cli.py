from tempfile import TemporaryDirectory
from unittest import TestCase, mock

from rflx import cli
from rflx.model import ModelError


class TestRflx(TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_main_noarg(self) -> None:
        self.assertEqual(cli.main(['rflx']),
                         2)

    def test_main_help(self) -> None:
        with self.assertRaises(SystemExit):
            cli.main(['rflx', '-h'])

    def test_main_check(self) -> None:
        self.assertEqual(cli.main(['rflx', 'check', 'specs/tlv.rflx']),
                         0)

    def test_main_check_parser_error(self) -> None:
        self.assertRegex(str(cli.main(['rflx', 'check', 'README.md'])),
                         r'parser error: ')

    def test_main_check_model_error(self) -> None:
        with mock.patch('rflx.cli.check', lambda x: raise_model_error()):
            self.assertRegex(str(cli.main(['rflx', 'check', 'README.md'])),
                             r'model error: ')

    def test_main_check_non_existent_file(self) -> None:
        self.assertRegex(str(cli.main(['rflx', 'check', 'non-existent file'])),
                         r'error: file not found: "non-existent file"$')

    def test_main_generate(self) -> None:
        tmpdir = TemporaryDirectory()
        self.assertEqual(cli.main(['rflx', 'generate', 'specs/tlv.rflx', tmpdir.name]),
                         0)

    def test_main_generate_prefix(self) -> None:
        tmpdir = TemporaryDirectory()
        self.assertEqual(cli.main(['rflx', 'generate', '-p', '', 'specs/tlv.rflx', tmpdir.name]),
                         0)

    def test_main_generate_no_output_files(self) -> None:
        tmpdir = TemporaryDirectory()
        self.assertEqual(cli.main(['rflx', 'generate', '-d', 'tests/package.rflx', tmpdir.name]),
                         0)

    def test_main_generate_non_existent_directory(self) -> None:
        self.assertRegex(str(cli.main(['rflx', 'generate', 'specs/tlv.rflx',
                                       'non-existent directory'])),
                         r'error: directory not found: "non-existent directory"$')

    def test_main_generate_missing_template_direcotry(self) -> None:
        tmpdir = TemporaryDirectory()
        with mock.patch('pkg_resources.resource_filename', lambda *x: 'non-existent directory'):
            self.assertRegex(str(cli.main(['rflx', 'generate', '-l', tmpdir.name])),
                             r'internal error: library directory not found$')

    def test_main_generate_missing_template_files(self) -> None:
        tmpdir = TemporaryDirectory()
        with mock.patch('pkg_resources.resource_filename', lambda *x: tmpdir.name):
            self.assertRegex(str(cli.main(['rflx', 'generate', '-l', tmpdir.name])),
                             r'internal error: library file not found')


def raise_model_error() -> None:
    raise ModelError
