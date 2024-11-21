from __future__ import annotations

import argparse
import importlib.resources
import json
import logging as py_logging
import os
import shutil
import subprocess
import sys
from collections.abc import Sequence
from enum import Enum
from importlib.abc import Traversable
from multiprocessing import cpu_count
from pathlib import Path
from typing import Final

from rflx.common import assert_never
from rflx.converter import iana
from rflx.error import (
    FatalError,
    fail,
)
from rflx.fatal_error import FatalErrorHandler
from rflx.generator import Debug, Generator, optimizer
from rflx.graph import create_message_graph, create_state_machine_graph, write_graph
from rflx.identifier import ID
from rflx.integration import Integration
from rflx.model import AlwaysVerify, Cache, Message, Model, NeverVerify, StateMachine
from rflx.pyrflx import PyRFLXError
from rflx.rapidflux import NO_LOCATION, ErrorEntry, RecordFluxError, Severity, logging
from rflx.specification import Parser
from rflx.validator import ValidationError, Validator
from rflx.version import version

DEFAULT_PREFIX = "RFLX"
DOC_DIR: Final[Path] = Path(str(importlib.resources.files("rflx"))) / "doc"

py_logging.basicConfig(level=py_logging.INFO, format="%(message)s")


class IDE(Enum):
    GNATSTUDIO = "gnatstudio"
    VSCODE = "vscode"
    NVIM = "nvim"
    VIM = "vim"

    def __str__(self) -> str:
        return self.value

    @staticmethod
    def ide(string: str) -> IDE:
        try:
            return IDE[string.upper()]
        except KeyError:
            raise ValueError from None


class UniqueStore(argparse.Action):
    """
    Action which allows at most one occurrence of the given option-value pair.

    The argparse module doesn't check for duplicate optional arguments having action 'store' (the
    default). However, such usage doesn't make sense (the value of the last matching argument
    remains, others are overwritten and ignored). This custom action class mimics the 'store' action
    in every other respect, but additionally prohibits such usage.

    Note: In argparse the 'store' action logic is implemented in argparse._StoreAction. However,
    since that is a protected class we can't directly inherit from it.
    """

    def __init__(  # type: ignore[no-untyped-def] # noqa: PLR0913
        self,
        option_strings,
        dest,
        nargs=None,
        const=None,
        default=None,
        type=None,  # noqa: A002
        choices=None,
        required=False,
        help=None,  # noqa: A002
        metavar=None,
    ):
        # Keep track whether the option has been set already
        self.is_set = False
        # Perform the 'store' action-related checks.
        # Note: This should be equivalent to the logic in argparse._StoreAction__init__(..).
        if nargs == 0:
            raise ValueError(
                "nargs for store actions must be != 0; if you "
                "have nothing to store, actions such as store "
                "true or store const may be more appropriate",
            )
        if const is not None and nargs != argparse.OPTIONAL:
            raise ValueError("nargs must be %r to supply const" % argparse.OPTIONAL)

        super().__init__(
            option_strings=option_strings,
            dest=dest,
            nargs=nargs,
            const=const,
            default=default,
            type=type,
            choices=choices,
            required=required,
            help=help,
            metavar=metavar,
        )

    def __call__(  # type: ignore [no-untyped-def]
        self,
        parser,
        namespace,
        values,
        option_string=None,
    ):
        if self.is_set:
            parser.error(f"{option_string} appears several times")
        else:
            self.is_set = True
            # Perform the 'store' action.
            # Note: This should be equivalent to the logic in argparse._StoreAction__call__(..).
            setattr(namespace, self.dest, values)


def run() -> int:
    return main(sys.argv)


def main(  # noqa: PLR0915
    argv: Sequence[str],
) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help="disable logging to standard output",
    )
    parser.add_argument("--version", action="store_true")
    parser.add_argument(
        "--no-caching",
        action="store_true",
        help=("ignore verification cache"),
    )
    parser.add_argument(
        "--no-verification",
        action="store_true",
        help=("skip time-consuming verification of model"),
    )
    parser.add_argument(
        "--max-errors",
        action=UniqueStore,
        type=int,
        default=0,
        metavar=("NUM"),
        help="exit after at most NUM errors",
    )
    parser.add_argument(
        "--workers",
        action=UniqueStore,
        type=int,
        default=cpu_count(),
        metavar=("NUM"),
        help="parallelize proofs among NUM workers (default: %(default)d)",
    )
    parser.add_argument(
        "--unsafe",
        action="store_true",
        help="allow unsafe options (WARNING: may lead to erronous behavior)",
    )
    parser.add_argument(
        "--legacy-errors",
        action="store_true",
        help="use old error message format",
    )

    subparsers = parser.add_subparsers(dest="subcommand")

    parser_check = subparsers.add_parser("check", help="check specification")
    parser_check.add_argument(
        "files",
        metavar="SPECIFICATION_FILE",
        type=Path,
        nargs="+",
        help="specification file",
    )
    parser_check.set_defaults(func=check)

    parser_generate = subparsers.add_parser("generate", help="generate code")
    parser_generate.add_argument(
        "-p",
        "--prefix",
        action=UniqueStore,
        type=str,
        default=DEFAULT_PREFIX,
        help="add prefix to generated packages (default: %(default)s)",
    )
    parser_generate.add_argument(
        "-n",
        "--no-library",
        help="omit generating library files",
        action="store_true",
    )
    parser_generate.add_argument(
        "-d",
        action=UniqueStore,
        dest="output_directory",
        type=Path,
        default=".",
        help="output directory",
    )
    parser_generate.add_argument(
        "--debug",
        action=UniqueStore,
        default=None,
        choices=["built-in", "external"],
        help="enable adding of debug output to generated code",
    )
    parser_generate.add_argument(
        "--ignore-unsupported-checksum",
        help="ignore checksum aspects during code generation",
        action="store_true",
    )
    parser_generate.add_argument(
        "--integration-files-dir",
        action=UniqueStore,
        help="directory for the .rfi files",
        type=Path,
    )
    parser_generate.add_argument(
        "--reproducible",
        action="store_true",
        help="ensure reproducible output",
    )
    parser_generate.add_argument(
        "files",
        metavar="SPECIFICATION_FILE",
        type=Path,
        nargs="*",
        help="specification file",
    )
    parser_generate.set_defaults(func=generate)

    parser_optimize = subparsers.add_parser(
        "optimize",
        help="optimize generated state machine code",
    )
    parser_optimize.add_argument(
        "project_file",
        metavar="PROJECT_FILE",
        type=Path,
        help="project file",
    )
    parser_optimize.set_defaults(func=optimize)

    parser_graph = subparsers.add_parser("graph", help="generate graphs")
    parser_graph.add_argument(
        "-f",
        "--format",
        action=UniqueStore,
        type=str,
        default="svg",
        choices=["dot", "jpg", "pdf", "png", "raw", "svg"],
        help="output format (default: %(default)s)",
    )
    parser_graph.add_argument(
        "-i",
        "--ignore",
        type=str,
        metavar="REGEX",
        action="append",
        help="ignore states with names matching regular expression",
    )
    parser_graph.add_argument(
        "files",
        metavar="SPECIFICATION_FILE",
        type=Path,
        nargs="+",
        help="specification file",
    )
    parser_graph.add_argument(
        "-d",
        action=UniqueStore,
        dest="output_directory",
        type=Path,
        default=".",
        help="output directory",
    )
    parser_graph.set_defaults(func=graph)

    parser_validate = subparsers.add_parser(
        "validate",
        help="validate specification against a set of known valid or invalid messages",
    )
    parser_validate.add_argument(
        "--split-disjunctions",
        action="store_true",
        help=("split disjunctions before model validation (may have severe performance impact)"),
    )
    parser_validate.add_argument(
        "specification",
        metavar="SPECIFICATION_FILE",
        type=Path,
        help="specification file",
    )
    parser_validate.add_argument(
        "message_identifier",
        metavar="MESSAGE_IDENTIFIER",
        type=str,
        help="identifier of the top-level message (e.g., Package::Message)",
    )
    parser_validate.add_argument(
        "-v",
        action="append",
        dest="valid_sample_path",
        type=Path,
        help="known valid sample file or directory",
        default=None,
    )
    parser_validate.add_argument(
        "-i",
        action="append",
        dest="invalid_sample_path",
        type=Path,
        help="known invalid sample file or directory",
        default=None,
    )
    parser_validate.add_argument(
        "-c",
        action=UniqueStore,
        dest="checksum_module",
        type=str,
        help="name of the module containing the checksum functions",
        default=None,
    )
    parser_validate.add_argument(
        "-o",
        action=UniqueStore,
        dest="output_file",
        type=Path,
        help="path to output file for validation report in JSON format (file must not exist)",
        default=None,
    )
    parser_validate.add_argument(
        "--abort-on-error",
        action="store_true",
        help=(
            "abort with exitcode 1 if a message is classified as a false positive or false negative"
        ),
    )
    parser_validate.add_argument(
        "--coverage",
        action="store_true",
        help=(
            "enable coverage calculation and print the combined link coverage"
            " of all provided messages"
        ),
    )
    parser_validate.add_argument(
        "--target-coverage",
        action=UniqueStore,
        metavar="PERCENTAGE",
        type=float,
        default=0,
        help="abort with exitcode 1 if the coverage threshold is not reached",
    )
    parser_validate.set_defaults(func=validate)

    parser_install = subparsers.add_parser("install", help="set up RecordFlux IDE integration")
    parser_install.add_argument(
        "ide",
        type=IDE.ide,
        choices=[
            ide
            for ide in IDE
            if ide != IDE.VSCODE or vscode_extension().is_file() or os.environ.get("RFLX_TESTING")
        ],
    )
    parser_install.add_argument(
        "--gnat-studio-dir",
        action=UniqueStore,
        dest="gnat_studio_dir",
        type=Path,
        help="path to the GNAT Studio settings directory (default: $HOME/.gnatstudio)",
        default=Path.home() / ".gnatstudio",
    )
    parser_install.set_defaults(func=install)

    parser_convert = subparsers.add_parser(
        "convert",
        help="convert foreign specifications into RecordFlux specifications",
    )
    parser_convert.add_argument(
        "--reproducible",
        action="store_true",
        help="ensure reproducible output",
    )

    convert_subparsers = parser_convert.add_subparsers(dest="subcommand")

    parser_iana = convert_subparsers.add_parser(
        "iana",
        help="convert IANA registry into RecordFlux specifications",
    )

    parser_iana.add_argument(
        "-a",
        "--always-valid",
        help="add an Always_Valid aspect to each type that does not "
        "explicitly cover all possible values for the type's length",
        action="store_true",
    )
    parser_iana.add_argument(
        "-d",
        action=UniqueStore,
        dest="output_directory",
        type=Path,
        default=".",
        help="output directory",
    )
    parser_iana.add_argument("file", type=argparse.FileType("r"), nargs="?", default=sys.stdin)
    parser_iana.set_defaults(func=convert_iana)

    parser_run_ls = subparsers.add_parser("run_ls", help="run language server")
    parser_run_ls.set_defaults(func=run_language_server)

    parser_doc = subparsers.add_parser("doc", help="open documentation")
    parser_doc.add_argument(
        "DOCUMENT",
        help="documentation to open: Language Reference Manual (lrm), User's Guide (ug)",
        choices=["lrm", "ug"],
    )
    parser_doc.set_defaults(func=doc)

    args = parser.parse_args(argv[1:])

    if args.version:
        print(version())  # noqa: T201
        return 0

    if not args.subcommand:
        parser.print_usage()
        return 2

    if args.quiet:
        logging.set_quiet(value=True)

    if not args.unsafe and args.no_verification:
        RecordFluxError(
            [
                ErrorEntry(
                    'unsafe option "--no-verification" given without "--unsafe"',
                    Severity.ERROR,
                    NO_LOCATION,
                ),
            ],
        ).print_messages()
        return 1

    RecordFluxError.set_max_error(args.max_errors)

    with FatalErrorHandler(
        lambda msg: print(msg, file=sys.stderr),  # noqa: T201
        unsafe=args.unsafe,
    ):
        try:
            args.func(args)
        except RecordFluxError as e:
            if args.legacy_errors:
                print(e, file=sys.stderr)  # noqa: T201
            else:
                e.print_messages()
            return 1

    return 0


def check(args: argparse.Namespace) -> None:
    parse(args.files, args.no_caching, args.no_verification, args.workers)


def generate(args: argparse.Namespace) -> None:
    # Eng/RecordFlux/Workarounds#28
    args.prefix = args.prefix if args.prefix != " " else ""

    if args.prefix and "" in args.prefix.split("."):
        fail(f'invalid prefix: "{args.prefix}"')

    if not args.output_directory.is_dir():
        fail(f'directory not found: "{args.output_directory}"')

    model, integration = parse(
        args.files,
        args.no_caching,
        args.no_verification,
        args.workers,
        args.integration_files_dir,
    )

    Generator(
        args.prefix,
        workers=args.workers,
        reproducible=args.reproducible,
        debug=(
            Debug.BUILTIN
            if args.debug == "built-in"
            else Debug.EXTERNAL if args.debug == "external" else Debug.NONE
        ),
        ignore_unsupported_checksum=args.ignore_unsupported_checksum,
    ).generate(
        model,
        integration,
        args.output_directory,
        library_files=not args.no_library,
        top_level_package=args.prefix == DEFAULT_PREFIX,
    )


def optimize(args: argparse.Namespace) -> None:
    if not args.project_file.is_file():
        fail(f'project file not found: "{args.project_file}"')

    optimizer.optimize(args.project_file)


def parse(
    files: Sequence[Path],
    no_caching: bool,
    no_verification: bool,
    workers: int = 1,
    integration_files_dir: Path | None = None,
) -> tuple[Model, Integration]:
    parser = Parser(
        cache(no_caching, no_verification),
        workers=workers,
        integration_files_dir=integration_files_dir,
    )
    error = RecordFluxError()
    present_files = []

    for f in files:
        if not f.is_file():
            error.push(ErrorEntry(f'file not found: "{f}"', Severity.ERROR, NO_LOCATION))
            continue

        present_files.append(Path(f))

    try:
        parser.parse(*present_files)
    except RecordFluxError as e:
        error.extend(e.entries)

    try:
        model = parser.create_model()
    except RecordFluxError as e:
        error.extend(e.entries)

    error.propagate()
    return model, parser.get_integration()


def graph(args: argparse.Namespace) -> None:
    if not args.output_directory.is_dir():
        fail(f'directory not found: "{args.output_directory}"')

    model, _ = parse(args.files, args.no_caching, args.no_verification)

    for d in model.declarations:
        filename = args.output_directory.joinpath(d.identifier.flat).with_suffix(f".{args.format}")
        if isinstance(d, Message):
            write_graph(create_message_graph(d), filename, fmt=args.format)
        if isinstance(d, StateMachine):
            write_graph(create_state_machine_graph(d, args.ignore), filename, fmt=args.format)

    locations: dict[str, dict[str, dict[str, dict[str, int]]]] = {
        str(package.location.source): {
            d.identifier.flat: {
                "start": {"line": d.location.start[0], "column": d.location.start[1]},
                "end": {"line": d.location.end[0], "column": d.location.end[1]},
            }
            for d in declarations
            if isinstance(d, (Message, StateMachine)) and d.location and d.location.end
        }
        for package, declarations in model.packages.items()
        if package.location
    }

    filename = args.output_directory.joinpath("locations.json")
    with filename.open("w", encoding="utf-8") as f:
        json.dump(locations, f)


def validate(args: argparse.Namespace) -> None:
    try:
        identifier = ID(args.message_identifier)
    except FatalError:
        fail(f'invalid identifier "{args.message_identifier}"')

    try:
        Validator(
            [args.specification],
            args.checksum_module,
            cache(args.no_caching, args.no_verification),
            split_disjunctions=args.split_disjunctions,
        ).validate(
            identifier,
            args.invalid_sample_path,
            args.valid_sample_path,
            args.output_file,
            args.abort_on_error,
            args.coverage,
            args.target_coverage,
        )
    except ValidationError as e:
        fail(str(e))
    except PyRFLXError as e:
        fatal_error = FatalError(e.__str__())
        raise fatal_error from e


def install_vim_ftdetect(config_dir: Path, editor: str) -> None:
    ftdetect_dir = config_dir / "ftdetect"
    ftdetect_file = ftdetect_dir / "recordflux.vim"
    ftdetect_dir.mkdir(parents=True, exist_ok=True)
    ftdetect_file.write_text("autocmd BufRead,BufNewFile *.rflx set filetype=recordflux\n")

    logging.info('Installed {editor} ftdetect file in "{path}"', editor=editor, path=ftdetect_file)


def install_syntax_file(config_dir: Path, editor: str) -> None:
    syntax_dir = config_dir / "syntax"
    syntax_dir.mkdir(parents=True, exist_ok=True)
    with importlib.resources.as_file(vim_syntax_file()) as syntax_file:
        file_path = syntax_dir / syntax_file.name

    shutil.copy(syntax_file, file_path)

    logging.info('Installed {editor} syntax file in "{path}"', editor=editor, path=file_path)


def install_vim_files(config_dir: Path, editor: str) -> None:
    try:
        install_vim_ftdetect(config_dir, editor)
        install_syntax_file(config_dir, editor)
    except OSError as e:
        fail(f'failed to install {editor} files: "{e}"')


def install(args: argparse.Namespace) -> None:
    if args.ide is IDE.GNATSTUDIO:
        gnatstudio_dir = importlib.resources.files("rflx") / "ide" / "gnatstudio"
        plugins_dir = args.gnat_studio_dir / "plug-ins"
        if not plugins_dir.exists():
            plugins_dir.mkdir(parents=True, exist_ok=True)
        print(f'Installing RecordFlux plugin into "{plugins_dir}"')  # noqa: T201
        shutil.copy(Path(str(gnatstudio_dir)) / "recordflux.py", plugins_dir)

    elif args.ide is IDE.VSCODE:
        with importlib.resources.as_file(vscode_extension()) as extension:
            try:
                subprocess.run(["code", "--install-extension", extension, "--force"], check=True)
            except (FileNotFoundError, subprocess.CalledProcessError) as e:
                fail(f"installation of VS Code extension failed: {e}")

    elif args.ide is IDE.NVIM:
        home_dir = os.environ.get("HOME")
        xdg_config_home = os.environ.get("XDG_CONFIG_HOME")
        if home_dir is None and xdg_config_home is None:
            RecordFluxError(
                [
                    ErrorEntry("could not find config directory", Severity.ERROR, NO_LOCATION),
                    ErrorEntry(
                        "make sure $HOME or $XDG_CONFIG_HOME variable is set",
                        Severity.HELP,
                        NO_LOCATION,
                    ),
                ],
            ).propagate()

        if xdg_config_home is not None:
            config_path = Path(xdg_config_home) / "nvim"
        else:
            assert home_dir is not None
            config_path = Path(home_dir) / ".config" / "nvim"

        install_vim_files(config_path, editor="nvim")

    elif args.ide is IDE.VIM:
        home_dir = os.environ.get("HOME")
        if home_dir is None:
            RecordFluxError(
                [
                    ErrorEntry("could not locate home directory", Severity.ERROR, NO_LOCATION),
                    ErrorEntry("make sure $HOME variable is set", Severity.HELP, NO_LOCATION),
                ],
            ).propagate()
        assert home_dir is not None
        install_vim_files(Path(home_dir) / ".vim", editor="vim")
    else:
        assert_never(args.ide)  # pragma: no cover


def convert_iana(args: argparse.Namespace) -> None:
    xml_str = args.file.read()
    iana.convert(
        xml_str,
        args.file,
        args.always_valid,
        args.output_directory,
        args.reproducible,
    )


def run_language_server(args: argparse.Namespace) -> None:
    from rflx.ls.server import server

    server.workers = args.workers
    server.start_io()


def doc(args: argparse.Namespace) -> None:
    assert args.DOCUMENT in ["lrm", "ug"]

    base_folders = {"lrm": "language_reference", "ug": "user_guide"}
    base_folder = base_folders[args.DOCUMENT]

    subprocess.run(
        ["xdg-open", f"file://{DOC_DIR}/{base_folder}/index.html"],
        check=True,
    )


def vscode_extension() -> Traversable:
    path = importlib.resources.files("rflx") / "ide" / "vscode" / "recordflux.vsix"
    assert isinstance(path, Traversable)
    return path


def vim_syntax_file() -> Traversable:
    path = importlib.resources.files("rflx") / "ide" / "vim" / "recordflux.vim"
    assert isinstance(path, Traversable)
    return path


def cache(no_caching: bool, no_verification: bool) -> Cache:
    return NeverVerify() if no_verification else (AlwaysVerify() if no_caching else Cache())
