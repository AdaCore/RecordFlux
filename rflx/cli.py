import argparse
from pathlib import Path
from typing import List, Tuple, Union

from rflx import __version__
from rflx.generator import Generator, InternalError
from rflx.graph import Graph
from rflx.model import ModelError
from rflx.parser import Parser, ParserError

DEFAULT_PREFIX = "RFLX"


class Error(Exception):
    pass


def main(argv: List[str]) -> Union[int, str]:
    parser = argparse.ArgumentParser()
    parser.add_argument("--version", action="store_true")

    subparsers = parser.add_subparsers(dest="subcommand")

    parser_check = subparsers.add_parser("check", help="check specification")
    parser_check.add_argument(
        "files", metavar="FILE", type=str, nargs="+", help="specification file"
    )
    parser_check.set_defaults(func=check)

    parser_generate = subparsers.add_parser("generate", help="generate code")
    parser_generate.add_argument(
        "-p",
        "--prefix",
        type=str,
        default="RFLX",
        help=("add prefix to generated packages " f"(default: {DEFAULT_PREFIX})"),
    )
    parser_generate.add_argument(
        "-d", "--directory", help="output directory", default=".", type=str
    )
    parser_generate.add_argument(
        "files", metavar="FILE", type=str, nargs="*", help="specification file"
    )
    parser_generate.set_defaults(func=generate)

    parser_graph = subparsers.add_parser("graph", help="generate graphs")
    parser_graph.add_argument(
        "-f",
        "--format",
        type=str,
        default="svg",
        choices=["dot", "jpg", "pdf", "png", "raw", "svg"],
        help=(f"output format (default: svg)"),
    )
    parser_graph.add_argument(
        "files", metavar="FILE", type=str, nargs="+", help="specification file"
    )
    parser_graph.add_argument("-d", "--directory", help="output directory", default=".", type=str)
    parser_graph.set_defaults(func=graph)

    args = parser.parse_args(argv[1:])

    if args.version:
        print(__version__)
        return 0

    if not args.subcommand:
        parser.print_usage()
        return 2

    try:
        args.func(args)
    except ParserError as e:
        return f"{parser.prog}: parser error: {e}"
    except ModelError as e:
        return f"{parser.prog}: model error: {e}"
    except InternalError as e:
        return f"{parser.prog}: internal error: {e}"
    except (Error, OSError) as e:
        return f"{parser.prog}: error: {e}"

    return 0


def check(args: argparse.Namespace) -> None:
    parse(args.files)


def generate(args: argparse.Namespace) -> None:
    directory = Path(args.directory)
    if not directory.is_dir():
        raise Error(f'directory not found: "{directory}"')

    messages, refinements = parse(args.files)
    if not messages and not refinements:
        return

    prefix = args.prefix
    if prefix and prefix[-1] != ".":
        prefix = f"{prefix}."

    generator = Generator(prefix)

    print("Generating... ", end="", flush=True)
    generator.generate(messages, refinements)
    written_files = generator.write_units(directory)
    written_files += generator.write_library_files(directory)
    print("OK")

    for f in written_files:
        print(f"Created {f}")


def parse(files: List) -> Tuple[List, List]:
    parser = Parser()

    for f in files:
        if not Path(f).is_file():
            raise Error(f'file not found: "{f}"')

        print(f"Parsing {f}... ", end="", flush=True)
        parser.parse(f)
        print("OK")

    return (parser.messages, parser.refinements)


def graph(args: argparse.Namespace) -> None:
    directory = Path(args.directory)
    if not directory.is_dir():
        raise Error(f'directory not found: "{directory}"')

    messages, _ = parse(args.files)

    for m in messages:
        message = m.full_name.replace(".", "_")
        filename = Path(directory).joinpath(message).with_suffix(f".{args.format}")
        with open(filename, "wb") as f:
            print(f"Creating graph {filename}... ", end="", flush=True)
            Graph(m).write(f, fmt=args.format)
            print("OK")
