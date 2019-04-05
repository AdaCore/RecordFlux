import argparse
from pathlib import Path
from typing import List, Tuple, Union

import pkg_resources

from rflx.expression import ExpressionError
from rflx.generator import Generator
from rflx.model import ModelError
from rflx.parser import Parser, ParserError

TEMPLATE_DIR = ('rflx', 'templates/')
LIBRARY_FILES = ('types.ads', 'types.adb')
DEFAULT_PREFIX = 'RFLX'


class Error(Exception):
    pass


class InternalError(Error):
    pass


def main(argv: List[str]) -> Union[int, str]:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='subcommand')

    parser_check = subparsers.add_parser('check', help='check specification')
    parser_check.add_argument('files', metavar='FILE', type=str, nargs='+',
                              help='specification file')
    parser_check.set_defaults(func=check)

    parser_generate = subparsers.add_parser('generate', help='generate code')
    parser_generate.add_argument('-d', '--dissector', action='store_true',
                                 help='generate dissector code')
    parser_generate.add_argument('-l', '--library', action='store_true',
                                 help='generate library code')
    parser_generate.add_argument('-p', '--prefix', type=str, default='RFLX',
                                 help=('add prefix to generated packages '
                                       f'(default: {DEFAULT_PREFIX})'))
    parser_generate.add_argument('files', metavar='FILE', type=str, nargs='*',
                                 help='specification file')
    parser_generate.add_argument('directory', metavar='DIRECTORY', type=str,
                                 help='output directory')
    parser_generate.set_defaults(func=generate)

    args = parser.parse_args(argv[1:])

    if not args.subcommand:
        parser.print_usage()
        return 2

    try:
        args.func(args)
    except ParserError as e:
        return f'{parser.prog}: parser error: {e}'
    except (ModelError, ExpressionError) as e:
        return f'{parser.prog}: model error: {e}'
    except InternalError as e:
        return f'{parser.prog}: internal error: {e}'
    except (Error, OSError) as e:
        return f'{parser.prog}: error: {e}'

    return 0


def check(args: argparse.Namespace) -> None:
    parse(args.files)


def generate(args: argparse.Namespace) -> None:
    directory = Path(args.directory)
    if not directory.is_dir():
        raise Error(f'directory not found: "{directory}"')

    messages, refinements = parse(args.files)
    if args.dissector or (not args.dissector and not args.library):
        generate_dissector(messages, refinements, directory, args.prefix)
    if args.library or (not args.dissector and not args.library):
        generate_library(directory, args.prefix)


def parse(files: List) -> Tuple[List, List]:
    parser = Parser()

    for f in files:
        if not Path(f).is_file():
            raise Error(f'file not found: "{f}"')

        print(f'Parsing {f}... ', end='')
        parser.parse(f)
        print('OK')

    return (parser.messages, parser.refinements)


def generate_dissector(messages: List, refinements: List, directory: Path, prefix: str) -> None:
    if not messages and not refinements:
        return

    if prefix and prefix[-1] != '.':
        prefix = f'{prefix}.'

    generator = Generator(prefix)

    print('Generating... ', end='')
    generator.generate_dissector(messages, refinements)
    written_files = generator.write_units(directory)
    print('OK')

    for f in written_files:
        print(f'Created {f}')


def generate_library(directory: Path, prefix: str) -> None:
    template_dir = Path(pkg_resources.resource_filename(*TEMPLATE_DIR))
    if not template_dir.is_dir():
        raise InternalError('library directory not found')

    if prefix == DEFAULT_PREFIX:
        filename = prefix.lower() + '.ads'
        file_path = Path(directory).joinpath(filename)

        with open(file_path, 'w') as library_file:
            library_file.write(f'package {prefix} is\n\nend {prefix};')
            print(f'Created {file_path}')

    if prefix and prefix[-1] != '.':
        prefix = f'{prefix}.'

    for template_filename in LIBRARY_FILES:
        if not template_dir.joinpath(template_filename).is_file():
            raise InternalError(f'library file not found: "{template_filename}"')

        filename = prefix.replace('.', '-').lower() + template_filename
        file_path = Path(directory).joinpath(filename)

        with open(template_dir.joinpath(template_filename)) as template_file:
            with open(file_path, 'w') as library_file:
                library_file.write(template_file.read().format(prefix=prefix))
                print(f'Created {file_path}')
