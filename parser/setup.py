#!/usr/bin/env python3

import os
import subprocess
from pathlib import Path
from typing import Any

import setuptools.command.build_py as orig
from setuptools import setup

base_dir = os.path.dirname(os.path.abspath(__file__))
LANGKIT = f"langkit @ file://localhost/{base_dir}/contrib/langkit#egg=langkit"


class InstallError(Exception):
    pass


# We cannot import langkit.* or language.* globally, as langkit needs to be installed first.
def manage_factory() -> Any:
    # pylint: disable=import-outside-toplevel
    from langkit.compile_context import CompileCtx
    from langkit.libmanage import ManageScript

    from language.lexer import rflx_lexer as lexer
    from language.parser import grammar

    class Manage(ManageScript):
        def create_context(self, args: Any) -> CompileCtx:
            return CompileCtx(lang_name="RFLX", lexer=lexer, grammar=grammar)

    return Manage()


def gprbuild(projectfile: str) -> None:
    """
    Run gprbuild on the given project file with library type is fixed to static PIC
    """
    subprocess.run(
        ["gprbuild", "-P", projectfile, "-p", "-XLIBRARY_TYPE=static-pic"],
        check=True,
    )


def get_gcc_location(name: str) -> str:
    """
    Result location of a file inside GCC installation
    """
    result = subprocess.check_output(["gcc", f"--print-file-name={name}"]).rstrip().decode("utf-8")
    if result == name:
        raise InstallError(f'File "{name}" not found')
    return result


class MakeParser(orig.build_py):
    description = "Build RecordFlux langkit parser"
    GNATCOLL_ICONV_DIR = f"{base_dir}/contrib/gnatcoll-bindings/iconv"
    GNATCOLL_GMP_DIR = f"{base_dir}/contrib/gnatcoll-bindings/gmp"

    def make_parser(self, build_dir: str = ".") -> None:
        Path(build_dir).mkdir(parents=True, exist_ok=True)
        gprbuild(f"{self.GNATCOLL_ICONV_DIR}/gnatcoll_iconv.gpr")
        gprbuild(f"{self.GNATCOLL_GMP_DIR}/gnatcoll_gmp.gpr")
        manage_factory().run(
            [
                "make",
                "--build-dir",
                build_dir,
                "--library-types",
                "static-pic",
                "--disable-all-mains",
                "--disable-warning",
                "undocumented-nodes",
                "--gargs",
                "-XLIBRARY_TYPE=static-pic "
                f"-aP {self.GNATCOLL_ICONV_DIR} "
                f"-aP {self.GNATCOLL_GMP_DIR} "
                f"-aP {base_dir}/contrib/langkit/support "
                f"-aP {base_dir}/contrib/langkit/langkit ",
            ]
        )


class BuildParser(MakeParser):
    # pylint: disable=too-many-ancestors

    def run(self) -> None:
        self.make_parser("build/langkit")


class BuildWithParser(MakeParser):
    # pylint: disable=too-many-ancestors

    def initialize_options(self) -> None:
        os.environ["GNATCOLL_ICONV_OPT"] = "-v"
        output_path = Path("python/librflxlang/")
        output_path.mkdir(parents=True, exist_ok=True)
        whole_archives = (
            get_gcc_location(a) for a in ["adalib/libgnat_pic.a", "adalib/libgnarl_pic.a"]
        )
        archives = (
            get_gcc_location(a)
            for a in [
                "gpr/static-pic/gpr/libgpr.a",
                "gnatcoll.static-pic/libgnatcoll.a",
            ]
        )

        self.make_parser()
        subprocess.run(
            [
                "gcc",
                "-shared",
                "-o",
                f"{output_path}/librflxlang.so",
                "-Wl,--whole-archive",
                "lib/static-pic/dev/librflxlang.a",
                "contrib/langkit/support/lib/static-pic/dev/liblangkit_support.a",
                f"{self.GNATCOLL_ICONV_DIR}/lib/static-pic/libgnatcoll_iconv.a",
                f"{self.GNATCOLL_GMP_DIR}/lib/static-pic/libgnatcoll_gmp.a",
                *whole_archives,
                "-Wl,--no-whole-archive",
                *archives,
                "-lgmp",
            ],
            check=True,
        )
        super().initialize_options()


with open("README.md") as f:
    readme = f.read()

setup(
    name="RecordFlux-language",
    version="0.2.0",
    description=("RecordFlux language"),
    long_description=readme,
    long_description_content_type="text/markdown",
    author="Alexander Senier",
    author_email="senier@componolit.com",
    url="https://github.com/Componolit/RecordFlux-language",
    license="AGPL-3.0",
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Environment :: Console",
        "License :: OSI Approved :: GNU Affero General Public License v3",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Ada",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Topic :: Communications",
        "Topic :: Security",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Code Generators",
        "Topic :: System :: Networking",
    ],
    python_requires=">=3.7",
    setup_requires=[LANGKIT],
    extras_require={
        "devel": [
            "black ==20.8b1",
            "flake8 >=3, <4",
            "isort >=5, <6",
            "mypy >=0.770",
            "pylint >=2.6.0, <3",
            "pytest >=5, <6",
            "pytest-cov >=2.10.0, <3",
            "pytest-xdist >=1.32.0, <2",
            LANGKIT,
        ]
    },
    cmdclass={"build_py": BuildWithParser, "generate_parser": BuildParser},
    packages=["librflxlang"],
    package_dir={"librflxlang": "python/librflxlang"},
    package_data={
        "librflxlang": [
            "librflxlang.so",
        ],
    },
)
