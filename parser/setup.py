#!/usr/bin/env python3

# type: ignore
import os
import shutil
import subprocess
from pathlib import Path

import setuptools.command.build_py as orig
from setuptools import setup

LANGKIT = (
    "git+https://github.com/AdaCore/langkit.git"
    "@45df941275a0844fafc960cfc6b775b1108f42f3#egg=langkit"
)


base_dir = os.path.dirname(os.path.abspath(__file__))


# Hack: To be able to place the shared libraries into the Python package
# (without setting LD_LIBRARY_PATH before starting the python interpreter)
# we need patch the RPATH of the parser library. While the parse library
# itself is loaded via dlopen() directly, the langkit_base library which
# is placed in the same directory cannot be found. To fix that, we add
# "$ORIGIN" to the RPATH of librecordfluxdsllang.so. The simplest way to
# do this is the `patchelf` tool.
def patch_rpath():
    sopath = "lib/librecordfluxdsllang/relocatable/dev/librecordfluxdsllang.so"
    rpath = subprocess.check_output(["patchelf", "--print-rpath", sopath])
    subprocess.run(
        ["patchelf", "--set-rpath", "$ORIGIN:" + rpath.decode("utf-8"), sopath], check=True
    )


# We cannot import langkit.* or language.* globally, as langkit needs to be installed first.
def manage_factory():
    # pylint: disable=import-outside-toplevel
    from langkit.compile_context import CompileCtx
    from langkit.libmanage import ManageScript

    from language.lexer import rflx_lexer as lexer
    from language.parser import rflx_grammar as grammar

    class Manage(ManageScript):
        def create_context(self, args):
            return CompileCtx(lang_name="RecordFluxDSL", lexer=lexer, grammar=grammar)

    return Manage()


class BuildWithParser(orig.build_py):
    def initialize_options(self):
        os.environ["GNATCOLL_ICONV_OPT"] = "-v"
        manage_factory().run(
            [
                "--build-dir",
                ".",
                "make",
                "--gargs",
                f"-aP {base_dir}/contrib/gnatcoll-bindings/iconv "
                f"-aP {base_dir}/contrib/gnatcoll-bindings/gmp",
            ]
        )
        patch_rpath()
        for l in [
            "lib/librecordfluxdsllang/relocatable/dev/librecordfluxdsllang.so",
            "lib/langkit_support/relocatable/dev/liblangkit_support.so",
            f"{base_dir}/contrib/gnatcoll-bindings/iconv/lib/relocatable/libgnatcoll_iconv.so.0",
            f"{base_dir}/contrib/gnatcoll-bindings/gmp/lib/relocatable/libgnatcoll_gmp.so.0",
        ]:
            source = Path(l)
            shutil.copy(source, Path("python/librecordfluxdsllang/") / source.name)
        super().initialize_options()


class BuildParser(orig.build_py):
    description = "Build RecordFlux langkit parser"

    def run(self):
        Path("build/langkit").mkdir(parents=True, exist_ok=True)
        manage_factory().run(["--build-dir", "build/langkit", "make"])


with open("README.md") as f:
    readme = f.read()

setup(
    name="RecordFlux-language",
    version="0.1",
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
    setup_requires=["langkit@" + LANGKIT],
    dependency_links=[LANGKIT],
    install_requires=["icontract >=2.3.4, <3"],
    extras_require={
        "devel": [
            "black ==20.8b1",
            "flake8 >=3, <4",
            "hypothesis >=5, <6",
            "isort >=5, <6",
            "mypy >=0.770",
            "pyicontract-lint >=2.0.0, <3",
            "pylint >=2.6.0, <3",
            "pytest >=5, <6",
            "pytest-xdist >=1.32.0, <2",
            "tqdm >=4, <5",
            "langkit@" + LANGKIT,
        ]
    },
    cmdclass={"build_py": BuildWithParser, "generate_parser": BuildParser},
    packages=["librecordfluxdsllang"],
    package_dir={"librecordfluxdsllang": "python/librecordfluxdsllang"},
    package_data={
        "librecordfluxdsllang": [
            "librecordfluxdsllang.so",
            "liblangkit_support.so",
            "libgnatcoll_iconv.so.0",
            "libgnatcoll_gmp.so.0",
        ],
    },
)
