import os
import pathlib
import subprocess
from typing import List

from setuptools import setup
from wheel.bdist_wheel import bdist_wheel

base_dir = pathlib.Path(__file__).parent.resolve()


class InstallError(Exception):
    pass


def gprbuild(projectfile: str, project_paths: List[str] = None) -> None:
    """Run gprbuild on the given project file with library type fixed to static PIC."""
    project_paths = project_paths or []
    env = {"GPR_PROJECT_PATH": ":".join(project_paths), **os.environ}
    subprocess.run(
        ["gprbuild", "-P", projectfile, "-p", "-XLIBRARY_TYPE=static-pic"],
        check=True,
        env=env,
    )


def get_gcc_location(name: str) -> str:
    """Return location of a file inside GCC installation."""
    result = subprocess.check_output(["gcc", f"--print-file-name={name}"]).rstrip().decode("utf-8")
    if result == name:
        raise InstallError(f'File "{name}" not found')
    return result


class BuildParser(bdist_wheel):  # type: ignore[misc]
    def run(self) -> None:
        gprbuild("gnatcoll-bindings/iconv/gnatcoll_iconv.gpr")
        gprbuild("gnatcoll-bindings/gmp/gnatcoll_gmp.gpr")
        gprbuild("langkit/support/langkit_support.gpr")
        gprbuild("librflxlang.gpr", ["langkit/support"])
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
        subprocess.run(
            [
                "gcc",
                "-shared",
                "-o",
                "python/librflxlang/librflxlang.so",
                "-Wl,--whole-archive",
                "lib/static-pic/dev/librflxlang.a",
                "langkit/support/lib/static-pic/dev/liblangkit_support.a",
                "gnatcoll-bindings/iconv/lib/static-pic/libgnatcoll_iconv.a",
                "gnatcoll-bindings/gmp/lib/static-pic/libgnatcoll_gmp.a",
                *whole_archives,
                "-Wl,--no-whole-archive",
                *archives,
                "-lgmp",
            ],
            check=True,
        )


setup(
    name="RecordFlux-parser",
    version="##VERSION##",
    description="RecordFlux parser",
    long_description=(base_dir / "README.md").read_text(),
    long_description_content_type="text/markdown",
    author="Alexander Senier",
    author_email="senier@componolit.com",
    url="https://github.com/Componolit/RecordFlux-parser",
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
        "Programming Language :: Python :: 3.9",
        "Topic :: Communications",
        "Topic :: Security",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Code Generators",
        "Topic :: System :: Networking",
    ],
    python_requires=">=3.7",
    cmdclass={"bdist_wheel": BuildParser},
    packages=[
        "librflxlang",
    ],
    package_dir={"librflxlang": "python/librflxlang"},
    package_data={"librflxlang": ["librflxlang.so"]},
)
