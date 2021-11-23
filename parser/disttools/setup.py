import pathlib
import subprocess

from setuptools import setup
from wheel.bdist_wheel import bdist_wheel

base_dir = pathlib.Path(__file__).parent.resolve()


class BuildParser(bdist_wheel):  # type: ignore[misc]
    def run(self) -> None:
        subprocess.run(
            ["gprbuild", "-j0", "-Plibrflxlang.gpr", "-XLIBRARY_TYPE=relocatable"], check=True
        )
        super().run()


setup(
    name="RecordFlux-parser",
    version="##VERSION##",
    description="RecordFlux parser",
    long_description=(base_dir / "README.md").read_text(encoding="utf-8"),
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
    package_data={"librflxlang": ["librflxlang.so", "py.typed", "*.pyi"]},
)
