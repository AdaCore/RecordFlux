import os
import shutil
import site
import subprocess

from setuptools import setup
from wheel.bdist_wheel import bdist_wheel

# https://github.com/pypa/pip/issues/7953
# PEP517 workaround
site.ENABLE_USER_SITE = True


class BuildParser(bdist_wheel):  # type: ignore[misc]
    def run(self) -> None:
        env = os.environ
        if "GPR_PROJECT_PATH" in env:
            env["GPR_PROJECT_PATH"] += ":"
        else:
            env["GPR_PROJECT_PATH"] = ""
        env["GPR_PROJECT_PATH"] += ":".join(
            [
                "src/langkit/langkit/support",
                "src/gnatcoll-bindings/gmp",
                "src/gnatcoll-bindings/iconv",
                "src/adasat",
            ]
        )
        env["GNATCOLL_ICONV_OPT"] = "-v"
        subprocess.run(
            [
                "gprbuild",
                "-p",
                "-j0",
                "-Psrc/librflxlang.gpr",
                "-XLIBRARY_TYPE=static-pic",
                "-XLIBRFLXLANG_LIBRARY_TYPE=relocatable",
                "-XLIBRFLXLANG_STANDALONE=encapsulated",
            ],
            env=env,
            check=True,
        )
        shutil.copy(
            "src/lib/relocatable/dev/librflxlang.so", "src/python/librflxlang/librflxlang.so"
        )
        super().run()


with open("README.md", encoding="utf-8") as f:
    readme = f.read()

setup(
    name="RecordFlux",
    description=(
        "A toolset for the formal specification and generation of verifiable binary parsers, "
        "message generators and protocol state machines."
    ),
    long_description=readme,
    long_description_content_type="text/markdown",
    author="Tobias Reiher",
    author_email="reiher@adacore.com",
    url="https://github.com/AdaCore/RecordFlux",
    license="AGPL-3.0",
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Environment :: Console",
        "License :: OSI Approved :: GNU Affero General Public License v3",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Ada",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Communications",
        "Topic :: Security",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Code Generators",
        "Topic :: System :: Networking",
    ],
    packages=["rflx", "rflx_ide", "rflx_lang"],
    package_dir={"rflx_ide": "ide", "rflx_lang": "src/python/librflxlang"},
    package_data={"rflx_lang": ["librflxlang.so", "py.typed", "*.pyi"]},
    include_package_data=True,
    cmdclass={"bdist_wheel": BuildParser},
    python_requires=">=3.8",
    install_requires=[
        "attrs >=20, <22",
        "icontract >=2.3.4, <3",
        "pydantic >=1, <2",
        "pydotplus >=2, <3",
        "ruamel.yaml >=0.17, <0.18",
        "z3-solver >=4, <5",
    ],
    extras_require={
        "devel": [
            "build >= 0.9.0, <1",
            "furo == 2022.4.7",
            "hypothesis >=6.14, <6.24",
            "pyicontract-lint >=2.1.2, <2.2",
            "pytest-timeout >=2, <3",
            "scapy ==2.4.5",
            "setuptools_scm >=6.2, <8",
            "sphinx >=4.5, <5",
            "sphinx-rtd-theme >= 1.1.1, <1.2",
            "tqdm >=4.61.1, <4.63",
            "types-pkg_resources >=0.1.3, <0.2",
        ]
    },
    scripts=["bin/rflx"],
)
