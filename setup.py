import re

from setuptools import find_packages, setup  # type: ignore

with open("rflx/__init__.py") as f:
    match = re.search(r'__version__ = "(.*?)"', f.read())
    assert match
    version = match.group(1)

with open("README.md") as f:
    readme = f.read()

setup(
    name="RecordFlux",
    version=version,
    description=(
        "A toolset for the formal specification of messages and the generation of "
        "verifiable binary parsers and message generators."
    ),
    long_description=readme,
    long_description_content_type="text/markdown",
    author="Tobias Reiher",
    author_email="reiher@componolit.com",
    url="https://github.com/Componolit/RecordFlux",
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
    packages=find_packages(include=("rflx", "rflx.*")),
    package_data={"rflx": ["py.typed", "templates/*"]},
    python_requires=">=3.7",
    install_requires=[
        "attrs >=20, <22",
        "icontract >=2.3.4, <3",
        "pydotplus >=2, <3",
        "z3-solver >=4, <5",
        "RecordFlux-language@git+https://github.com/Componolit/RecordFlux-language.git"
        "@v0.6.0#egg=RecordFlux-language",
    ],
    extras_require={
        "devel": [
            "black ==20.8b1",
            "flake8 >=3.9.2, <3.10",
            "hypothesis >=6.14, <6.15",
            "isort >=5.9.1, <5.10",
            "mypy ==0.910",
            "pyicontract-lint >=2.1.2, <2.2",
            "pylint >=2.8.3, <2.9",
            "pytest >=6.2.4, <6.3",
            "pytest-cov >=2.12.1, <2.13",
            "pytest-xdist >=2.3.0, <2.4",
            "tqdm >=4.61.1, <4.62",
            "types-pkg_resources >=0.1.3, <0.2",
        ]
    },
    scripts=["bin/rflx"],
)
