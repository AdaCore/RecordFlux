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
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Topic :: Communications",
        "Topic :: Security",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Code Generators",
        "Topic :: System :: Networking",
    ],
    packages=find_packages(exclude=("tests",)),
    package_data={"rflx": ["templates/*"]},
    python_requires=">=3.6",
    install_requires=[
        "icontract >=2.3.4, <3",
        "pydotplus >=2, <3",
        "pyparsing >=2.4.0, <3",
        "z3-solver >=4, <5",
    ],
    extras_require={
        "devel": [
            "black",
            "coverage >=4, <5",
            "flake8 >=3, <4",
            "isort >=4, <5",
            "mypy >=0.770",
            "pyicontract-lint >=2.0.0, <3",
            "pylint >=2.5.0, <3",
            "pytest >=5, <6",
            "pytest-xdist >=1, <2",
        ]
    },
    scripts=["bin/rflx"],
)
