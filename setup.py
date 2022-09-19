import site

from setuptools import find_packages, setup

# https://github.com/pypa/pip/issues/7953
# PEP517 workaround
site.ENABLE_USER_SITE = True

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
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Topic :: Communications",
        "Topic :: Security",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Code Generators",
        "Topic :: System :: Networking",
    ],
    packages=find_packages(include=("rflx", "rflx.*", "ide")),
    package_data={"rflx": ["py.typed", "templates/*"], "ide": ["gnatstudio/*"]},
    python_requires=">=3.8",
    install_requires=[
        "attrs >=20, <22",
        "icontract >=2.3.4, <3",
        "pydantic >=1, <2",
        "pydotplus >=2, <3",
        "ruamel.yaml >=0.17, <0.18",
        "z3-solver >=4, <5",
        "RecordFlux-parser ==0.13.0",
    ],
    extras_require={
        "devel": [
            "furo == 2022.4.7",
            "hypothesis >=6.14, <6.24",
            "pyicontract-lint >=2.1.2, <2.2",
            "pytest-timeout >=2, <3",
            "setuptools_scm >=6.2, <8",
            "sphinx >=4.5, <5",
            "sphinx-copybutton >=0.5, <0.6",
            "tqdm >=4.61.1, <4.63",
            "types-pkg_resources >=0.1.3, <0.2",
        ]
    },
    scripts=["bin/rflx"],
)
