import re
import site

from setuptools import find_packages, setup

# ISSUE: pypa/pip#7953
# PEP517 workaround
site.ENABLE_USER_SITE = True

with open("rflx/__init__.py", encoding="utf-8") as f:
    match = re.search(r'__version__ = "(.*?)"', f.read())
    assert match
    version = match.group(1)

with open("README.md", encoding="utf-8") as f:
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
        "ruamel.yaml >=0.17, <0.18",
        "z3-solver >=4, <5",
        "RecordFlux-parser ==0.9.2",
    ],
    extras_require={
        "devel": [
            "hypothesis >=6.14, <6.24",
            "pyicontract-lint >=2.1.2, <2.2",
            "tqdm >=4.61.1, <4.63",
            "types-pkg_resources >=0.1.3, <0.2",
        ]
    },
    scripts=["bin/rflx"],
)
