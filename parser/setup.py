import re

from setuptools import find_packages, setup  # type: ignore

with open("README.md") as f:
    readme = f.read()

setup(
    name="RecordFlux",
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
    packages=find_packages(exclude=("tests",)),
    package_data={"rflx": ["py.typed", "templates/*"]},
    python_requires=">=3.7",
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
            "langkit@git+https://github.com/AdaCore/langkit.git@master#egg=langkit",
        ]
    },
)
