from setuptools import setup

with open("README.md", encoding="utf-8") as file:
    readme = file.read()

setup(
    name="RecordFlux-LS",
    description=(
        "The RecordFlux Language Server provides language support for RecordFlux: "
        "syntax highlighting, code completion, error checking..."
    ),
    long_description=readme,
    long_description_content_type="text/markdown",
    author="Audran Dessaint",
    author_email="dessaint@adacore.com",
    url="https://github.com/AdaCore/RecordFlux-LSP",
    license="AGPL-3.0",
    classifiers=[],
    packages=["rflx_ls"],
    package_dir={"rflx_ls": "src/rflx_ls"},
    python_requires=">=3.8",
    install_requires=["pygls==1.0.1", "RecordFlux==0.11.0"],
    extras_require={"devel": ["pytest==7.0", "pytest-asyncio==0.21", "pytest-cov==4.0"]},
    scripts=["bin/rflx_ls"],
)
