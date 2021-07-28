# Development

## Installation

The required dependencies are listed in the [README](../README.md#installation). After cloning the repository, `pip` can be used to install the project in editable mode. The use of a [virtual environment](https://docs.python.org/3/tutorial/venv.html) is recommended.

```Console
$ virtualenv -p python3 venv
$ source venv/bin/activate
$ pip3 install -e ".[devel]"
```

## Tools

Make targets for common development tasks are:

- `all` Execute `check`, `test` and `prove`
- `check` Run general checks and static code analysis tools for Python code
- `test` Execute tests for Python code and SPARK code
- `prove` Run GNATprove on SPARK tests and example apps
- `format` Perform automatic code formatting on Python code
- `install_devel` Install project in editable mode
- `upgrade_devel` Upgrade all development dependencies

Additional tools can be found in `tools/`.

## Pull requests

We accept pull requests [via GitHub](https://github.com/Componolit/RecordFlux/compare). To contribute to the project, fork it under your own GitHub user and perform your changes on a topic branch. Ideally, create an issue in the upstream repository describing the problem you would like to solve and your intention to work on it. This will help us to point you to potential prior work and discuss your idea. Your branch should be named `issue_<ISSUE_NUMBER>`, e.g. `issue_694` where #694 is the ticket you created. For small (!) changes descriptive branch names without a ticket are acceptable.

When submitting a pull request, your topic branch should be rebased to the current upstream `main` branch. Verify that all automatic checks performed by `make check`, `make test` and `make prove` succeed before submitting the PR. For Python code we follow and automatically enforce the coding style of [Black](https://pypi.org/project/black/). You can format your code automatically using the `make format` target on the command line. For Ada code (examples as well as generated code) please follow [our Ada style guide](https://github.com/Componolit/ada-style).

We enforce 100% branch coverage for Python code using [pytest](https://pytest.org). Make sure to add relevant test cases to achieve that for your code. See [the test documentation](/tests/README.md) and have a look at the existing test cases in the `tests` directory to get an idea of the structure of our test suite. Our Python code is also statically type-checked using [mypy](http://mypy-lang.org/). Make sure to include the required type annotations with your code.

Your code will be reviewed by at least one core developer before inclusion into the project. Don't be discouraged should we have many comments and ask you for a lot of changes to your pull request. This even happens to the most experienced developers in our project and we consider these discussions an essential part of the development process and a necessity to maintain high quality. Don't hesitate to open an issue if you have any question or submit the pull request in draft mode first.
