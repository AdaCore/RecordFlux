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

Additional tools can be found in `tools/`.
