# RecordFlux Specifications

This repository contains specifications of real-world binary formats using [RecordFlux](https://github.com/Componolit/RecordFlux/).

## Development

For developing new specifications a recent version of [RecordFlux](https://github.com/Componolit/RecordFlux/) and some testing tools need to be installed. The use of a [virtual environment](https://docs.python.org/3/tutorial/venv.html) is recommended. Apart from [GNAT](https://www.adacore.com/download), the installation of all Python dependencies can be done in three simple steps:

```Console
$ virtualenv -p python3 venv
$ source venv/bin/activate
$ pip3 install -r requirements.txt
```

The tests can be executed using `make test`. New specifications in the top-level directory are automatically detected. Each message specification is validated by valid and invalid binary samples, which are expected in `tests/data/<package_name>/<message_name>/valid` and `tests/data/<package_name>/<message_name>/invalid`, respectively.


