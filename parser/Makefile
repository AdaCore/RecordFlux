VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := language tests setup.py

build-dir := build

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python clean

check: check_black check_isort check_flake8 check_pylint check_mypy check_contracts

check_black:
	black --check --diff --line-length 100 $(python-packages)

check_isort:
	isort --check --diff $(python-packages)

check_flake8:
	flake8 $(python-packages)

check_pylint: install_parser
	pylint $(python-packages)

check_mypy: install_parser
	mypy --pretty $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: check test_python

test_python: install_parser
	python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis" tests

install_parser:
	pip3 install .[Devel]

clean:
	rm -rf $(build-dir) .hypothesis .mypy_cache .pytest_cache .egg
