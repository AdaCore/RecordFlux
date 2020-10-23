VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := language # tests

build-dir := build

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python clean

check: generate_parser check_black check_isort check_flake8 check_pylint check_mypy check_contracts

check_black:
	black --check --diff --line-length 100 $(python-packages)

check_isort:
	isort --check --diff $(python-packages)

check_flake8:
	flake8 $(python-packages)

check_pylint:
	pylint $(python-packages)

check_mypy:
	mypy --pretty $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: check test_python_coverage

test_python:
	python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis"

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=rflx --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered -m "not hypothesis" tests

generate_parser:
	mkdir -p $(build-dir)
	PYTHONPATH=. ./manage.py --build-dir $(build-dir)/langkit make
	pip3 install build/langkit/python

clean:
	rm -rf $(build-dir) .coverage .hypothesis .mypy_cache .pytest_cache
