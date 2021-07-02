VERBOSE ?= @

python-packages := tests tools

.PHONY: all check check_black check_isort check_flake8 check_pylint check_mypy format test test_python

all: check test

check: check_black check_isort check_flake8 check_pylint check_mypy

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

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: test_python_coverage

test_python:
	python3 -m pytest -n$(shell nproc) -vv

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=tools --cov-branch --cov-fail-under=58 --cov-report=term-missing:skip-covered tests

test_python_validation_tool:
	python3 -m pytest -n$(shell nproc) -vv tests/test_validation_tool.py
