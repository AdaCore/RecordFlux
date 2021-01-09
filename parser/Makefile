VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := language tests setup.py

build-dir := build
parser-installed := $(build-dir)/parser_installed-$(shell git rev-parse HEAD)

DUMMY := $(shell mkdir -p $(build-dir))

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python clean

check: check_black check_isort check_flake8 check_pylint check_mypy

check_black:
	black --check --diff --line-length 100 $(python-packages)

check_isort:
	isort --check --diff $(python-packages)

check_flake8:
	flake8 $(python-packages)

check_pylint: $(parser-installed)
	pylint $(python-packages)

check_mypy: $(parser-installed)
	mypy --pretty $(python-packages)

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: check test_python_coverage

test_python: $(parser-installed)
	python3 -m pytest -n$(shell nproc)  -vv --ignore=tests/data tests

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=librflxlang --cov-branch --cov-fail-under=71 --cov-report=term-missing:skip-covered --ignore=tests/data tests

$(parser-installed):
	pip3 install --user .[Devel]
	@touch $@

clean:
	rm -rf $(build-dir) .mypy_cache .pytest_cache .egg
