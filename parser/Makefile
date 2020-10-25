VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := language tests

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

check_pylint: generate_parser
	pylint $(python-packages)

check_mypy: generate_parser
	mypy --pretty $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: check test_python

test_python: generate_parser
	LD_LIBRARY_PATH=build/langkit/lib/librecordfluxdsllang/relocatable/dev python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis"

generate_parser:
	mkdir -p $(build-dir)
	GNATCOLL_ICONV_OPT="-v" ADA_PROJECT_PATH=contrib/gnatcoll-bindings/iconv:contrib/gnatcoll-bindings/gmp PYTHONPATH=. ./manage.py --build-dir $(build-dir)/langkit make
	pip3 install build/langkit/python

clean:
	rm -rf $(build-dir) .hypothesis .mypy_cache .pytest_cache
