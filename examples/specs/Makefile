VERBOSE ?= @

python-packages := tests tools

IANA_REGISTRIES_DIR := tests/iana_registries

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

test_python_iana_to_rflx:
	python3 -m pytest -n$(shell nproc) -vv --cov=tools.iana_to_rflx --cov-branch tests/test_iana_to_rflx.py --cov-fail-under=100 --cov-report=term-missing:skip-covered

generate_iana: generate_iana_protocol_numbers generate_iana_tls_parameters generate_iana_bootp_dhcp_parameters generate_iana_arp_parameters

generate_iana_protocol_numbers:
	python3 ./tools/iana_to_rflx.py ./iana_registries/protocol-numbers.xml -a

generate_iana_tls_parameters:
	python3 ./tools/iana_to_rflx.py ./iana_registries/tls-parameters.xml -a

generate_iana_bootp_dhcp_parameters:
	python3 ./tools/iana_to_rflx.py ./iana_registries/bootp-dhcp-parameters.xml

generate_iana_arp_parameters:
	python3 ./tools/iana_to_rflx.py ./iana_registries/arp-parameters.xml
