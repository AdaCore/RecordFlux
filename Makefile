VERBOSE ?= @
TEST_PROCS ?= $(shell nproc)

SHELL := /bin/bash
PYTEST := python3 -m pytest -n$(TEST_PROCS) -vv

python-packages := bin doc/conf.py examples/apps rflx tests tools stubs setup.py

build-dir := build

.PHONY: check check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_pydocstyle check_doc \
	format \
	test test_python test_python_unit test_python_integration test_python_property test_python_property_verification test_python_optimized test_python_coverage test_apps test_compilation test_binary_size test_specs test_runtime test_installation \
	prove prove_tests prove_python_tests prove_apps \
	install_gnatstudio install_devel install_devel_edge upgrade_devel install_gnat printenv_gnat \
	generate \
	doc \
	clean

all: check test prove

check: check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_pydocstyle check_doc

check_packages:
	tools/check_packages.py $(python-packages)

check_dependencies:
	tools/check_dependencies.py

check_black:
	black --check --diff --line-length 100 $(python-packages) ide/gnatstudio

check_isort:
	isort --check --diff $(python-packages) ide/gnatstudio

check_flake8:
	pflake8 $(python-packages) ide/gnatstudio

check_pylint:
	pylint $(python-packages)

check_mypy:
	mypy --pretty $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

check_pydocstyle:
	pydocstyle $(python-packages)

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(python-packages) ide/gnatstudio
	isort $(python-packages) ide/gnatstudio

test: test_python_coverage test_python_property test_apps test_compilation test_binary_size test_specs test_runtime test_installation

test_python:
	$(PYTEST) -m "not hypothesis" tests

test_python_unit:
	$(PYTEST) tests/unit

test_python_integration:
	$(PYTEST) tests/integration

test_python_property:
	$(PYTEST) -m "not verification" tests/property

test_python_property_verification:
	$(PYTEST) -m "verification" -s tests/property

test_python_optimized:
	PYTHONOPTIMIZE=1 $(PYTEST) -m "not verification and not hypothesis" tests

test_python_coverage:
	$(PYTEST) --cov=rflx --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered -m "not hypothesis" tests

test_apps:
	$(MAKE) -C examples/apps/ping test_python
	$(MAKE) -C examples/apps/ping test_spark
	$(MAKE) -C examples/apps/dhcp_client test

test_compilation:
	# Skip test for FSF GNAT to prevent violations of restriction "No_Secondary_Stack" in AUnit units
	[[ "${GNAT}" == fsf* ]] || $(MAKE) -C tests/spark build_strict
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test
	$(MAKE) -C examples/apps/ping build
	$(MAKE) -C examples/apps/dhcp_client build
	$(PYTEST) -m "compilation and not verification" tests
	$(MAKE) -C tests/spark test NOPREFIX=1
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test_optimized

test_binary_size:
	$(MAKE) -C examples/apps/dhcp_client binary_size

test_specs:
	cd examples/specs && $(PYTEST) tests/test_specs.py

test_runtime:
	rm -rf $(build-dir)/ada-runtime
	git clone --depth=1 --branch recordflux https://github.com/Componolit/ada-runtime $(build-dir)/ada-runtime
	$(MAKE) -C build/ada-runtime
	mkdir -p build/aunit
	echo "project AUnit is end AUnit;" > build/aunit/aunit.gpr
	cd tests/spark && gprbuild -Ptest --RTS=../../build/ada-runtime/build/posix/obj -Xmode=runtime_compatible -aP ../../build/aunit

test_installation:
	rm -rf $(build-dir)/venv
	python3 -m venv $(build-dir)/venv
	$(build-dir)/venv/bin/pip install .
	$(build-dir)/venv/bin/rflx --version

prove: prove_tests prove_python_tests prove_apps

prove_tests:
	$(MAKE) -C tests/spark prove

prove_tests_cvc4:
	$(MAKE) -C tests/spark prove_cvc4

prove_python_tests:
	$(PYTEST) -m "verification and not hypothesis" tests

prove_apps:
	$(MAKE) -C examples/apps/ping prove
	$(MAKE) -C examples/apps/dhcp_client prove

install_gnatstudio:
	install -m 644 ide/gnatstudio/recordflux.py ${HOME}/.gnatstudio/plug-ins/recordflux.py

install_devel:
	tools/check_pip_version.py
	$(MAKE) -C .config/python-style install_devel
	pip3 install -e ".[devel]"

install_git_hooks:
	install -m 755 tools/pre-{commit,push} .git/hooks/

upgrade_devel:
	tools/upgrade_dependencies.py

install_devel_edge: install_devel
	$(MAKE) -C .config/python-style install_devel_edge

install_gnat: FSF_GNAT_VERSION ?= 11.2.4
install_gnat: GPRBUILD_VERSION ?= 22.0.1
install_gnat:
	test -d build/alire || ( \
	    mkdir -p build && \
	    cd build && \
	    alr -n init --lib alire && \
	    cd alire && \
	    alr toolchain --select --local gnat_native=$(FSF_GNAT_VERSION) gprbuild=$(GPRBUILD_VERSION) && \
	    alr -n with aunit gnatcoll_iconv gnatcoll_gmp \
	)

printenv_gnat:
	@test -d build/alire && (\
	    cd build/alire && \
	    alr printenv \
	) || true

generate:
	tools/generate_spark_test_code.py

doc: check_doc
	$(MAKE) -C doc html

clean:
	rm -rf $(build-dir) .coverage .hypothesis .mypy_cache .pytest_cache
	$(MAKE) -C tests/spark clean
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C doc clean
