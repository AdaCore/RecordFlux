VERBOSE ?= @

python-packages := bin examples/apps rflx tests tools stubs setup.py

build-dir := build

.PHONY: check check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_pydocstyle check_doc \
	format \
	test test_python test_python_unit test_python_integration test_python_property test_python_property_verification test_python_optimized test_python_coverage test_spark test_spark_optimized test_apps test_specs test_runtime test_installation \
	prove prove_tests prove_apps \
	install_gnatstudio install_devel install_devel_edge upgrade_devel install_gnat printenv_gnat \
	clean update_proof reset_proof clean_proof

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
	flake8 $(python-packages) ide/gnatstudio

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

test: test_python_coverage test_python_property test_spark test_apps test_specs test_runtime test_installation

test_python: $(proof_sessions)
	python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis" tests

test_python_unit: $(proof_sessions)
	python3 -m pytest -n$(shell nproc) -vv tests/unit

test_python_integration: $(proof_sessions)
	python3 -m pytest -n$(shell nproc) -vv tests/integration

test_python_property:
	python3 -m pytest -vv -m "not verification" tests/property

test_python_property_verification:
	python3 -m pytest -vv -m "verification" -s tests/property

test_python_optimized: $(proof_sessions)
	PYTHONOPTIMIZE=1 python3 -m pytest -n$(shell nproc) -vv -m "not verification and not hypothesis" tests

test_python_coverage: $(proof_sessions)
	python3 -m pytest -n$(shell nproc) -vv --cov=rflx --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered -m "not hypothesis" tests

test_spark:
	$(MAKE) -C tests/spark test

test_spark_optimized:
	$(MAKE) -C tests/spark test_optimized

test_apps:
	$(MAKE) -C examples/apps/ping test_python
	$(MAKE) -C examples/apps/ping test_spark
	$(MAKE) -C examples/apps/dhcp_client test

test_specs:
	cd examples/specs && python3 -m pytest -n$(shell nproc) -vv tests/test_specs.py

test_runtime:
	rm -rf $(build-dir)/ada-runtime
	git clone --depth=1 --branch recordflux https://github.com/Componolit/ada-runtime $(build-dir)/ada-runtime
	$(MAKE) -C build/ada-runtime
	mkdir -p build/aunit
	echo "project AUnit is end AUnit;" > build/aunit/aunit.gpr
	cd tests/spark && gprbuild -Ptest --RTS=../../build/ada-runtime/build/posix/obj -Xtype=unchecked -aP ../../build/aunit

test_installation:
	rm -rf $(build-dir)/venv
	virtualenv -p python3 $(build-dir)/venv
	$(build-dir)/venv/bin/pip install .
	$(build-dir)/venv/bin/rflx --version

prove: prove_tests prove_apps

prove_tests:
	$(MAKE) -C tests/spark prove

prove_tests_cvc4:
	$(MAKE) -C tests/spark prove_cvc4

prove_apps:
	$(MAKE) -C examples/apps/ping prove
	$(MAKE) -C examples/apps/dhcp_client prove

install_gnatstudio:
	install -m 644 ide/gnatstudio/recordflux.py ${HOME}/.gnatstudio/plug-ins/recordflux.py

install_devel:
	$(MAKE) -C .config/python-style install_devel
	pip3 install -e ".[devel]"

upgrade_devel:
	tools/upgrade_dependencies.py

install_devel_edge: install_devel
	$(MAKE) -C .config/python-style install_devel_edge

install_gnat:
	alr toolchain --install gnat_native=11.2.1 && \
	mkdir -p build && \
	cd build && \
	alr init --lib -n alire && \
	cd alire && \
	alr with -n aunit

printenv_gnat:
	@test -d build/alire && \
	cd build/alire && \
	alr printenv

clean:
	rm -rf $(build-dir) .coverage .hypothesis .mypy_cache .pytest_cache
	$(MAKE) -C tests/spark clean
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean

update_proof:
	$(MAKE) -C tests/spark update_proof
	$(MAKE) -C examples/apps/ping update_proof
	$(MAKE) -C examples/apps/dhcp_client update_proof

reset_proof:
	$(MAKE) -C tests/spark reset_proof
	$(MAKE) -C examples/apps/ping reset_proof
	$(MAKE) -C examples/apps/dhcp_client reset_proof

clean_proof:
	$(MAKE) -C tests/spark clean_proof
	$(MAKE) -C examples/apps/ping clean_proof
	$(MAKE) -C examples/apps/dhcp_client clean_proof
