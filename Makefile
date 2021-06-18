VERBOSE ?= @

python-packages := bin examples/apps rflx tests tools stubs setup.py

build-dir := build
noprefix-dir := build/noprefix

project := test
ifdef TEST
	test-bin := $(build-dir)/test_$(TEST)
else
	test-bin := $(build-dir)/test
endif
test-files := $(wildcard tests/spark/generated/rflx-*.ad? tests/spark/*.ad? examples/specs/*.rflx test.gpr)

ifneq ($(NOPREFIX),)
project := $(noprefix-dir)/test
test-bin := $(noprefix-dir)/$(build-dir)/test
test-files := $(addprefix $(noprefix-dir)/, $(subst /rflx-,/,$(test-files)))
endif

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python test_spark test_apps test_specs test_runtime \
	prove prove_tests prove_apps clean

all: check test prove

check: check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_doc

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
	PYTHONPATH=. pylint $(python-packages)

check_mypy:
	mypy --pretty $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(python-packages) ide/gnatstudio
	isort $(python-packages) ide/gnatstudio

test: test_python_coverage test_python_property test_spark test_runtime test_installation

test_python:
	python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis" tests

test_python_unit:
	python3 -m pytest -n$(shell nproc) -vv tests/unit

test_python_integration:
	python3 -m pytest -n$(shell nproc) -vv tests/integration

test_python_property:
	python3 -m pytest -vv tests/property

test_python_verification:
	python3 -m pytest -vv -m "verification" -s tests

test_python_optimized:
	python3 -O -m pytest -n$(shell nproc) -vv -m "not hypothesis" tests

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=rflx --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered -m "not hypothesis" tests

test_spark: $(test-files)
	gprbuild -P$(project) -Xtest=$(TEST)
	$(test-bin)

test_spark_optimized: $(test-files)
	gprbuild -P$(project) -Xoptimization=yes
	$(test-bin)

test_apps:
	python3 examples/apps/icmp_socket.py
	$(MAKE) -C examples/apps/ping test_python
	$(MAKE) -C examples/apps/ping test_spark

test_specs:
	cd examples/specs && python3 -m pytest -n$(shell nproc) -vv tests/test_specs.py

test_runtime:
	rm -rf $(build-dir)/ada-runtime
	git clone --depth=1 --branch recordflux https://github.com/Componolit/ada-runtime $(build-dir)/ada-runtime
	$(MAKE) -C build/ada-runtime
	gprbuild -Ptest --RTS=build/ada-runtime/build/posix/obj -Xaunit=no -Xoptimization=yes

test_installation:
	rm -rf $(build-dir)/pip
	python3 setup.py sdist
	pip3 install RecordFlux --no-deps --no-index --find-links dist/ --target $(build-dir)/pip

prove: prove_tests prove_apps

prove_tests: $(test-files)
	gnatprove -P$(project) -Xtest=$(TEST) $(GNATPROVE_ARGS)

prove_tests_cvc4: $(test-files)
	gnatprove -P$(project) --prover=cvc4 --steps=200000 --timeout=120 --warnings=continue -u rflx-ipv4 -u rflx-ipv4-packet -u rflx-in_ipv4 -u rflx-in_ipv4-contains -u rflx-in_ipv4-tests $(GNATPROVE_ARGS)

prove_apps:
	$(MAKE) -C examples/apps/ping prove

install_gnatstudio:
	install -m 644 ide/gnatstudio/recordflux.py ${HOME}/.gnatstudio/plug-ins/recordflux.py

clean:
	rm -rf $(build-dir) .coverage .hypothesis .mypy_cache .pytest_cache
	$(MAKE) -C examples/apps/ping clean

remove-prefix = $(VERBOSE) \
	mkdir -p $(dir $@) && \
	sed 's/\(RFLX\.\|rflx-\)//g' $< > $@.tmp && \
	mv $@.tmp $@

clean_proof:
	rm -rf tests/spark/proof/*
	$(MAKE) -C examples/apps/ping clean_proof

$(noprefix-dir)/tests/spark/generated/%: tests/spark/generated/rflx-%
	$(remove-prefix)

$(noprefix-dir)/tests/spark/%: tests/spark/rflx-%
	$(remove-prefix)

$(noprefix-dir)/examples/specs/%: examples/specs/%
	$(VERBOSE)mkdir -p $(dir $@)
	$(VERBOSE)cp $< $@

$(noprefix-dir)/%: %
	$(remove-prefix)
