VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := bin examples/apps rflx tests tools stubs setup.py

build-dir := build
noprefix-dir := build/noprefix

project := test
test-bin := $(build-dir)/test
test-files := $(wildcard tests/spark/generated/rflx-*.ad? tests/spark/*.ad? examples/specs/*.rflx test.gpr)

ifneq ($(NOPREFIX),)
project := $(noprefix-dir)/test
test-bin := $(noprefix-dir)/$(build-dir)/test
test-files := $(addprefix $(noprefix-dir)/, $(subst /rflx-,/,$(test-files)))
endif

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python test_spark prove_spark test_examples clean

check: check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_doc

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

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(python-packages) ide/gnatstudio
	isort $(python-packages) ide/gnatstudio

test: check test_python_coverage test_python_property test_spark prove_spark

test_python:
	python3 -m pytest -n$(shell nproc) -vv -m "not hypothesis"

test_python_unit:
	python3 -m pytest -n$(shell nproc) -vv tests/unit

test_python_integration:
	python3 -m pytest -n$(shell nproc) -vv tests/integration

test_python_property:
	python3 -m pytest -vv tests/property

test_python_verification:
	python3 -m pytest -vv -m "verification" -s

test_python_optimized:
	python3 -O -m pytest -n$(shell nproc) -vv -m "not hypothesis"

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=rflx --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered -m "not hypothesis" tests

test_spark: $(test-files)
	gprbuild -P$(project)
	$(test-bin)

test_spark_optimized: $(test-files)
	gprbuild -P$(project) -Xoptimization=yes
	$(test-bin)

test_examples:
	python3 -m pytest -n$(shell nproc) -vv -m "root or not root" tests/integration/example_apps_test.py

prove:
	prove_tests
	prove_apps

prove_tests: $(test-files)
	gnatprove -P$(project) $(GNATPROVE_ARGS)

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

$(noprefix-dir)/tests/spark/generated/%: tests/spark/generated/rflx-%
	$(remove-prefix)

$(noprefix-dir)/tests/spark/%: tests/spark/rflx-%
	$(remove-prefix)

$(noprefix-dir)/examples/specs/%: examples/specs/%
	$(VERBOSE)mkdir -p $(dir $@)
	$(VERBOSE)cp $< $@

$(noprefix-dir)/%: %
	$(remove-prefix)
