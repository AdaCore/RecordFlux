VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := bin examples rflx tests tools stubs setup.py

build-dir := build
noprefix-dir := build/noprefix

project := test
test-bin := $(build-dir)/test
test-files := $(wildcard generated/rflx-*.ad? tests/*.ad? tests/*.raw specs/*.rflx test.gpr)

ifneq ($(NOPREFIX),)
project := $(noprefix-dir)/test
test-bin := $(noprefix-dir)/$(build-dir)/test
test-files := $(addprefix $(noprefix-dir)/, $(subst /rflx-,/,$(test-files)))
endif

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python test_spark prove_spark clean

check: check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_doc

check_black:
	black -l 100 --check $(python-packages) ide/gnatstudio

check_isort:
	isort -rc -c $(python-packages) ide/gnatstudio

check_flake8:
	flake8 $(python-packages) ide/gnatstudio

check_pylint:
	pylint $(python-packages)

check_mypy:
	mypy $(python-packages)

check_contracts:
	pyicontract-lint $(python-packages)

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(python-packages) ide/gnatstudio
	isort -rc $(python-packages) ide/gnatstudio

test: check test_python test_spark prove_spark

test_python:
	python3 -m pytest -n$(shell nproc) -vv

test_python_optimized:
	python3 -O -m pytest -n$(shell nproc) -vv

test_python_coverage:
	coverage run --branch --source=rflx -m pytest -vv

test_spark: $(test-files)
	gprbuild -P$(project)
	$(test-bin)

test_spark_optimized: $(test-files)
	gprbuild -P$(project) -Xoptimization=yes
	$(test-bin)

prove_spark: $(test-files)
	gnatprove -P$(project) $(GNATPROVE_ARGS)

prove_spark_cvc4: $(test-files)
	gnatprove -P$(project) --prover=cvc4 --steps=200000 --timeout=120 --warnings=continue -u rflx-ipv4 -u rflx-ipv4-packet -u rflx-in_ipv4 -u rflx-in_ipv4-contains -u rflx-in_ipv4-tests $(GNATPROVE_ARGS)

install_gnatstudio:
	install -m 644 ide/gnatstudio/recordflux.py ${HOME}/.gnatstudio/plug-ins/recordflux.py

clean:
	gprclean -Ptest
	gnatprove -Ptest --clean
	test -d $(noprefix-dir) && rm -r $(noprefix-dir) || true
	rmdir $(build-dir)

remove-prefix = $(VERBOSE) \
	mkdir -p $(dir $@) && \
	sed 's/\(RFLX\.\|rflx-\)//g' $< > $@.tmp && \
	mv $@.tmp $@

$(noprefix-dir)/generated/%: generated/rflx-%
	$(remove-prefix)

$(noprefix-dir)/tests/%: tests/rflx-%
	$(remove-prefix)

$(noprefix-dir)/specs/%: specs/%
	$(VERBOSE)mkdir -p $(dir $@)
	$(VERBOSE)cp $< $@

$(noprefix-dir)/%: %
	$(remove-prefix)
