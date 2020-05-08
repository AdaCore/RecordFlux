VERBOSE ?= @
export MYPYPATH = $(PWD)/stubs

python-packages := bin examples rflx tests tools stubs

build-dir := build
noprefix-dir := build/noprefix

project := test
test-bin := $(build-dir)/test
test-files := $(wildcard generated/rflx-*.ad? tests/*.ad? tests/*.raw test.gpr)

ifneq ($(NOPREFIX),)
project := $(noprefix-dir)/test
test-bin := $(noprefix-dir)/$(build-dir)/test
test-files := $(addprefix $(noprefix-dir)/, $(subst /rflx-,/,$(test-files)))
endif

.PHONY: check check_black check_isort check_flake8 check_pylint check_mypy format \
	test test_python test_spark prove_spark clean

check: check_black check_isort check_flake8 check_pylint check_mypy check_doc

check_black:
	black -l 100 --check $(python-packages)

check_isort:
	isort -rc -c $(python-packages)

check_flake8:
	flake8 $(python-packages)

check_pylint:
	pylint $(python-packages)

check_mypy:
	mypy $(python-packages)

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(python-packages)
	isort -rc $(python-packages)

test: check test_python test_spark prove_spark

test_python:
	python3 -m unittest -vb

test_python_coverage:
	coverage run --branch --source=rflx -m unittest -vb

test_spark: $(test-files)
	gprbuild -P$(project)
	$(test-bin)

test_spark_optimized: $(test-files)
	gprbuild -P$(project) -Xoptimization=yes
	$(test-bin)

prove_spark: $(test-files)
	gnatprove -P$(project) $(GNATPROVE_ARGS)

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

$(noprefix-dir)/%: %
	$(remove-prefix)
