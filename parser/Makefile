VERBOSE ?= @

VERSION = 0.11.0
BUILDDIR = $(PWD)/build

ifneq ($(MAKECMDGOALS),clean)
DUMMY := $(shell mkdir -p $(BUILDDIR))
ifeq ($(DISTDIR),)
DISTDIR := $(shell mktemp -d --tmpdir=$(BUILDDIR) dist-XXXXXXXX)
endif
endif

export MYPYPATH = $(PWD)/stubs

python-packages := language tests disttools/setup.py disttools/gprgen.py

.PHONY: all check check_black check_isort check_flake8 check_pylint check_mypy check_pydocstyle format \
	test test_python test_python_coverage install install_devel install_devel_edge clean

all: check test

check: check_black check_isort check_flake8 check_pylint check_mypy check_pydocstyle

check_black:
	black --check --diff --line-length 100 $(python-packages)

check_isort:
	isort --check --diff $(python-packages)

check_flake8:
	pflake8 $(python-packages)

check_pylint:
	pylint $(python-packages)

check_mypy:
	mypy --pretty $(python-packages)

check_pydocstyle:
	pydocstyle $(python-packages)

format:
	black -l 100 $(python-packages)
	isort $(python-packages)

test: test_python_coverage

test_python:
	python3 -m pytest -n$(shell nproc) -vv tests

test_python_coverage:
	python3 -m pytest -n$(shell nproc) -vv --cov=librflxlang --cov-branch --cov-fail-under=77 --cov-report=term-missing:skip-covered tests

install: $(BUILDDIR)/RecordFlux-parser-$(VERSION).tar.gz
	pip3 install --force-reinstall $<

install_devel: install
	$(MAKE) -C .config/python-style install_devel

install_devel_edge: install
	$(MAKE) -C .config/python-style install_devel_edge

dist: $(BUILDDIR)/RecordFlux-parser-$(VERSION).tar.gz
	@echo "============================================================================================================"
	@echo "Source distribution generated at $<."
	@echo "To upload to PyPI use"
	@echo "  $$ twine upload $<"
	@echo "To upload to Test PyPI use"
	@echo "  $$ twine upload -r testpypi $<"
	@echo "============================================================================================================"

$(BUILDDIR)/RecordFlux-parser-$(VERSION).tar.gz: $(DISTDIR)/gdbinit.py disttools/setup.py
	$(VERBOSE)(cd $(DISTDIR) && python3 -m build --sdist --outdir=$(BUILDDIR))
	$(VERBOSE)rm -rf $(DISTDIR)
	$(VERBOSE)ls -l $@

$(DISTDIR)/gdbinit.py: language/generate.py language/lexer.py language/parser.py language/rflx_ast.py
	$(VERBOSE)pip3 install --upgrade -r requirements.txt
	$(VERBOSE)python3 -m venv $(DISTDIR)/.venv
	$(VERBOSE)$(DISTDIR)/.venv/bin/python -m pip --quiet install contrib/langkit
	$(VERBOSE)PYTHONPATH=$(PWD) $(DISTDIR)/.venv/bin/python language/generate.py $(DISTDIR) $(VERSION)
	$(VERBOSE)cp -a $(PWD)/contrib/langkit $(DISTDIR)/
	$(VERBOSE)cp -a $(PWD)/contrib/gnatcoll-bindings $(DISTDIR)/
	$(VERBOSE)ln -sf $(PWD)/disttools/MANIFEST.in $(DISTDIR)/MANIFEST.in
	$(VERBOSE)cp disttools/setup.py $(DISTDIR)/setup.py
	$(VERBOSE)cp disttools/pyproject.toml $(DISTDIR)/pyproject.toml
	$(VERBOSE)mv $(DISTDIR)/librflxlang.gpr $(BUILDDIR)/librflxlang.gpr.bak
	$(VERBOSE)./disttools/gprgen.py rflxlang $(DISTDIR) $(BUILDDIR)/librflxlang.gpr.bak
	$(VERBOSE)sed -i -e 's/##VERSION##/$(VERSION)/g' $(DISTDIR)/setup.py
	$(VERBOSE)cp README.md $(DISTDIR)/README.md
	$(VERBOSE)touch $(DISTDIR)/python/librflxlang/py.typed

install_gnat:
	alr toolchain --install gnat_native=11.2.1 && \
	mkdir -p build && \
	cd build && \
	alr -n init --lib alire && \
	cd alire && \
	alr -n with gnatcoll_iconv gnatcoll_gmp

printenv_gnat:
	@test -d build/alire && \
	cd build/alire && \
	alr printenv

clean:
	rm -rf .mypy_cache .pytest_cache .egg build
