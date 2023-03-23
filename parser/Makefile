-include devutils/Makefile.common

.DEFAULT_GOAL := all

VERBOSE ?= @
TEST_PROCS ?= $(shell nproc)
RECORDFLUX_ORIGIN ?= https://github.com/Componolit
ADACORE_ORIGIN ?= https://github.com/AdaCore

VERSION = 0.13.0
BUILDDIR = $(PWD)/build
PYTHON_PACKAGES = language tests disttools/setup.py disttools/gprgen.py
DEVUTILS_HEAD = a5fac2d569a54c3f0d8a65b3e07efeebb471f21e
GNATCOLL_HEAD = 25459f07a2e96eb0f28dcfd5b03febcb72930987
LANGKIT_HEAD = 02c2040c95cf8174f7e98969d751cde9639bd2bd

PYTEST := python3 -m pytest -n$(TEST_PROCS) -vv

export PYTHONPATH := $(PWD)

# Switch to a specific revision of the git repository.
#
# @param $(1) directory of the git repository
# @param $(2) commit id
define checkout_repo
$(shell test -d $(1) && git -C $(1) fetch && git -C $(1) -c advice.detachedHead=false checkout $(2) > /dev/null)
endef

# Get the HEAD revision of the git repository.
#
# @param $(1) directory of the git repository
# @param $(2) default value
repo_head = $(shell test -d $(1) && git -C $(1) rev-parse HEAD || echo $(2))

# Switch to the expected revision of the git repository, if the current HEAD is not the expected one.
#
# @param $(1) directory of the git repository
# @param $(2) expected revision
reinit_repo = $(if $(filter-out $(2),$(call repo_head,$(1),$(2))),$(call checkout_repo,$(1),$(2)),)

# Remove the git repository, if no changes are present.
#
# The function looks for changed and untracked files as well as commits that are not pushed to a
# remote branch. If the repository is unchanged, it will be removed completely.
#
# @param $(1) directory of the git repository
define remove_repo
$(if
	$(or
		$(shell test -d $(1) && git -C $(1) status --porcelain),
		$(shell test -d $(1) && git -C $(1) log --branches --not --remotes --format=oneline)
	),
	$(info Keeping $(1) due to local changes),
	$(shell rm -rf $(1))
)
endef

ifneq ($(MAKECMDGOALS),clean)
DUMMY := $(shell mkdir -p $(BUILDDIR))
ifeq ($(DISTDIR),)
DISTDIR := $(shell mktemp -d --tmpdir=$(BUILDDIR) dist-XXXXXXXX)
endif
endif

$(call reinit_repo,devutils,$(DEVUTILS_HEAD))
$(call reinit_repo,contrib/gnatcoll-bindings,$(GNATCOLL_HEAD))
$(call reinit_repo,contrib/langkit,$(LANGKIT_HEAD))

export MYPYPATH = $(PWD)/stubs

.PHONY: all

all: check test

.PHONY: init deinit

init: devutils contrib/gnatcoll-bindings contrib/langkit
	$(VERBOSE)$(call checkout_repo,devutils,$(DEVUTILS_HEAD))
	$(VERBOSE)$(call checkout_repo,contrib/gnatcoll-bindings,$(GNATCOLL_HEAD))
	$(VERBOSE)$(call checkout_repo,contrib/langkit,$(LANGKIT_HEAD))
	$(VERBOSE)rm -f contrib/langkit/langkit/py.typed
	$(VERBOSE)ln -sf devutils/pyproject.toml

deinit:
	$(VERBOSE)$(call remove_repo,devutils)
	$(VERBOSE)$(call remove_repo,contrib/gnatcoll-bindings)
	$(VERBOSE)$(call remove_repo,contrib/langkit)
	$(VERBOSE)rm pyproject.toml

devutils:
	$(VERBOSE)git clone $(RECORDFLUX_ORIGIN)/RecordFlux-devutils.git devutils

contrib/gnatcoll-bindings:
	$(VERBOSE)mkdir -p contrib
	$(VERBOSE)git clone $(ADACORE_ORIGIN)/gnatcoll-bindings.git contrib/gnatcoll-bindings

contrib/langkit:
	$(VERBOSE)mkdir -p contrib
	$(VERBOSE)git clone $(ADACORE_ORIGIN)/langkit.git contrib/langkit

.PHONY: test test_coverage

test: test_coverage

test_coverage:
	$(PYTEST) --cov=librflxlang --cov-branch --cov-fail-under=74 --cov-report=term-missing:skip-covered tests

.PHONY: install install_devel install_devel_edge

install: $(BUILDDIR)/RecordFlux-parser-$(VERSION).tar.gz
	pip3 install --force-reinstall $<

install_devel: install
	$(MAKE) -C devutils install_devel

install_devel_edge: install
	$(MAKE) -C devutils install_devel_edge

.PHONY: dist

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

.PHONY: install_gnat printenv_gnat

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

.PHONY: clean

clean:
	rm -rf .mypy_cache .pytest_cache .egg build
