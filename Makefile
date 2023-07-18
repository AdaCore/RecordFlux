-include devutils/Makefile.common

.DEFAULT_GOAL := all

VERBOSE ?= @
TEST_PROCS ?= $(shell nproc)
RECORDFLUX_ORIGIN ?= https://github.com/AdaCore
GNATCOLL_ORIGIN ?= https://github.com/AdaCore
LANGKIT_ORIGIN ?= https://github.com/AdaCore
ADASAT_ORIGIN?= https://github.com/AdaCore
VERSION ?= $(shell python3 -c "import setuptools_scm; print(setuptools_scm.get_version())")
SDIST ?= dist/RecordFlux-$(VERSION).tar.gz

BUILD_DIR = build
SRC_DIR = src
MAKEFILE_DIR := $(dir $(abspath Makefile))
BUILD_SRC_DIR := $(MAKEFILE_DIR)/$(BUILD_DIR)/$(SRC_DIR)
PYTHON_PACKAGES = bin doc/language_reference/conf.py doc/user_guide/conf.py examples/apps ide language rflx tests tools stubs setup.py
DEVUTILS_HEAD = 7f4319741b23b926aca093467e097005d4a1f642
GNATCOLL_HEAD = 25459f07a2e96eb0f28dcfd5b03febcb72930987
LANGKIT_HEAD = 65e2dab678b2606e3b0eada64b7ef4fd8cae91bb
ADASAT_HEAD = f948e2271aec51f9313fa41ff3c00230a483f9e8

SHELL = /bin/bash
PYTEST = python3 -m pytest -n$(TEST_PROCS) -vv --timeout=7200

# Use GNATprove's file-based caching by default and ensure the directory exists.
GNATPROVE_CACHE ?= file:$(MAKEFILE_DIR)/$(BUILD_DIR)/gnatprove_cache

ifneq (,$(findstring file:,$(GNATPROVE_CACHE)))
GNATPROVE_CACHE_DIR = $(subst file:,,$(GNATPROVE_CACHE))
endif

export GNATPROVE_CACHE := $(GNATPROVE_CACHE)
export PYTHONPATH := $(MAKEFILE_DIR)

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

$(shell $(call reinit_repo,devutils,$(DEVUTILS_HEAD)))
$(shell $(call reinit_repo,contrib/gnatcoll-bindings,$(GNATCOLL_HEAD)))
$(shell $(call reinit_repo,contrib/langkit,$(LANGKIT_HEAD)))
$(shell $(call reinit_repo,contrib/adasat,$(ADASAT_HEAD)))

.PHONY: all

all: check test prove

.PHONY: init deinit

init: devutils contrib/gnatcoll-bindings contrib/langkit contrib/adasat
	$(VERBOSE)$(call checkout_repo,devutils,$(DEVUTILS_HEAD))
	$(VERBOSE)$(call checkout_repo,contrib/gnatcoll-bindings,$(GNATCOLL_HEAD))
	$(VERBOSE)$(call checkout_repo,contrib/langkit,$(LANGKIT_HEAD))
	$(VERBOSE)$(call checkout_repo,contrib/adasat,$(ADASAT_HEAD))
	$(VERBOSE)rm -f contrib/langkit/langkit/py.typed
	$(VERBOSE)ln -sf devutils/pyproject.toml

deinit:
	$(VERBOSE)$(call remove_repo,devutils)
	$(VERBOSE)$(call remove_repo,contrib/gnatcoll-bindings)
	$(VERBOSE)$(call remove_repo,contrib/langkit)
	$(VERBOSE)$(call remove_repo,contrib/adasat)
	$(VERBOSE)rm -f pyproject.toml

devutils:
	$(VERBOSE)git clone $(RECORDFLUX_ORIGIN)/RecordFlux-devutils.git devutils

contrib/gnatcoll-bindings:
	$(VERBOSE)mkdir -p contrib
	$(VERBOSE)git clone $(GNATCOLL_ORIGIN)/gnatcoll-bindings.git contrib/gnatcoll-bindings

contrib/langkit:
	$(VERBOSE)mkdir -p contrib
	$(VERBOSE)git clone $(LANGKIT_ORIGIN)/langkit.git contrib/langkit

contrib/adasat:
	$(VERBOSE)mkdir -p contrib
	$(VERBOSE)git clone $(ADASAT_ORIGIN)/adasat.git contrib/adasat

.PHONY: check check_packages check_dependencies check_contracts check_doc

check: check_packages check_dependencies common_check check_contracts check_doc

check_packages:
	tools/check_packages.py $(PYTHON_PACKAGES)

check_dependencies:
	tools/check_dependencies.py

check_contracts:
	pyicontract-lint $(PYTHON_PACKAGES)

check_doc:
	tools/check_doc.py -d doc -x doc/user_guide/gfdl.rst

.PHONY: test test_coverage test_unit_coverage test_property test_tools test_ide test_optimized test_compilation test_binary_size test_specs test_installation test_apps

test: test_coverage test_unit_coverage test_language_coverage test_property test_tools test_ide test_optimized test_compilation test_binary_size test_specs test_installation test_apps

# A separate invocation of `coverage report` is needed to exclude `src` in the coverage report.
# Currently, pytest's CLI does not allow the specification of omitted directories
# (cf. https://github.com/pytest-dev/pytest-cov/issues/373).

test_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tests/integration --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/integration
	coverage report --fail-under=100 --show-missing --skip-covered --omit="src/*"

test_unit_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tools --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/tools
	coverage report --fail-under=94.68 --show-missing --skip-covered --omit="src/*"

test_language_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx_lang --cov-branch --cov-fail-under=73.8 --cov-report=term-missing:skip-covered tests/language

test_property:
	$(PYTEST) tests/property

test_tools:
	$(PYTEST) tests/tools

test_ide:
	$(PYTEST) tests/ide

test_optimized:
	PYTHONOPTIMIZE=1 $(PYTEST) tests/unit tests/integration tests/compilation

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
	$(PYTEST) tests/compilation
	$(MAKE) -C tests/spark test NOPREFIX=1
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test_optimized

test_binary_size:
	$(MAKE) -C examples/apps/dhcp_client binary_size

test_specs:
	$(PYTEST) tests/examples/specs_test.py

test_installation: export PYTHONPATH=
test_installation: $(SDIST)
	rm -rf $(BUILD_DIR)/venv $(BUILD_DIR)/test_installation
	mkdir -p $(BUILD_DIR)/test_installation
	python3 -m venv $(BUILD_DIR)/venv
	$(BUILD_DIR)/venv/bin/pip install $(SDIST)
	$(BUILD_DIR)/venv/bin/rflx --version
	HOME=$(BUILD_DIR)/test_installation $(BUILD_DIR)/venv/bin/rflx setup_ide
	test -f $(BUILD_DIR)/test_installation/.gnatstudio/plug-ins/recordflux.py

.PHONY: prove prove_tests prove_python_tests prove_apps prove_property_tests

prove: prove_tests prove_python_tests prove_apps

prove_tests: $(GNATPROVE_CACHE_DIR)
	$(MAKE) -C tests/spark prove

prove_python_tests: export GNATPROVE_PROCS=1
prove_python_tests: $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/verification

prove_apps: $(GNATPROVE_CACHE_DIR)
	$(MAKE) -C examples/apps/ping prove
	$(MAKE) -C examples/apps/dhcp_client prove

prove_property_tests: $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/property_verification

$(GNATPROVE_CACHE_DIR):
	mkdir -p $(GNATPROVE_CACHE_DIR)

.PHONY: install_build_deps install install_devel upgrade_devel install_devel_edge install_git_hooks install_gnat printenv_gnat

install_build_deps:
	pip3 install -U pip>=22.2
	pip3 install packaging setuptools_scm wheel build contrib/langkit

install: install_build_deps $(SDIST)
	$(MAKE) -C devutils install_devel
	pip3 install --force-reinstall "$(SDIST)[devel]"

install_devel: install_build_deps parser
	$(MAKE) -C devutils install_devel
	pip3 install --force-reinstall -e ".[devel]" --config-settings editable_mode=strict

install_devel_edge: install_devel
	$(MAKE) -C devutils install_devel_edge

install_git_hooks:
	install -m 755 tools/pre-{commit,push} .git/hooks/

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

.PHONY: generate

generate:
	tools/generate_spark_test_code.py

.PHONY: doc html_doc pdf_doc

doc: html_doc pdf_doc

html_doc: check_doc build_html_doc

pdf_doc: check_doc build_pdf_doc

.PHONY: build_doc build_doc_user_guide build_doc_language_reference

build_doc: build_html_doc build_pdf_doc

build_doc_user_guide: build_html_doc_user_guide build_pdf_doc_user_guide

build_doc_language_reference: build_html_doc_language_reference build_pdf_doc_language_reference

.PHONY: build_html_doc build_html_doc_language_reference build_html_doc_user_guide

build_html_doc: build_html_doc_language_reference build_html_doc_user_guide

build_html_doc_language_reference:
	$(MAKE) -C doc/language_reference html

build_html_doc_user_guide:
	$(MAKE) -C doc/user_guide html

.PHONY: build_pdf_doc build_pdf_doc_language_reference build_pdf_doc_user_guide

build_pdf_doc: build_pdf_doc_language_reference build_pdf_doc_user_guide

build_pdf_doc_language_reference:
	$(MAKE) -C doc/language_reference latexpdf

build_pdf_doc_user_guide:
	$(MAKE) -C doc/user_guide latexpdf

.PHONY: dist

dist: $(SDIST)

$(SDIST): $(SRC_DIR)/python/librflxlang pyproject.toml setup.py MANIFEST.in $(wildcard bin/*) $(wildcard rflx/*)
	python3 -m build --sdist

$(SRC_DIR)/python/librflxlang: export PYTHONPATH=$(MAKEFILE_DIR)
$(SRC_DIR)/python/librflxlang: $(wildcard language/*.py)
	mkdir -p $(BUILD_SRC_DIR)
	language/generate.py $(BUILD_SRC_DIR) $(VERSION)
	cp -a $(MAKEFILE_DIR)/contrib/langkit $(BUILD_SRC_DIR)/
	cp -a $(MAKEFILE_DIR)/contrib/gnatcoll-bindings $(BUILD_SRC_DIR)/
	cp -a $(MAKEFILE_DIR)/contrib/adasat $(BUILD_SRC_DIR)/
	rm -rf $(SRC_DIR)
	mv $(BUILD_SRC_DIR) $(SRC_DIR)

.PHONY: parser

parser: $(SRC_DIR)/python/librflxlang/librflxlang.so

$(SRC_DIR)/python/librflxlang/librflxlang.so: export GPR_PROJECT_PATH := \
	$(GPR_PROJECT_PATH):src/langkit/langkit/support:src/gnatcoll-bindings/gmp:src/gnatcoll-bindings/iconv:src/adasat
$(SRC_DIR)/python/librflxlang/librflxlang.so: export GNATCOLL_ICONV_OPT := -v
$(SRC_DIR)/python/librflxlang/librflxlang.so: $(wildcard language/*.py) | $(SRC_DIR)/python/librflxlang
	gprbuild -p -j0 -Psrc/librflxlang.gpr \
		-XLIBRARY_TYPE=static-pic \
		-XLIBRFLXLANG_LIBRARY_TYPE=relocatable \
		-XLIBRFLXLANG_STANDALONE=encapsulated
	cp $(SRC_DIR)/lib/relocatable/dev/librflxlang.so $@

.PHONY: clean

clean:
	rm -rf $(BUILD_DIR)/[^_]* $(SRC_DIR) .coverage .coverage.* .hypothesis .mypy_cache .pytest_cache .ruff_cache
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C doc/language_reference clean
	$(MAKE) -C doc/user_guide clean
