-include devutils/Makefile.common
include Makefile.common

.DEFAULT_GOAL := all

VERBOSE ?= @
TEST_PROCS ?= $(shell nproc)
RECORDFLUX_ORIGIN ?= https://github.com/AdaCore
GNATCOLL_ORIGIN ?= https://github.com/AdaCore
LANGKIT_ORIGIN ?= https://github.com/AdaCore
ADASAT_ORIGIN?= https://github.com/AdaCore
VERSION ?= $(shell python3 -c "import setuptools_scm; print(setuptools_scm.get_version())")
SDIST ?= dist/RecordFlux-$(VERSION).tar.gz
VSIX ?= ide/vscode/recordflux.vsix

GENERATED_DIR = generated
BUILD_GENERATED_DIR := $(MAKEFILE_DIR)/$(BUILD_DIR)/$(GENERATED_DIR)
PYTHON_PACKAGES = bin doc/language_reference/conf.py doc/user_guide/conf.py examples/apps ide language rflx tests tools stubs setup.py
DEVUTILS_HEAD = ee74c35d71f4153aa58ab2b7efc8342c97d8ce31
GNATCOLL_HEAD = 25459f07a2e96eb0f28dcfd5b03febcb72930987
LANGKIT_HEAD = 65e2dab678b2606e3b0eada64b7ef4fd8cae91bb
ADASAT_HEAD = f948e2271aec51f9313fa41ff3c00230a483f9e8

SHELL = /bin/bash
PYTEST = python3 -m pytest -n$(TEST_PROCS) -vv --timeout=7200

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

.PHONY: check check_dependencies check_contracts check_doc

check: check_dependencies common_check check_contracts check_doc

check_dependencies:
	tools/check_dependencies.py

check_contracts:
	pyicontract-lint $(PYTHON_PACKAGES)

check_doc:
	tools/check_doc.py -d doc -x doc/user_guide/gfdl.rst

.PHONY: test test_coverage test_unit_coverage test_property test_tools test_ide test_optimized test_compilation test_binary_size test_specs test_installation test_apps

test: test_coverage test_unit_coverage test_language_coverage test_property test_tools test_ide test_optimized test_compilation test_binary_size test_specs test_installation test_apps

# A separate invocation of `coverage report` is needed to exclude `$(GENERATED_DIR)` in the coverage report.
# Currently, pytest's CLI does not allow the specification of omitted directories
# (cf. https://github.com/pytest-dev/pytest-cov/issues/373).

test_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tests/integration --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/integration
	coverage report --fail-under=100 --show-missing --skip-covered --omit="$(GENERATED_DIR)/*"

test_unit_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tools --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/tools
	coverage report --fail-under=94.68 --show-missing --skip-covered --omit="$(GENERATED_DIR)/*"

test_language_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx_lang --cov-branch --cov-fail-under=73.8 --cov-report=term-missing:skip-covered tests/language

test_property:
	$(PYTEST) tests/property

test_tools:
	$(PYTEST) tests/tools

test_ide:
	$(PYTEST) tests/ide
	# TODO(eng/recordflux/RecordFlux#1361): Execute `test` instead of `build`
	$(MAKE) -C ide/vscode build

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
	HOME=$(BUILD_DIR)/test_installation $(BUILD_DIR)/venv/bin/rflx install gnatstudio
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

.PHONY: install_build_deps install install_devel upgrade_devel install_devel_edge install_git_hooks install_gnat printenv_gnat

install_build_deps:
	pip3 install -U "pip>=22.2"
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

$(SDIST): $(GENERATED_DIR)/python/librflxlang $(VSIX) pyproject.toml setup.py MANIFEST.in $(wildcard bin/*) $(wildcard rflx/*)
	python3 -m build --sdist

$(GENERATED_DIR)/python/librflxlang: export PYTHONPATH=$(MAKEFILE_DIR)
$(GENERATED_DIR)/python/librflxlang: $(wildcard language/*.py)
	mkdir -p $(BUILD_GENERATED_DIR)
	language/generate.py $(BUILD_GENERATED_DIR) $(VERSION)
	cp -a $(MAKEFILE_DIR)/contrib/langkit $(BUILD_GENERATED_DIR)/
	cp -a $(MAKEFILE_DIR)/contrib/gnatcoll-bindings $(BUILD_GENERATED_DIR)/
	cp -a $(MAKEFILE_DIR)/contrib/adasat $(BUILD_GENERATED_DIR)/
	rm -rf $(GENERATED_DIR)
	mv $(BUILD_GENERATED_DIR) $(GENERATED_DIR)

$(VSIX):
	@echo $(VSIX)
	$(MAKE) -C ide/vscode dist

.PHONY: parser

parser: $(GENERATED_DIR)/python/librflxlang/librflxlang.so

$(GENERATED_DIR)/python/librflxlang/librflxlang.so: export GPR_PROJECT_PATH := \
	$(GPR_PROJECT_PATH):$(GENERATED_DIR)/langkit/langkit/support:$(GENERATED_DIR)/gnatcoll-bindings/gmp:$(GENERATED_DIR)/gnatcoll-bindings/iconv:$(GENERATED_DIR)/adasat
$(GENERATED_DIR)/python/librflxlang/librflxlang.so: export GNATCOLL_ICONV_OPT := -v
$(GENERATED_DIR)/python/librflxlang/librflxlang.so: $(wildcard language/*.py) | $(GENERATED_DIR)/python/librflxlang
	gprbuild -p -j0 -P$(GENERATED_DIR)/librflxlang.gpr \
		-XLIBRARY_TYPE=static-pic \
		-XLIBRFLXLANG_LIBRARY_TYPE=relocatable \
		-XLIBRFLXLANG_STANDALONE=encapsulated
	cp $(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so $@

.PHONY: clean

clean:
	rm -rf $(BUILD_DIR)/[^_]* $(GENERATED_DIR) .coverage .coverage.* .hypothesis .mypy_cache .pytest_cache .ruff_cache
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C doc/language_reference clean
	$(MAKE) -C doc/user_guide clean
	$(MAKE) -C ide/vscode clean
