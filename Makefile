include Makefile.common

.DEFAULT_GOAL := all

# --- Parameters ---

PYTHON ?= python3
TEST_PROCS ?= $(shell nproc)
DEVUTILS_ORIGIN ?= https://github.com/AdaCore
GNATCOLL_ORIGIN ?= https://github.com/AdaCore
LANGKIT_ORIGIN ?= https://github.com/AdaCore
ADASAT_ORIGIN ?= https://github.com/AdaCore
VERSION ?= $(shell test -f pyproject.toml && test -f $(POETRY) && $(POETRY) version -s)
PYTHON_VERSIONS ?= 3.8 3.9 3.10 3.11
NO_GIT_CHECKOUT ?=

# --- Dependencies ---

POETRY_VERSION = 1.7.1
POETRY_DYNAMIC_VERSIONING_VERSION = 1.2.0
POETRY_PLUGIN_EXPORT_VERSION = 1.6.0

# --- Repository structure ---

PYTHON_PACKAGES := doc/language_reference/conf.py doc/user_guide/conf.py examples/apps language rflx tests tools stubs build.py
APPS := $(filter-out __init__.py,$(notdir $(wildcard examples/apps/*)))
SDIST = dist/recordflux-$(VERSION).tar.gz
VSIX = rflx/ide/vscode/recordflux.vsix
BIN_DIR := $(MAKEFILE_DIR)/.bin
GENERATED_DIR := generated
BUILD_GENERATED_DIR := $(MAKEFILE_DIR)/$(BUILD_DIR)/$(GENERATED_DIR)

# --- External repositories ---

DEVUTILS_DIR = devutils
GNATCOLL_DIR = contrib/gnatcoll-bindings
LANGKIT_DIR = contrib/langkit
ADASAT_DIR = contrib/adasat

DEVUTILS_HEAD = 0ce63302c719b173806d6943847344d28072986e
GNATCOLL_HEAD = 25459f07a2e96eb0f28dcfd5b03febcb72930987
LANGKIT_HEAD = 65e2dab678b2606e3b0eada64b7ef4fd8cae91bb
ADASAT_HEAD = f948e2271aec51f9313fa41ff3c00230a483f9e8

# --- Devutils ---

PYTEST = $(POETRY) run pytest -n$(TEST_PROCS) -vv --timeout=7200
RUFF = $(POETRY) run ruff
BLACK = $(POETRY) run black
MYPY = $(POETRY) run mypy --exclude rflx/lang
KACL_CLI = $(POETRY) run kacl-cli

DEVUTILS_DEPENDENCIES = $(RFLX)

-include $(DEVUTILS_DIR)/Makefile.common

# --- Environment variables ---

export PYTHONPATH := $(MAKEFILE_DIR)

# --- Helper functions: Management of external repositories ---

# Switch to a specific revision of the git repository.
#
# @param $(1) directory of the git repository
# @param $(2) commit id
define checkout_repo
$(if
	$(wildcard $(1)),
	$(if
		$(shell git -C $(1) fetch && git -C $(1) -c advice.detachedHead=false checkout $(2) && echo 1),
		,
		$(error Checkout of "$(2)" in "$(1)" failed)
	)
)
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
update_repo = $(if $(filter-out $(2),$(call repo_head,$(1),$(2))),$(call checkout_repo,$(1),$(2)),)

# Remove the git repository, if no changes are present.
#
# The function looks for changed and untracked files as well as commits that are not pushed to a
# remote branch. If the repository is unchanged, it will be removed completely.
#
# @param $(1) directory of the git repository
define remove_repo
((test -d $(1) && test -n "$$(git -C $(1) status --porcelain)") \
|| (test -d $(1) && test -n "$$(git -C $(1) log --branches --not --remotes --format=oneline)")) \
&& echo "Keeping $(1) due to local changes" \
|| echo "Removing $(1)" && rm -rf $(1)
endef

# --- Updating external repositories ---

ifeq ($(NO_GIT_CHECKOUT), )
$(shell $(call update_repo,$(DEVUTILS_DIR),$(DEVUTILS_HEAD)))
$(shell $(call update_repo,$(GNATCOLL_DIR),$(GNATCOLL_HEAD)))
$(shell $(call update_repo,$(LANGKIT_DIR),$(LANGKIT_HEAD)))
$(shell $(call update_repo,$(ADASAT_DIR),$(ADASAT_HEAD)))
endif

# --- Default ---

.PHONY: all

all: check test prove

# --- Setup ---

# `poetry install` always changes the modification time of `pyproject.toml`. To prevent a
# reinstallation on each invocation, `pyproject.toml` is added as an order-only dependency.
PROJECT_MANAGEMENT = $(POETRY) pyproject.toml.in $(DEVUTILS_DIR) | pyproject.toml
CONTRIB = $(GNATCOLL_DIR) $(LANGKIT_DIR) $(ADASAT_DIR)

.PHONY: init

init: $(CONTRIB) $(PROJECT_MANAGEMENT)

# --- Setup: Poetry ---

$(POETRY): $(POETRY_VENV)
	$(POETRY_VENV)/bin/pip install poetry==$(POETRY_VERSION) poetry-dynamic-versioning==$(POETRY_DYNAMIC_VERSIONING_VERSION) poetry-plugin-export==$(POETRY_PLUGIN_EXPORT_VERSION)

$(POETRY_VENV):
	$(PYTHON) -m venv --clear $(POETRY_VENV)

pyproject.toml: pyproject.toml.in $(DEVUTILS_DIR)
	cat pyproject.toml.in <(grep -v "^\[build-system\]\|^requires = \|^build-backend = \|^\[tool.setuptools_scm\]" $(DEVUTILS_DIR)/pyproject.toml) > pyproject.toml

# --- Setup: External repositories ---

$(DEVUTILS_DIR):
ifeq ($(NO_GIT_CHECKOUT), )
	git clone $(DEVUTILS_ORIGIN)/RecordFlux-devutils.git $(DEVUTILS_DIR)
	git -C $(DEVUTILS_DIR) -c advice.detachedHead=false checkout $(DEVUTILS_HEAD)
endif
	@test -d $(DEVUTILS_DIR) || (echo "$(DEVUTILS_DIR)" is missing; exit 1)

contrib:
	mkdir -p contrib

$(GNATCOLL_DIR): | contrib
ifeq ($(NO_GIT_CHECKOUT), )
	git clone $(GNATCOLL_ORIGIN)/gnatcoll-bindings.git $(GNATCOLL_DIR)
	git -C $(GNATCOLL_DIR) -c advice.detachedHead=false checkout $(GNATCOLL_HEAD)
endif
	@test -d $(GNATCOLL_DIR) || (echo "$(GNATCOLL_DIR)" is missing; exit 1)

$(LANGKIT_DIR): | contrib
ifeq ($(NO_GIT_CHECKOUT), )
	git clone $(LANGKIT_ORIGIN)/langkit.git $(LANGKIT_DIR)
	git -C $(LANGKIT_DIR) -c advice.detachedHead=false checkout $(LANGKIT_HEAD)
	rm -f $(LANGKIT_DIR)/langkit/py.typed
endif
	@test -d $(LANGKIT_DIR) || (echo "$(LANGKIT_DIR)" is missing; exit 1)

$(ADASAT_DIR): | contrib
ifeq ($(NO_GIT_CHECKOUT), )
	git clone $(ADASAT_ORIGIN)/adasat.git $(ADASAT_DIR)
	git -C $(ADASAT_DIR) -c advice.detachedHead=false checkout $(ADASAT_HEAD)
endif
	@test -d $(ADASAT_DIR) || (echo "$(ADASAT_DIR)" is missing; exit 1)

# --- Setup: Build dependencies ---

PYTHON_VERSION = $(shell test -f $(DEVEL_VENV)/bin/python && $(DEVEL_VENV)/bin/python --version | sed 's/Python \(3\.[0-9]*\)\.[0-9]*/\1/')
BUILD_DEPS = $(DEVEL_VENV)/lib/python$(PYTHON_VERSION)/site-packages/langkit

$(BUILD_DEPS):: export PYTHONPATH=
$(BUILD_DEPS): $(CONTRIB) $(DEVEL_VENV) $(PROJECT_MANAGEMENT)
	$(POETRY) install -v --no-root --only=build

# --- Setup: Langkit-based parser ---

PARSER = rflx/lang/librflxlang.so

.PHONY: parser

parser: $(PARSER)

$(PARSER): $(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so
	mkdir -p rflx/lang
	cp $(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so rflx/lang
	cp $(GENERATED_DIR)/python/librflxlang/* rflx/lang

$(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so: export GPR_PROJECT_PATH := \
	$(GPR_PROJECT_PATH):$(GENERATED_DIR)/langkit/langkit/support:$(GENERATED_DIR)/gnatcoll-bindings/gmp:$(GENERATED_DIR)/gnatcoll-bindings/iconv:$(GENERATED_DIR)/adasat
$(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so: export GNATCOLL_ICONV_OPT ?= -v
$(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so: $(wildcard language/*.py) $(GENERATED_DIR)/python/librflxlang
	gprbuild -p -j0 -P$(GENERATED_DIR)/librflxlang.gpr \
		-XLIBRARY_TYPE=static-pic \
		-XLIBRFLXLANG_LIBRARY_TYPE=relocatable \
		-XLIBRFLXLANG_STANDALONE=encapsulated

$(GENERATED_DIR)/python/librflxlang: export PYTHONPATH=$(MAKEFILE_DIR)
$(GENERATED_DIR)/python/librflxlang: $(BUILD_DEPS) $(wildcard language/*.py) | $(POETRY)
	mkdir -p $(BUILD_GENERATED_DIR)
	$(POETRY) run -- language/generate.py $(BUILD_GENERATED_DIR) $(VERSION)
	cp -a $(MAKEFILE_DIR)/$(LANGKIT_DIR) $(BUILD_GENERATED_DIR)/
	cp -a $(MAKEFILE_DIR)/$(GNATCOLL_DIR) $(BUILD_GENERATED_DIR)/
	cp -a $(MAKEFILE_DIR)/$(ADASAT_DIR) $(BUILD_GENERATED_DIR)/
	rm -rf $(GENERATED_DIR)
	mv $(BUILD_GENERATED_DIR) $(GENERATED_DIR)

# --- Setup: Development dependencies ---

.PHONY: install_devel

install_devel: $(RFLX)

$(RFLX):: export PYTHONPATH=
$(RFLX): $(DEVEL_VENV) $(CONTRIB) $(PARSER) $(PROJECT_MANAGEMENT)
	$(POETRY) install -v --sync

$(DEVEL_VENV):
	$(PYTHON) -m venv --clear $(DEVEL_VENV)

# --- Optional setup: Installation from source distribution ---

install:: export PYTHONPATH=
install: $(SDIST) $(PROJECT_MANAGEMENT)
	$(POETRY) run pip install --force-reinstall $(SDIST)

# --- Optional setup: Installation of FSF GNAT ---

.PHONY: install_gnat printenv_gnat

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

# --- Development environment ---

.PHONY: activate

activate: $(BIN_DIR)/poetry
	@echo . $(DEVEL_VENV)/bin/activate
	@echo PATH=$$PWD/.bin:\$$PATH

$(BIN_DIR)/poetry:
	@mkdir -p $(BIN_DIR)
	@ln -sf $(POETRY) $(BIN_DIR)

# --- Checks ---

.PHONY: check check_poetry check_dependencies check_contracts check_doc

check: check_poetry check_dependencies check_contracts check_doc

check_poetry: $(RFLX)
	$(POETRY) check

check_dependencies: $(RFLX)
	$(POETRY) run tools/check_dependencies.py

check_contracts: $(RFLX)
	$(POETRY) run pyicontract-lint $(PYTHON_PACKAGES)

check_doc: $(RFLX)
	$(POETRY) run tools/check_doc.py -d doc -x doc/user_guide/gfdl.rst
	$(POETRY) run $(MAKE) -C doc/user_guide check_help
	$(POETRY) run tools/check_grammar.py --document doc/language_reference/language_reference.rst --verbal-map doc/language_reference/verbal_mapping.json examples/specs/*.rflx examples/apps/*/specs/*.rflx tests/data/specs/*.rflx tests/data/specs/parse_only/*.rflx
	$(POETRY) run tools/check_grammar.py --invalid --document doc/language_reference/language_reference.rst --verbal-map doc/language_reference/verbal_mapping.json tests/data/specs/invalid/{incorrect_comment_only,incorrect_empty_file,incorrect_specification,incorrect_null_field}.rflx

# --- Tests ---

.PHONY: test test_rflx test_examples test_coverage test_unit_coverage test_language_coverage test_end_to_end test_property test_tools test_ide test_optimized test_compilation test_binary_size test_installation test_specs test_apps

test: test_rflx test_examples

test_rflx: test_coverage test_unit_coverage test_language_coverage test_end_to_end test_property test_tools test_ide test_optimized test_compilation test_binary_size test_installation

test_examples: test_specs test_apps

# A separate invocation of `coverage report` is needed to exclude `$(GENERATED_DIR)` in the coverage report.
# Currently, pytest's CLI does not allow the specification of omitted directories
# (cf. https://github.com/pytest-dev/pytest-cov/issues/373).

test_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tests/integration --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/integration
	$(POETRY) run coverage report --fail-under=100 --show-missing --skip-covered --omit="rflx/lang/*"

test_unit_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tools --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/tools
	$(POETRY) run coverage report --fail-under=95.0 --show-missing --skip-covered --omit="rflx/lang/*"

test_language_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx/lang --cov-branch --cov-fail-under=73.8 --cov-report=term-missing:skip-covered tests/language

test_end_to_end: $(RFLX)
	$(PYTEST) tests/end_to_end

test_property: $(RFLX)
	$(PYTEST) tests/property

test_tools: $(RFLX)
	$(PYTEST) --cov=tools --cov-branch --cov-fail-under=43.6 --cov-report=term-missing:skip-covered tests/tools

test_ide: $(RFLX)
	$(PYTEST) tests/ide
	# TODO(eng/recordflux/RecordFlux#1361): Execute `test` instead of `build`
	$(MAKE) -C rflx/ide/vscode check build

test_optimized: $(RFLX)
	PYTHONOPTIMIZE=1 $(PYTEST) tests/unit tests/integration tests/compilation

test_apps: $(RFLX)
	$(foreach app,$(APPS),$(MAKE) -C examples/apps/$(app) test || exit;)

test_compilation: $(RFLX)
	# Skip test for FSF GNAT to prevent violations of restriction "No_Secondary_Stack" in AUnit units
	[[ "${GNAT}" == fsf* ]] || $(MAKE) -C tests/spark build_strict
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test
	$(MAKE) -C examples/apps/ping build
	$(MAKE) -C examples/apps/dhcp_client build
	$(MAKE) -C examples/apps/spdm_responder lib
	$(MAKE) -C examples/apps/dccp build
	$(PYTEST) tests/compilation
	$(MAKE) -C tests/spark test NOPREFIX=1
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test_optimized
	$(MAKE) -C doc/examples build

test_binary_size: $(RFLX)
	$(MAKE) -C examples/apps/dhcp_client test_binary_size
	$(MAKE) -C examples/apps/spdm_responder test_binary_size

test_specs: TEST_PROCS=1
test_specs: $(RFLX)
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

# --- Tests: SPARK proofs ---

.PHONY: prove prove_tests prove_python_tests prove_apps prove_property_tests

prove: prove_tests prove_python_tests prove_apps prove_doc

prove_tests: $(GNATPROVE_CACHE_DIR)
	$(MAKE) -C tests/spark prove

prove_python_tests: export GNATPROVE_PROCS=1
prove_python_tests: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/verification

prove_apps: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(foreach app,$(APPS),$(MAKE) -C examples/apps/$(app) prove || exit;)

prove_doc: $(RFLX)
	$(MAKE) -C doc/examples prove

prove_property_tests: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/property_verification

# --- Fuzzing ---

fuzz_parser: FUZZER_TIME=360
fuzz_parser: $(RFLX)
	$(POETRY) run tools/fuzz_driver.py --crash-dir $(BUILD_DIR)/crashes --max-time $(FUZZER_TIME) $(MAKEFILE_DIR)/tests/data/specs $(MAKEFILE_DIR)/tests/features/*/

show_fuzz_parser_results: $(RFLX)
	$(POETRY) run tools/fuzz_driver.py --crash-dir $(BUILD_DIR)/crashes --regression

# --- Development tools ---

.PHONY: git_hooks

git_hooks:
	install -m 755 tools/pre-{commit,push} .git/hooks/

.PHONY: generate generate_apps

generate: $(RFLX)
	tools/generate_spark_test_code.py

generate_apps: $(RFLX)
	$(foreach app,$(APPS),$(MAKE) -C examples/apps/$(app) generate || exit;)

.PHONY: anod_build_dependencies anod_poetry_dependencies

anod_build_dependencies: $(POETRY)
	@echo "requirements:"
	@$(POETRY) export --with=build --without=dev --without-hashes | grep -v "@ file" | sed "s/\(.*\)/    - '\1'/"
	@echo "platforms:"
	@echo "    - x86_64-linux"

anod_poetry_dependencies: $(POETRY)
	@echo "requirements:"
	@echo "    - 'poetry==$(POETRY_VERSION)'"
	@echo "    - 'poetry-dynamic-versioning==$(POETRY_DYNAMIC_VERSIONING_VERSION)'"
	@echo "    - 'poetry-plugin-export==$(POETRY_PLUGIN_EXPORT_VERSION)'"
	@echo "    - 'wheel'"
	@echo "platforms:"
	@echo "    - x86_64-linux"

# --- Documentation ---

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

build_html_doc_language_reference: $(RFLX)
	$(MAKE) -C doc/language_reference html

build_html_doc_user_guide: $(RFLX)
	$(MAKE) -C doc/user_guide html

.PHONY: build_pdf_doc build_pdf_doc_language_reference build_pdf_doc_user_guide

build_pdf_doc: build_pdf_doc_language_reference build_pdf_doc_user_guide

build_pdf_doc_language_reference: $(RFLX)
	$(MAKE) -C doc/language_reference latexpdf

build_pdf_doc_user_guide: $(RFLX)
	$(MAKE) -C doc/user_guide latexpdf

# --- Packaging ---

PACKAGE_SRC := $(shell find rflx -type f)

.PHONY: dist sdist wheel pypi_dist anod_dist

dist: sdist wheel

sdist: $(SDIST)

$(SDIST): $(BUILD_DEPS) $(PARSER) $(VSIX) pyproject.toml $(PACKAGE_SRC)
	$(POETRY) build -vv --no-cache -f sdist

# The build directory is removed to ensure a deterministic result. Otherwise, Poetry will reuse
# files in build/lib, even with the `--no-cache` option.
wheel: clean_build $(BUILD_DEPS) $(PARSER) $(VSIX) pyproject.toml $(PACKAGE_SRC)
	$(POETRY) build -vv --no-cache -f wheel

# Build distributions for all defined Python versions without local version identifier.
pypi_dist: $(PROJECT_MANAGEMENT)
	$(MAKE) sdist POETRY_DYNAMIC_VERSIONING_BYPASS=$$(echo $(VERSION) | sed 's/+.*//')
	$(foreach version,$(PYTHON_VERSIONS),$(POETRY) env use $(version) && $(MAKE) wheel POETRY_DYNAMIC_VERSIONING_BYPASS=$$(echo $(VERSION) | sed 's/+.*//') || exit;)

anod_dist: $(BUILD_DEPS) $(PARSER) pyproject.toml $(PACKAGE_SRC)
	$(POETRY) build -vv --no-cache

# --- Build: VS Code extension ---

$(VSIX):
	@echo $(VSIX)
	$(MAKE) -C rflx/ide/vscode dist

# --- Clean ---

.PHONY: clean clean_build clean_all

clean:
	rm -rf $(BUILD_DIR) .coverage .coverage.* .hypothesis .mypy_cache .pytest_cache .ruff_cache doc/language_reference/build doc/user_guide/build
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C examples/apps/spdm_responder clean
	$(MAKE) -C examples/apps/dccp clean
	$(MAKE) -C rflx/ide/vscode clean
	$(MAKE) -C doc/examples clean

clean_all: clean

clean_build:
	rm -rf $(BUILD_DIR)

clean_all: clean clean_build
	rm -rf $(DEVEL_VENV) $(POETRY_VENV) $(BIN_DIR) $(GENERATED_DIR) rflx/lang pyproject.toml
	test -d $(LANGKIT_DIR) && touch $(LANGKIT_DIR)/langkit/py.typed || true
	@$(call remove_repo,$(DEVUTILS_DIR))
	@$(call remove_repo,$(GNATCOLL_DIR))
	@$(call remove_repo,$(LANGKIT_DIR))
	@$(call remove_repo,$(ADASAT_DIR))
