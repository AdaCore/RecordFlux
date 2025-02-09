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
PYTHON_VERSIONS ?= 3.9 3.10 3.11 3.12
CARGO_HOME ?= $(MAKEFILE_DIR)/.cargo-home
NO_GIT_CHECKOUT ?=
CHECK_VENV_FULL_SYNC ?=
# Engine for CI simulation. Supported values are podman and docker.
CONTAINER_ENGINE ?= podman
CI_CONTAINER ?= recordflux-ci
CI_SIM_CLEAN_SETUP ?= 1

# --- Dependencies ---

POETRY_VERSION = 1.8.2
POETRY_DYNAMIC_VERSIONING_VERSION = 1.3.0
POETRY_PLUGIN_EXPORT_VERSION = 1.7.1
PYPISERVER_VERSION = 2.3.2

# --- Repository structure ---

APPS := $(filter-out __init__.py,$(notdir $(wildcard examples/apps/*)))
DIST_DIR := $(MAKEFILE_DIR)/dist
SDIST = $(DIST_DIR)/recordflux-$(VERSION).tar.gz
VSIX = rflx/ide/vscode/recordflux.vsix
BIN_DIR := $(MAKEFILE_DIR)/.bin
GENERATED_DIR := generated
BUILD_GENERATED_DIR := $(MAKEFILE_DIR)/$(BUILD_DIR)/$(GENERATED_DIR)
COVERAGE_DIR := $(MAKEFILE_DIR)/$(BUILD_DIR)/coverage

# --- External repositories ---

DEVUTILS_DIR = devutils
GNATCOLL_DIR = contrib/gnatcoll-bindings
LANGKIT_DIR = contrib/langkit
ADASAT_DIR = contrib/adasat

DEVUTILS_HEAD = 4e878cc2b2224a2ff6b24ac1f9210c2b307af25b
GNATCOLL_HEAD = f988b2052d01310b830d63e86e19c8dc77d382a2
LANGKIT_HEAD = 07218ed24a932e747b98416ceae74e1620276ecc
ADASAT_HEAD = f20d814a4b26508d0dd6ee48a92bc560b122dad5

# --- Devutils ---

PYTEST = $(POETRY) run pytest -n$(TEST_PROCS) -vv --timeout=7200
RUFF = $(POETRY) run ruff
RUFF_ARGS = --exclude=rflx/lang
BLACK = $(POETRY) run black
BLACK_ARGS = --exclude=rflx/lang
MYPY = $(POETRY) run mypy
MYPY_ARGS = --exclude=rflx/lang
KACL_CLI = $(POETRY) run kacl-cli

PYTHON_PACKAGES := doc/language_reference/conf.py doc/user_guide/conf.py examples/apps language rflx tests tools stubs build.py
DEVUTILS_DEPENDENCIES = $(RFLX)

include $(DEVUTILS_DIR)/Makefile.common

# --- Environment variables ---

export PYTHONPATH := $(MAKEFILE_DIR)
export CARGO_HOME := $(CARGO_HOME)
export PATH := $(CARGO_HOME)/bin:$(PATH)

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

# --- Other helper functions and definitions ---

SHORT_VERSION = $(firstword $(subst +, ,$(VERSION)))

# Check the version of the current Python interpreter.
#
# @param $(1) expected Python version (major.minor)
define assert_python_version
	current_version=$$($(POETRY) run python -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')") && \
	if [ "$$current_version" != "$(1)" ]; then \
		interpreter_path=$$($(POETRY) run python -c "import sys; print(sys.executable)") && \
		echo "Error: Expected Python version $(1), but got $$current_version"; \
		echo "Interpreter path: $$interpreter_path"; \
		exit 1; \
	fi
endef

# Check if the source distribution for the given RecordFlux version has been built.
#
# @param $(1) expected RecordFlux version
define assert_sdist_exists
	$(eval FILE_PATTERN := recordflux-$(1).tar.gz)
	if ! ls $(DIST_DIR)/$(FILE_PATTERN) > /dev/null 2>&1; then \
		echo "Error: No source package found for pattern $(FILE_PATTERN) in $(DIST_DIR)."; \
		exit 1; \
	fi
endef

# Check if a wheel for the given RecordFlux and Python version has been built.
#
# @param $(1) expected RecordFlux version
# @param $(2) expected Python version (major.minor)
define assert_wheel_exists
	$(eval PYTHON_TAG := cp$(subst .,,$(2)))
	$(eval FILE_PATTERN := recordflux-$(1)-$(PYTHON_TAG)-$(PYTHON_TAG)*.whl)
	if ! ls $(DIST_DIR)/$(FILE_PATTERN) > /dev/null 2>&1; then \
		echo "Error: No wheel file found for pattern $(FILE_PATTERN) in $(DIST_DIR)."; \
		exit 1; \
	fi
endef

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
	echo "# NOTE: This is an automatically generated file, any modifications will be discarded" > $@
	cat pyproject.toml.in <(grep -v "^\[build-system\]\|^requires = \|^build-backend = \|^\[tool.setuptools_scm\]" $(DEVUTILS_DIR)/pyproject.toml) >> $@

# --- Setup: External repositories ---

$(DEVUTILS_DIR) $(DEVUTILS_DIR)/Makefile.common:
ifeq ($(NO_GIT_CHECKOUT), )
ifndef MAKE_RESTARTS
	git clone $(DEVUTILS_ORIGIN)/RecordFlux-devutils.git $(DEVUTILS_DIR)
	git -C $(DEVUTILS_DIR) -c advice.detachedHead=false checkout $(DEVUTILS_HEAD)
endif
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

	@# TODO(eng/recordflux/RecordFlux#1645): Workaround, remove when fixed
	$(DEVEL_VENV)/bin/pip uninstall --yes setuptools
	$(POETRY) install -v --no-root --only=build

	touch $(BUILD_DEPS)

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

# --- Setup: RapidFlux ---

RAPIDFLUX := rflx/rapidflux.so
RAPIDFLUX_SRC := $(shell find librapidflux rapidflux -type f)
RAPIDFLUX_DEV_BINS := $(addprefix $(CARGO_HOME)/bin/,$(shell tr '\n' ' ' < rust-requirements.txt))

.PHONY: rapidflux rapidflux_devel

# This rule generates symbolic links for each dependency binary,
# linking to the installed version, serving as rule targets.
# These links facilitate enforcing 'make' to rerun 'cargo install'
# for upgrades, downgrades, or in case of package deletion.
$(CARGO_HOME)/bin/%:
	mkdir -p $(CARGO_HOME)
	cargo install --locked $(notdir $@)
	$(RM) $(word 1,$(subst @, ,$@))@* $@
	ln -s $(word 1,$(subst @, ,$@)) $@


rapidflux_devel: $(RAPIDFLUX_DEV_BINS)

rapidflux: rapidflux_devel $(RAPIDFLUX)

$(RAPIDFLUX): target/debug/librapidflux.so
	cp target/debug/librapidflux.so $@

target/debug/librapidflux.so: $(RAPIDFLUX_SRC)
	PYO3_PYTHON=$(DEVEL_VENV)/bin/python cargo build

# --- Setup: Development dependencies ---

.PHONY: install

install: $(RFLX) rapidflux_devel

$(RFLX):: export PYTHONPATH=
$(RFLX): $(DEVEL_VENV) $(CONTRIB) $(PARSER) $(RAPIDFLUX) $(PROJECT_MANAGEMENT)
	$(POETRY) install -v

$(DEVEL_VENV):
	$(PYTHON) -m venv --clear $(DEVEL_VENV)

# --- Optional setup: Installation of FSF GNAT ---

ALR ?= alr

.PHONY: install_gnat printenv_gnat

install_gnat: FSF_GNAT_VERSION ?= 14.1.3
install_gnat: GPRBUILD_VERSION ?= 22.0.1
install_gnat:
	$(RM) -r build/alire
	mkdir -p build && \
	cd build && \
	$(ALR) -n init --lib alire && \
	cd alire && \
	$(ALR) toolchain --select --local gnat_native=$(FSF_GNAT_VERSION) gprbuild=$(GPRBUILD_VERSION) && \
	$(ALR) -n with aunit gnatcoll_iconv gnatcoll_gmp && \
	$(ALR) build --stop-after=post-fetch

printenv_gnat:
	@test -d build/alire && (\
	    cd build/alire && \
	    $(ALR) printenv \
	) || true

# --- Development environment ---

.PHONY: activate

activate: $(BIN_DIR)/poetry
	@echo . $(DEVEL_VENV)/bin/activate
	@echo PATH=$$PWD/.bin:$$PWD/.cargo-home/bin:\$$PATH

$(BIN_DIR)/poetry:
	@mkdir -p $(BIN_DIR)
	@ln -sf $(POETRY) $(BIN_DIR)

# --- Checks ---

.PHONY: check check_code check_poetry check_rapidflux check_doc check_unit_test_file_coverage

check: check_code check_doc

check_code: check_poetry common_check check_rapidflux check_unit_test_file_coverage

check_rapidflux:
	cargo fmt --check
	cargo clippy --tests -- -D warnings
	$(POETRY) run ./tools/check_rust_attributes.py $$(find librapidflux -name '*.rs')

check_poetry: export PYTHONPATH=
check_poetry: $(RFLX)
	@echo "Checking the consistency between pyproject.toml and poetry.lock"
	$(POETRY) check
	@echo "Checking the consistency between poetry.lock and the environment"
	@SYNC_OPT=$(if $(CHECK_VENV_FULL_SYNC),--sync,); \
		OUTPUT=$$($(POETRY) install $$SYNC_OPT --dry-run | grep "-" | grep -v "Already installed"); \
		if [ -n "$$OUTPUT" ]; then \
			echo "The virtual environment is out of sync with the lock file." ; \
			echo "To synchronize the environment run:" ; \
			echo "  $(realpath --relative-to=$(PWD) $(POETRY)) install $$SYNC_OPT" ; \
			echo "Changes to be made to the environment:" ; \
			echo "$$OUTPUT" ; \
			echo "Alternatively, revise the dependencies in 'pyproject.toml.in' and update 'pyproject.toml' and the lock file by running:" ; \
			echo "  make pyproject.toml && \\" ; \
			echo "  $(realpath --relative-to=$(PWD) $(POETRY)) lock [--no-update]" ; \
			false ; \
		fi

check_unit_test_file_coverage:
	$(POETRY) run tools/check_unit_test_file_coverage.py --source-dir $(MAKEFILE_DIR)/rflx/ --test-dir $(MAKEFILE_DIR)/tests/unit/ --ignore ide/vscode/node_modules/flatted/python

check_doc: $(RFLX) build_html_doc
	# Make sure checked-in docs are up to date.
	# Trailing CRs are being ignored as Sphinx generates HTML files without the newline at the end of the file (required by POSIX).
	# Git fixes the issue and that would otherwise make the comparison fail.
	diff -x .buildinfo -x objects.inv --strip-trailing-cr -ur doc/user_guide/build/html/ rflx/doc/user_guide/ || (echo "Error: packaged documentation is out of date. Run 'make generate_doc' to update." >&2; exit 1)
	diff -x .buildinfo -x objects.inv --strip-trailing-cr -ur doc/language_reference/build/html/ rflx/doc/language_reference/ || (echo "Error: packaged documentation is out of date. Run 'make generate_doc' to update." >&2; exit 1)
	$(POETRY) run tools/check_doc.py -d doc -x doc/user_guide/gfdl.rst
	$(POETRY) run $(MAKE) -C doc/user_guide check_help
	$(POETRY) run tools/check_grammar.py --document doc/language_reference/language_reference.rst --verbal-map doc/language_reference/verbal_mapping.json examples/specs/*.rflx examples/apps/*/specs/*.rflx tests/data/specs/*.rflx tests/data/specs/parse_only/*.rflx
	$(POETRY) run tools/check_grammar.py --invalid --document doc/language_reference/language_reference.rst --verbal-map doc/language_reference/verbal_mapping.json tests/data/specs/invalid/{incorrect_comment_only,incorrect_empty_file,incorrect_specification,incorrect_null_field}.rflx

# --- Formatting ---

.PHONY: fmt

fmt: format
	cargo fmt

# --- Tests ---

.PHONY: test test_rflx test_rapidflux test_rapidflux_coverage test_rapidflux_mutation test_rapidflux_doc test_examples test_coverage test_unit_coverage test_per_unit_coverage test_language_coverage test_end_to_end test_property test_tools test_ide test_optimized test_compilation test_binary_size test_specs test_apps test_package

test: test_rflx test_rapidflux test_examples

test_rflx: test_coverage test_unit_coverage test_per_unit_coverage test_language_coverage test_end_to_end test_property test_tools test_ide test_optimized test_compilation test_binary_size

test_rapidflux_coverage: rapidflux_devel
	@tools/test_rapidflux_coverage.sh

# nextest cannot be used with `doctests` with stable Rust.
# See: https://github.com/nextest-rs/nextest/issues/16
test_rapidflux_doc: rapidflux_devel
	cargo test --package librapidflux --doc --no-fail-fast

test_rapidflux: test_rapidflux_coverage test_rapidflux_doc test_rapidflux_mutation

test_rapidflux_mutation: rapidflux_devel
	cargo mutants -j 4 --package librapidflux --timeout 300 --output $(BUILD_DIR)

test_examples: test_specs test_apps

# A separate invocation of `coverage report` is needed to exclude `$(GENERATED_DIR)` in the coverage report.
# Currently, pytest's CLI does not allow the specification of omitted directories
# (cf. https://github.com/pytest-dev/pytest-cov/issues/373).

test_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tests/integration --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/integration tests/compilation
	$(POETRY) run coverage report --fail-under=100 --show-missing --skip-covered --omit="rflx/lang/*"

test_unit_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tools --cov-branch --cov-fail-under=0 --cov-report= tests/unit tests/tools
	$(POETRY) run coverage report --fail-under=95.0 --show-missing --skip-covered --omit="rflx/lang/*"

$(COVERAGE_DIR)/%.coverage: $(MAKEFILE_DIR)/rflx/%.py $(MAKEFILE_DIR)/tests/unit/%_test.py $(RFLX)
	@$(eval TMP_DIR := $(shell mktemp -d))
	@COVERAGE_FILE=$(TMP_DIR)/coverage $(POETRY) run coverage run --branch --include=rflx/$*.py -m pytest -n8 --dist=no --timeout=7200 --no-header --no-summary --quiet tests/unit/$*_test.py > /dev/null 2> /dev/null
	@mkdir -p $(dir $@)
	@COVERAGE_FILE=$@ $(POETRY) run coverage combine --keep $(TMP_DIR)/coverage* > /dev/null 2> /dev/null
	@rm -rf $(TMP_DIR)
	@echo Measured: $*


COVERAGE_FILES = \
	$(filter-out \
		$(COVERAGE_DIR)/ide/vscode/node_modules/%, \
		$(foreach file, $(shell find $(MAKEFILE_DIR)/rflx -type f -name "*.py" ! -name "__init__.py"), \
			$(addprefix $(COVERAGE_DIR)/, \
				$(addsuffix .coverage, \
					$(basename $(subst $(MAKEFILE_DIR)/rflx/,,$(file))) \
				) \
			) \
		) \
	)

$(COVERAGE_DIR)/_full.coverage: $(COVERAGE_FILES)
	@COVERAGE_FILE=$@ $(POETRY) run coverage combine --keep $^ > /dev/null

test_per_unit_coverage:: $(COVERAGE_DIR)/_full.coverage
	@$(POETRY) run coverage report --fail-under=32.0 --show-missing --skip-covered --data-file=$<

test_language_coverage: $(RFLX)
	timeout -k 60 7200 $(PYTEST) --cov=rflx/lang --cov-branch --cov-fail-under=73.8 --cov-report=term-missing:skip-covered tests/language

test_end_to_end: $(RFLX)
	$(PYTEST) tests/end_to_end

test_property: $(RFLX)
	$(PYTEST) tests/property

test_tools: $(RFLX)
	$(PYTEST) --cov=tools --cov-branch --cov-fail-under=0 --cov-report= tests/tools
	$(POETRY) run coverage report --fail-under=52.0 --show-missing --skip-covered --omit="rflx/*"

test_ide: $(RFLX)
	$(PYTEST) tests/ide
	# TODO(eng/recordflux/RecordFlux#1361): Execute `test` instead of `build`
	$(MAKE) -C rflx/ide/vscode check build

test_optimized: $(RFLX)
	PYTHONOPTIMIZE=1 $(PYTEST) tests/unit tests/integration tests/compilation

test_apps: $(RFLX) rapidflux_devel
	$(foreach app,$(APPS),$(POETRY) run $(MAKE) -C examples/apps/$(app) test || exit;)

test_compilation: $(RFLX)
	# Skip test for FSF GNAT to prevent violations of restriction "No_Secondary_Stack" in AUnit units
	[[ "${GNAT}" == fsf* ]] || $(MAKE) -C tests/spark build_strict
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test
	$(MAKE) -C examples/apps/ping build
	$(MAKE) -C examples/apps/dhcp_client build
	$(MAKE) -C examples/apps/spdm_responder lib
	$(MAKE) -C examples/apps/dccp build
	$(MAKE) -C tests/spark test NOPREFIX=1
	$(MAKE) -C tests/spark clean
	$(MAKE) -C tests/spark test_optimized
	$(MAKE) -C doc/examples build

test_binary_size: $(RFLX)
	$(MAKE) -C examples/apps/dhcp_client test_binary_size
	$(MAKE) -C examples/apps/spdm_responder test_binary_size

test_specs: $(RFLX)
	$(PYTEST) tests/examples/specs_test.py

test_package: export PYTHONPATH=
test_package: $(PROJECT_MANAGEMENT)
ifndef PACKAGE
	$(error PACKAGE is undefined, set PACKAGE to the name or path of the package to be tested)
endif
	$(POETRY) run python -m venv --clear $(BUILD_DIR)/venv
	$(BUILD_DIR)/venv/bin/pip install $(PIP_ARGS) $(PACKAGE) pytest
	$(BUILD_DIR)/venv/bin/rflx --version
	mkdir -p $(BUILD_DIR)/tests
	cp -r tests/{__init__.py,const.py,utils.py,end_to_end,data} $(BUILD_DIR)/tests/
	cd $(BUILD_DIR) && source venv/bin/activate && rflx --version && venv/bin/pytest -vv tests/end_to_end
	$(RM) -r $(BUILD_DIR)/{venv,tests}

# --- Tests: SPARK proofs ---

.PHONY: prove prove_tests prove_python_tests prove_apps prove_property_tests

prove: prove_tests prove_python_tests prove_apps prove_doc

prove_tests: $(GNATPROVE_CACHE_DIR)
	$(POETRY) run $(MAKE) -C tests/spark prove

prove_python_tests: export GNATPROVE_PROCS=1
prove_python_tests: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/verification

prove_apps: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(foreach app,$(APPS),$(POETRY) run $(MAKE) -C examples/apps/$(app) prove || exit;)

prove_doc: $(RFLX)
	$(POETRY) run $(MAKE) -C doc/examples prove

prove_property_tests: $(RFLX) $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/property_verification

# --- Fuzzing ---

fuzz_parser: FUZZER_TIME=360
fuzz_parser: $(RFLX)
	$(POETRY) run tools/fuzz_driver.py --crash-dir $(BUILD_DIR)/crashes --max-time $(FUZZER_TIME) $(MAKEFILE_DIR)/tests/data/specs $(MAKEFILE_DIR)/tests/features/*/

show_fuzz_parser_results: $(RFLX)
	$(POETRY) run tools/fuzz_driver.py --crash-dir $(BUILD_DIR)/crashes --regression

# --- Simulation of CI jobs using podman ---

# For simulation the current directory is assumed to be mounted to the
# simulation container. During the simulation the DEVEL_VENV and POETRY_VENV
# folders will contain references to the container's filesystem. Hence, they are
# removed below before starting the simulation to avoid confusion. After the end
# of simulation runs they should be cleaned up via `make clean_ci_sim` to allow
# for local use again.

$(CI_SIM_VENV): export PYTHONPATH=
$(CI_SIM_VENV):
	$(PYTHON) -m venv --clear $(CI_SIM_VENV)
	$(CI_SIM_VENV)/bin/python -m pip -v install ruamel-yaml

.PHONY: ci_sim_%

ci_sim_%: $(CI_SIM_VENV) $(BUILD_DIR)
	@if [ -z "$(CI_USER)" ]; then \
		echo "Error: Environment variable CI_USER is not set." >&2; \
		exit 1; \
	fi
	rm -rf $(DEVEL_VENV)
	rm -rf $(POETRY_VENV)
	{ \
	  printf "%s\n" \
	    "set -euxo pipefail" \
	    ". /it/bin/ci-init-script" \
	    "export E3_CATHOD_CACHE_DIR=~/cathod-cache && mkdir -p \$$E3_CATHOD_CACHE_DIR" \
	    "export CI_PROJECT_DIR=~/RecordFlux"; \
	  $(CI_SIM_VENV)/bin/python tools/extract_ci_jobs.py $(MAKEFILE_DIR)/.gitlab-ci.yml \
	  --var CLEAN_RECORDFLUX_SETUP=$(CI_SIM_CLEAN_SETUP) \
	  --job $*; \
	} | $(CONTAINER_ENGINE) exec -i -u $(CI_USER) -w /home/$(CI_USER) $(CI_CONTAINER) bash 2>&1 | tee  $(BUILD_DIR)/ci_sim_$*.log

# --- Development tools ---

.PHONY: git_hooks

git_hooks:
	install -m 755 tools/pre-{commit,push} .git/hooks/

.PHONY: generate generate_apps generate_doc

generate: $(RFLX)
	$(POETRY) run tools/generate_spark_test_code.py

generate_apps: $(RFLX)
	$(foreach app,$(APPS),$(POETRY) run $(MAKE) -C examples/apps/$(app) generate || exit;)

generate_doc: build_html_doc
	@rm -rf rflx/doc
	@mkdir -p rflx/doc
	@cp -a doc/user_guide/build/html rflx/doc/user_guide
	@cp -a doc/language_reference/build/html rflx/doc/language_reference
	@# Do not distribute Sphinx build artifacts
	@find rflx/doc/user_guide rflx/doc/language_reference -type f \( -name ".buildinfo" -o -name "objects.inv" \) -exec rm -v {} \;

.PHONY: python_wheels_archive anod_build_dependencies anod_poetry_dependencies

python_wheels_archive: TARGET_DIR=$(BUILD_DIR)
python_wheels_archive: ARCHIVE_NAME=recordflux-$(shell date '+%Y%m%d')-python-wheels.tar.gz
python_wheels_archive: export PYTHON_EXECUTABLE=$(POETRY) run $(PYTHON)
python_wheels_archive: $(PROJECT_MANAGEMENT) $(CONTRIB)
	tools/create_python_wheels_archive.sh $(TARGET_DIR) $(ARCHIVE_NAME)

anod_rflx_dependencies: $(POETRY)
	@$(POETRY) export --with=build --with=pytest --without=dev --without-hashes | grep -v "@ file"

anod_extra_dependencies: $(POETRY)
	@echo "poetry==$(POETRY_VERSION)"
	@echo "poetry-dynamic-versioning==$(POETRY_DYNAMIC_VERSIONING_VERSION)"
	@echo "poetry-plugin-export==$(POETRY_PLUGIN_EXPORT_VERSION)"
	@echo "wheel"
	@echo "pypiserver==$(PYPISERVER_VERSION)"

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
	$(MAKE) test_package PACKAGE=$(SDIST)

$(SDIST): $(BUILD_DEPS) $(PARSER) $(VSIX) pyproject.toml $(PACKAGE_SRC)
	POETRY_DYNAMIC_VERSIONING_BYPASS=$(VERSION) $(POETRY) build -vv --no-cache -f sdist

# The build directory and RapidFlux libraries are removed to ensure a deterministic result.
# Otherwise, Poetry will reuse files in build/lib, even with the `--no-cache` option, and the
# resulting wheel will contain more than one RapidFlux library.
wheel: RAPIDFLUX_PLATFORM := rflx/$(shell $(POETRY) run $(PYTHON) -c 'import sysconfig; print("rapidflux" + sysconfig.get_config_var("EXT_SUFFIX"))')
wheel: PYTHON_TAG := $(shell $(POETRY) run python -c 'import sys; print(f"cp{sys.version_info.major}{sys.version_info.minor}")')
wheel: export PYTHONPATH=
wheel: clean_build $(BUILD_DEPS) $(PARSER) $(VSIX) pyproject.toml $(PACKAGE_SRC)
	$(RM) rflx/rapidflux*.so
	@# Build library
	PYO3_PYTHON=$(DEVEL_VENV)/bin/python cargo build --release
	cp target/release/librapidflux.so $(RAPIDFLUX_PLATFORM)
	@# Build wheel
	POETRY_DYNAMIC_VERSIONING_BYPASS=$(VERSION) $(POETRY) build -vv --no-cache -f wheel
	$(RM) $(RAPIDFLUX_PLATFORM)
	@# Test wheel
	$(MAKE) test_package PACKAGE=$(DIST_DIR)/recordflux-$(VERSION)-$(PYTHON_TAG)-$(PYTHON_TAG)-manylinux_*_x86_64.whl

# Build distributions for all defined Python versions without local version identifier.
pypi_dist: $(PROJECT_MANAGEMENT)
	$(MAKE) sdist VERSION=$(SHORT_VERSION)
	@$(call assert_sdist_exists,$(SHORT_VERSION))
	$(foreach version,$(PYTHON_VERSIONS),$(POETRY) env use $(version) && $(POETRY) env info && $(call assert_python_version,$(version)) && $(MAKE) wheel VERSION=$(SHORT_VERSION) || exit;)
	@$(foreach version, $(PYTHON_VERSIONS), $(call assert_wheel_exists,$(SHORT_VERSION),$(version)))

anod_dist: $(BUILD_DEPS) $(PARSER) $(RAPIDFLUX) pyproject.toml $(PACKAGE_SRC)
	$(POETRY) build -vv --no-cache

# --- Build: VS Code extension ---

.PHONY: vscode

vscode: $(VSIX)

$(VSIX): $(RFLX)
	$(MAKE) -C rflx/ide/vscode dist VERSION=$(VERSION)

# --- Audit ---

.PHONY: audit

audit: $(RFLX) rapidflux_devel
	@mkdir -p $(BUILD_DIR)
	@$(POETRY) export --with=build --with=pytest --with=dev | grep -v "@ file" > $(BUILD_DIR)/requirements.txt
	@echo Auditing Python dependencies
	@$(POETRY) run pip-audit --disable-pip -r $(BUILD_DIR)/requirements.txt
	@echo Auditing Rust dependencies
	@cargo audit

# --- Workflow optimization utilities ---

.PHONY: touch_build_tree

# Touch the main build artifacts to mark them up to date to prevent their
# recompilation when that isn't required and desirable. The order of touching
# must correspond to the relative build order.
touch_build_tree:
	@# Langkit-based parser
	touch $(BUILD_DEPS)
	touch $(GENERATED_DIR)/python/librflxlang
	touch $(GENERATED_DIR)/lib/relocatable/dev/librflxlang.so
	touch $(PARSER)
	@# RapidFlux
	find librapidflux rapidflux -type f -exec touch {} +
	touch target/debug/librapidflux.so
	touch $(RAPIDFLUX)
	@# RecordFlux
	touch $(DEVEL_VENV)/bin/rflx

# --- Clean ---

.PHONY: clean clean_build clean_cache clean_ci_sim clean_all

clean: clean_build clean_cache
	rm -rf .coverage .coverage.* .hypothesis doc/language_reference/build doc/user_guide/build
	$(MAKE) -C examples/apps/wireguard clean
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C examples/apps/spdm_responder clean
	$(MAKE) -C examples/apps/dccp clean
	$(MAKE) -C rflx/ide/vscode clean
	$(MAKE) -C doc/examples clean

clean_build:
	rm -rf $(BUILD_DIR)

clean_cache:
	find -name ".*_cache" -type d -exec rm -vrf {} +

clean_ci_sim:
	rm -rf $(CI_SIM_VENV)
	rm -rf $(DEVEL_VENV)
	rm -rf $(POETRY_VENV)

clean_all: clean clean_ci_sim
	$(RM) -r $(DEVEL_VENV) $(POETRY_VENV) $(BIN_DIR) $(GENERATED_DIR) $(CARGO_HOME) rflx/lang rflx/rapidflux*.so pyproject.toml
	test -d $(LANGKIT_DIR) && touch $(LANGKIT_DIR)/langkit/py.typed || true
	@$(call remove_repo,$(DEVUTILS_DIR))
	@$(call remove_repo,$(GNATCOLL_DIR))
	@$(call remove_repo,$(LANGKIT_DIR))
	@$(call remove_repo,$(ADASAT_DIR))
