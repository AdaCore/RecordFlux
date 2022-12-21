VERBOSE ?= @
TEST_PROCS ?= $(shell nproc)
RECORDFLUX_ORIGIN ?= https://github.com/Componolit

BUILD_DIR = build
PYTHON_PACKAGES = bin doc/conf.py examples/apps rflx tests tools stubs setup.py
PYTHON_STYLE_HEAD = 7f2584cd5e72fb6e7a80dcc8cce296f5821dcf8e

SHELL = /bin/bash
PYTEST = python3 -m pytest -n$(TEST_PROCS) -vv --timeout=7200

# Use GNATprove's file-based caching by default and ensure the directory exists.
GNATPROVE_CACHE ?= file:$(PWD)/$(BUILD_DIR)/gnatprove_cache

ifneq (,$(findstring file:,$(GNATPROVE_CACHE)))
GNATPROVE_CACHE_DIR = $(subst file:,,$(GNATPROVE_CACHE))
endif

export GNATPROVE_CACHE := $(GNATPROVE_CACHE)

# Switch to a specific revision of the git repository.
#
# @param $(1) directory of the git repository
# @param $(2) commit id
define checkout_repo
$(shell test -d $(1) && git -C $(1) fetch && git -C $(1) -c advice.detachedHead=false checkout $(2))
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

$(shell $(call reinit_repo,.config/python-style,$(PYTHON_STYLE_HEAD)))

.PHONY: all

all: check test prove

.PHONY: init deinit

init: .config/python-style
	$(VERBOSE)$(call checkout_repo,.config/python-style,$(PYTHON_STYLE_HEAD))
	$(VERBOSE)ln -sf .config/python-style/pyproject.toml
	$(VERBOSE)git update-index --skip-worktree pyproject.toml

deinit:
	$(VERBOSE)$(call remove_repo,.config/python-style)
	$(VERBOSE)ln -sf .config/pyproject.toml
	$(VERBOSE)git update-index --no-skip-worktree pyproject.toml

.config/python-style:
	$(VERBOSE)git clone $(RECORDFLUX_ORIGIN)/python-style.git .config/python-style

.PHONY: check check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_pydocstyle check_doc \
	format \
	test test_python test_python_unit test_python_integration test_python_property test_python_property_verification test_python_optimized test_python_coverage test_apps test_compilation test_binary_size test_specs test_installation \
	prove prove_tests prove_python_tests prove_apps \
	install_devel install_devel_edge upgrade_devel install_gnat printenv_gnat \
	generate \
	doc \
	dist \
	clean

check: check_packages check_dependencies check_black check_isort check_flake8 check_pylint check_mypy check_contracts check_pydocstyle check_doc

check_packages:
	tools/check_packages.py $(PYTHON_PACKAGES)

check_dependencies:
	tools/check_dependencies.py

check_black:
	black --check --diff --line-length 100 $(PYTHON_PACKAGES) ide/gnatstudio

check_isort:
	isort --check --diff $(PYTHON_PACKAGES) ide/gnatstudio

check_flake8:
	pflake8 $(PYTHON_PACKAGES) ide/gnatstudio

check_pylint:
	pylint $(PYTHON_PACKAGES)

check_mypy:
	mypy --pretty $(PYTHON_PACKAGES)

check_contracts:
	pyicontract-lint $(PYTHON_PACKAGES)

check_pydocstyle:
	pydocstyle $(PYTHON_PACKAGES)

check_doc:
	tools/check_doc.py

format:
	black -l 100 $(PYTHON_PACKAGES) ide/gnatstudio
	isort $(PYTHON_PACKAGES) ide/gnatstudio

test: test_python_coverage test_python_unit_coverage test_python_property test_compilation test_binary_size test_specs test_installation test_apps

test_python: test_python_unit test_python_integration test_python_compilation prove_python_tests test_python_tools test_python_ide

test_python_unit:
	$(PYTEST) tests/unit

test_python_integration:
	$(PYTEST) tests/integration

test_python_compilation:
	$(PYTEST) tests/compilation

test_python_property:
	$(PYTEST) tests/property

test_python_tools:
	$(PYTEST) tests/tools

test_python_ide:
	$(PYTEST) tests/ide

test_python_property_verification:
	$(PYTEST) tests/property_verification

test_python_optimized:
	PYTHONOPTIMIZE=1 $(PYTEST) tests/unit tests/integration tests/compilation

test_python_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov=tests/integration --cov-branch --cov-fail-under=100 --cov-report=term-missing:skip-covered tests/unit tests/integration

test_python_unit_coverage:
	timeout -k 60 7200 $(PYTEST) --cov=rflx --cov=tests/unit --cov-branch --cov-fail-under=97.02 --cov-report=term-missing:skip-covered tests/unit

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

test_installation:
	rm -rf $(BUILD_DIR)/venv $(BUILD_DIR)/test_installation
	mkdir -p $(BUILD_DIR)/test_installation
	python3 -m venv $(BUILD_DIR)/venv
	$(BUILD_DIR)/venv/bin/pip install .
	$(BUILD_DIR)/venv/bin/rflx --version
	HOME=$(BUILD_DIR)/test_installation $(BUILD_DIR)/venv/bin/rflx setup_ide
	test -f $(BUILD_DIR)/test_installation/.gnatstudio/plug-ins/recordflux.py

prove: prove_tests prove_python_tests prove_apps

prove_tests: $(GNATPROVE_CACHE_DIR)
	$(MAKE) -C tests/spark prove

prove_python_tests: export GNATPROVE_PROCS=1
prove_python_tests: $(GNATPROVE_CACHE_DIR)
	$(PYTEST) tests/verification

prove_apps: $(GNATPROVE_CACHE_DIR)
	$(MAKE) -C examples/apps/ping prove
	$(MAKE) -C examples/apps/dhcp_client prove

$(GNATPROVE_CACHE_DIR):
	mkdir -p $(GNATPROVE_CACHE_DIR)

install_devel:
	tools/check_pip_version.py
	$(MAKE) -C .config/python-style install_devel
	pip3 install -e ".[devel]"

install_git_hooks:
	install -m 755 tools/pre-{commit,push} .git/hooks/

upgrade_devel:
	tools/upgrade_dependencies.py

install_devel_edge: install_devel
	$(MAKE) -C .config/python-style install_devel_edge

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

generate:
	tools/generate_spark_test_code.py

doc: check_doc
	$(MAKE) -C doc html

dist:
	python3 -m build --sdist

clean:
	rm -rf $(BUILD_DIR) .coverage .hypothesis .mypy_cache .pytest_cache
	$(MAKE) -C tests/spark clean
	$(MAKE) -C examples/apps/ping clean
	$(MAKE) -C examples/apps/dhcp_client clean
	$(MAKE) -C doc clean
