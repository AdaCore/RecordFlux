-include devutils/Makefile.common

.DEFAULT_GOAL := all

VERBOSE ?= @
RECORDFLUX_ORIGIN ?= https://github.com/AdaCore

PYTHON_PACKAGES = server
DEVUTILS_HEAD = b196fbb5b791f6b80a91a5f78d6ef99e895a1e8c

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

$(shell $(call reinit_repo,devutils,$(DEVUTILS_HEAD)))

.PHONY: all

all: check test 

.PHONY: init deinit

init: devutils
	$(VERBOSE)$(call checkout_repo,devutils,$(DEVUTILS_HEAD))
	$(VERBOSE)ln -sf devutils/pyproject.toml

deinit:
	$(VERBOSE)$(call remove_repo,devutils)
	$(VERBOSE)rm pyproject.toml

devutils:
	$(VERBOSE)git clone $(RECORDFLUX_ORIGIN)/RecordFlux-devutils.git devutils

.PHONY: install install_server install_client

install: install_server install_client

install_server:
	cd server && pip3 install .

install_client: install_client_devel
	cd client && npm run compile && npm install -g @vscode/vsce && vsce package && code --install-extension recordflux-0.0.1.vsix

.PHONY: install_devel install_client_devel install_server_devel

install_devel: install_server_devel install_client

install_client_devel:
	cd client && . ~/.nvm/nvm.sh && nvm install && npm install

install_server_devel:
	$(MAKE) -C devutils install_devel
	pip3 install -e server[devel]

.PHONY: test test_server test_client coverage

test: test_server test_client

test_server:
	cd server && export PYTHONPATH=src && python3 -m pytest -vv --cov=src --cov-branch --cov-fail-under=80 --cov-report=term-missing:skip-covered .

test_client:
	cd client && npm test	