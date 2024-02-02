BUILD_PROCS   ?= $(shell nproc)
SPHINXOPTS    ?= -j$(BUILD_PROCS)
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = .
BUILDDIR      = build

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help check_help regenerate_help clean_help Makefile

%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) -W --keep-going -n $(O)

SHELL = /bin/bash
COMMANDS = $(shell rflx |& sed -n 's/.*{\(.*\)}.*/\1/p' | tr ',' ' ')

help_file = 90-rflx-$(1)--help.txt

EXPECTED_HELP_FILES = 90-rflx--help.txt $(foreach CMD,$(COMMANDS),$(call help_file,$(CMD)))
EXISTING_HELP_FILES = $(wildcard 90-rflx-*-help.txt)
UNEXPECTED_HELP_FILES = $(filter-out $(EXPECTED_HELP_FILES),$(EXISTING_HELP_FILES))
MISSING_HELP_FILES = $(filter-out $(EXISTING_HELP_FILES),$(EXPECTED_HELP_FILES))

NORMALIZE = sed -e "s/workers (default: [0-9]*)/workers (default: NPROC)/" -e "s/optional arguments:/options:/"
CHECK = diff 90-rflx--help.txt <(rflx --help | $(NORMALIZE)) && $(foreach CMD,$(COMMANDS),diff $(call help_file,$(CMD)) <(rflx $(CMD) --help | $(NORMALIZE)) &&) exit

check_help:
	@test -z "$(MISSING_HELP_FILES)" || ( echo "Missing help files: $(MISSING_HELP_FILES)" && exit 1 )
	@test -z "$(UNEXPECTED_HELP_FILES)" || ( echo "Unexpected help files: $(UNEXPECTED_HELP_FILES)" && exit 1 )
	@( $(CHECK) ) || ( echo "Help texts are outdated" && exit 1 )

90-rflx-%--help.txt::
	@rflx $* --help | $(NORMALIZE) > $@

90-rflx--help.txt:
	@rflx --help | $(NORMALIZE) > $@

# Regenerate rflx help texts for the documentation
# We do not do this automatically. The results are checked in as we don't
# have rflx available during documentation creation.
regenerate_help:: clean_help $(EXPECTED_HELP_FILES)

clean_help:
	@rm -f 90-rflx-*-help.txt
