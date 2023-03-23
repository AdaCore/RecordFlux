BUILD_PROCS   ?= $(shell nproc)
SPHINXOPTS    ?= -j$(BUILD_PROCS)
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = .
BUILDDIR      = build

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help regenerate_help Makefile

%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

HELP_FILES = \
   90-rflx--help.txt \
   90-rflx-check--help.txt \
   90-rflx-generate--help.txt \
   90-rflx-graph--help.txt \
   90-rflx-validate--help.txt \
   90-rflx-setup_ide--help.txt \
   90-rflx-convert--help.txt

90-rflx-%--help.txt::
	@rflx $* --help > $@

90-rflx--help.txt:
	@rflx --help > $@

# Regenerate rflx help texsts for the documentation
# We do not do this automatically. The results are checked in as we don't
# have rflx available during documentation creation.
regenerate_help:: $(HELP_FILES)
