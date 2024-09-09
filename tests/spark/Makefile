include ../../Makefile.common

project := test
ifdef TEST
	test_bin := build/test/test_$(TEST)
else
	test_bin := build/test/test
endif
test_files := $(wildcard test.gpr *.ad? generated/rflx-*.ad?)

noprefix_dir := ../../build/noprefix

ifneq ($(NOPREFIX),)
project := $(noprefix_dir)/test
test_files := $(addprefix $(noprefix_dir)/, $(subst rflx-,,$(test_files)))
endif

.PHONY: test test_optimized prove clean

test: $(test_files)
	gprbuild -P$(project) -Xtest=$(TEST) -Xgnat=$(GNAT) -Xmode=asserts_enabled
	cd ../.. && $(test_bin)

test_optimized: $(test_files)
	gprbuild -P$(project) -Xgnat=$(GNAT) -Xmode=optimized
	cd ../.. && $(test_bin)

build_strict: $(test_files)
	gprbuild -P$(project) -Xgnat=$(GNAT) -Xmode=strict

prove: $(GNATPROVE_CACHE_DIR) $(test_files)
	$(GNATPROVE) -P$(project) -Xtest=$(TEST)

clean:
	gprclean -P$(project)

remove-prefix = $(VERBOSE) \
	mkdir -p $(dir $@) && \
	sed 's/\(RFLX\.\|rflx-\)//g' $< > $@.tmp && \
	mv $@.tmp $@

$(noprefix_dir)/generated/%: ../../tests/spark/generated/rflx-%
	$(remove-prefix)

$(noprefix_dir)/%: ../../tests/spark/rflx-%
	$(remove-prefix)

$(noprefix_dir)/%: %
	$(remove-prefix)
