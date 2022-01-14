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

.PHONY: test test_optimized prove prove_cvc4 clean

test: $(test_files)
	gprbuild -P$(project) -Xtest=$(TEST) -Xgnat=$(GNAT)
	cd ../.. && $(test_bin)

test_optimized: $(test_files)
	gprbuild -P$(project) -Xtype=optimized -Xgnat=$(GNAT)
	cd ../.. && $(test_bin)

prove: $(proof_sessions) $(test_files)
	gnatprove -P$(project) -Xtest=$(TEST) --memcached-server=localhost:11211 $(GNATPROVE_ARGS)

prove_cvc4: $(test_files)
	gnatprove -P$(project) --prover=cvc4 --steps=200000 --timeout=120 --warnings=continue -u rflx-ipv4 -u rflx-ipv4-packet -u rflx-in_ipv4 -u rflx-in_ipv4-contains -u rflx-in_ipv4-tests --memcached-server=localhost:11211 $(GNATPROVE_ARGS)

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
