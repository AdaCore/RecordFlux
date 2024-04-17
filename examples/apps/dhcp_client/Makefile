include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = generated/rflx-dhcp_client.ads
BIN = obj/dhcp_client
BIN_OPTIMIZED = obj_optimized/dhcp_client
CWD := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))

.PHONY: test build prove generate clean test_binary_size

test: $(BIN) test_binary_size
	../../../devutils/linux/run $(CWD) tests/run

test_binary_size: $(BIN_OPTIMIZED)
	test $(shell size -A -d obj_optimized/dhcp_client | grep .text | awk '{ print $$2 }') -le 173000

build: $(BIN)

prove: $(GNATPROVE_CACHE_DIR) $(GENERATED)
	$(GNATPROVE) -Pdhcp_client

generate: $(GENERATED)

clean:
	$(RM) -r generated/* obj/*

$(GENERATED): $(SPECS) | generated
	rflx generate -d generated --debug built-in $^

generated:
	mkdir -p generated

$(BIN): $(GENERATED) $(wildcard generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=asserts_enabled

$(BIN_OPTIMIZED): $(GENERATED) $(wildcard generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=optimized
