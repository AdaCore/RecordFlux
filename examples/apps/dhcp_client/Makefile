include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = build/generated/rflx-dhcp_client.ads
BIN = build/obj/dhcp_client
BIN_OPTIMIZED = build/obj_optimized/dhcp_client

.PHONY: test build prove generate clean test_binary_size

test: $(BIN) test_binary_size
	tests/run

test_binary_size: $(BIN_OPTIMIZED)
	test $(shell size -A -d build/obj_optimized/dhcp_client | grep .text | awk '{ print $$2 }') -le 173000

build: $(BIN)

prove: $(GNATPROVE_CACHE_DIR) $(GENERATED)
	$(GNATPROVE) -Pdhcp_client

generate: $(GENERATED)

clean:
	rm -rf build

$(GENERATED): $(SPECS) | build/generated
	rflx generate -d build/generated --debug built-in $^

build/generated:
	mkdir -p build/generated

$(BIN): $(GENERATED) $(wildcard build/generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=asserts_enabled

$(BIN_OPTIMIZED): $(GENERATED) $(wildcard build/generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=optimized
