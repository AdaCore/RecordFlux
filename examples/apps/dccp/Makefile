include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = build/generated/rflx-dccp-packet.ads
BIN = build/obj/dccp_client build/obj/dccp_server
GRAPHS = graphs/DCCP_Option.svg graphs/DCCP_Packet.svg

.PHONY: test build prove generate clean

test: $(BIN)
	rflx validate --coverage -v tests/samples/valid -- $(SPECS) DCCP::Packet
	tests/run

build: $(BIN)

prove: $(GNATPROVE_CACHE_DIR) $(GENERATED)
	$(GNATPROVE) -Pdccp

generate: $(GENERATED) $(GRAPHS)

clean:
	rm -rf build

$(GENERATED): $(SPECS) | build/generated
	rflx generate -d build/generated $^

build/generated:
	mkdir -p build/generated

$(BIN): $(GENERATED) $(wildcard build/generated/*)
	gprbuild -Pdccp -Xgnat=$(GNAT)

$(GRAPHS): $(SPECS)
	rflx graph -d graphs $^
