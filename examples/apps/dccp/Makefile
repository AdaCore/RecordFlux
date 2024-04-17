include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = generated/rflx-dccp-packet.ads
BIN = obj/dccp_client obj/dccp_server
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
	$(RM) -r generated/* obj/*

$(GENERATED): $(SPECS) | generated
	rflx generate -d generated $^

generated:
	mkdir -p generated

$(BIN): $(GENERATED) $(wildcard generated/*)
	gprbuild -Pdccp -Xgnat=$(GNAT)

$(GRAPHS): $(SPECS)
	rflx graph -d graphs $^
