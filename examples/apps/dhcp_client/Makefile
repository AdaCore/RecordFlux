specs = $(wildcard specs/*.rflx)
src = generated/rflx-dhcp_client.ads
bin = obj/dhcp_client

.PHONY: test build prove generate clean clean_proof

test: $(bin)
	sudo tests/run

build: $(bin)

prove: $(src)
	gnatprove -Pdhcp_client

generate: $(src)

clean:
	rm -rf generated/* obj

clean_proof:
	rm -rf proof/*

$(src): $(specs)
	rflx generate -d generated --debug $^

$(bin): $(src)  $(wildcard src/*)
	gprbuild -Pdhcp_client
