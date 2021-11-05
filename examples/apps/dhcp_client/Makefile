specs = $(wildcard specs/*.rflx)
src = generated/rflx-dhcp_client.ads
bin = obj/dhcp_client
proof_parent_dir = .

include ../../../Makefile.common

.PHONY: test build prove generate clean

test: $(bin)
	sudo tests/run

build: $(bin)

prove: $(src) $(proof_sessions)
	gnatprove -Pdhcp_client

generate: $(src)

clean:
	rm -rf generated/* obj

$(src): $(specs)
	rflx generate -d generated --debug $^

$(bin): $(src) $(wildcard src/*)
	gprbuild -Pdhcp_client
