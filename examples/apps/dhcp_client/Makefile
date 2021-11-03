specs = $(wildcard specs/*.rflx)
src = generated/rflx-dhcp_client.ads
bin = obj/dhcp_client
proof_archive = proof.tar.zst
proof_sessions = $(shell find proof -type f -name why3session.xml -o -name why3shapes.gz)

.PHONY: test build prove generate clean init_proof update_proof reset_proof clean_proof

test: $(bin)
	sudo tests/run

build: $(bin)

prove: $(src)
	gnatprove -Pdhcp_client

generate: $(src)

clean:
	rm -rf generated/* obj

$(proof_archive): $(proof_sessions)
	tar -c --zstd -f $@ $^

update_proof: $(proof_archive)

init_proof:
	$(if $(shell tar -tlf $(proof_archive) | egrep -v 'why3session.xml|why3shapes.gz'),$(error unexpected files in ${proof_archive}))
	tar -x -f $(proof_archive)

reset_proof: clean_proof init_proof

clean_proof:
	rm -rf proof/*

$(src): $(specs)
	rflx generate -d generated --debug $^

$(bin): $(src) $(wildcard src/*)
	gprbuild -Pdhcp_client
