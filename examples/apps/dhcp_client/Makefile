specs = $(wildcard specs/*.rflx)
src = generated/rflx-dhcp_client.ads
bin = obj/dhcp_client
proof_archive = proof.tar.zst
proof_archive_type = $(shell file -bi $(proof_archive))
proof_sessions = proof/sessions
proof_session_files = $(shell find proof -type f -name why3session.xml -o -name why3shapes.gz)

.PHONY: test build prove generate clean update_proof reset_proof clean_proof

test: $(bin)
	sudo tests/run

build: $(bin)

prove: $(src) $(proof_sessions)
	gnatprove -Pdhcp_client

generate: $(src)

clean:
	rm -rf generated/* obj

update_proof: $(proof_archive)

reset_proof:
	rm -rf proof/*

clean_proof:
	rm -rf proof/*
	mkdir $(proof_sessions)

$(src): $(specs)
	rflx generate -d generated --debug $^

$(bin): $(src) $(wildcard src/*)
	gprbuild -Pdhcp_client

ifneq (,$(findstring zstd; charset=binary,$(proof_archive_type)))
$(proof_sessions):
	$(if $(shell tar -tlf $(proof_archive) | egrep -v 'why3session.xml|why3shapes.gz'),$(error unexpected files in ${proof_archive}))
	tar -x -f $(proof_archive)
else
$(proof_sessions):
	$(warning skipping extraction of '$(proof_archive)' with unexpected format '$(proof_archive_type)')
endif

$(proof_archive): $(proof_session_files)
	tar -c --zstd -f $@ $^
