specs = $(wildcard specs/*.rflx)
src = generated/rflx.ads
bin = obj/ping
proof_archive = proof.tar.zst
proof_archive_type = $(shell file -bi $(proof_archive))
proof_sessions = proof/sessions
proof_session_files = $(shell find proof -type f -name why3session.xml -o -name why3shapes.gz)

.PHONY: test test_python test_spark build prove generate clean update_proof reset_proof clean_proof

test: test_python test_spark

test_python:
	sudo -n timeout 5 `which python3` ping.py 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

test_spark: $(bin)
	sudo -n timeout 3 obj/ping 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

build: $(bin)

prove: $(src) $(proof_sessions)
	gnatprove -Pping

generate: $(src)

clean:
	rm -rf generated/* obj python

update_proof: $(proof_archive)

reset_proof:
	rm -rf proof/*

clean_proof:
	rm -rf proof/*
	mkdir $(proof_sessions)

$(src): $(specs)
	rflx generate -d generated --ignore-unsupported-checksum $^

$(bin): $(src) $(wildcard src/*)
	gprbuild -Pping

ifneq (,$(findstring zstd; charset=binary,"$(proof_archive_type)"))
$(proof_sessions):
	$(if $(shell tar -tlf $(proof_archive) | egrep -v 'why3session.xml|why3shapes.gz'),$(error unexpected files in ${proof_archive}))
	tar -x -f $(proof_archive)
else
$(proof_sessions):
	$(warning skipping extraction of '$(proof_archive)' with unexpected format '$(proof_archive_type)')
endif

$(proof_archive): $(proof_session_files)
	tar -c --zstd -f $@ $^
