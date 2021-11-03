specs = $(wildcard specs/*.rflx)
src = generated/rflx.ads
bin = obj/ping
proof_archive = proof.tar.zst
proof_sessions = $(shell find proof -type f -name why3session.xml -o -name why3shapes.gz)

.PHONY: test test_python test_spark build prove generate clean init_proof update_proof reset_proof clean_proof

test: test_python test_spark

test_python:
	sudo -n timeout 5 `which python3` ping.py 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

test_spark: $(bin)
	sudo -n timeout 3 obj/ping 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

build: $(bin)

prove: $(src)
	gnatprove -Pping

generate: $(src)

clean:
	rm -rf generated/* obj python

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
	rflx generate -d generated --ignore-unsupported-checksum $^

$(bin): $(src) $(wildcard src/*)
	gprbuild -Pping
