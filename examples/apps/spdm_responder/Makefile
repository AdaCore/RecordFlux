SHELL := /bin/bash

GENERATED_FILES := rflx.ads \
    rflx-rflx_arithmetic.adb \
    rflx-rflx_arithmetic.ads \
    rflx-rflx_builtin_types.ads \
    rflx-rflx_builtin_types-conversions.ads \
    rflx-rflx_generic_types.ads \
    rflx-rflx_generic_types-generic_operations.adb \
    rflx-rflx_generic_types-generic_operations.ads \
    rflx-rflx_generic_types-generic_operators.ads \
    rflx-rflx_message_sequence.adb \
    rflx-rflx_message_sequence.ads \
    rflx-rflx_scalar_sequence.adb \
    rflx-rflx_scalar_sequence.ads \
    rflx-rflx_types.ads \
    rflx-rflx_types-operations.ads \
    rflx-rflx_types-operators.ads \
    rflx-spdm.ads \
    rflx-spdm-algorithms_response.adb \
    rflx-spdm-algorithms_response.ads \
    rflx-spdm-alg_struct.adb \
    rflx-spdm-alg_struct.ads \
    rflx-spdm-capabilities_response.adb \
    rflx-spdm-capabilities_response.ads \
    rflx-spdm-certificate_response.adb \
    rflx-spdm-certificate_response.ads \
    rflx-spdm-digests_response.adb \
    rflx-spdm-digests_response.ads \
    rflx-spdm-error_response.adb \
    rflx-spdm-error_response.ads \
    rflx-spdm-ext_alg.adb \
    rflx-spdm-ext_alg.ads \
    rflx-spdm-ext_algs.ads \
    rflx-spdm-get_capabilities_request.adb \
    rflx-spdm-get_capabilities_request.ads \
    rflx-spdm-get_certificate_request.adb \
    rflx-spdm-get_certificate_request.ads \
    rflx-spdm-get_digests_request.adb \
    rflx-spdm-get_digests_request.ads \
    rflx-spdm-get_measurements_request.adb \
    rflx-spdm-get_measurements_request.ads \
    rflx-spdm-get_version_request.adb \
    rflx-spdm-get_version_request.ads \
    rflx-spdm-measurements_response.adb \
    rflx-spdm-measurements_response.ads \
    rflx-spdm-measurement_block.adb \
    rflx-spdm-measurement_block.ads \
    rflx-spdm-measurement_record.ads \
    rflx-spdm-negotiate_algorithms_request.adb \
    rflx-spdm-negotiate_algorithms_request.ads \
    rflx-spdm-request.adb \
    rflx-spdm-request.ads \
    rflx-spdm-req_alg_structs.ads \
    rflx-spdm-response.adb \
    rflx-spdm-response.ads \
    rflx-spdm-response_not_ready_data.adb \
    rflx-spdm-response_not_ready_data.ads \
    rflx-spdm-resp_alg_structs.ads \
    rflx-spdm-slot_mask.adb \
    rflx-spdm-slot_mask.ads \
    rflx-spdm-version_number_entries.ads \
    rflx-spdm-version_number_entry.adb \
    rflx-spdm-version_number_entry.ads \
    rflx-spdm-version_response.adb \
    rflx-spdm-version_response.ads \
    rflx-spdm_responder.ads \
    rflx-spdm_responder-digests_data.adb \
    rflx-spdm_responder-digests_data.ads \
    rflx-spdm_responder-session.adb \
    rflx-spdm_responder-session.ads \
    rflx-spdm_responder-session_allocator.adb \
    rflx-spdm_responder-session_allocator.ads \
    rflx-spdm-nonce.adb \
    rflx-spdm-nonce.ads \
    rflx-spdm-dmtf_measurement_field.adb \
    rflx-spdm-dmtf_measurement_field.ads \
    rflx-spdm_responder-signature.adb \
    rflx-spdm_responder-signature.ads \
    rflx-spdm_responder-opaque_data.adb \
    rflx-spdm_responder-opaque_data.ads \
    rflx-spdm-challenge_auth_response.adb \
    rflx-spdm-challenge_auth_response.ads \
    rflx-spdm-challenge_request.adb \
    rflx-spdm-challenge_request.ads \
    rflx-spdm-key_exchange_request.adb \
    rflx-spdm-key_exchange_request.ads \
    rflx-spdm-key_exchange_response.adb \
    rflx-spdm-key_exchange_response.ads \
    rflx-spdm-finish_request.adb \
    rflx-spdm-finish_request.ads \
    rflx-spdm-finish_response.adb \
    rflx-spdm-finish_response.ads \
    rflx-spdm-key_update_ack_response.adb \
    rflx-spdm-key_update_ack_response.ads \
    rflx-spdm-key_update_request.adb \
    rflx-spdm-key_update_request.ads \
    rflx-spdm-end_session_request.adb \
    rflx-spdm-end_session_request.ads \
    rflx-spdm-end_session_response.adb \
    rflx-spdm-end_session_response.ads \
    rflx-spdm_responder-hash.adb \
    rflx-spdm_responder-hash.ads \
    rflx-spdm_responder-exchange_data.adb \
    rflx-spdm_responder-exchange_data.ads \
    rflx-spdm_responder-measurement_summary.adb \
    rflx-spdm_responder-measurement_summary.ads \
    rflx-spdm-respond_if_ready_request.adb \
    rflx-spdm-respond_if_ready_request.ads

GENERATED = $(addprefix build/generated/,$(GENERATED_FILES))

SOURCE_SPECIFICATIONS = \
	specs/spdm_responder.rflx \
	specs/spdm.rflx

MISSING_SPECIFICATIONS = $(filter-out $(SOURCE_SPECIFICATIONS),$(wildcard specs/*.rflx))
ifneq ($(MISSING_SPECIFICATIONS),)
$(error Unhandled specifications: $(MISSING_SPECIFICATIONS))
endif

SPECIFICATIONS = $(addprefix specs/, $(foreach spec, $(SOURCE_SPECIFICATIONS), $(notdir $(spec))))

INTEGRATION_FILES = \
	specs/spdm_responder.rfi

RFLX ?= $(shell command -v rflx)

.PHONY: all

all: check test prove

.PHONY: check check_spec

check: check_spec

check_spec: $(SPECIFICATIONS) | $(INTEGRATION_FILES) $(RFLX)
	timeout -k 60 900 $(RFLX) check $^

.PHONY: test test_cross test_size

test: test_cross test_size lib

test_cross: build/arm/example/main build/riscv64/example/main libarm libriscv64

test_size: build/arm/example/main build/riscv64/example/main
	tools/check_size.py arm build/arm/example/main .text 45000
	tools/check_size.py riscv64 build/riscv64/example/main .text 37000

build/lib/libspdm.a: $(GENERATED)
	gprbuild -j0 -P spdm

build/%/lib/libspdm.a: $(GENERATED)
	gprbuild -j0 -P spdm -XTARGET=$*

build/%/example/main: $(GENERATED)
	gprbuild -j0 -P build.gpr -XTARGET=$*
	test -f $@

build/generated:
	mkdir -p build/generated

build/generated/%: $(SPECIFICATIONS) | build/generated $(INTEGRATION_FILES) $(RFLX)
	timeout -k 60 3600 $(RFLX) generate $^ -d build/generated

.PHONY: lib libarm libriscv64

lib: build/lib/libspdm.a

libarm: build/arm/lib/libspdm.a

libriscv64: build/riscv64/lib/libspdm.a

.PHONY: prove

prove: $(GENERATED)
	gnatprove -P build_lib.gpr -j0 -XTARGET=riscv64 -u responder
	gnatprove -P build.gpr -j0 -XTARGET=riscv64 -u main

.PHONY: generate

generate: $(GENERATED)

.PHONY: clean

clean:
	rm -rf build
