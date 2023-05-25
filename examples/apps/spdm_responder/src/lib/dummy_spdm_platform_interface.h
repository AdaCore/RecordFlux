#ifndef __DUMMY_SPDM_PLATFORM_INTERFACE_H__
#define __DUMMY_SPDM_PLATFORM_INTERFACE_H__

#define errx(...) {}
#define printf(...)
#define malloc(...) 0
#define realloc(...) 0
#define free(...)
#define memset(...)
#define read_responder_public_certificate_chain(...) 1
#define memcpy(...)
#define getrandom(...) 0
#define strlen(...) 0
#define spdm_get_hash_size(...) 0
#define spdm_get_measurement_hash_size(...) 0
#define spdm_hash_new(...) 0
#define spdm_hash_free(...)
#define spdm_hash_update(...) 1
#define spdm_hash_final(...) 1
#define spdm_hash_all(...) 1
#define spdm_responder_data_sign(...) 1
#define spdm_get_asym_signature_size(...) 0
#define spdm_get_dhe_pub_key_size(...) 0
#define spdm_secured_message_dhe_new(...) 0
#define spdm_secured_message_dhe_generate_key(...)
#define spdm_dhe_compute_key(...) 0
#define spdm_secured_message_dhe_free(...)
typedef struct {
    unsigned char alpha;
    unsigned char update_version_number;
    unsigned char minor_version;
    unsigned char major_version;
} spdm_version_number_t;
#define __unused_cross__ __attribute__((unused))
#define SPDM_MEASUREMENTS 0x60
#define SPDM_KEY_EXCHANGE_RSP 0x64

typedef unsigned long uintn;
typedef unsigned char boolean;
typedef unsigned char uint8;
typedef unsigned short uint16;

#endif
