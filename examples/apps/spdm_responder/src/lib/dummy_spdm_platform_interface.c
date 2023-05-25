#include <string.h>
#include <dummy_spdm_platform_interface.h>
#include <spdm_platform_interface.h>

#define SESSION_TRANSCRIPT 0
#define MEASUREMENT_TRANSCRIPT 1

struct transcript {
    unsigned used;
    unsigned char kind;
    int size;
    void *data;
};

struct instance {
    unsigned char base_hash_algo;
    long base_asym_algo;
    unsigned char measurement_hash_algo;
    unsigned char dhe_named_group;
    int valid_nonce;
    unsigned char nonce[32];
    struct transcript transcripts[8];
    unsigned char dhe_key[512];
    unsigned dhe_key_size;
    unsigned char secure_session;
    unsigned transcript_stage;
    uint16 transcript_headers[16];
};

void spdm_platform_initialize(instance_t **instance)
{
    *instance = malloc(sizeof(instance_t));
    if(!*instance){
        errx(1, "failed to create instance");
    }
    memset(*instance, 0, sizeof(instance_t));
    (*instance)->transcript_headers[0]  = 0x8410; //[GET_VERSION].*
    (*instance)->transcript_headers[1]  = 0x0410; //[VERSION].*
    (*instance)->transcript_headers[2]  = 0xe111; //[GET_CAPABILITIES].*
    (*instance)->transcript_headers[3]  = 0x6111; //[CAPABILITIES].*
    (*instance)->transcript_headers[4]  = 0xe311; //[NEGOTIATE_ALGORITHMS].*
    (*instance)->transcript_headers[5]  = 0x6311; //[ALGORITHMS].*
    (*instance)->transcript_headers[6]  = 0xffff; //Hash of the specified certificate chain in DER format (i.e., KEY_EXCHANGE Param2)
    (*instance)->transcript_headers[7]  = 0xe411; //[KEY_EXCHANGE].*
    (*instance)->transcript_headers[8]  = 0x6411; //[KEY_EXCHANGE_RSP].* except the `Signature` and `ResponderVerifyData` fields.
    (*instance)->transcript_headers[9]  = 0xffff; //[KEY_EXCHANGE_RSP].Signature ([KEY_EXCHANGE_RSP].* except the `ResponderVerifyData` field.)
    (*instance)->transcript_headers[10] = 0xffff; //[KEY_EXCHANGE_RSP].ResponderVerifyData ([KEY_EXCHANGE].*)
    //Only if mutual authentication is used
    //(*instance)->transcript_headers[10] = 0xffff; //Hash of the specified certificate chain in DER format (i.e., FINISH Param2)
    (*instance)->transcript_headers[11] = 0xe511; //[FINISH].SPDM Header Fields
    (*instance)->transcript_headers[12] = 0xffff; //[FINISH].RequesterVerifyData
    (*instance)->transcript_headers[13] = 0x6511; //[FINISH_RSP].SPDM Header fields
    //Only if Handshake_In_The_Clear is set
    //(*instance)->transcript_headers[14] = 0xffff; //[FINISH_RSP].*
}

unsigned char spdm_platform_config_ct_exponent(__attribute__((unused)) instance_t *instance) {
    return 10;
}

boolean spdm_platform_config_cap_meas_fresh(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_meas(__attribute__((unused)) instance_t *instance) {
    //  The Responder supports MEASUREMENTS response and can perform signature generation.
    return 2;
}

boolean spdm_platform_config_cap_chal(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_cert(__attribute__((unused)) instance_t *instance) {
    //  Supported in WP 2.3.7
    return 1;
}

boolean spdm_platform_config_cap_cache(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_key_upd(__attribute__((unused)) instance_t *instance) {
    return 1;
}

boolean spdm_platform_config_cap_hbeat(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_encap(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_mut_auth(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_pub_key_id(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_mac(__attribute__((unused)) instance_t *instance) {
    return 1;
}

boolean spdm_platform_config_cap_encrypt(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_psk(__attribute__((unused)) instance_t *instance) {
    return 0;
}

boolean spdm_platform_config_cap_key_ex(__attribute__((unused)) instance_t *instance) {
    return 1;
}

boolean spdm_platform_config_cap_handshake_in_the_clear(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_select_measurement_hash_algo(instance_t *instance,
                                                         boolean tpm_alg_sha_256,
                                                         boolean tpm_alg_sha_384,
                                                         boolean tpm_alg_sha_512,
                                                         boolean tpm_alg_sha3_256,
                                                         boolean tpm_alg_sha3_384,
                                                         boolean tpm_alg_sha3_512,
                                                         boolean raw_bit_streams_only)
{
    if (tpm_alg_sha3_512) {
        instance->measurement_hash_algo = 64;
    } else if (tpm_alg_sha3_384) {
        instance->measurement_hash_algo = 32;
    } else if (tpm_alg_sha3_256) {
        instance->measurement_hash_algo = 16;
    } else if (tpm_alg_sha_512) {
        instance->measurement_hash_algo = 8;
    } else if (tpm_alg_sha_384) {
        instance->measurement_hash_algo = 4;
    } else if (tpm_alg_sha_256) {
        instance->measurement_hash_algo = 2;
    } else if (raw_bit_streams_only) {
        instance->measurement_hash_algo = 1;
    } else {
        // No mode set, unsupported
        instance->measurement_hash_algo = 0;
    }
    printf("instance->measurement_hash_algo=%u\n", instance->measurement_hash_algo);
    return instance->measurement_hash_algo;
}

long spdm_platform_select_base_asym_algo(instance_t *instance,
                                         boolean tpm_alg_ecdsa_ecc_nist_p384,
                                         boolean tpm_alg_rsapss_4096,
                                         boolean tpm_alg_rsassa_4096,
                                         boolean tpm_alg_ecdsa_ecc_nist_p256,
                                         boolean tpm_alg_rsapss_3072,
                                         boolean tpm_alg_rsassa_3072,
                                         boolean tpm_alg_rsapss_2048,
                                         boolean tpm_alg_rsassa_2048,
                                         boolean tpm_alg_ecdsa_ecc_nist_p521)
{
    if (tpm_alg_ecdsa_ecc_nist_p521) instance->base_asym_algo = 256;
    else if (tpm_alg_ecdsa_ecc_nist_p384) instance->base_asym_algo = 128;
    else if (tpm_alg_ecdsa_ecc_nist_p256) instance->base_asym_algo = 16;
    else if (tpm_alg_rsapss_4096) instance->base_asym_algo = 64;
    else if (tpm_alg_rsassa_4096) instance->base_asym_algo = 32;
    else if (tpm_alg_rsapss_3072) instance->base_asym_algo = 8;
    else if (tpm_alg_rsassa_3072) instance->base_asym_algo = 4;
    else if (tpm_alg_rsapss_2048) instance->base_asym_algo = 2;
    else if (tpm_alg_rsassa_2048) instance->base_asym_algo = 1;
    else errx(3, "No Base Asym Algo selected");
    printf("instance->base_asym_algo=%ld\n", instance->base_asym_algo);
    return instance->base_asym_algo;
}

unsigned char spdm_platform_select_base_hash_algo(instance_t *instance,
                                                  boolean tpm_alg_sha_256,
                                                  boolean tpm_alg_sha_384,
                                                  boolean tpm_alg_sha_512,
                                                  boolean tpm_alg_sha3_256,
                                                  boolean tpm_alg_sha3_384,
                                                  boolean tpm_alg_sha3_512)
{
    if (tpm_alg_sha3_512) instance->base_hash_algo = 32;
    else if (tpm_alg_sha3_384) instance->base_hash_algo = 16;
    else if (tpm_alg_sha3_256) instance->base_hash_algo = 8;
    else if (tpm_alg_sha_512) instance->base_hash_algo = 4;
    else if (tpm_alg_sha_384) instance->base_hash_algo = 2;
    else if (tpm_alg_sha_256) instance->base_hash_algo = 1;
    else errx(3, "No Base Hash Algo selected");
    printf("instance->base_hash_algo=%u\n", instance->base_hash_algo);
    return instance->base_hash_algo;
}

unsigned char spdm_platform_select_dhe(instance_t *instance,
                                       boolean secp521r1,
                                       boolean secp384r1,
                                       boolean secp256r1,
                                       boolean ffdhe4096,
                                       boolean ffdhe3072,
                                       boolean ffdhe2048)
{
    if (secp521r1) instance->dhe_named_group = 32;
    else if (secp384r1) instance->dhe_named_group = 16;
    else if (secp256r1) instance->dhe_named_group = 8;
    else if (ffdhe4096) instance->dhe_named_group = 4;
    else if (ffdhe3072) instance->dhe_named_group = 2;
    else if (ffdhe2048) instance->dhe_named_group = 1;
    else errx(4, "No DHE selected");
    printf("instance->dhe_named_group=%u\n", instance->dhe_named_group);
    return instance->dhe_named_group;
}

unsigned char spdm_platform_select_aead(__attribute__((unused)) instance_t *instance,
                                        boolean chacha20_poly1305,
                                        boolean aes_256_gcm,
                                        boolean aes_128_gcm)
{
    if (chacha20_poly1305) return 4;
    if (aes_256_gcm) return 2;
    if (aes_128_gcm) return 1;
    errx(5, "No AEAD selected");
    return 0;
}

long spdm_platform_select_rbaa(__attribute__((unused)) instance_t *instance,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p384,
                               boolean ra_tpm_alg_rsapss_4096,
                               boolean ra_tpm_alg_rsassa_4096,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p256,
                               boolean ra_tpm_alg_rsapss_3072,
                               boolean ra_tpm_alg_rsassa_3072,
                               boolean ra_tpm_alg_rsapss_2048,
                               boolean ra_tpm_alg_rsassa_2048,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p521)
{
    if (ra_tpm_alg_ecdsa_ecc_nist_p521) return 256;
    if (ra_tpm_alg_ecdsa_ecc_nist_p384) return 128;
    if (ra_tpm_alg_rsapss_4096) return 64;
    if (ra_tpm_alg_rsassa_4096) return 32;
    if (ra_tpm_alg_ecdsa_ecc_nist_p256) return 16;
    if (ra_tpm_alg_rsapss_3072) return 8;
    if (ra_tpm_alg_rsassa_3072) return 4;
    if (ra_tpm_alg_rsapss_2048) return 2;
    if (ra_tpm_alg_rsassa_2048) return 1;
    errx(6, "Invalid RBBA selected");
    return 0;
}

void spdm_platform_get_digests_data(__unused_cross__ instance_t *instance,
                                    __unused_cross__ char *data,
                                    long *length,
                                    unsigned char *slot_mask)
{
    __unused_cross__ void *raw_data;
    __unused_cross__ uintn size = 0;
    const long hash_size = spdm_get_hash_size(instance->base_hash_algo);
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    if(*length < hash_size){
        *length = 0;
        *slot_mask = 0;
        return;
    }
    res = spdm_hash_all(instance->base_hash_algo, raw_data, size, (void *)data);
    if(!res){
        errx(1, "failed to hash certificate");
    }
    if(*length < 2 * hash_size){
        *length = hash_size;
        *slot_mask = 1;
        return;
    }
    memcpy(data + hash_size, data, hash_size);
    if(*length < 3 * hash_size){
        *length = 2 * hash_size;
        *slot_mask = 3;
        return;
    }
    memcpy(data + 2 * hash_size, data, hash_size);
    *slot_mask = 7;
    *length = 3 * hash_size;
}

boolean spdm_platform_validate_certificate_request(__attribute__((unused)) instance_t *instance,
                                                   unsigned char slot,
                                                   __unused_cross__ unsigned short offset,
                                                   __unused_cross__ unsigned short length)
{
    if (slot > 3) return 0;
    printf("slot=%d, offset=%d, length=%d\n", slot, offset, length);
    return 1;
}

void spdm_platform_get_certificate_data (__unused_cross__ instance_t *instance,
                                         __unused_cross__ char *data,
                                         __attribute__((unused)) unsigned char slot,
                                         __unused_cross__ unsigned short offset,
                                         unsigned short *length,
                                         unsigned short *total_length)
{
    __unused_cross__ void *raw_data;
    uintn size = 0;
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    if(offset > size){
        *length = 0;
        return;
    }
    *total_length = size;
    if (size - offset < *length) {
        *length = size - offset;
    }
    memcpy(data, &((unsigned char *)raw_data)[offset], *length);
}

const char *measurements[] = {"[6:0]=00h immutable rom",
                              "[6:0]=01h mutable firmware",
                              "[6:0]=02h hardware configuration",
                              "[6:0]=03h firmware configuration",
                              "[6:0]=04h measurement manifest",
                              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};


unsigned char spdm_platform_get_number_of_indices (__attribute__((unused)) instance_t *instance)
{
    return sizeof(measurements) / sizeof(const char *);
}

unsigned char spdm_platform_get_number_of_indices_tcb (__attribute__((unused)) instance_t *instance)
{
    return sizeof(measurements) / sizeof(const char *);
}

void spdm_platform_get_nonce(instance_t *instance,
                             __unused_cross__ void *nonce)
{
    if(getrandom(instance->nonce, 32, 0) < 0){
        errx(2, "failed to get nonce");
    }
    instance->valid_nonce = 1;
    memcpy(nonce, instance->nonce, 32);
}

void spdm_platform_get_dmtf_measurement_field(instance_t *instance,
                                              unsigned index,
                                              unsigned *representation,
                                              unsigned *type,
                                              unsigned *length,
                                              __unused_cross__ void *buffer)
{
    if(index < 1 || index > spdm_platform_get_number_of_indices(instance)){
        errx(2, "invalid measurement index");
    }
    __unused_cross__ const char *measurement = measurements[index - 1];
    const unsigned meas_length = strlen(measurement);
    *representation = 1;
    *type = index < 6 ? index - 1 : 0;
    if(*length < meas_length){
        memcpy(buffer, measurement, *length);
    }else{
        memcpy(buffer, measurement, meas_length);
        *length = meas_length;
    }
}

void spdm_platform_get_meas_opaque_data(__attribute__((unused)) instance_t *instance,
                                        __attribute__((unused)) void *data,
                                        unsigned *length)
{
    *length = 0;
}

unsigned spdm_platform_get_new_transcript(instance_t *instance,
                                          unsigned char kind)
{
    if(kind > 1){
        return 0xff;
    }
    for(int i = 0; i < 8; i++){
        if(!(instance->transcripts[i].used)){
            instance->transcripts[i].used = 1;
            instance->transcripts[i].kind = kind;
            instance->transcripts[i].size = 0;
            return i;
        }
    }
    return 0xff;
}

boolean spdm_platform_valid_transcript_id(instance_t *instance,
                                          unsigned transcript)
{
    return transcript <= 8 && instance->transcripts[transcript].used;
}

unsigned spdm_platform_reset_transcript(instance_t *instance,
                                        unsigned transcript,
                                        unsigned char kind)
{
    if(!spdm_platform_valid_transcript_id(instance, transcript)){
        return transcript;
    }
    if(instance->transcripts[transcript].used){
        free(instance->transcripts[transcript].data);
        instance->transcripts[transcript].size = 0;
        instance->transcripts[transcript].used = 0;
        instance->transcripts[transcript].data = 0;
        if(instance->transcripts[transcript].kind == SESSION_TRANSCRIPT){
            instance->transcript_stage = 0;
        }
    }
    return spdm_platform_get_new_transcript(instance, kind);
}

boolean spdm_platform_update_transcript(instance_t *instance,
                                        unsigned transcript,
                                        __unused_cross__ const void *data,
                                        __unused_cross__ unsigned offset,
                                        unsigned length)
{
    if(instance->transcripts[transcript].kind == SESSION_TRANSCRIPT){
        printf("transcript stage %u: %04hx\n",
               instance->transcript_stage,
               instance->transcript_headers[instance->transcript_stage]);
        if(instance->transcript_headers[instance->transcript_stage] != 0xffff
                && instance->transcript_headers[instance->transcript_stage] != *(uint16 *)data){
            errx(1, "unexpected transcript stage, expected %04hx, got %04hx",
                    instance->transcript_headers[instance->transcript_stage],
                    *(uint16 *)data);
        }
        instance->transcript_stage = instance->transcript_stage + 1;
    }
    if(!spdm_platform_valid_transcript_id(instance, transcript)){
        errx(1, "invalid transcript slot: %u", transcript);
        return 0;
    }
    instance->transcripts[transcript].data = realloc(instance->transcripts[transcript].data,
                                                     instance->transcripts[transcript].size + length);
    if(!instance->transcripts[transcript].data){
        errx(1, "realloc failed");
        return 0;
    }
    memcpy(instance->transcripts[transcript].data + instance->transcripts[transcript].size, data + offset, length);
    instance->transcripts[transcript].size = instance->transcripts[transcript].size + length;
    return 1;
}

boolean spdm_platform_update_transcript_nonce(instance_t *instance,
                                              unsigned transcript)
{
    if(!instance->valid_nonce){
        return 0;
    }
    instance->valid_nonce = 0;
    return spdm_platform_update_transcript(instance, transcript, instance->nonce, 0, 32);
}

void spdm_platform_get_signature(instance_t *instance,
                                 unsigned transcript,
                                 __attribute__((unused)) unsigned char slot,
                                 __unused_cross__ void *signature,
                                 unsigned *length)
{
    __unused_cross__ const spdm_version_number_t version = {0, 0, 1, 1};
    __unused_cross__ const unsigned hash_size = spdm_get_hash_size(instance->base_hash_algo);
    __unused_cross__ unsigned char hash_data[*length];
    if(slot >= 3){
        *length = 0;
        return;
    }
    uintn sig_size = *length;
    if(!spdm_platform_valid_transcript_id(instance, transcript)){
        errx(1, "invalid transcript slot: %u", transcript);
    }
    boolean res = spdm_hash_all(instance->base_hash_algo,
                                instance->transcripts[transcript].data,
                                instance->transcripts[transcript].size,
                                hash_data);
    if(!res){
        errx(1, "failed to hash summary");
    }
    __unused_cross__ unsigned code = 0;
    if(instance->transcripts[transcript].kind == MEASUREMENT_TRANSCRIPT){
        code = SPDM_MEASUREMENTS;
    }
    if(instance->transcripts[transcript].kind == SESSION_TRANSCRIPT){
        code = SPDM_KEY_EXCHANGE_RSP;
    }
    if(!spdm_responder_data_sign(version,
                                 code,
                                 instance->base_asym_algo,
                                 instance->base_hash_algo,
                                 1,
                                 (const uint8 *)&hash_data,
                                 hash_size,
                                 signature,
                                 &sig_size)){
        sig_size = 0;
        printf("failed to sign\n");
    }
    *length = sig_size;
    printf("signature_length=%u\n", *length);
}

void spdm_platform_get_exchange_data (__unused_cross__ instance_t *instance,
                                      __unused_cross__ void *data,
                                      unsigned length)
{
    uintn dhe_key_size = spdm_get_dhe_pub_key_size(instance->dhe_named_group);
    __unused_cross__ void *dhe_context = spdm_secured_message_dhe_new(instance->dhe_named_group);
    __unused_cross__ uint8 dhe_key[dhe_key_size];
    spdm_secured_message_dhe_generate_key(instance->dhe_named_group, dhe_context, dhe_key, &dhe_key_size);
    uintn dhe_priv_key_size = sizeof(instance->dhe_key);
    if(!spdm_dhe_compute_key(
            instance->dhe_named_group,
            dhe_context,
            data, length,
            instance->dhe_key, &dhe_priv_key_size)){
        errx(1, "failed to compute dhe key");
    }
    instance->dhe_key_size = dhe_priv_key_size;
    spdm_secured_message_dhe_free(instance->dhe_named_group, dhe_context);
    if(length < dhe_key_size){
        errx(1, "insufficient size for dhe key");
    }
    memcpy(data, dhe_key, dhe_key_size);
}

unsigned char spdm_platform_get_heartbeat_period (__attribute__((unused)) instance_t *instance)
{
    return 0;
}

boolean spdm_platform_valid_session_id (__attribute__((unused)) instance_t *instance,
                                        __attribute__((unused)) unsigned short session_id)
{
    return 1;
}

unsigned short spdm_platform_get_session_id (__attribute__((unused)) instance_t *instance,
                                             unsigned short session_id)
{
    return ~session_id;
}

boolean spdm_platform_use_mutual_auth (__attribute__((unused)) instance_t *instance)
{
    return 0;
}

void spdm_platform_get_summary_hash(__unused_cross__ instance_t *instance,
                                    __unused_cross__ const void *summary,
                                    __unused_cross__ unsigned summary_length,
                                    __unused_cross__ void *hash,
                                    unsigned *hash_length)
{
    boolean res = spdm_hash_all(instance->base_hash_algo, summary, summary_length, hash);
    if(!res){
        errx(1, "failed to hash summary");
    }
    *hash_length = spdm_get_hash_size(instance->base_hash_algo);
}

unsigned char spdm_platform_update_transcript_cert(instance_t *instance,
                                             unsigned transcript,
                                             unsigned char slot)
{
    if(slot != 0){
        return 0;
    }
    __unused_cross__ void *raw_data = 0;
    uintn size = 0;
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    return spdm_platform_update_transcript(instance, transcript, raw_data, 0, size);
}

void spdm_platform_get_key_ex_opaque_data(__attribute__((unused)) instance_t *instance,
                                          __attribute__((unused)) const void *req_data,
                                          __attribute__((unused)) unsigned req_length,
                                          __attribute__((unused)) void *data,
                                          __attribute__((unused)) unsigned *length)
{
    if(req_length > *length){
        *length = 0;
        return;
    }
    *length = req_length;
    memcpy(data, req_data, *length);
}

void spdm_platform_get_key_ex_verify_data(__unused_cross__ instance_t *instance,
                                          __attribute__((unused)) unsigned transcript,
                                          __attribute__((unused)) unsigned char slot,
                                          __attribute__((unused)) void *data,
                                          unsigned *length)
{
    *length = spdm_get_hash_size(instance->base_hash_algo);
}

boolean spdm_platform_validate_finish_signature(__attribute__((unused)) instance_t *instance,
                                                __attribute__((unused)) unsigned transcript,
                                                __attribute__((unused)) const void *signature,
                                                __attribute__((unused)) unsigned length,
                                                __attribute__((unused)) unsigned char slot)
{
    return 1;
}

boolean spdm_platform_validate_finish_hmac(__attribute__((unused)) instance_t *instance,
                                           __attribute__((unused)) unsigned transcript,
                                           __attribute__((unused)) const void *hmac,
                                           __attribute__((unused)) unsigned length,
                                           __attribute__((unused)) unsigned char slot)
{
    return 1;
}

void spdm_platform_get_finish_verify_data(__unused_cross__ instance_t *instance,
                                          __attribute__((unused)) unsigned transcript,
                                          __attribute__((unused)) unsigned char slot,
                                          __attribute__((unused)) void *data,
                                          unsigned *length)
{
    *length = spdm_get_hash_size(instance->base_hash_algo);
}

unsigned char spdm_platform_set_session_phase(instance_t *instance,
                                              unsigned char phase,
                                              __attribute__((unused)) unsigned transcript,
                                              __attribute__((unused)) unsigned char slot)
{
    if(phase < 2){
        return spdm_platform_reset_session_phase(instance);
    }
    instance->secure_session = phase;
    return phase;
}

unsigned char spdm_platform_reset_session_phase(instance_t *instance)
{
    if(instance->secure_session > 1){
        memset(instance->dhe_key, 0, 512);
        instance->dhe_key_size = 0;
    }
    instance->secure_session = 1;
    return 1;
}

boolean spdm_platform_key_update(__attribute__((unused)) instance_t *instance,
                                 __attribute__((unused)) unsigned operation,
                                 __attribute__((unused)) unsigned tag)
{
    return 1;
}
