#ifndef __SPDM_PLATFORM_INTERFACE__
#define __SPDM_PLATFORM_INTERFACE__

/**
 * @file spdm_platform_interface.h
 *
 * @brief C based example platform implementation.
 *
 * This implementation provides an example on how to implement
 * a platform in C. It binds with the SPDM_C_Responder SPARK/Ada
 * implementation. Note that for pure SPARK/Ada implementation
 * this file is not required.
 */

/**
 * Boolean type to interact with Ada.
 * ```
 * True  => 1
 * False => 0
 * ```
 * All other values are undefined behaviour.
 */
typedef unsigned char boolean;

/**
 * @struct instance
 *
 * Implementation defined.
 *
 * This struct can be defined freely by the implementer to
 * hold any value required by the platform. Memory management
 * for this struct is also the responsibility of the
 * implementer.
 */
struct instance;

/**
 * Type name definition for struct instance.
 */
typedef struct instance instance_t;

/**
 * Ensure initialization of *instance.
 *
 * This function is both imlemented and called by the platform code.
 * It is called before the start of the state machine to ensure that
 * *instance is properly initialized.
 * If a platform implementation has a different implementation or
 * different means of initializing *instance this function can be removed.
 * It does not provide any functionality for the state machine itself.
 * When this function is removed, Plat_Initialize in spdm_c_responder.ads
 * needs to be removed, too.
 *
 * @param instance Pointer to instance pointer.
 */
void spdm_platform_initialize(instance_t **instance);

/**
 * Return CT exponent (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return CT exponent, 8 bit integer.
 */
unsigned char spdm_platform_config_ct_exponent(instance_t *instance);

/**
 * Indicate whether measurements without restart are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if measurements without restart are supported.
 */
boolean spdm_platform_config_cap_meas_fresh(instance_t *instance);

/**
 * Check which type of measurements are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return 0 if unsupported, 1 for plain measurements, 2 for signed measurements.
 */
unsigned char spdm_platform_config_cap_meas(instance_t *instance);

/**
 * Indicate whether challenge authentication is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if challenge authentication is supported.
 */
boolean spdm_platform_config_cap_chal(instance_t *instance);

/**
 * Indicate whether digests and certificate responses are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if digests and certificate responses are supported.
 */
boolean spdm_platform_config_cap_cert(instance_t *instance);

/**
 * Indicate whether responder is able to cache the negotiated state after reset (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if caching is supported.
 */
boolean spdm_platform_config_cap_cache(instance_t *instance);

/**
 * Indicate whether key update is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if key update is supported.
 */
boolean spdm_platform_config_cap_key_upd(instance_t *instance);

/**
 * Indicate whether heartbeat messages are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if heartbeat messages are supported.
 */
boolean spdm_platform_config_cap_hbeat(instance_t *instance);

/**
 * Indicate whether encapsulated messages are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if encapsulated messages are supported.
 */
boolean spdm_platform_config_cap_encap(instance_t *instance);

/**
 * Indicate whether mutual authentication is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if mutual authentication is supported.
 */
boolean spdm_platform_config_cap_mut_auth(instance_t *instance);

/**
 * Indicate whether the public key of the responder was provisioned to the requester (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if the public key was provisioned.
 */
boolean spdm_platform_config_cap_pub_key_id(instance_t *instance);

/**
 * Indicate whether message authentication is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if message authentication is supported.
 */
boolean spdm_platform_config_cap_mac(instance_t *instance);

/**
 * Indicate whether message encryption is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if message encryption is supported.
 */
boolean spdm_platform_config_cap_encrypt(instance_t *instance);

/**
 * Indicate whether pre-shared keys are supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if pre-shared keys are supported.
 */
boolean spdm_platform_config_cap_psk(instance_t *instance);

/**
 * Indicate whether key exchange is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if key exchange is supported.
 */
boolean spdm_platform_config_cap_key_ex(instance_t *instance);

/**
 * Indicate whether handshake without encryption or authentication is supported (DSP0274_1.1.0 [178]).
 *
 * @param instance Platform instance.
 * @return True if handshake in the clear is supported.
 */
boolean spdm_platform_config_cap_handshake_in_the_clear(instance_t *instance);

/**
 * Select measurement hash algorithm (DSP0274_1.1.0 [185]).
 *
 * The arguments describe the hash algorithms supported by the requester.
 * This function must select one of the provided algorithms or return 0.
 *
 * @param instance Platform instance.
 * @param tpm_alg_sha_256 SHA-256 supported and requested.
 * @param tpm_alg_sha_384 SHA-384 supported and requested.
 * @param tpm_alg_sha_512 SHA-512 supported and requested.
 * @param tpm_alg_sha3_256 SHA3-256 supported and requested.
 * @param tpm_alg_sha3_384 SHA3-384 supported and requested.
 * @param tpm_alg_sha3_512 SHA3-512 supported and requested.
 * @param raw_bit_streams_only Raw bit streams supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported        => 0
 *         raw_bit_streams_only => 1
 *         tpm_alg_sha_256      => 2
 *         tpm_alg_sha_384      => 4
 *         tpm_alg_sha_512      => 8
 *         tpm_alg_sha3_256     => 16
 *         tpm_alg_sha3_384     => 32
 *         tpm_alg_sha3_512     => 64
 *```
 */
unsigned char spdm_platform_select_measurement_hash_algo(instance_t *instance,
                                                         boolean tpm_alg_sha_256,
                                                         boolean tpm_alg_sha_384,
                                                         boolean tpm_alg_sha_512,
                                                         boolean tpm_alg_sha3_256,
                                                         boolean tpm_alg_sha3_384,
                                                         boolean tpm_alg_sha3_512,
                                                         boolean raw_bit_streams_only);

/**
 * Select base asymmetric algorithm (DSP0274_1.1.0 [185]).
 *
 * The arguments describe the signature algorithms supported by the requester.
 * This function must select one of the provided algorithms or return 0.
 *
 * @param instance Platform instance.
 * @param tpm_alg_ecdsa_ecc_nist_p384 ECDSA-ECC-384 supported and requested.
 * @param tpm_alg_rsapss_4096 RSAPSS-4096 supported and requested.
 * @param tpm_alg_rsassa_4096 RSASSA-4096 supported and requested.
 * @param tpm_alg_ecdsa_ecc_nist_p256 ECDSA-ECC-256 supported and requested.
 * @param tpm_alg_rsapss_3072 RSAPSS-3072 supported and requested.
 * @param tpm_alg_rsassa_3072 RSASSA-3072 supported and requested.
 * @param tpm_alg_rsapss_2048 RSAPSS-2048 supported and requested.
 * @param tpm_alg_rsassa_2048 RSASSA-2048 supported and requested.
 * @param tpm_alg_ecdsa_ecc_nist_p521 ECDSA-ECC-521 supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported               => 0
 *         tpm_alg_rsassa_2048         => 1
 *         tpm_alg_rsapss_2048         => 2
 *         tpm_alg_rsassa_3072         => 4
 *         tpm_alg_rsapss_3072         => 8
 *         tpm_alg_ecdsa_ecc_nist_p256 => 16
 *         tpm_alg_rsassa_4096         => 32
 *         tpm_alg_rsapss_4096         => 64
 *         tpm_alg_ecdsa_ecc_nist_p384 => 128
 *         tpm_alg_ecdsa_ecc_nist_p521 => 256
 * ```
 */
long spdm_platform_select_base_asym_algo(instance_t *instance,
                                         boolean tpm_alg_ecdsa_ecc_nist_p384,
                                         boolean tpm_alg_rsapss_4096,
                                         boolean tpm_alg_rsassa_4096,
                                         boolean tpm_alg_ecdsa_ecc_nist_p256,
                                         boolean tpm_alg_rsapss_3072,
                                         boolean tpm_alg_rsassa_3072,
                                         boolean tpm_alg_rsapss_2048,
                                         boolean tpm_alg_rsassa_2048,
                                         boolean tpm_alg_ecdsa_ecc_nist_p521);

/**
 * Select base hash algorithm (DSP0274_1.1.0 [185]).
 *
 * The arguments describe the hash algorithms supported by the requester.
 * This function must select one of the provided algorithms or return 0.
 *
 * @param instance Platform instance.
 * @param tpm_alg_sha_256 SHA-256 supported and requested.
 * @param tpm_alg_sha_384 SHA-384 supported and requested.
 * @param tpm_alg_sha_512 SHA-512 supported and requested.
 * @param tpm_alg_sha3_256 SHA3-256 supported and requested.
 * @param tpm_alg_sha3_384 SHA3-384 supported and requested.
 * @param tpm_alg_sha3_512 SHA3-512 supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported        => 0
 *         tpm_alg_sha_256      => 1
 *         tpm_alg_sha_384      => 2
 *         tpm_alg_sha_512      => 4
 *         tpm_alg_sha3_256     => 8
 *         tpm_alg_sha3_384     => 16
 *         tpm_alg_sha3_512     => 32
 * ```
 */
unsigned char spdm_platform_select_base_hash_algo(instance_t *instance,
                                                  boolean tpm_alg_sha_256,
                                                  boolean tpm_alg_sha_384,
                                                  boolean tpm_alg_sha_512,
                                                  boolean tpm_alg_sha3_256,
                                                  boolean tpm_alg_sha3_384,
                                                  boolean tpm_alg_sha3_512);

/**
 * Select the Diffie-Hellman Ephemeral group (DSP0274_1.1.0 [189]).
 *
 * The arguments describe the group supported by the requester.
 * This function must select one of the provided groups or return 0.
 *
 * @param instance Platform instance.
 * @param secp521r1 SECP521R1 supported and requested.
 * @param secp384r1 SECP384R1 supported and requested.
 * @param secp256r1 SECP256R1 supported and requested.
 * @param ffdhe4096 FFDHE4092 supported and requested.
 * @param ffdhe3072 FFDHE3072 supported and requested.
 * @param ffdhe2048 FFDHE2048 supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported => 0
 *         ffdhe2048     => 1
 *         ffdhe3072     => 2
 *         ffdhe4096     => 4
 *         secp256r1     => 8
 *         secp384r1     => 16
 *         secp521r1     => 32
 * ```
 */
unsigned char spdm_platform_select_dhe(instance_t *instance,
                                       boolean secp521r1,
                                       boolean secp384r1,
                                       boolean secp256r1,
                                       boolean ffdhe4096,
                                       boolean ffdhe3072,
                                       boolean ffdhe2048);

/**
 * Select the AEAD algorithm (DSP0274_1.1.0 [190]).
 *
 * The arguments describe the algorithm supported by the requester.
 * This function must select one of the provided algorithms or return 0.
 *
 * @param instance Platform instance.
 * @param chacha20_poly1305 CHACHA20-POLY135 supported and requested.
 * @param aes_256_gcm AES-256-GCM supported and requested.
 * @param aes_128_gcm AES-128-GCM supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported     => 0
 *         aes_128_gcm       => 1
 *         aes_256_gcm       => 2
 *         chacha20_poly1305 => 4
 * ```
 */
unsigned char spdm_platform_select_aead(instance_t *instance,
                                        boolean chacha20_poly1305,
                                        boolean aes_256_gcm,
                                        boolean aes_128_gcm);

/**
 * Select base asymmetric algorithm (DSP0274_1.1.0 [191]).
 *
 * The arguments describe the key signature algorithms supported by the requester.
 * This function must select one of the provided algorithms or return 0.
 *
 * @param instance Platform instance.
 * @param ra_tpm_alg_ecdsa_ecc_nist_p384 ECDSA-ECC-384 supported and requested.
 * @param ra_tpm_alg_rsapss_4096 RSAPSS-4096 supported and requested.
 * @param ra_tpm_alg_rsassa_4096 RSASSA-4096 supported and requested.
 * @param ra_tpm_alg_ecdsa_ecc_nist_p256 ECDSA-ECC-256 supported and requested.
 * @param ra_tpm_alg_rsapss_3072 RSAPSS-3072 supported and requested.
 * @param ra_tpm_alg_rsassa_3072 RSASSA-3072 supported and requested.
 * @param ra_tpm_alg_rsapss_2048 RSAPSS-2048 supported and requested.
 * @param ra_tpm_alg_rsassa_2048 RSASSA-2048 supported and requested.
 * @param ra_tpm_alg_ecdsa_ecc_nist_p521 ECDSA-ECC-521 supported and requested.
 * @return Enum with the following values:
 * ```
 *         not supported                  => 0
 *         ra_tpm_alg_rsassa_2048         => 1
 *         ra_tpm_alg_rsapss_2048         => 2
 *         ra_tpm_alg_rsassa_3072         => 4
 *         ra_tpm_alg_rsapss_3072         => 8
 *         ra_tpm_alg_ecdsa_ecc_nist_p256 => 16
 *         ra_tpm_alg_rsassa_4096         => 32
 *         ra_tpm_alg_rsapss_4096         => 64
 *         ra_tpm_alg_ecdsa_ecc_nist_p384 => 128
 *         ra_tpm_alg_ecdsa_ecc_nist_p521 => 256
 * ```
 */
long spdm_platform_select_rbaa(instance_t *instance,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p384,
                               boolean ra_tpm_alg_rsapss_4096,
                               boolean ra_tpm_alg_rsassa_4096,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p256,
                               boolean ra_tpm_alg_rsapss_3072,
                               boolean ra_tpm_alg_rsassa_3072,
                               boolean ra_tpm_alg_rsapss_2048,
                               boolean ra_tpm_alg_rsassa_2048,
                               boolean ra_tpm_alg_ecdsa_ecc_nist_p521);

/**
 * Get digests for digests response (DSP0274_1.1.0 [232]).
 *
 * @param instance Platform instance.
 * @param data Target buffer, its maximum size is given by the initial value of *length.
 * @param length Contains a pointer to the maximum size of data. On return the size must
 *               be changed to the actual size of the data copied to data.
 * @param slot_mask Bit mask that contains one bit for each slot. If a slot contains a
 *                  certificate chain the according bit needs to be set. This parameter
 *                  is not initialized when this function is called.
 */
void spdm_platform_get_digests_data(instance_t *instance,
                                    char *data,
                                    long *length,
                                    unsigned char *slot_mask);

/**
 * Validate an incoming certificate request (DSP0274_1.1.0 [238]).
 *
 * @param instance Platform instance.
 * @param slot Certificate slot number.
 * @param offset Certificate portion offset.
 * @param length Certificate portion length.
 * @return Success.
 */
boolean spdm_platform_validate_certificate_request(instance_t *instance,
                                                   unsigned char slot,
                                                   unsigned short offset,
                                                   unsigned short length);

/**
 * Provide requested certificate chain.
 *
 * @param instance Platform instance.
 * @param data Target buffer, its maximum size is given by the initial value of length.
 * @param slot Requested certificate slot.
 * @param offset Offset in the certificate chain.
 * @param length Contains a pointer to the maximum size of data. On return the size
 *               must be changed to the actual size of the data copied to data.
 * @param total_length Pointer to the total length of the requested certificate.
 *                     Must be filled by that function and must be greater or
 *                     equal to offset plus the final value of *length.
 */
void spdm_platform_get_certificate_data (instance_t *instance,
                                         char *data,
                                         unsigned char slot,
                                         unsigned short offset,
                                         unsigned short *length,
                                         unsigned short *total_length);

/**
 * Get the number of measurement indices (DSP0274_1.1.0 [327]).
 *
 * Returns the number of indices available from the responder. This
 * may be zero if none are available and up to 254.
 *
 * @param instance Platform instance.
 * @return Number of measurements.
 */
unsigned char spdm_platform_get_number_of_indices (instance_t *instance);

/**
 * Get the number of measurement indices that include the trusted computing base (DSP0274_1.1.0 [422]).
 *
 * Otherwise it has the same behaviour as spdm_platform_get_number_of_indices.
 *
 * @param instance Platform instance.
 * @return Number of measurements in TCB.
 */
unsigned char spdm_platform_get_number_of_indices_tcb (instance_t *instance);

/**
 * Generate a nonce for cryptographic operations.
 *
 * The platform must always keep the latest generated nonce and shall
 * add it to the transcript when spdm_platform_update_transcript_nonce
 * is called. Only after this function is called the nonce can be marked
 * as valid.
 *
 * @param instance Platform instance.
 * @param nonce 32 byte long nonce buffer.
 */
void spdm_platform_get_nonce(instance_t *instance, void *nonce);

/**
 * Return a DMTF measurement field (DSP0274_1.1.0 [335]).
 *
 * @param instance Platform instance.
 * @param index Requested measurement index.
 * @param representation Representation enum with the following values:
 *                       Digest         => 0
 *                       Raw bit stream => 1
 * @param type DMTF value type enum with the following values:
 *             Immutable_ROM          => 0
 *             Mutable_Firmware       => 1
 *             Hardware_Configuration => 2
 *             Firmware_Configuration => 3
 *             Measured_Manifest      => 4
 * @param length Length of the target buffer. The initial value marks the
 *               maximum size of the buffer. On return it must be set to
 *               the actual size of the data copied into the buffer.
 * @param buffer Measurement data buffer.
 */
void spdm_platform_get_dmtf_measurement_field (instance_t *instance,
                                               unsigned index,
                                               unsigned *representation,
                                               unsigned *type,
                                               unsigned *length,
                                               void *buffer);

/**
 * Provide opaque data for the measurement response (DSP0274_1.1.0 [327]).
 *
 * @param instance Platform instance.
 * @param data Opaque data buffer.
 * @param length Opaque data buffer length. The initial value is the maximum size of
 *               the buffer. On return length must be set to the size of the data
 *               copied to that buffer. It must not exceed 1024.
 */
void spdm_platform_get_meas_opaque_data(instance_t *instance,
                                        void *data,
                                        unsigned *length);

/**
 * Register a new transcript with the platform.
 *
 * The returned ID must be used on all subsequent operations on the same transcript.
 *
 * @param instance Platform instance.
 * @param kind Transcript kind enum with the following values:
 *             Session transcript     => 0
 *             Measurement transcript => 1
 * @return 32 bit transcript ID. On success spdm_platform_valid_transcript_id
 *         must return true on this ID.
 */
unsigned spdm_platform_get_new_transcript(instance_t *instance,
                                          unsigned char kind);

/**
 * Indicate whether a transcript ID is valid.
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @return True if transcript ID is valid.
 */
boolean spdm_platform_valid_transcript_id(instance_t *instance,
                                          unsigned transcript);

/**
 * Reset an already registered transcript.
 *
 * This operation may change the transcript kind, too. The returned transcript ID
 * may be different from the provided one. It has the same behaviour as
 * spdm_platform_get_new_transcript except that it allows reusing an existing resource.
 *
 * @param instance Platform instance.
 * @param transcript Old transcript ID.
 * @param kind Transcript kind for the new transcript.
 * @return New 32 bit transcript ID.
 */
unsigned spdm_platform_reset_transcript(instance_t *instance,
                                        unsigned transcript,
                                        unsigned char kind);

/**
 * Append a chunk of data to the transcript.
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param data Transcript data to be appended.
 * @param offset Offset in data.
 * @param length Length of data to be appended to the transcript,
 *               length + offset is less or equal to the size of data.
 * @return Success.
 */
boolean spdm_platform_update_transcript(instance_t *instance,
                                        unsigned transcript,
                                        const void *data,
                                        unsigned offset,
                                        unsigned length);

/**
 * Append the latest generated nonce to the transcript.
 *
 * Append the latest nonce generated by spdm_platform_get_nonce to the
 * transcript. The nonce must be marked as invalid after this operation.
 * If the nonce is already marked as invalid when this function is called
 * the operation must fail.
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @return Success.
 */
boolean spdm_platform_update_transcript_nonce(instance_t *instance,
                                              unsigned transcript);

/**
 * Generate signature from current transcript.
 *
 * Generate the signature from the current state of the transcript. This
 * does not invalidate or reset the transcript.
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param slot Slot ID of the signing key.
 * @param signature Signature buffer.
 * @param length Signature buffer length. The initial value contains the maximum length
 *               of the signature buffer. On return the value shall be the correct
 *               length for the signature algorithm selected in Negotiate_Algorithms.
 *               On error length can be set to 0.
 */
void spdm_platform_get_signature(instance_t *instance,
                                 unsigned transcript,
                                 unsigned char slot,
                                 void *signature,
                                 unsigned *length);

/**
 * Generate responder exchange data from requester exchange data (DSP0274_1.1.0 [421]).
 *
 * Process the exchange data sent with the key exchange request
 * and generate the appropriate response exchange data.
 *
 * @param instance Platform instance.
 * @param data Exchange data buffer. Initially contains the exchange
 *             request data. Must be filled with the response exchange data.
 * @param length Length of the request exchange data and the data buffer.
 */
void spdm_platform_get_exchange_data (instance_t *instance,
                                      void *data,
                                      unsigned length);

/**
 * Get heartbeat period (DSP0274_1.1.0 [422]).
 *
 * @param instance Platform instance.
 * @return Heartbeat period.
 */
unsigned char spdm_platform_get_heartbeat_period (instance_t *instance);

/**
 * Check session ID sent by the requester for validity (DSP0274_1.1.0 [421]).
 *
 * @param instance Platform instance.
 * @param session_id Session ID.
 * @return ID valid.
 */
boolean spdm_platform_valid_session_id (instance_t *instance,
                                        unsigned short session_id);

/**
 * Get session ID for key exchange response (DSP0274_1.1.0 [422]).
 *
 * @param instance Platform instance.
 * @param session_id Request session ID
 * @return Response session ID.
 */
unsigned short spdm_platform_get_session_id (instance_t *instance,
                                             unsigned short session_id);

/**
 * Request mutual authentication in key exchange response (DSP0274_1.1.0 [422]).
 *
 * Returning true only enables regular mutual authentication. Encapsulated
 * and implicit mutual authentication are not supported.
 *
 * @param instance Platform instance.
 * @return Mutual auth requested.
 */
boolean spdm_platform_use_mutual_auth (instance_t *instance);

/**
 * Get the hash over the measurement summary (DSP0274_1.1.0 [422]).
 *
 * @param instance Platform instance.
 * @param summary Measurement summary.
 * @param summary_length Length of the measurement_hash summary.
 * @param hash Target buffer for the generated hash.
 * @param hash_length Length of the hash buffer. The initial value
 *                    is the maximum length of this buffer. On return
 *                    this value shall be set to the size required by
 *                    the hash algorithm.
 */
void spdm_platform_get_summary_hash(instance_t *instance,
                                    const void *summary,
                                    unsigned summary_length,
                                    void *hash,
                                    unsigned *hash_length);

/**
 * Append the selected certificate chain to the transcript.
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param slot Slot ID for certificate selection.
 * @return Success, Boolean.
 */
unsigned char spdm_platform_update_transcript_cert(instance_t *instance,
                                                   unsigned transcript,
                                                   unsigned char slot);

/**
 * Handle key exchange opaque data (DSP0274_1.1.0 [422]).
 *
 * @param instance Platform instance.
 * @param req_data Buffer containing the request opaque data.
 * @param req_length Length of the data sent by the requester.
 * @param data Buffer for the response opaque data.
 * @param length Initial value is the maximum length of data. Shall be
 *               set to the size of the opaque data for the response.
 */
void spdm_platform_get_key_ex_opaque_data(instance_t *instance,
                                          const void *req_data,
                                          unsigned req_length,
                                          void *data,
                                          unsigned *length);

/**
 * Generate the responder verify data for key exchange (DSP0274_1.1.0 [422]).
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param slot Slot ID of the signing key.
 * @param data Verify data buffer.
 * @param length Length of the verify data buffer. The initial value is
 *               is the maximum length of that buffer. It shall be set to
 *               the length of the negotiated hash algorithm.
 */
void spdm_platform_get_key_ex_verify_data(instance_t *instance,
                                          unsigned transcript,
                                          unsigned char slot,
                                          void *data,
                                          unsigned *length);

/**
 * Validate the finish signature sent by the requester (DSP0274_1.1.0 [432]).
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param signature Signature sent by the requester.
 * @param length Length of the signature.
 * @param slot Slot ID of the signing key.
 * @return Success.
 */
boolean spdm_platform_validate_finish_signature(instance_t *instance,
                                                unsigned transcript,
                                                const void *signature,
                                                unsigned length,
                                                unsigned char slot);

/**
 * Validate the finish HMAC sent by the requester (DSP0274_1.1.0 [432]).
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param hmac HMAC sent by the requester.
 * @param length Length of the HMAC.
 * @param slot Slot ID used for the HMAC generation.
 * @return Success.
 */
boolean spdm_platform_validate_finish_hmac(instance_t *instance,
                                           unsigned transcript,
                                           const void *hmac,
                                           unsigned length,
                                           unsigned char slot);

/**
 * Generate the responder verify data for the finish response (DSP0274_1.1.0 [433]).
 *
 * @param instance Platform instance.
 * @param transcript Transcript ID.
 * @param slot Slot ID for the used key.
 * @param data Verify data buffer.
 * @param length Length of the verify data buffer. The initial value is
 *               is the maximum length of that buffer. It shall be set to
 *               the length of the negotiated hash algorithm.
 */
void spdm_platform_get_finish_verify_data(instance_t *instance,
                                          unsigned transcript,
                                          unsigned char slot,
                                          void *data,
                                          unsigned *length);

/**
 * Set the current session phase.
 *
 * Set the current session phase to the phase passed as argument. If
 * an error occurs the session phase is set to the error value, otherwise
 * it's set to the requested phase.
 *
 * The allowed values for the requested and returned session phase are
 *     Error          => 0
 *     No session     => 1
 *     Handshake      => 2
 *     Secure session => 3
 * For the values 0 and 1 this function should behave identically to
 * spdm_platform_reset_session_phase.
 *
 * @param instance Platform instance.
 * @param phase Requested session phase.
 * @return Updated session phase.
 */
unsigned char spdm_platform_set_session_phase(instance_t *instance,
                                              unsigned char phase,
                                              unsigned transcript,
                                              unsigned char slot);

/**
 * Reset the current session.
 *
 * @param instance Platform instance.
 * @return Updated session phase. Return 0 if an error occurred otherwise 1.
 */
unsigned char spdm_platform_reset_session_phase(instance_t *instance);

/**
 * Perform a key update operation.
 *
 * @param instance Platform instance.
 * @param operation Key update operation, enum with the following values:
 *                  Update key      => 1
 *                  Update all keys => 2
 *                  Verifiy new key => 3
 * @param tag Key update tag.
 * @result Success.
 */
boolean spdm_platform_key_update(instance_t *instance,
                                 unsigned operation,
                                 unsigned tag);

#endif // __SPDM_PLATFORM_INTERFACE__
