with Interfaces.C;

package body SPDM_C_Responder with
   SPARK_Mode
is

   procedure Plat_Initialize (Ctx : in out Context)
   is
      procedure C_Interface (Instance : out System.Address) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_initialize";
   begin
      C_Interface (Ctx.Instance);
   end Plat_Initialize;

   overriding
   procedure Plat_Cfg_CT_Exponent
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.CT_Exponent)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_ct_exponent";
   begin
      if not RFLX.SPDM.Valid_CT_Exponent (RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance))) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.CT_Exponent (C_Interface (Ctx.Instance));
   end Plat_Cfg_CT_Exponent;

   overriding
   procedure Plat_Cfg_Cap_Meas_Fresh
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas_fresh";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Meas_Fresh;

   overriding
   procedure Plat_Cfg_Cap_Meas
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Meas_Cap)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas";
      Value : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM.Valid_Meas_Cap (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Cap_Meas;

   overriding
   procedure Plat_Cfg_Cap_Chal
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_chal";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Chal;

   overriding
   procedure Plat_Cfg_Cap_Cert
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cert";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Cert;

   overriding
   procedure Plat_Cfg_Cap_Cache
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cache";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Cache;

   overriding
   procedure Plat_Cfg_Cap_Key_Upd
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_upd";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Key_Upd;

   overriding
   procedure Plat_Cfg_Cap_Hbeat
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_hbeat";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Hbeat;

   overriding
   procedure Plat_Cfg_Cap_Encap
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encap";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Encap;

   overriding
   procedure Plat_Cfg_Cap_Mut_Auth
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mut_auth";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Mut_Auth;

   overriding
   procedure Plat_Cfg_Cap_Pub_Key_ID
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_pub_key_id";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Pub_Key_ID;

   overriding
   procedure Plat_Cfg_Cap_MAC
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mac";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_MAC;

   overriding
   procedure Plat_Cfg_Cap_Encrypt
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encrypt";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Encrypt;

   overriding
   procedure Plat_Cfg_Cap_PSK
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.PSK_Resp_Cap)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_psk";
      Value : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM.Valid_PSK_Resp_Cap (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Cap_PSK;

   overriding
   procedure Plat_Cfg_Cap_Key_Ex
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_ex";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Key_Ex;

   overriding
   procedure Plat_Cfg_Cap_Handshake_In_The_Clear
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_handshake_in_the_clear";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Cfg_Cap_Handshake_In_The_Clear;

   function C_Bool (Value : Boolean) return Interfaces.C.unsigned_char is
      (if Value then 1 else 0);

   overriding
   procedure Plat_Cfg_Sel_Measurement_Hash_Algo
      (Ctx                  : in out Context;
       TPM_ALG_SHA_256      :        Boolean;
       TPM_ALG_SHA_384      :        Boolean;
       TPM_ALG_SHA_512      :        Boolean;
       TPM_ALG_SHA3_256     :        Boolean;
       TPM_ALG_SHA3_384     :        Boolean;
       TPM_ALG_SHA3_512     :        Boolean;
       Raw_Bit_Streams_Only :        Boolean;
       Result               :    out RFLX.SPDM.Measurement_Hash_Algo)
   is
      function C_Interface
         (Instance             : System.Address;
          TPM_ALG_SHA_256      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_384      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_512      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_256     : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_384     : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_512     : Interfaces.C.unsigned_char;
          Raw_Bit_Streams_Only : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_measurement_hash_algo";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance             => Ctx.Instance,
                TPM_ALG_SHA_256      => C_Bool (TPM_ALG_SHA_256),
                TPM_ALG_SHA_384      => C_Bool (TPM_ALG_SHA_384),
                TPM_ALG_SHA_512      => C_Bool (TPM_ALG_SHA_512),
                TPM_ALG_SHA3_256     => C_Bool (TPM_ALG_SHA3_256),
                TPM_ALG_SHA3_384     => C_Bool (TPM_ALG_SHA3_384),
                TPM_ALG_SHA3_512     => C_Bool (TPM_ALG_SHA3_512),
                Raw_Bit_Streams_Only => C_Bool (Raw_Bit_Streams_Only)));
   begin
      if not RFLX.SPDM.Valid_Measurement_Hash_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Measurement_Hash_Algo;

   overriding
   procedure Plat_Cfg_Sel_Base_Asym_Algo
      (Ctx                         : in out Context;
       TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       TPM_ALG_RSAPSS_4096         :        Boolean;
       TPM_ALG_RSASSA_4096         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       TPM_ALG_RSAPSS_3072         :        Boolean;
       TPM_ALG_RSASSA_3072         :        Boolean;
       TPM_ALG_RSAPSS_2048         :        Boolean;
       TPM_ALG_RSASSA_2048         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       Result                      :    out RFLX.SPDM.Base_Asym_Algo)
   is
      function C_Interface
         (Instance                    : System.Address;
          TPM_ALG_ECDSA_ECC_NIST_P384 : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_4096         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_4096         : Interfaces.C.unsigned_char;
          TPM_ALG_ECDSA_ECC_NIST_P256 : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_3072         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_3072         : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_2048         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_2048         : Interfaces.C.unsigned_char;
          TPM_ALG_ECDSA_ECC_NIST_P521 : Interfaces.C.unsigned_char) return Interfaces.C.long
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_base_asym_algo";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance                    => Ctx.Instance,
                TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P384),
                TPM_ALG_RSAPSS_4096         => C_Bool (TPM_ALG_RSAPSS_4096),
                TPM_ALG_RSASSA_4096         => C_Bool (TPM_ALG_RSASSA_4096),
                TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P256),
                TPM_ALG_RSAPSS_3072         => C_Bool (TPM_ALG_RSAPSS_3072),
                TPM_ALG_RSASSA_3072         => C_Bool (TPM_ALG_RSASSA_3072),
                TPM_ALG_RSAPSS_2048         => C_Bool (TPM_ALG_RSAPSS_2048),
                TPM_ALG_RSASSA_2048         => C_Bool (TPM_ALG_RSASSA_2048),
                TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P521)));
   begin
      if not RFLX.SPDM.Valid_Base_Asym_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Base_Asym_Algo;

   overriding
   procedure Plat_Cfg_Sel_Base_Hash_Algo
      (Ctx              : in out Context;
       TPM_ALG_SHA_256  :        Boolean;
       TPM_ALG_SHA_384  :        Boolean;
       TPM_ALG_SHA_512  :        Boolean;
       TPM_ALG_SHA3_256 :        Boolean;
       TPM_ALG_SHA3_384 :        Boolean;
       TPM_ALG_SHA3_512 :        Boolean;
       Result           :    out RFLX.SPDM.Base_Hash_Algo)
   is
      function C_Interface
         (Instance         : System.Address;
          TPM_ALG_SHA_256  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_384  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_512  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_256 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_384 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_512 : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_base_hash_algo";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance         => Ctx.Instance,
                TPM_ALG_SHA_256  => C_Bool (TPM_ALG_SHA_256),
                TPM_ALG_SHA_384  => C_Bool (TPM_ALG_SHA_384),
                TPM_ALG_SHA_512  => C_Bool (TPM_ALG_SHA_512),
                TPM_ALG_SHA3_256 => C_Bool (TPM_ALG_SHA3_256),
                TPM_ALG_SHA3_384 => C_Bool (TPM_ALG_SHA3_384),
                TPM_ALG_SHA3_512 => C_Bool (TPM_ALG_SHA3_512)));
   begin
      if not RFLX.SPDM.Valid_Base_Hash_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Base_Hash_Algo;

   overriding
   procedure Plat_Cfg_Sel_DHE
      (Ctx           : in out Context;
       Req_SecP521r1 :        Boolean;
       Req_SecP384r1 :        Boolean;
       Req_SecP256r1 :        Boolean;
       Req_FFDHE4096 :        Boolean;
       Req_FFDHE3072 :        Boolean;
       Req_FFDHE2048 :        Boolean;
       Result        :    out RFLX.SPDM_Responder.DHE_Algo)
   is
      function C_Interface
         (Instance  : System.Address;
          SecP521r1 : Interfaces.C.unsigned_char;
          SecP384r1 : Interfaces.C.unsigned_char;
          SecP256r1 : Interfaces.C.unsigned_char;
          FFDHE4096 : Interfaces.C.unsigned_char;
          FFDHE3072 : Interfaces.C.unsigned_char;
          FFDHE2048 : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_dhe";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance  => Ctx.Instance,
                SecP521r1 => C_Bool (Req_SecP521r1),
                SecP384r1 => C_Bool (Req_SecP384r1),
                SecP256r1 => C_Bool (Req_SecP256r1),
                FFDHE4096 => C_Bool (Req_FFDHE4096),
                FFDHE3072 => C_Bool (Req_FFDHE3072),
                FFDHE2048 => C_Bool (Req_FFDHE2048)));
   begin
      if not RFLX.SPDM_Responder.Valid_DHE_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Value);
   end Plat_Cfg_Sel_DHE;

   overriding
   procedure Plat_Cfg_Sel_AEAD
      (Ctx                   : in out Context;
       Req_ChaCha20_Poly1305 :        Boolean;
       Req_AES_256_GCM       :        Boolean;
       Req_AES_128_GCM       :        Boolean;
       Result                :    out RFLX.SPDM_Responder.AEAD_Algo)
   is
      function C_Interface
         (Instance             : System.Address;
          AA_ChaCha20_Poly1305 : Interfaces.C.unsigned_char;
          AA_AES_256_GCM       : Interfaces.C.unsigned_char;
          AA_AES_128_GCM       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_aead";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance             => Ctx.Instance,
                AA_ChaCha20_Poly1305 => C_Bool (Req_ChaCha20_Poly1305),
                AA_AES_256_GCM       => C_Bool (Req_AES_256_GCM),
                AA_AES_128_GCM       => C_Bool (Req_AES_128_GCM)));
   begin
      if not RFLX.SPDM_Responder.Valid_AEAD_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Value);
   end Plat_Cfg_Sel_AEAD;

   overriding
   procedure Plat_Cfg_Sel_RBAA
      (Ctx                             : in out Context;
       Req_TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       Req_TPM_ALG_RSAPSS_4096         :        Boolean;
       Req_TPM_ALG_RSASSA_4096         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       Req_TPM_ALG_RSAPSS_3072         :        Boolean;
       Req_TPM_ALG_RSASSA_3072         :        Boolean;
       Req_TPM_ALG_RSAPSS_2048         :        Boolean;
       Req_TPM_ALG_RSASSA_2048         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       Result                          :    out RFLX.SPDM.Base_Asym_Algo)
   is
      function C_Interface
         (Instance                       : System.Address;
          RA_TPM_ALG_ECDSA_ECC_NIST_P384 : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_4096         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_4096         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_ECDSA_ECC_NIST_P256 : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_3072         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_3072         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_2048         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_2048         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_ECDSA_ECC_NIST_P521 : Interfaces.C.unsigned_char) return Interfaces.C.long
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_rbaa";
      Value : constant RFLX.RFLX_Types.Base_Integer :=
         (RFLX.RFLX_Types.Base_Integer
            (C_Interface
               (Instance                       => Ctx.Instance,
                RA_TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P384),
                RA_TPM_ALG_RSAPSS_4096         => C_Bool (Req_TPM_ALG_RSAPSS_4096),
                RA_TPM_ALG_RSASSA_4096         => C_Bool (Req_TPM_ALG_RSASSA_4096),
                RA_TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P256),
                RA_TPM_ALG_RSAPSS_3072         => C_Bool (Req_TPM_ALG_RSAPSS_3072),
                RA_TPM_ALG_RSASSA_3072         => C_Bool (Req_TPM_ALG_RSASSA_3072),
                RA_TPM_ALG_RSAPSS_2048         => C_Bool (Req_TPM_ALG_RSAPSS_2048),
                RA_TPM_ALG_RSASSA_2048         => C_Bool (Req_TPM_ALG_RSASSA_2048),
                RA_TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P521))));
   begin
      if not RFLX.SPDM.Valid_Base_Asym_Algo (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_RBAA;

   overriding
   procedure Plat_Get_Digests_Data
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM_Responder.Digests_Data.Structure)
   with SPARK_Mode => Off
   is
      Slot_Mask : Interfaces.C.unsigned_char;
      Length    : Interfaces.C.long;

      procedure C_Interface (Instance : System.Address;
                             Data     : System.Address;
                             Length   : System.Address;
                             Slots    : System.Address) with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_get_digests_data";
      use type Interfaces.C.unsigned_char;
   begin
      Length := Interfaces.C.long (Result.Value'Length);
      C_Interface (Instance => Ctx.Instance,
                   Data     => Result.Value'Address,
                   Length   => Length'Address,
                   Slots    => Slot_Mask'Address);
      Result.Length := RFLX.SPDM_Responder.Digests_Length (Length);
      Result.Slot_0_Present := RFLX.SPDM.Slot_Present (Slot_Mask and 16#01#);
      Result.Slot_1_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#02#) / 16#02#);
      Result.Slot_2_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#04#) / 16#04#);
      Result.Slot_3_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#08#) / 16#08#);
      Result.Slot_4_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#10#) / 16#10#);
      Result.Slot_5_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#20#) / 16#20#);
      Result.Slot_6_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#40#) / 16#40#);
      Result.Slot_7_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#80#) / 16#80#);
   end Plat_Get_Digests_Data;

   overriding
   procedure Plat_Valid_Certificate_Request
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address;
                            Slot     : Interfaces.C.unsigned_char;
                            Offset   : Interfaces.C.unsigned_short;
                            Length   : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_validate_certificate_request";
      use type Interfaces.C.unsigned_char;
   begin
      Result := 0 /= C_Interface (Instance => Ctx.Instance,
                                  Slot     => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                                  Offset   => Interfaces.C.unsigned_short (Offset),
                                  Length   => Interfaces.C.unsigned_short (Length));
   end Plat_Valid_Certificate_Request;

   overriding
   procedure Plat_Get_Certificate_Response
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out RFLX.SPDM.Certificate_Response.Structure)
   with SPARK_Mode => Off
   is
      procedure C_Interface (Instance     :        System.Address;
                             Data         :        System.Address;
                             Slot         :        Interfaces.C.unsigned_char;
                             Offset       :        Interfaces.C.unsigned_short;
                             Length       : in out Interfaces.C.unsigned_short;
                             Total_Length :    out Interfaces.C.unsigned_short)
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_get_certificate_data";
      use type Interfaces.C.unsigned_short;
      use type RFLX.SPDM.Length_16;
      Max_Length               : constant RFLX.SPDM.Length_16 := 508;
      --  Note: Result must have a size of at least Max_Length + 7
      Cert_Length              : Interfaces.C.unsigned_short;
      Total_Length             : Interfaces.C.unsigned_short;
      Portion_Remainder_Length : RFLX.RFLX_Types.Base_Integer;
   begin
      if Length <= Max_Length then
         Cert_Length := Interfaces.C.unsigned_short (Length);
      else
         Cert_Length := Interfaces.C.unsigned_short (Max_Length);
      end if;
      C_Interface (Instance     => Ctx.Instance,
                   Data         => Result.Cert_Chain'Address,
                   Slot         => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   Offset       => Interfaces.C.unsigned_short (Offset),
                   Length       => Cert_Length,
                   Total_Length => Total_Length);
      Result.Slot := Slot;
      Result.Param_2 := 0;
      if Cert_Length = Interfaces.C.unsigned_short (Max_Length) then
         Portion_Remainder_Length := RFLX.RFLX_Types.Base_Integer (Max_Length);
      else
         Portion_Remainder_Length := RFLX.RFLX_Types.Base_Integer (Cert_Length);
      end if;
      if not RFLX.SPDM.Valid_Length_16 (Portion_Remainder_Length) then
         raise Constraint_Error;
      end if;
      Result.Portion_Length := RFLX.SPDM.To_Actual (Portion_Remainder_Length);
      Portion_Remainder_Length :=
         RFLX.RFLX_Types.Base_Integer (Total_Length - Cert_Length - Interfaces.C.unsigned_short (Offset));
      if not RFLX.SPDM.Valid_Length_16 (Portion_Remainder_Length) then
         raise Constraint_Error;
      end if;
      Result.Remainder_Length := RFLX.SPDM.To_Actual (Portion_Remainder_Length);
   end Plat_Get_Certificate_Response;

   overriding
   procedure Plat_Get_Number_Of_Indices
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_number_of_indices";
      Count : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM.Valid_Measurement_Count (Count) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Count);
   end Plat_Get_Number_Of_Indices;

   overriding
   procedure Plat_Get_Number_Of_Indices_TCB
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_number_of_indices_tcb";
      Count : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM.Valid_Measurement_Count (Count) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Count);
   end Plat_Get_Number_Of_Indices_TCB;

   overriding
   procedure Plat_Get_Nonce (Ctx    : in out Context;
                             Result :    out RFLX.SPDM.Nonce.Structure)
   is
      procedure C_Interface (Instance :     System.Address;
                             Nonce    : out RFLX.RFLX_Types.Bytes) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_nonce";
   begin
      C_Interface (Ctx.Instance, Result.Data);
   end Plat_Get_Nonce;

   overriding
   procedure Plat_Get_DMTF_Measurement_Field (Ctx    : in out Context;
                                              Index  :        RFLX.SPDM.Index;
                                              Result :    out RFLX.SPDM.DMTF_Measurement_Field.Structure)
   is
      procedure C_Interface (Instance       :        System.Address;
                             Index          :        Interfaces.C.unsigned;
                             Representation :    out Interfaces.C.unsigned;
                             Value_Type     :    out Interfaces.C.unsigned;
                             Length         : in out Interfaces.C.unsigned;
                             Data           : in out RFLX.RFLX_Types.Bytes) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_dmtf_measurement_field";
      Value_Representation : Interfaces.C.unsigned;
      Value_Type : Interfaces.C.unsigned;
   begin
      Result.Measurement_Value_Length := Result.Measurement_Value'Length;
      C_Interface (Ctx.Instance,
                   Interfaces.C.unsigned (Index),
                   Value_Representation,
                   Value_Type,
                   Interfaces.C.unsigned (Result.Measurement_Value_Length),
                   Result.Measurement_Value);
      if
         not RFLX.SPDM.Valid_DMTF_Spec_Measurement_Value_Representation
                (RFLX.RFLX_Types.Base_Integer (Value_Representation))
         or not RFLX.SPDM.Valid_DMTF_Spec_Measurement_Value_Type (RFLX.RFLX_Types.Base_Integer (Value_Type))
      then
         raise Constraint_Error;
      end if;
      Result.Measurement_Value_Representation :=
         RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Value_Representation));
      Result.Measurement_Value_Type := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Value_Type));
   end Plat_Get_DMTF_Measurement_Field;

   overriding
   procedure Plat_Get_Meas_Opaque_Data (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM_Responder.Opaque_Data.Structure)
   is
      procedure C_Interface (Instance :        System.Address;
                             Data     :        System.Address;
                             Length   : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_meas_opaque_data";
      Length : Interfaces.C.unsigned := Result.Data'Length;
   begin
      C_Interface (Ctx.Instance, Result.Data'Address, Length);
      if not RFLX.SPDM.Valid_Length_16 (RFLX.RFLX_Types.Base_Integer (Length)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Length));
   end Plat_Get_Meas_Opaque_Data;

   overriding
   procedure Plat_Get_New_Transcript (Ctx    : in out Context;
                                      Kind   :        RFLX.SPDM_Responder.Transcript_Kind;
                                      Result :    out RFLX.SPDM_Responder.Transcript_ID)
   is
      function C_Interface (Instance : System.Address;
                            Kind     : Interfaces.C.unsigned_char) return Interfaces.C.unsigned with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_new_transcript";
      Transcript : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface (Ctx.Instance, Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Kind))));
   begin
      if not RFLX.SPDM_Responder.Valid_Transcript_ID (Transcript) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Transcript);
   end Plat_Get_New_Transcript;

   overriding
   procedure Plat_Valid_Transcript_ID (Ctx        : in out Context;
                                       Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                       Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_valid_transcript_id";
   begin
      Result := C_Interface (Ctx.Instance, Interfaces.C.unsigned (Transcript)) > 0;
   end Plat_Valid_Transcript_ID;

   overriding
   procedure Plat_Reset_Transcript (Ctx        : in out Context;
                                    Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                    Kind       :        RFLX.SPDM_Responder.Transcript_Kind;
                                    Result     :    out RFLX.SPDM_Responder.Transcript_ID)
   is
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Kind       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned with
         Import,
         Convention => C,
         External_Name => "spdm_platform_reset_transcript";
      New_Transcript : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface (Ctx.Instance,
                          Interfaces.C.unsigned (Transcript),
                          Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Kind))));
   begin
      if not RFLX.SPDM_Responder.Valid_Transcript_ID (New_Transcript) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (New_Transcript);
   end Plat_Reset_Transcript;

   overriding
   procedure Plat_Update_Transcript (Ctx        : in out Context;
                                     Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                     Data       :        RFLX.RFLX_Types.Bytes;
                                     Offset     :        RFLX.SPDM.Length_16;
                                     Length     :        RFLX.SPDM.Length_16;
                                     Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      use type RFLX.SPDM.Length_16;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Data       : System.Address;
                            Offset     : Interfaces.C.unsigned;
                            Length     : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_update_transcript";
      Data_Length : Interfaces.C.unsigned;
   begin
      if Data'Length < Length then
         Data_Length := Data'Length;
      else
         Data_Length := Interfaces.C.unsigned (Length);
      end if;
      Result := C_Interface (Ctx.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Data'Address,
                             Interfaces.C.unsigned (Offset),
                             Data_Length) > 0;
   end Plat_Update_Transcript;

   overriding
   procedure Plat_Update_Transcript_Nonce (Ctx        : in out Context;
                                           Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                           Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_update_transcript_nonce";
   begin
      Result := C_Interface (Ctx.Instance, Interfaces.C.unsigned (Transcript)) > 0;
   end Plat_Update_Transcript_Nonce;

   overriding
   procedure Plat_Get_Signature (Ctx        : in out Context;
                                 Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                 Slot       :        RFLX.SPDM.Slot;
                                 Result     :    out RFLX.SPDM_Responder.Signature.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Signature  :    out RFLX.RFLX_Types.Bytes;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_signature";
      Length : Interfaces.C.unsigned := Result.Data'Length;
   begin
      C_Interface (Ctx.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   Result.Data,
                   Length);
      if not RFLX.SPDM.Valid_Signature_Length (RFLX.RFLX_Types.Base_Integer (Length)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Length));
   end Plat_Get_Signature;

   overriding
   procedure Plat_Get_Exchange_Data (Ctx           : in out Context;
                                     Exchange_Data :        RFLX.RFLX_Types.Bytes;
                                     Result        :    out RFLX.SPDM_Responder.Exchange_Data.Structure)
   is
      use type RFLX.RFLX_Types.Index;
      procedure C_Interface (Instance : System.Address;
                             Data     : System.Address;
                             Length   : Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_exchange_data";
      Size : constant Interfaces.C.unsigned := Exchange_Data'Length;
   begin
      Result.Pad := 0;
      Result.Data (Result.Data'First .. Result.Data'First + Exchange_Data'Length - 1) := Exchange_Data;
      C_Interface (Ctx.Instance, Result.Data'Address, Size);
      if not RFLX.SPDM.Valid_Exchange_Data_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Exchange_Data;

   overriding
   procedure Plat_Get_Heartbeat_Period (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM.Heartbeat_Period)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_heartbeat_period";
      Period : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM.Valid_Heartbeat_Period (Period) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Period);
   end Plat_Get_Heartbeat_Period;

   overriding
   procedure Plat_Valid_Session_ID (Ctx            : in out Context;
                                    Req_Session_ID :        RFLX.SPDM.Session_ID;
                                    Result         :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Session_ID : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_valid_session_id";
   begin
      Result := C_Interface (Ctx.Instance, Interfaces.C.unsigned_short (Req_Session_ID)) > 0;
   end Plat_Valid_Session_ID;

   overriding
   procedure Plat_Get_Session_ID (Ctx            : in out Context;
                                  Req_Session_ID :        RFLX.SPDM.Session_ID;
                                  Result         :    out RFLX.SPDM.Session_ID)
   is
      function C_Interface (Instance   : System.Address;
                            Session_ID : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_short with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_session_id";
      Session_ID : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance, Interfaces.C.unsigned_short (Req_Session_ID)));
   begin
      if not RFLX.SPDM.Valid_Session_ID (Session_ID) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Session_ID);
   end Plat_Get_Session_ID;

   overriding
   procedure Plat_Use_Mutual_Auth (Ctx    : in out Context;
                                   Result :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_use_mutual_auth";
   begin
      Result := C_Interface (Ctx.Instance) > 0;
   end Plat_Use_Mutual_Auth;

   overriding
   procedure Plat_Get_Summary_Hash (Ctx    : in out Context;
                                    Data   :        RFLX.RFLX_Types.Bytes;
                                    Result :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance    : System.Address;
                             Summary     : System.Address;
                             Length      : Interfaces.C.unsigned;
                             Hash        : System.Address;
                             Hash_Length : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_summary_hash";
      Hash_Length : Interfaces.C.unsigned := Result.Data'Length;
   begin
      C_Interface (Ctx.Instance,
                   Data'Address,
                   Data'Length,
                   Result.Data'Address,
                   Hash_Length);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Hash_Length)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Hash_Length));
   end Plat_Get_Summary_Hash;

   overriding
   procedure Plat_Update_Transcript_Cert (Ctx        : in out Context;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Slot       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_update_transcript_cert";
   begin
      Result := C_Interface (Ctx.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Update_Transcript_Cert;

   overriding
   procedure Plat_Get_Key_Ex_Opaque_Data (Ctx          : in out Context;
                                          Request_Data :        RFLX.RFLX_Types.Bytes;
                                          Result       :    out RFLX.SPDM_Responder.Opaque_Data.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Req_Data   :        System.Address;
                             Req_Length :        Interfaces.C.unsigned;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_key_ex_opaque_data";
         Size : Interfaces.C.unsigned := Result.Data'Length;
   begin
      C_Interface (Ctx.Instance, Request_Data'Address, Request_Data'Length, Result.Data'Address, Size);
      if not RFLX.SPDM.Valid_Length_16 (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Key_Ex_Opaque_Data;

   overriding
   procedure Plat_Get_Key_Ex_Verify_Data (Ctx        : in out Context;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          Result     :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_key_ex_verify_data";
      Size : Interfaces.C.unsigned := Interfaces.C.unsigned (Result.Data'Length);
   begin
      C_Interface (Ctx.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   Result.Data'Address,
                   Size);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Key_Ex_Verify_Data;

   overriding
   procedure Plat_Validate_Finish_Signature (Ctx        : in out Context;
                                             Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                             Signature  :        RFLX.RFLX_Types.Bytes;
                                             Slot       :        RFLX.SPDM.Slot;
                                             Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Signature  : System.Address;
                            Length     : Interfaces.C.unsigned;
                            Slot       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_validate_finish_signature";
   begin
      Result := C_Interface (Ctx.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Signature'Address,
                             Signature'Length,
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Validate_Finish_Signature;

   overriding
   procedure Plat_Validate_Finish_HMAC (Ctx        : in out Context;
                                        Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                        HMAC       :        RFLX.RFLX_Types.Bytes;
                                        Slot       :        RFLX.SPDM.Slot;
                                        Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            HMAC       : System.Address;
                            Length     : Interfaces.C.unsigned;
                            Slot       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_validate_finish_hmac";
   begin
      Result := C_Interface (Ctx.Instance,
                             Interfaces.C.unsigned (Transcript),
                             HMAC'Address,
                             HMAC'Length,
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Validate_Finish_HMAC;

   overriding
   procedure Plat_Get_Finish_Verify_Data (Ctx        : in out Context;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          Result     :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_finish_verify_data";
      Size : Interfaces.C.unsigned := Interfaces.C.unsigned (Result.Data'Length);
   begin
      C_Interface (Ctx.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   Result.Data'Address,
                   Size);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Finish_Verify_Data;

   overriding
   procedure Plat_Set_Session_Phase (Ctx        : in out Context;
                                     Phase      :        RFLX.SPDM_Responder.Session_Phase;
                                     Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                     Slot       :        RFLX.SPDM.Slot;
                                     Result     :    out RFLX.SPDM_Responder.Session_Phase)
   is
      function C_Interface (Instance   : System.Address;
                            Phase      : Interfaces.C.unsigned_char;
                            Transcript : Interfaces.C.unsigned;
                            Slot       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_set_session_phase";
      Current_Phase : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface (Ctx.Instance,
                          Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Phase)),
                          Interfaces.C.unsigned (Transcript),
                          Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))));
   begin
      if not RFLX.SPDM_Responder.Valid_Session_Phase (Current_Phase) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Current_Phase);
   end Plat_Set_Session_Phase;

   overriding
   procedure Plat_Reset_Session_Phase (Ctx    : in out Context;
                                       Result :    out RFLX.SPDM_Responder.Session_Phase)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_reset_session_phase";
      Current_Phase : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer (C_Interface (Ctx.Instance));
   begin
      if not RFLX.SPDM_Responder.Valid_Session_Phase (Current_Phase) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Current_Phase);
   end Plat_Reset_Session_Phase;

   overriding
   procedure Plat_Key_Update (Ctx       : in out Context;
                              Operation :        RFLX.SPDM.Key_Operation;
                              Tag       :        RFLX.SPDM.Key_Update_Tag;
                              Result    :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance  : System.Address;
                            Operation : Interfaces.C.unsigned;
                            Tag       : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_key_update";
   begin
      Result := C_Interface
         (Ctx.Instance,
          Interfaces.C.unsigned (RFLX.SPDM.To_Base_Integer (Operation)),
          Interfaces.C.unsigned (RFLX.SPDM.To_Base_Integer (Tag))) > 0;
   end Plat_Key_Update;

   overriding
   procedure Null_Hash (Ctx    : in out Context;
                        Length :        RFLX.SPDM.Hash_Length;
                        Result :    out RFLX.SPDM_Responder.Hash.Structure)
   is
   begin
      Result.Data := (others => 0);
      Result.Length := Length;
   end Null_Hash;

   overriding
   procedure Null_Signature (Ctx    : in out Context;
                             Length :        RFLX.SPDM.Signature_Length;
                             Result :    out RFLX.SPDM_Responder.Signature.Structure)
   is
   begin
      Result.Data := (others => 0);
      Result.Length := Length;
   end Null_Signature;
end SPDM_C_Responder;
