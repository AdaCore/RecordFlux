with System;
with Interfaces.C;
with RFLX.SPDM_Responder.Session_Environment;

--  @summary
--  Example platform implementation for SPDM.
--
--  @description
--  This package serves as both an example and a default C binding for the
--  platform code required by the SPDM responder. It includes the minimum required
--  implementation for all subprograms declared in RFLX.SPDM_Responder.Session.
--  The implementations of these subprograms contain a binding for a C
--  interface. This binding is not required for pure SPARK/Ada projects and can
--  be replaced.
--  SPARK mode is disabled for this package body because the C binding requires
--  Ada features that are not part of the SPARK language.
package body RFLX.SPDM_Responder.Session with
   SPARK_Mode
is
   --  Return CT exponent (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result CT exponent.
   procedure Plat_Cfg_CT_Exponent
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM.CT_Exponent)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_ct_exponent";
   begin
      if not RFLX.SPDM.Valid_CT_Exponent (RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance))) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.CT_Exponent (C_Interface (State.Instance));
   end Plat_Cfg_CT_Exponent;

   --  Indicate whether measurements without restart are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if measurements without restart are supported.
   procedure Plat_Cfg_Cap_Meas_Fresh
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas_fresh";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Meas_Fresh;

   --  Indicate which type of measurements are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Measurement capability.
   procedure Plat_Cfg_Cap_Meas
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM.Meas_Cap)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas";
      Value : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM.Valid_Meas_Cap (Value) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Cap_Meas;

   --  Indicate whether challenge authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if challenge authentication is supported.
   procedure Plat_Cfg_Cap_Chal
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_chal";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Chal;

   --  Indicate whether digests and certificate responses are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if digests and certificate responses are supported.
   procedure Plat_Cfg_Cap_Cert
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cert";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Cert;

   --  Indicate whether responder is able to cache the negotiated state after reset (DSP0274_1.1.0 [178]).
   --  Indicate whether key update is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if caching is supported.
   procedure Plat_Cfg_Cap_Cache
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cache";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Cache;

   --  Indicate whether key update is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if key update is supported.
   procedure Plat_Cfg_Cap_Key_Upd
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_upd";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Key_Upd;

   --  Indicate whether heartbeat messages are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if heartbeat messages are supported.
   procedure Plat_Cfg_Cap_Hbeat
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_hbeat";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Hbeat;

   --  Indicate whether encapsulated messages are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if encapsulated messages are supported.
   procedure Plat_Cfg_Cap_Encap
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encap";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Encap;

   --  Indicate whether mutual authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if mutual authentication is supported.
   procedure Plat_Cfg_Cap_Mut_Auth
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mut_auth";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Mut_Auth;

   --  Indicate whether the public key of the responder was provisioned to the requester (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if the public key was provisioned.
   procedure Plat_Cfg_Cap_Pub_Key_ID
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_pub_key_id";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Pub_Key_ID;

   --  Indicate whether message authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if message authentication is supported.
   procedure Plat_Cfg_Cap_MAC
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mac";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_MAC;

   --  Indicate whether message encryption is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if message encryption is supported.
   procedure Plat_Cfg_Cap_Encrypt
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encrypt";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Encrypt;

   --  Indicate whether pre-shared keys are supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if pre-shared keys are supported.
   procedure Plat_Cfg_Cap_PSK
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM.PSK_Resp_Cap)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_psk";
      Value : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM.Valid_PSK_Resp_Cap (Value) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Cap_PSK;

   --  Indicate whether key exchange is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if key exchange is supported.
   procedure Plat_Cfg_Cap_Key_Ex
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_ex";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Key_Ex;

   --  Indicate whether handshake without encryption or authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result True if handshake in the clear is supported.
   procedure Plat_Cfg_Cap_Handshake_In_The_Clear
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out Boolean)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_handshake_in_the_clear";
      use type Interfaces.C.unsigned_char;
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Cfg_Cap_Handshake_In_The_Clear;

   function C_Bool (Value : Boolean) return Interfaces.C.unsigned_char is
      (if Value then 1 else 0);

   --  Select measurement hash algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the hash algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param TPM_ALG_SHA_256 SHA-256 supported and requested.
   --  @param TPM_ALG_SHA_384 SHA-384 supported and requested.
   --  @param TPM_ALG_SHA_512 SHA-512 supported and requested.
   --  @param TPM_ALG_SHA3_256 SHA3-256 supported and requested.
   --  @param TPM_ALG_SHA3_384 SHA3-384 supported and requested.
   --  @param TPM_ALG_SHA3_512 SHA3-512 supported and requested.
   --  @param Raw_Bit_Streams_Only Raw bit streams supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_Measurement_Hash_Algo
      (State                : in out RFLX.SPDM_Responder.Session_Environment.State;
       TPM_ALG_SHA_256      :        Boolean;
       TPM_ALG_SHA_384      :        Boolean;
       TPM_ALG_SHA_512      :        Boolean;
       TPM_ALG_SHA3_256     :        Boolean;
       TPM_ALG_SHA3_384     :        Boolean;
       TPM_ALG_SHA3_512     :        Boolean;
       Raw_Bit_Streams_Only :        Boolean;
       RFLX_Result               :    out RFLX.SPDM.Measurement_Hash_Algo)
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
               (Instance             => State.Instance,
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
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Measurement_Hash_Algo;

   --  Select base asymmetric algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the signature algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P384 ECDSA-ECC-384 supported and requested.
   --  @param TPM_ALG_RSAPSS_4096 RSAPSS-4096 supported and requested.
   --  @param TPM_ALG_RSASSA_4096 RSASSA-4096 supported and requested.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P256 ECDSA-ECC-256 supported and requested.
   --  @param TPM_ALG_RSAPSS_3072 RSAPSS-3072 supported and requested.
   --  @param TPM_ALG_RSASSA_3072 RSASSA-3072 supported and requested.
   --  @param TPM_ALG_RSAPSS_2048 RSAPSS-2048 supported and requested.
   --  @param TPM_ALG_RSASSA_2048 RSASSA-2048 supported and requested.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P521 ECDSA-ECC-521 supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_Base_Asym_Algo
      (State                       : in out RFLX.SPDM_Responder.Session_Environment.State;
       TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       TPM_ALG_RSAPSS_4096         :        Boolean;
       TPM_ALG_RSASSA_4096         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       TPM_ALG_RSAPSS_3072         :        Boolean;
       TPM_ALG_RSASSA_3072         :        Boolean;
       TPM_ALG_RSAPSS_2048         :        Boolean;
       TPM_ALG_RSASSA_2048         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       RFLX_Result                      :    out RFLX.SPDM.Base_Asym_Algo)
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
               (Instance                    => State.Instance,
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
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Base_Asym_Algo;

   --  Select base hash algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the hash algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param TPM_ALG_SHA_256 SHA-256 supported and requested.
   --  @param TPM_ALG_SHA_384 SHA-384 supported and requested.
   --  @param TPM_ALG_SHA_512 SHA-512 supported and requested.
   --  @param TPM_ALG_SHA3_256 SHA3-256 supported and requested.
   --  @param TPM_ALG_SHA3_384 SHA3-384 supported and requested.
   --  @param TPM_ALG_SHA3_512 SHA3-512 supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_Base_Hash_Algo
      (State            : in out RFLX.SPDM_Responder.Session_Environment.State;
       TPM_ALG_SHA_256  :        Boolean;
       TPM_ALG_SHA_384  :        Boolean;
       TPM_ALG_SHA_512  :        Boolean;
       TPM_ALG_SHA3_256 :        Boolean;
       TPM_ALG_SHA3_384 :        Boolean;
       TPM_ALG_SHA3_512 :        Boolean;
       RFLX_Result           :    out RFLX.SPDM.Base_Hash_Algo)
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
               (Instance         => State.Instance,
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
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_Base_Hash_Algo;

   --  Select the Diffie-Hellman Ephemeral group (DSP0274_1.1.0 [189]).
   --
   --  The arguments describe the group supported by the requester.
   --  This function should select one of the provided groups.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Req_SecP521r1 SECP521R1 supported and requested.
   --  @param Req_SecP384r1 SECP384R1 supported and requested.
   --  @param Req_SecP256r1 SECP256R1 supported and requested.
   --  @param Req_FFDHE4096 FFDHE4092 supported and requested.
   --  @param Req_FFDHE3072 FFDHE3072 supported and requested.
   --  @param Req_FFDHE2048 FFDHE2048 supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_DHE
      (State         : in out RFLX.SPDM_Responder.Session_Environment.State;
       Req_SecP521r1 :        Boolean;
       Req_SecP384r1 :        Boolean;
       Req_SecP256r1 :        Boolean;
       Req_FFDHE4096 :        Boolean;
       Req_FFDHE3072 :        Boolean;
       Req_FFDHE2048 :        Boolean;
       RFLX_Result        :    out RFLX.SPDM_Responder.DHE_Algo)
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
               (Instance  => State.Instance,
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
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (Value);
   end Plat_Cfg_Sel_DHE;

   --  Select the AEAD algorithm (DSP0274_1.1.0 [190]).
   --
   --  The arguments describe the algorithm supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Req_ChaCha20_Poly1305 CHACHA20-POLY135 supported and requested.
   --  @param Req_AES_256_GCM AES-256-GCM supported and requested.
   --  @param Req_AES_128_GCM AES-128-GCM supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_AEAD
      (State                 : in out RFLX.SPDM_Responder.Session_Environment.State;
       Req_ChaCha20_Poly1305 :        Boolean;
       Req_AES_256_GCM       :        Boolean;
       Req_AES_128_GCM       :        Boolean;
       RFLX_Result                :    out RFLX.SPDM_Responder.AEAD_Algo)
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
               (Instance             => State.Instance,
                AA_ChaCha20_Poly1305 => C_Bool (Req_ChaCha20_Poly1305),
                AA_AES_256_GCM       => C_Bool (Req_AES_256_GCM),
                AA_AES_128_GCM       => C_Bool (Req_AES_128_GCM)));
   begin
      if not RFLX.SPDM_Responder.Valid_AEAD_Algo (Value) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (Value);
   end Plat_Cfg_Sel_AEAD;

   --  Select base asymmetric algorithm (DSP0274_1.1.0 [191]).
   --
   --  The arguments describe the key signature algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P384 ECDSA-ECC-384 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_4096 RSAPSS-4096 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_4096 RSASSA-4096 supported and requested.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P256 ECDSA-ECC-256 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_3072 RSAPSS-3072 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_3072 RSASSA-3072 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_2048 RSAPSS-2048 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_2048 RSASSA-2048 supported and requested.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P521 ECDSA-ECC-521 supported and requested.
   --  @param RFLX_Result Selected algorithm.
   procedure Plat_Cfg_Sel_RBAA
      (State                           : in out RFLX.SPDM_Responder.Session_Environment.State;
       Req_TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       Req_TPM_ALG_RSAPSS_4096         :        Boolean;
       Req_TPM_ALG_RSASSA_4096         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       Req_TPM_ALG_RSAPSS_3072         :        Boolean;
       Req_TPM_ALG_RSASSA_3072         :        Boolean;
       Req_TPM_ALG_RSAPSS_2048         :        Boolean;
       Req_TPM_ALG_RSASSA_2048         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       RFLX_Result                          :    out RFLX.SPDM.Base_Asym_Algo)
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
               (Instance                       => State.Instance,
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
      RFLX_Result := RFLX.SPDM.To_Actual (Value);
   end Plat_Cfg_Sel_RBAA;

   --  Get digests for digests response (DSP0274_1.1.0 [232]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Digests data.
   procedure Plat_Get_Digests_Data
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM_Responder.Digests_Data.Structure)
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
      Length := Interfaces.C.long (RFLX_Result.Value'Length);
      C_Interface (Instance => State.Instance,
                   Data     => RFLX_Result.Value'Address,
                   Length   => Length'Address,
                   Slots    => Slot_Mask'Address);
      RFLX_Result.Length := RFLX.SPDM_Responder.Digests_Length (Length);
      RFLX_Result.Slot_0_Present := RFLX.SPDM.Slot_Present (Slot_Mask and 16#01#);
      RFLX_Result.Slot_1_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#02#) / 16#02#);
      RFLX_Result.Slot_2_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#04#) / 16#04#);
      RFLX_Result.Slot_3_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#08#) / 16#08#);
      RFLX_Result.Slot_4_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#10#) / 16#10#);
      RFLX_Result.Slot_5_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#20#) / 16#20#);
      RFLX_Result.Slot_6_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#40#) / 16#40#);
      RFLX_Result.Slot_7_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#80#) / 16#80#);
   end Plat_Get_Digests_Data;

   --  Validate an incoming certificate request (DSP0274_1.1.0 [238]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Slot Certificate slot number.
   --  @param Offset Certificate portion offset.
   --  @param Length Certificate portion length.
   --  @param RFLX_Result Success.
   procedure Plat_Valid_Certificate_Request
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       RFLX_Result :    out Boolean)
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
      RFLX_Result := 0 /= C_Interface (Instance => State.Instance,
                                  Slot     => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                                  Offset   => Interfaces.C.unsigned_short (Offset),
                                  Length   => Interfaces.C.unsigned_short (Length));
   end Plat_Valid_Certificate_Request;

   --  Provide requested certificate chain (DSP0274_1.1.0 [238]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Slot Requested certificate slot.
   --  @param Offset Offset in the certificate chain.
   --  @param Length Length of the requested certificate portion.
   --  @param RFLX_Result Certificate response including certificate data, portion length and
   --               remainder length (DSP0274_1.1.0 [239]).
   procedure Plat_Get_Certificate_Response
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       RFLX_Result :    out RFLX.SPDM.Certificate_Response.Structure)
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
      --  Note: RFLX_Result must have a size of at least Max_Length + 7
      Cert_Length              : Interfaces.C.unsigned_short;
      Total_Length             : Interfaces.C.unsigned_short;
      Portion_Remainder_Length : RFLX.RFLX_Types.Base_Integer;
   begin
      if Length <= Max_Length then
         Cert_Length := Interfaces.C.unsigned_short (Length);
      else
         Cert_Length := Interfaces.C.unsigned_short (Max_Length);
      end if;
      C_Interface (Instance     => State.Instance,
                   Data         => RFLX_Result.Cert_Chain'Address,
                   Slot         => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   Offset       => Interfaces.C.unsigned_short (Offset),
                   Length       => Cert_Length,
                   Total_Length => Total_Length);
      RFLX_Result.Slot := Slot;
      RFLX_Result.Param_2 := 0;
      if Cert_Length = Interfaces.C.unsigned_short (Max_Length) then
         Portion_Remainder_Length := RFLX.RFLX_Types.Base_Integer (Max_Length);
      else
         Portion_Remainder_Length := RFLX.RFLX_Types.Base_Integer (Cert_Length);
      end if;
      if not RFLX.SPDM.Valid_Length_16 (Portion_Remainder_Length) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Portion_Length := RFLX.SPDM.To_Actual (Portion_Remainder_Length);
      Portion_Remainder_Length :=
         RFLX.RFLX_Types.Base_Integer (Total_Length - Cert_Length - Interfaces.C.unsigned_short (Offset));
      if not RFLX.SPDM.Valid_Length_16 (Portion_Remainder_Length) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Remainder_Length := RFLX.SPDM.To_Actual (Portion_Remainder_Length);
   end Plat_Get_Certificate_Response;

   --  Get the number of measurement indices (DSP0274_1.1.0 [327]).
   --
   --  Returns the number of indices available from the responder. This
   --  may be zero if none are available and up to 254.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Number of measurements.
   procedure Plat_Get_Number_Of_Indices
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM.Measurement_Count)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_number_of_indices";
      Count : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM.Valid_Measurement_Count (Count) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Count);
   end Plat_Get_Number_Of_Indices;

   --  Get the number of measurement indices that include the trusted computing base (DSP0274_1.1.0 [422]).
   --
   --  Otherwise it has the same behaviour as spdm_platform_get_number_of_indices.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Number of measurements in TCB.
   procedure Plat_Get_Number_Of_Indices_TCB
      (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
       RFLX_Result :    out RFLX.SPDM.Measurement_Count)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_number_of_indices_tcb";
      Count : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM.Valid_Measurement_Count (Count) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Count);
   end Plat_Get_Number_Of_Indices_TCB;

   --  Generate a nonce for cryptographic operations.
   --
   --  The platform must always keep the latest generated nonce and shall
   --  add it to the transcript when Plat_Update_Transcript_Nonce
   --  is called. Only after this function is called the nonce can be marked
   --  as valid.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Nonce containing 32 byte long buffer.
   procedure Plat_Get_Nonce (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                             RFLX_Result :    out RFLX.SPDM.Nonce.Structure)
   is
      procedure C_Interface (Instance :     System.Address;
                             Nonce    : out RFLX.RFLX_Types.Bytes) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_nonce";
   begin
      C_Interface (State.Instance, RFLX_Result.Data);
   end Plat_Get_Nonce;

   --  Return a DMTF measurement field (DSP0274_1.1.0 [335]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Index Requested measurement index.
   --  @param RFLX_Result Complete DMTF measurement field.
   procedure Plat_Get_DMTF_Measurement_Field (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                              Index  :        RFLX.SPDM.Index;
                                              RFLX_Result :    out RFLX.SPDM.DMTF_Measurement_Field.Structure)
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
      RFLX_Result.Measurement_Value_Length := RFLX_Result.Measurement_Value'Length;
      C_Interface (State.Instance,
                   Interfaces.C.unsigned (Index),
                   Value_Representation,
                   Value_Type,
                   Interfaces.C.unsigned (RFLX_Result.Measurement_Value_Length),
                   RFLX_Result.Measurement_Value);
      if
         not RFLX.SPDM.Valid_DMTF_Spec_Measurement_Value_Representation
                (RFLX.RFLX_Types.Base_Integer (Value_Representation))
         or not RFLX.SPDM.Valid_DMTF_Spec_Measurement_Value_Type (RFLX.RFLX_Types.Base_Integer (Value_Type))
      then
         raise Constraint_Error;
      end if;
      RFLX_Result.Measurement_Value_Representation :=
         RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Value_Representation));
      RFLX_Result.Measurement_Value_Type := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Value_Type));
   end Plat_Get_DMTF_Measurement_Field;

   --  Provide opaque data for the measurement response (DSP0274_1.1.0 [327]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Opaque data.
   procedure Plat_Get_Meas_Opaque_Data (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                        RFLX_Result :    out RFLX.SPDM_Responder.Opaque_Data.Structure)
   is
      procedure C_Interface (Instance :        System.Address;
                             Data     :        System.Address;
                             Length   : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_meas_opaque_data";
      Length : Interfaces.C.unsigned := RFLX_Result.Data'Length;
   begin
      C_Interface (State.Instance, RFLX_Result.Data'Address, Length);
      if not RFLX.SPDM.Valid_Length_16 (RFLX.RFLX_Types.Base_Integer (Length)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Length));
   end Plat_Get_Meas_Opaque_Data;

   --  Register a new transcript with the platform.
   --
   --  The returned ID must be used on all subsequent operations on the same transcript.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Kind Transcript kind.
   --  @param RFLX_Result Transcript ID. On success Plat_Valid_Transcript_ID
   --         must return true on this ID.
   procedure Plat_Get_New_Transcript (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                      Kind   :        RFLX.SPDM_Responder.Transcript_Kind;
                                      RFLX_Result :    out RFLX.SPDM_Responder.Transcript_ID)
   is
      function C_Interface (Instance : System.Address;
                            Kind     : Interfaces.C.unsigned_char) return Interfaces.C.unsigned with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_new_transcript";
      Transcript : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface (State.Instance, Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Kind))));
   begin
      if not RFLX.SPDM_Responder.Valid_Transcript_ID (Transcript) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (Transcript);
   end Plat_Get_New_Transcript;

   --  Indicate whether a transcript ID is valid.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param RFLX_Result True if transcript ID is valid.
   procedure Plat_Valid_Transcript_ID (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                       Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                       RFLX_Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_valid_transcript_id";
   begin
      RFLX_Result := C_Interface (State.Instance, Interfaces.C.unsigned (Transcript)) > 0;
   end Plat_Valid_Transcript_ID;

   --  Reset an already registered transcript.
   --
   --  This operation may change the transcript kind, too. The returned transcript ID
   --  may be different from the provided one. It has the same behaviour as
   --  Plat_Get_New_Transcript except that it allows reusing an existing resource.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Old transcript ID.
   --  @param Kind Transcript kind for the new transcript.
   --  @param RFLX_Result New transcript ID.
   procedure Plat_Reset_Transcript (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                    Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                    Kind       :        RFLX.SPDM_Responder.Transcript_Kind;
                                    RFLX_Result     :    out RFLX.SPDM_Responder.Transcript_ID)
   is
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Kind       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned with
         Import,
         Convention => C,
         External_Name => "spdm_platform_reset_transcript";
      New_Transcript : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer
            (C_Interface (State.Instance,
                          Interfaces.C.unsigned (Transcript),
                          Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Kind))));
   begin
      if not RFLX.SPDM_Responder.Valid_Transcript_ID (New_Transcript) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (New_Transcript);
   end Plat_Reset_Transcript;

   --  Append a chunk of data to the transcript.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Data Transcript data to be appended.
   --  @param Offset Offset in data.
   --  @param Length Length of data to be appended to the transcript,
   --                Length + Offset is less or equal to the size of data.
   --  @param RFLX_Result Success.
   procedure Plat_Update_Transcript (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                     Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                     Data       :        RFLX.RFLX_Types.Bytes;
                                     Offset     :        RFLX.SPDM.Length_16;
                                     Length     :        RFLX.SPDM.Length_16;
                                     RFLX_Result     :    out Boolean)
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
      RFLX_Result := C_Interface (State.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Data'Address,
                             Interfaces.C.unsigned (Offset),
                             Data_Length) > 0;
   end Plat_Update_Transcript;

   --  Append the latest generated nonce to the transcript.
   --
   --  Append the latest nonce generated by Plat_Get_Nonce to the
   --  transcript. The nonce must be marked as invalid after this operation.
   --  If the nonce is already marked as invalid when this function is called
   --  the operation must fail.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param RFLX_Result Success.
   procedure Plat_Update_Transcript_Nonce (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                           Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                           RFLX_Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_update_transcript_nonce";
   begin
      RFLX_Result := C_Interface (State.Instance, Interfaces.C.unsigned (Transcript)) > 0;
   end Plat_Update_Transcript_Nonce;

   --  Generate signature from current transcript.
   --
   --  Generate the signature from the current state of the transcript. This
   --  does not invalidate or reset the transcript.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID of the signing key.
   --  @param RFLX_Result Signature. In case of an error RFLX_Result.Length must be set to 0.
   procedure Plat_Get_Signature (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                 Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                 Slot       :        RFLX.SPDM.Slot;
                                 RFLX_Result     :    out RFLX.SPDM_Responder.Signature.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Signature  :    out RFLX.RFLX_Types.Bytes;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_signature";
      Length : Interfaces.C.unsigned := RFLX_Result.Data'Length;
   begin
      C_Interface (State.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   RFLX_Result.Data,
                   Length);
      if not RFLX.SPDM.Valid_Signature_Length (RFLX.RFLX_Types.Base_Integer (Length)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Length));
   end Plat_Get_Signature;

   --  Generate responder exchange data from requester exchange data (DSP0274_1.1.0 [421]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Exchange_Data Data sent by the requester.
   --  @param RFLX_Result Exchange data to be send with the response.
   procedure Plat_Get_Exchange_Data (State         : in out RFLX.SPDM_Responder.Session_Environment.State;
                                     Exchange_Data :        RFLX.RFLX_Types.Bytes;
                                     RFLX_Result        :    out RFLX.SPDM_Responder.Exchange_Data.Structure)
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
      RFLX_Result.Pad := 0;
      RFLX_Result.Data (RFLX_Result.Data'First .. RFLX_Result.Data'First + Exchange_Data'Length - 1) := Exchange_Data;
      C_Interface (State.Instance, RFLX_Result.Data'Address, Size);
      if not RFLX.SPDM.Valid_Exchange_Data_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Exchange_Data;

   --  Get heartbeat period (DSP0274_1.1.0 [422]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Heartbeat period.
   procedure Plat_Get_Heartbeat_Period (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                        RFLX_Result :    out RFLX.SPDM.Heartbeat_Period)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_heartbeat_period";
      Period : constant RFLX.RFLX_Types.Base_Integer := RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM.Valid_Heartbeat_Period (Period) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Period);
   end Plat_Get_Heartbeat_Period;

   --  Check session ID sent by the requester for validity (DSP0274_1.1.0 [421]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Req_Session_ID Requester session ID.
   --  @param RFLX_Result If True, the ID is valid.
   procedure Plat_Valid_Session_ID (State          : in out RFLX.SPDM_Responder.Session_Environment.State;
                                    Req_Session_ID :        RFLX.SPDM.Session_ID;
                                    RFLX_Result         :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Session_ID : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_valid_session_id";
   begin
      RFLX_Result := C_Interface (State.Instance, Interfaces.C.unsigned_short (Req_Session_ID)) > 0;
   end Plat_Valid_Session_ID;

   --  Get session ID for key exchange response (DSP0274_1.1.0 [422]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Req_Session_ID Requester session ID
   --  @param RFLX_Result Response session ID.
   procedure Plat_Get_Session_ID (State          : in out RFLX.SPDM_Responder.Session_Environment.State;
                                  Req_Session_ID :        RFLX.SPDM.Session_ID;
                                  RFLX_Result         :    out RFLX.SPDM.Session_ID)
   is
      function C_Interface (Instance   : System.Address;
                            Session_ID : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_short with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_session_id";
      Session_ID : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance, Interfaces.C.unsigned_short (Req_Session_ID)));
   begin
      if not RFLX.SPDM.Valid_Session_ID (Session_ID) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM.To_Actual (Session_ID);
   end Plat_Get_Session_ID;

   --  Request mutual authentication in key exchange response (DSP0274_1.1.0 [422]).
   --
   --  Returning true only enables regular mutual authentication. Encapsulated
   --  and implicit mutual authentication are not supported.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Mutual auth requested.
   procedure Plat_Use_Mutual_Auth (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                   RFLX_Result :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_use_mutual_auth";
   begin
      RFLX_Result := C_Interface (State.Instance) > 0;
   end Plat_Use_Mutual_Auth;

   --  Get the hash over the measurement summary (DSP0274_1.1.0 [422]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Data Measurement summary data.
   --  @param RFLX_Result Measurement summary hash.
   procedure Plat_Get_Summary_Hash (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                    Data   :        RFLX.RFLX_Types.Bytes;
                                    RFLX_Result :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance    : System.Address;
                             Summary     : System.Address;
                             Length      : Interfaces.C.unsigned;
                             Hash        : System.Address;
                             Hash_Length : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_summary_hash";
      Hash_Length : Interfaces.C.unsigned := RFLX_Result.Data'Length;
   begin
      C_Interface (State.Instance,
                   Data'Address,
                   Data'Length,
                   RFLX_Result.Data'Address,
                   Hash_Length);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Hash_Length)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Hash_Length));
   end Plat_Get_Summary_Hash;

   --  Append the selected certificate chain to the transcript.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID for certificate selection.
   --  @param RFLX_Result Success.
   procedure Plat_Update_Transcript_Cert (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          RFLX_Result     :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance   : System.Address;
                            Transcript : Interfaces.C.unsigned;
                            Slot       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_update_transcript_cert";
   begin
      RFLX_Result := C_Interface (State.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Update_Transcript_Cert;

   --  Handle key exchange opaque data (DSP0274_1.1.0 [422]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Request_Data Opaque data sent by the requester.
   --  @param RFLX_Result Opaque data to be sent with the response.
   procedure Plat_Get_Key_Ex_Opaque_Data (State        : in out RFLX.SPDM_Responder.Session_Environment.State;
                                          Request_Data :        RFLX.RFLX_Types.Bytes;
                                          RFLX_Result       :    out RFLX.SPDM_Responder.Opaque_Data.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Req_Data   :        System.Address;
                             Req_Length :        Interfaces.C.unsigned;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_key_ex_opaque_data";
         Size : Interfaces.C.unsigned := RFLX_Result.Data'Length;
   begin
      C_Interface (State.Instance, Request_Data'Address, Request_Data'Length, RFLX_Result.Data'Address, Size);
      if not RFLX.SPDM.Valid_Length_16 (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Key_Ex_Opaque_Data;

   --  Generate the responder verify data for key exchange (DSP0274_1.1.0 [422]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID of the signing key.
   --  @param RFLX_Result Key exchange verify data.
   procedure Plat_Get_Key_Ex_Verify_Data (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          RFLX_Result     :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_key_ex_verify_data";
      Size : Interfaces.C.unsigned := Interfaces.C.unsigned (RFLX_Result.Data'Length);
   begin
      C_Interface (State.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   RFLX_Result.Data'Address,
                   Size);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Key_Ex_Verify_Data;

   --  Validate the finish signature sent by the requester (DSP0274_1.1.0 [432]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Signature Signature sent by the requester.
   --  @param Slot Slot ID of the signing key.
   --  @param RFLX_Result Success.
   procedure Plat_Validate_Finish_Signature (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                             Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                             Signature  :        RFLX.RFLX_Types.Bytes;
                                             Slot       :        RFLX.SPDM.Slot;
                                             RFLX_Result     :    out Boolean)
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
      RFLX_Result := C_Interface (State.Instance,
                             Interfaces.C.unsigned (Transcript),
                             Signature'Address,
                             Signature'Length,
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Validate_Finish_Signature;

   --  Validate the finish HMAC sent by the requester (DSP0274_1.1.0 [432]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param HMAC HMAC sent by the requester.
   --  @param Slot Slot ID used for the HMAC generation.
   --  @param RFLX_Result Success.
   procedure Plat_Validate_Finish_HMAC (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                        Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                        HMAC       :        RFLX.RFLX_Types.Bytes;
                                        Slot       :        RFLX.SPDM.Slot;
                                        RFLX_Result     :    out Boolean)
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
      RFLX_Result := C_Interface (State.Instance,
                             Interfaces.C.unsigned (Transcript),
                             HMAC'Address,
                             HMAC'Length,
                             Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))) > 0;
   end Plat_Validate_Finish_HMAC;

   --  Generate the responder verify data for the finish response (DSP0274_1.1.0 [433]).
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID for the used key.
   --  @param RFLX_Result Responder verify data for finish response.
   procedure Plat_Get_Finish_Verify_Data (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          RFLX_Result     :    out RFLX.SPDM_Responder.Hash.Structure)
   is
      procedure C_Interface (Instance   :        System.Address;
                             Transcript :        Interfaces.C.unsigned;
                             Slot       :        Interfaces.C.unsigned_char;
                             Data       :        System.Address;
                             Length     : in out Interfaces.C.unsigned) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_get_finish_verify_data";
      Size : Interfaces.C.unsigned := Interfaces.C.unsigned (RFLX_Result.Data'Length);
   begin
      C_Interface (State.Instance,
                   Interfaces.C.unsigned (Transcript),
                   Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot)),
                   RFLX_Result.Data'Address,
                   Size);
      if not RFLX.SPDM.Valid_Hash_Length (RFLX.RFLX_Types.Base_Integer (Size)) then
         raise Constraint_Error;
      end if;
      RFLX_Result.Length := RFLX.SPDM.To_Actual (RFLX.RFLX_Types.Base_Integer (Size));
   end Plat_Get_Finish_Verify_Data;

   --  Set the current session phase.
   --
   --  Set the current session phase to the phase passed as argument. If
   --  an error occurs the session phase must be set to the error value, otherwise
   --  it must be set to the requested phase.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Phase Requested session phase.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID requested by the requester.
   --  @param RFLX_Result Updated session phase.
   procedure Plat_Set_Session_Phase (State      : in out RFLX.SPDM_Responder.Session_Environment.State;
                                     Phase      :        RFLX.SPDM_Responder.Session_Phase;
                                     Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                     Slot       :        RFLX.SPDM.Slot;
                                     RFLX_Result     :    out RFLX.SPDM_Responder.Session_Phase)
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
            (C_Interface (State.Instance,
                          Interfaces.C.unsigned_char (RFLX.SPDM_Responder.To_Base_Integer (Phase)),
                          Interfaces.C.unsigned (Transcript),
                          Interfaces.C.unsigned_char (RFLX.SPDM.To_Base_Integer (Slot))));
   begin
      if not RFLX.SPDM_Responder.Valid_Session_Phase (Current_Phase) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (Current_Phase);
   end Plat_Set_Session_Phase;

   --  Reset current session.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param RFLX_Result Updated session phase. Return Session_Error if an error
   --                occurred, otherwise No_Session.
   procedure Plat_Reset_Session_Phase (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                                       RFLX_Result :    out RFLX.SPDM_Responder.Session_Phase)
   is
      function C_Interface (Instance : System.Address) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_reset_session_phase";
      Current_Phase : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer (C_Interface (State.Instance));
   begin
      if not RFLX.SPDM_Responder.Valid_Session_Phase (Current_Phase) then
         raise Constraint_Error;
      end if;
      RFLX_Result := RFLX.SPDM_Responder.To_Actual (Current_Phase);
   end Plat_Reset_Session_Phase;

   --  Perform a key update operation.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Operation Key update operation.
   --  @param Tag Key update tag.
   --  @param RFLX_Result Success.
   procedure Plat_Key_Update (State     : in out RFLX.SPDM_Responder.Session_Environment.State;
                              Operation :        RFLX.SPDM.Key_Operation;
                              Tag       :        RFLX.SPDM.Key_Update_Tag;
                              RFLX_Result    :    out Boolean)
   is
      use type Interfaces.C.unsigned_char;
      function C_Interface (Instance  : System.Address;
                            Operation : Interfaces.C.unsigned;
                            Tag       : Interfaces.C.unsigned) return Interfaces.C.unsigned_char with
         Import,
         Convention => C,
         External_Name => "spdm_platform_key_update";
   begin
      RFLX_Result := C_Interface
         (State.Instance,
          Interfaces.C.unsigned (RFLX.SPDM.To_Base_Integer (Operation)),
          Interfaces.C.unsigned (RFLX.SPDM.To_Base_Integer (Tag))) > 0;
   end Plat_Key_Update;

   --  Initialization function for empty hashes.
   --
   --  This is a helper function for the specification and does not interact with any
   --  platform code. It must not be changed. If a custom implementation is done it must
   --  return a Hash with the requested length and zero initialized data.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Length Length of the zeroed hash.
   --  @param RFLX_Result Zeroed out hash data.
   procedure Null_Hash (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                        Length :        RFLX.SPDM.Hash_Length;
                        RFLX_Result :    out RFLX.SPDM_Responder.Hash.Structure)
   is
   begin
      RFLX_Result.Data := (others => 0);
      RFLX_Result.Length := Length;
   end Null_Hash;

   --  Initialization function for empty signatures.
   --
   --  This is a helper function for the specification and does not interact with any
   --  platform code. It must not be changed. if a custom implementation is done it must
   --  return a Signature with the requested length and zero initialized data.
   --
   --  @param State RFLX.SPDM_Responder.Session_Environment.State.
   --  @param Length Length of the zeroed signature.
   --  @param RFLX_Result Zeroed out signature data.
   procedure Null_Signature (State  : in out RFLX.SPDM_Responder.Session_Environment.State;
                             Length :        RFLX.SPDM.Signature_Length;
                             RFLX_Result :    out RFLX.SPDM_Responder.Signature.Structure)
   is
   begin
      RFLX_Result.Data := (others => 0);
      RFLX_Result.Length := Length;
   end Null_Signature;

end RFLX.SPDM_Responder.Session;
