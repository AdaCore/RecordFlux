pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Messages with
  SPARK_Mode
is

   type Integer is mod 2**32 with
     Size =>
       32;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Messages.Integer) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Messages.Integer) return RFLX.Messages.Integer is
     (Val);

   function To_Actual (Val : RFLX.Messages.Integer) return RFLX.Messages.Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Measurement_Hash_Algo_Base is mod 2**32;

   type Measurement_Hash_Algo is (Raw_Bit_Streams_Only, MH_TPM_ALG_SHA_256, MH_TPM_ALG_SHA_384, MH_TPM_ALG_SHA_512, MH_TPM_ALG_SHA3_256, MH_TPM_ALG_SHA3_384, MH_TPM_ALG_SHA3_512) with
     Size =>
       32;
   for Measurement_Hash_Algo use (Raw_Bit_Streams_Only => 0, MH_TPM_ALG_SHA_256 => 1, MH_TPM_ALG_SHA_384 => 2, MH_TPM_ALG_SHA_512 => 4, MH_TPM_ALG_SHA3_256 => 8, MH_TPM_ALG_SHA3_384 => 16, MH_TPM_ALG_SHA3_512 => 32);

   function Valid (Val : RFLX.Messages.Measurement_Hash_Algo_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 | 4 | 8 | 16 | 32 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.Messages.Measurement_Hash_Algo) return RFLX.Messages.Measurement_Hash_Algo_Base is
     ((case Enum is
          when Raw_Bit_Streams_Only =>
             0,
          when MH_TPM_ALG_SHA_256 =>
             1,
          when MH_TPM_ALG_SHA_384 =>
             2,
          when MH_TPM_ALG_SHA_512 =>
             4,
          when MH_TPM_ALG_SHA3_256 =>
             8,
          when MH_TPM_ALG_SHA3_384 =>
             16,
          when MH_TPM_ALG_SHA3_512 =>
             32));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.Messages.Measurement_Hash_Algo_Base) return RFLX.Messages.Measurement_Hash_Algo is
     ((case Val is
          when 0 =>
             Raw_Bit_Streams_Only,
          when 1 =>
             MH_TPM_ALG_SHA_256,
          when 2 =>
             MH_TPM_ALG_SHA_384,
          when 4 =>
             MH_TPM_ALG_SHA_512,
          when 8 =>
             MH_TPM_ALG_SHA3_256,
          when 16 =>
             MH_TPM_ALG_SHA3_384,
          when 32 =>
             MH_TPM_ALG_SHA3_512,
          when others =>
             raise Program_Error))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

end RFLX.Messages;
