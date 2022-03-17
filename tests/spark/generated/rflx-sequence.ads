pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Sequence with
  SPARK_Mode
is

   type Length is mod 2**8 with
     Size =>
       8;

   use type RFLX.RFLX_Types.U64;

   function Valid_Length (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 255);

   function To_U64 (Val : RFLX.Sequence.Length) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Sequence.Length is
     (RFLX.Sequence.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

   type Modular_Integer is mod 2**16 with
     Size =>
       16;

   function Valid_Modular_Integer (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 65535);

   function To_U64 (Val : RFLX.Sequence.Modular_Integer) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Sequence.Modular_Integer is
     (RFLX.Sequence.Modular_Integer (Val))
    with
     Pre =>
       Valid_Modular_Integer (Val);

   type Range_Integer is range 1 .. 100 with
     Size =>
       8;

   function Valid_Range_Integer (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val >= 1
      and Val <= 100);

   function To_U64 (Val : RFLX.Sequence.Range_Integer) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Sequence.Range_Integer is
     (RFLX.Sequence.Range_Integer (Val))
    with
     Pre =>
       Valid_Range_Integer (Val);

   type Enumeration is (Zero, One, Two) with
     Size =>
       8;
   for Enumeration use (Zero => 0, One => 1, Two => 2);

   function Valid_Enumeration (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val in 0 | 1 | 2);

   function To_U64 (Enum : RFLX.Sequence.Enumeration) return RFLX.RFLX_Types.U64 is
     ((case Enum is
          when Zero =>
             0,
          when One =>
             1,
          when Two =>
             2));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Sequence.Enumeration is
     ((case Val is
          when 0 =>
             Zero,
          when 1 =>
             One,
          when 2 =>
             Two,
          when others =>
             RFLX.Sequence.Enumeration'Last))
    with
     Pre =>
       Valid_Enumeration (Val);

   pragma Warnings (On, "unreachable branch");

   type AV_Enumeration_Enum is (AV_Zero, AV_One, AV_Two) with
     Size =>
       8;
   for AV_Enumeration_Enum use (AV_Zero => 0, AV_One => 1, AV_Two => 2);

   type AV_Enumeration (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : AV_Enumeration_Enum;
            when False =>
               Raw : RFLX_Types.U64;
         end case;
      end record;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid_AV_Enumeration (Val : RFLX.RFLX_Types.U64) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_U64 (Enum : RFLX.Sequence.AV_Enumeration_Enum) return RFLX.RFLX_Types.U64 is
     ((case Enum is
          when AV_Zero =>
             0,
          when AV_One =>
             1,
          when AV_Two =>
             2));

   function To_Actual (Enum : AV_Enumeration_Enum) return RFLX.Sequence.AV_Enumeration is
     ((True, Enum));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Sequence.AV_Enumeration is
     ((case Val is
          when 0 =>
             (True, AV_Zero),
          when 1 =>
             (True, AV_One),
          when 2 =>
             (True, AV_Two),
          when others =>
             (False, Val)))
    with
     Pre =>
       Valid_AV_Enumeration (Val);

   function To_U64 (Val : RFLX.Sequence.AV_Enumeration) return RFLX.RFLX_Types.U64 is
     ((if Val.Known then To_U64 (Val.Enum) else Val.Raw));

end RFLX.Sequence;
