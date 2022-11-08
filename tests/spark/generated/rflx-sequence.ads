pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Sequence with
  SPARK_Mode
is

   type Length is range 0 .. 2**8 - 1 with
     Size =>
       8;

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Length (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.Sequence.Length) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Sequence.Length is
     (RFLX.Sequence.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

   type Integer is range 1 .. 100 with
     Size =>
       16;

   function Valid_Integer (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val >= 1
      and Val <= 100);

   function To_Base_Integer (Val : RFLX.Sequence.Integer) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Sequence.Integer is
     (RFLX.Sequence.Integer (Val))
    with
     Pre =>
       Valid_Integer (Val);

   type Enumeration is (Zero, One, Two) with
     Size =>
       8;
   for Enumeration use (Zero => 0, One => 1, Two => 2);

   function Valid_Enumeration (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2);

   function To_Base_Integer (Enum : RFLX.Sequence.Enumeration) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when Zero =>
             0,
          when One =>
             1,
          when Two =>
             2));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Sequence.Enumeration is
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
               Raw : RFLX_Types.Base_Integer;
         end case;
      end record;

   function Valid_AV_Enumeration (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val < 2**8);

   function Valid_AV_Enumeration (Val : AV_Enumeration) return Boolean is
     ((if Val.Known then True else Valid_AV_Enumeration (Val.Raw) and Val.Raw not in 0 | 1 | 2));

   function To_Base_Integer (Enum : RFLX.Sequence.AV_Enumeration_Enum) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when AV_Zero =>
             0,
          when AV_One =>
             1,
          when AV_Two =>
             2));

   function To_Actual (Enum : AV_Enumeration_Enum) return RFLX.Sequence.AV_Enumeration is
     ((True, Enum));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Sequence.AV_Enumeration is
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

   function To_Base_Integer (Val : RFLX.Sequence.AV_Enumeration) return RFLX.RFLX_Types.Base_Integer is
     ((if Val.Known then To_Base_Integer (Val.Enum) else Val.Raw));

end RFLX.Sequence;
