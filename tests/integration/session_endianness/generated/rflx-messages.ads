pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Messages with
  SPARK_Mode
is

   type Integer is range 0 .. 2**32 - 1 with
     Size =>
       32;

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Integer (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.Messages.Integer) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Messages.Integer is
     (RFLX.Messages.Integer (Val))
    with
     Pre =>
       Valid_Integer (Val);

   type Enum_T is (Enum_A, Enum_B, Enum_C, Enum_D, Enum_E, Enum_F, Enum_G) with
     Size =>
       32;
   for Enum_T use (Enum_A => 0, Enum_B => 1, Enum_C => 2, Enum_D => 4, Enum_E => 8, Enum_F => 16, Enum_G => 32);

   function Valid_Enum_T (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 4 | 8 | 16 | 32);

   function To_Base_Integer (Enum : RFLX.Messages.Enum_T) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when Enum_A =>
             0,
          when Enum_B =>
             1,
          when Enum_C =>
             2,
          when Enum_D =>
             4,
          when Enum_E =>
             8,
          when Enum_F =>
             16,
          when Enum_G =>
             32));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Messages.Enum_T is
     ((case Val is
          when 0 =>
             Enum_A,
          when 1 =>
             Enum_B,
          when 2 =>
             Enum_C,
          when 4 =>
             Enum_D,
          when 8 =>
             Enum_E,
          when 16 =>
             Enum_F,
          when 32 =>
             Enum_G,
          when others =>
             RFLX.Messages.Enum_T'Last))
    with
     Pre =>
       Valid_Enum_T (Val);

   pragma Warnings (On, "unreachable branch");

end RFLX.Messages;
