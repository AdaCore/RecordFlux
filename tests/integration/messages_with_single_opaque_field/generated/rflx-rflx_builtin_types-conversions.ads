pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with RFLX.RFLX_Arithmetic;

package RFLX.RFLX_Builtin_Types.Conversions with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Conversions);

   function Valid_Boolean (Val : RFLX.RFLX_Arithmetic.U64) return Boolean is
     (case Val is
         when 0 | 1 =>
            True,
         when others =>
            False);

   function To_U64 (Enum : Boolean) return RFLX.RFLX_Arithmetic.U64 is
     (case Enum is
         when False =>
            0,
         when True =>
            1);

   function To_Actual (Val : RFLX.RFLX_Arithmetic.U64) return Boolean is
     (case Val is
         when 0 =>
            False,
         when 1 =>
            True,
         when others =>
            False)
    with
     Pre =>
       Valid_Boolean (Val);

end RFLX.RFLX_Builtin_Types.Conversions;
