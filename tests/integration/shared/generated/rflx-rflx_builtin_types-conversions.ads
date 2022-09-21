pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

with RFLX.RFLX_Arithmetic;

package RFLX.RFLX_Builtin_Types.Conversions with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Always_Return)
is

   pragma Annotate (GNATprove, Always_Return, Conversions);

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

   function Valid_Boolean (Val : RFLX.RFLX_Arithmetic.Base_Integer) return Boolean is
     (case Val is
         when 0 | 1 =>
            True,
         when others =>
            False);

   function To_Base_Integer (Enum : Boolean) return RFLX.RFLX_Arithmetic.Base_Integer is
     (case Enum is
         when False =>
            0,
         when True =>
            1);

   function To_Actual (Val : RFLX.RFLX_Arithmetic.Base_Integer) return Boolean is
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
