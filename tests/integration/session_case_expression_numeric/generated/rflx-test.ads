pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Test with
  SPARK_Mode
is

   type Tiny_Int is range 1 .. 4 with
     Size =>
       8;

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Tiny_Int (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val >= 1
      and Val <= 4);

   function To_Base_Integer (Val : RFLX.Test.Tiny_Int) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Test.Tiny_Int is
     (RFLX.Test.Tiny_Int (Val))
    with
     Pre =>
       Valid_Tiny_Int (Val);

end RFLX.Test;
