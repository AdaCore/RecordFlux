pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Test with
  SPARK_Mode
is

   type Size is range 0 .. 2**32 - 1 with
     Size =>
       32;

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Size (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Int (Val : RFLX.Test.Size) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Test.Size is
     (RFLX.Test.Size (Val))
    with
     Pre =>
       Valid_Size (Val);

end RFLX.Test;
