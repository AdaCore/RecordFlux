pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Test with
  SPARK_Mode
is

   type Size is range 0 .. 2**32 - 1 with
     Size =>
       32;

   use type RFLX.RFLX_Types.U64;

   function Valid_Size (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 4294967295);

   function To_U64 (Val : RFLX.Test.Size) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.Test.Size is
     (RFLX.Test.Size (Val))
    with
     Pre =>
       Valid_Size (Val);

end RFLX.Test;
