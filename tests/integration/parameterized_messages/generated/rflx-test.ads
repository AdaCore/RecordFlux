pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Test with
  SPARK_Mode
is

   type Length is range 1 .. 2**14 - 1 with
     Size =>
       16;

   use type RFLX.RFLX_Types.S63;

   function Valid_Length (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val >= 1
      and Val <= 16383);

   function To_S63 (Val : RFLX.Test.Length) return RFLX.RFLX_Types.S63 is
     (RFLX.RFLX_Types.S63 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.Test.Length is
     (RFLX.Test.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

end RFLX.Test;
