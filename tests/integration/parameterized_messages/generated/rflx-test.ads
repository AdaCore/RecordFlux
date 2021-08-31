pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Test with
  SPARK_Mode
is

   type Length_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Length is range 1 .. 2**14 - 1 with
     Size =>
       16;

   function Valid (Val : RFLX.Test.Length_Base) return Boolean is
     (Val >= 1
      and Val <= 16383);

   function To_Base (Val : RFLX.Test.Length) return RFLX.Test.Length_Base is
     (RFLX.Test.Length_Base (Val));

   function To_Actual (Val : RFLX.Test.Length_Base) return RFLX.Test.Length is
     (RFLX.Test.Length (Val))
    with
     Pre =>
       Valid (Val);

end RFLX.Test;
