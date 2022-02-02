pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Test with
  SPARK_Mode
is

   type Result_Base is mod 2**2;

   type Result is (M_Valid, M_Invalid) with
     Size =>
       2;
   for Result use (M_Valid => 0, M_Invalid => 1);

   function Valid (Val : RFLX.Test.Result_Base) return Boolean is
     ((case Val is
          when 0 | 1 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.Test.Result) return RFLX.Test.Result_Base is
     ((case Enum is
          when M_Valid =>
             0,
          when M_Invalid =>
             1));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.Test.Result_Base) return RFLX.Test.Result is
     ((case Val is
          when 0 =>
             M_Valid,
          when 1 =>
             M_Invalid,
          when others =>
             RFLX.Test.Result'Last))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

end RFLX.Test;
