------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                          Copyright (C) AdaCore                           --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.Test with
  SPARK_Mode
is

   type Result is (M_Valid, M_Invalid) with
     Size =>
       2;
   for Result use (M_Valid => 0, M_Invalid => 1);

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Result (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1);

   function To_Base_Integer (Enum : RFLX.Test.Result) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when M_Valid =>
             0,
          when M_Invalid =>
             1));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Test.Result is
     ((case Val is
          when 0 =>
             M_Valid,
          when 1 =>
             M_Invalid,
          when others =>
             RFLX.Test.Result'Last))
    with
     Pre =>
       Valid_Result (Val);

   pragma Warnings (On, "unreachable branch");

   type Length is range 0 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Length (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.Test.Length) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.Test.Length is
     (RFLX.Test.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

end RFLX.Test;