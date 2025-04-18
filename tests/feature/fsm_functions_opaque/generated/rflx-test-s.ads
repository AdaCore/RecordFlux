------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                          Copyright (C) AdaCore                           --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Streams);
pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.S_Environment;
with RFLX.RFLX_Types;

package RFLX.Test.S
with
  SPARK_Mode
is

   procedure Check_Size (State : in out RFLX.Test.S_Environment.State; Size : RFLX.Test.Size; Data : RFLX_Types.Bytes; RFLX_Result : out Boolean);

end RFLX.Test.S;
