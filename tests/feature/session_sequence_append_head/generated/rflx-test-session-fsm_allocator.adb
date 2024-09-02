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

package body RFLX.Test.Session.FSM_Allocator with
  SPARK_Mode
is

   procedure Initialize (S : out Slots; M : Memory) with
     SPARK_Mode =>
       Off
   is
   begin
      S.Slot_Ptr_1 := M.Slot_1'Unrestricted_Access;
      S.Slot_Ptr_2 := M.Slot_2'Unrestricted_Access;
      S.Slot_Ptr_3 := M.Slot_3'Unrestricted_Access;
      S.Slot_Ptr_4 := M.Slot_4'Unrestricted_Access;
      S.Slot_Ptr_5 := M.Slot_5'Unrestricted_Access;
      S.Slot_Ptr_6 := M.Slot_6'Unrestricted_Access;
      S.Slot_Ptr_7 := M.Slot_7'Unrestricted_Access;
   end Initialize;

   procedure Finalize (S : in out Slots) with
     SPARK_Mode =>
       Off
   is
   begin
      S.Slot_Ptr_1 := null;
      S.Slot_Ptr_2 := null;
      S.Slot_Ptr_3 := null;
      S.Slot_Ptr_4 := null;
      S.Slot_Ptr_5 := null;
      S.Slot_Ptr_6 := null;
      S.Slot_Ptr_7 := null;
   end Finalize;

end RFLX.Test.Session.FSM_Allocator;