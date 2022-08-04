pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session_Allocator with
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
      S.Slot_Ptr_8 := M.Slot_8'Unrestricted_Access;
      S.Slot_Ptr_9 := M.Slot_9'Unrestricted_Access;
      S.Slot_Ptr_10 := M.Slot_10'Unrestricted_Access;
      S.Slot_Ptr_11 := M.Slot_11'Unrestricted_Access;
      S.Slot_Ptr_12 := M.Slot_12'Unrestricted_Access;
      S.Slot_Ptr_13 := M.Slot_13'Unrestricted_Access;
      S.Slot_Ptr_14 := M.Slot_14'Unrestricted_Access;
      S.Slot_Ptr_15 := M.Slot_15'Unrestricted_Access;
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
      S.Slot_Ptr_8 := null;
      S.Slot_Ptr_9 := null;
      S.Slot_Ptr_10 := null;
      S.Slot_Ptr_11 := null;
      S.Slot_Ptr_12 := null;
      S.Slot_Ptr_13 := null;
      S.Slot_Ptr_14 := null;
      S.Slot_Ptr_15 := null;
   end Finalize;

end RFLX.Test.Session_Allocator;
