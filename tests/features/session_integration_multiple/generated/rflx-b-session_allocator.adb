pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.B.Session_Allocator with
  SPARK_Mode
is

   procedure Initialize (S : out Slots; M : Memory) with
     SPARK_Mode =>
       Off
   is
   begin
      S.Slot_Ptr_1 := M.Slot_1'Unrestricted_Access;
   end Initialize;

   procedure Finalize (S : in out Slots) with
     SPARK_Mode =>
       Off
   is
   begin
      S.Slot_Ptr_1 := null;
   end Finalize;

end RFLX.B.Session_Allocator;
