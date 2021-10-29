pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session_Allocator with
  SPARK_Mode
is

   Slot_1 : aliased RFLX_Types.Bytes := (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);

   Slot_2 : aliased RFLX_Types.Bytes := (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);

   procedure Initialize with
     SPARK_Mode =>
       Off
   is
   begin
      Slot_Ptr_1 := Slot_1'Unrestricted_Access;
      Slot_Ptr_2 := Slot_2'Unrestricted_Access;
   end Initialize;

end RFLX.Test.Session_Allocator;
