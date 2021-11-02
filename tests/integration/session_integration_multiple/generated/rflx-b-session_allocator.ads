pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Index;
use type RFLX.RFLX_Types.Bytes_Ptr;

package RFLX.B.Session_Allocator with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Elaborate_Body;

   subtype Slot_Ptr_Type_2048 is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type_2048 = null
       or else (Slot_Ptr_Type_2048'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type_2048'Last = RFLX_Types.Index'First + 2047);

   Slot_Ptr_1 : Slot_Ptr_Type_2048;

   function Initialized return Boolean is
     (Slot_Ptr_1 /= null);

   procedure Initialize with
     Post =>
       Initialized;

   function Global_Allocated return Boolean is
     (Slot_Ptr_1 = null);

end RFLX.B.Session_Allocator;
