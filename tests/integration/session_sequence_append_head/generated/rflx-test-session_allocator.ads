pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Index;
use type RFLX.RFLX_Types.Bytes_Ptr;

package RFLX.Test.Session_Allocator with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Elaborate_Body;

   subtype Slot_Ptr_Type is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type = null
       or else (Slot_Ptr_Type'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type'Last = RFLX_Types.Index'First + 4095);

   Slot_Ptr_1 : Slot_Ptr_Type;

   Slot_Ptr_2 : Slot_Ptr_Type;

   Slot_Ptr_3 : Slot_Ptr_Type;

   Slot_Ptr_4 : Slot_Ptr_Type;

   Slot_Ptr_5 : Slot_Ptr_Type;

   function Initialized return Boolean is
     (Slot_Ptr_1 /= null
      and Slot_Ptr_2 /= null
      and Slot_Ptr_3 /= null
      and Slot_Ptr_4 /= null
      and Slot_Ptr_5 /= null);

   procedure Initialize with
     Post =>
       Initialized;

   function Global_Allocated return Boolean is
     (Slot_Ptr_1 = null
      and Slot_Ptr_2 = null
      and Slot_Ptr_3 /= null
      and Slot_Ptr_4 /= null
      and Slot_Ptr_5 /= null);

end RFLX.Test.Session_Allocator;
