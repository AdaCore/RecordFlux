pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Index;
use type RFLX.RFLX_Types.Bytes_Ptr;

package RFLX.Test.Session_Allocator with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Always_Return)
is

   type Memory is
      record
         Slot_1 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 1023) := (others => 0);
         Slot_2 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 8191) := (others => 0);
      end record;

   subtype Slot_Ptr_Type_1024 is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type_1024 = null
       or else (Slot_Ptr_Type_1024'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type_1024'Last = RFLX_Types.Index'First + 1023);

   subtype Slot_Ptr_Type_8192 is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type_8192 = null
       or else (Slot_Ptr_Type_8192'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type_8192'Last = RFLX_Types.Index'First + 8191);

   type Slots is
      record
         Slot_Ptr_1 : Slot_Ptr_Type_1024;
         Slot_Ptr_2 : Slot_Ptr_Type_8192;
      end record;

   function Initialized (S : Slots) return Boolean is
     (S.Slot_Ptr_1 /= null
      and S.Slot_Ptr_2 /= null);

   function Uninitialized (S : Slots) return Boolean is
     (S.Slot_Ptr_1 = null
      and S.Slot_Ptr_2 = null);

   procedure Initialize (S : out Slots; M : Memory) with
     Post =>
       Initialized (S);

   procedure Finalize (S : in out Slots) with
     Post =>
       Uninitialized (S);

   function Global_Allocated (S : Slots) return Boolean is
     (S.Slot_Ptr_1 = null
      and S.Slot_Ptr_2 /= null);

end RFLX.Test.Session_Allocator;
