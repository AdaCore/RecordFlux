pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
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
         Slot_1 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_2 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_3 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_4 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_5 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 8095) := (others => 0);
         Slot_6 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 8095) := (others => 0);
         Slot_7 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 8095) := (others => 0);
         Slot_8 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_9 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_10 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_11 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_12 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_13 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         Slot_14 : aliased RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
      end record;

   subtype Slot_Ptr_Type_4096 is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type_4096 = null
       or else (Slot_Ptr_Type_4096'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type_4096'Last = RFLX_Types.Index'First + 4095);

   subtype Slot_Ptr_Type_8096 is RFLX_Types.Bytes_Ptr with
     Dynamic_Predicate =>
       Slot_Ptr_Type_8096 = null
       or else (Slot_Ptr_Type_8096'First = RFLX_Types.Index'First
                and then Slot_Ptr_Type_8096'Last = RFLX_Types.Index'First + 8095);

   type Slots is
      record
         Slot_Ptr_1 : Slot_Ptr_Type_4096;
         Slot_Ptr_2 : Slot_Ptr_Type_4096;
         Slot_Ptr_3 : Slot_Ptr_Type_4096;
         Slot_Ptr_4 : Slot_Ptr_Type_4096;
         Slot_Ptr_5 : Slot_Ptr_Type_8096;
         Slot_Ptr_6 : Slot_Ptr_Type_8096;
         Slot_Ptr_7 : Slot_Ptr_Type_8096;
         Slot_Ptr_8 : Slot_Ptr_Type_4096;
         Slot_Ptr_9 : Slot_Ptr_Type_4096;
         Slot_Ptr_10 : Slot_Ptr_Type_4096;
         Slot_Ptr_11 : Slot_Ptr_Type_4096;
         Slot_Ptr_12 : Slot_Ptr_Type_4096;
         Slot_Ptr_13 : Slot_Ptr_Type_4096;
         Slot_Ptr_14 : Slot_Ptr_Type_4096;
      end record;

   function Initialized (S : Slots) return Boolean is
     (S.Slot_Ptr_1 /= null
      and S.Slot_Ptr_2 /= null
      and S.Slot_Ptr_3 /= null
      and S.Slot_Ptr_4 /= null
      and S.Slot_Ptr_5 /= null
      and S.Slot_Ptr_6 /= null
      and S.Slot_Ptr_7 /= null
      and S.Slot_Ptr_8 /= null
      and S.Slot_Ptr_9 /= null
      and S.Slot_Ptr_10 /= null
      and S.Slot_Ptr_11 /= null
      and S.Slot_Ptr_12 /= null
      and S.Slot_Ptr_13 /= null
      and S.Slot_Ptr_14 /= null);

   function Uninitialized (S : Slots) return Boolean is
     (S.Slot_Ptr_1 = null
      and S.Slot_Ptr_2 = null
      and S.Slot_Ptr_3 = null
      and S.Slot_Ptr_4 = null
      and S.Slot_Ptr_5 = null
      and S.Slot_Ptr_6 = null
      and S.Slot_Ptr_7 = null
      and S.Slot_Ptr_8 = null
      and S.Slot_Ptr_9 = null
      and S.Slot_Ptr_10 = null
      and S.Slot_Ptr_11 = null
      and S.Slot_Ptr_12 = null
      and S.Slot_Ptr_13 = null
      and S.Slot_Ptr_14 = null);

   procedure Initialize (S : out Slots; M : Memory) with
     Post =>
       Initialized (S);

   procedure Finalize (S : in out Slots) with
     Post =>
       Uninitialized (S);

   function Global_Allocated (S : Slots) return Boolean is
     (S.Slot_Ptr_1 = null
      and S.Slot_Ptr_2 = null
      and S.Slot_Ptr_3 = null
      and S.Slot_Ptr_4 = null
      and S.Slot_Ptr_5 /= null
      and S.Slot_Ptr_6 /= null
      and S.Slot_Ptr_7 /= null
      and S.Slot_Ptr_8 /= null
      and S.Slot_Ptr_9 /= null
      and S.Slot_Ptr_10 /= null
      and S.Slot_Ptr_11 /= null
      and S.Slot_Ptr_12 /= null
      and S.Slot_Ptr_13 /= null
      and S.Slot_Ptr_14 /= null);

end RFLX.Test.Session_Allocator;
