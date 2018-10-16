with Types; use Types;

package Ethernet
  with SPARK_Mode
is

   type UINT48 is mod (2**48);
   function Convert_To_UINT48 is new Convert_To_Mod (UINT48);

   type UINT16 is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_UINT16 is new Convert_To_Int (UINT16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UINT48 return UINT48 is
      (UINT48'First)
     with
       Pre => False;

   function Unreachable_UINT16 return UINT16 is
      (UINT16'First)
     with
       Pre => False;

   function Unreachable return Boolean is
      (False)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Valid_Destination_0 (Buffer : Bytes) return Boolean is
      (Buffer'Length >= 6);

   function Destination_0 (Buffer : Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer (Buffer'First .. (Buffer'First + 5))))
     with
       Pre => Valid_Destination_0 (Buffer);

   function Valid_Destination (Buffer : Bytes) return Boolean is
      (Valid_Destination_0 (Buffer));

   function Destination (Buffer : Bytes) return UINT48 is
      ((if Valid_Destination_0 (Buffer) then Destination_0 (Buffer) else Unreachable_UINT48))
     with
       Pre => Valid_Destination (Buffer);

   function Valid_Source_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_Destination_0 (Buffer) and then Buffer'Length >= 12));

   function Source_0_0 (Buffer : Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer ((Buffer'First + 6) .. (Buffer'First + 11))))
     with
       Pre => Valid_Source_0_0 (Buffer);

   function Valid_Source (Buffer : Bytes) return Boolean is
      (Valid_Source_0_0 (Buffer));

   function Source (Buffer : Bytes) return UINT48 is
      ((if Valid_Source_0_0 (Buffer) then Source_0_0 (Buffer) else Unreachable_UINT48))
     with
       Pre => Valid_Source (Buffer);

   function Valid_TPID_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_Source_0_0 (Buffer) and then Buffer'Length >= 14));

   function TPID_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => Valid_TPID_0_0_0 (Buffer);

   function Valid_TPID (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (TPID_0_0_0 (Buffer) /= 33024 or TPID_0_0_0 (Buffer) = 33024)));

   function TPID (Buffer : Bytes) return UINT16 is
      ((if Valid_TPID_0_0_0 (Buffer) then TPID_0_0_0 (Buffer) else Unreachable_UINT16))
     with
       Pre => Valid_TPID (Buffer);

   function Valid_TCI_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (Buffer'Length >= 16 and then TPID_0_0_0 (Buffer) = 33024)));

   function TCI_0_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 14) .. (Buffer'First + 15))))
     with
       Pre => Valid_TCI_0_0_0_0 (Buffer);

   function Valid_TCI (Buffer : Bytes) return Boolean is
      (Valid_TCI_0_0_0_0 (Buffer));

   function TCI (Buffer : Bytes) return UINT16 is
      ((if Valid_TCI_0_0_0_0 (Buffer) then TCI_0_0_0_0 (Buffer) else Unreachable_UINT16))
     with
       Pre => Valid_TCI (Buffer);

   function Valid_EtherType_0_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_TCI_0_0_0_0 (Buffer) and then Buffer'Length >= 18));

   function EtherType_0_0_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 16) .. (Buffer'First + 17))))
     with
       Pre => Valid_EtherType_0_0_0_0_0 (Buffer);

   function Valid_EtherType_0_0_0_1 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (Buffer'Length >= 14 and then TPID_0_0_0 (Buffer) /= 33024)));

   function EtherType_0_0_0_1 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => Valid_EtherType_0_0_0_1 (Buffer);

   function Valid_EtherType (Buffer : Bytes) return Boolean is
      (((Valid_EtherType_0_0_0_1 (Buffer) and then (EtherType_0_0_0_1 (Buffer) >= 1536 or EtherType_0_0_0_1 (Buffer) <= 1500)) or (Valid_EtherType_0_0_0_0_0 (Buffer) and then (EtherType_0_0_0_0_0 (Buffer) >= 1536 or EtherType_0_0_0_0_0 (Buffer) <= 1500))));

   function EtherType (Buffer : Bytes) return UINT16 is
      ((if Valid_EtherType_0_0_0_0_0 (Buffer) then EtherType_0_0_0_0_0 (Buffer) elsif Valid_EtherType_0_0_0_1 (Buffer) then EtherType_0_0_0_1 (Buffer) else Unreachable_UINT16))
     with
       Pre => Valid_EtherType (Buffer);

   function Valid_Payload_0_0_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_0_0 (Buffer) and then (Buffer'Length >= (Natural (EtherType_0_0_0_0_0 (Buffer)) + 18) and then EtherType_0_0_0_0_0 (Buffer) <= 1500)));

   function Payload_0_0_0_0_0_0_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Valid_Payload_0_0_0_0_0_0 (Buffer) and then Buffer'First <= (Natural'Last + (-18)));

   function Payload_0_0_0_0_0_0_Last (Buffer : Bytes) return Natural is
      ((Natural (Convert_To_UINT16 (Buffer ((Buffer'First + 16) .. (Buffer'First + 17)))) + Buffer'First + 17))
     with
       Pre => Valid_Payload_0_0_0_0_0_0 (Buffer);

   function Valid_Payload_0_0_0_0_0_1 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_0_0 (Buffer) and then (Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then EtherType_0_0_0_0_0 (Buffer) >= 1536)));

   function Payload_0_0_0_0_0_1_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Valid_Payload_0_0_0_0_0_1 (Buffer) and then Buffer'First <= (Natural'Last + (-18)));

   function Payload_0_0_0_0_0_1_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => Valid_Payload_0_0_0_0_0_1 (Buffer);

   function Valid_Payload_0_0_0_1_0 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_1 (Buffer) and then (Buffer'Length >= (Natural (EtherType_0_0_0_1 (Buffer)) + 14) and then EtherType_0_0_0_1 (Buffer) <= 1500)));

   function Payload_0_0_0_1_0_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Valid_Payload_0_0_0_1_0 (Buffer) and then Buffer'First <= (Natural'Last + (-14)));

   function Payload_0_0_0_1_0_Last (Buffer : Bytes) return Natural is
      ((Natural (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13)))) + Buffer'First + 13))
     with
       Pre => Valid_Payload_0_0_0_1_0 (Buffer);

   function Valid_Payload_0_0_0_1_1 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_1 (Buffer) and then (Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then EtherType_0_0_0_1 (Buffer) >= 1536)));

   function Payload_0_0_0_1_1_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Valid_Payload_0_0_0_1_1 (Buffer) and then Buffer'First <= (Natural'Last + (-14)));

   function Payload_0_0_0_1_1_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => Valid_Payload_0_0_0_1_1 (Buffer);

   function Valid_Payload (Buffer : Bytes) return Boolean is
      (((((Valid_Payload_0_0_0_1_1 (Buffer) and then ((Buffer'Last + (-Buffer'First) + (-13)) >= 46 and then (Buffer'Last + (-Buffer'First) + (-13)) <= 1500)) or (Valid_Payload_0_0_0_0_0_0 (Buffer) and then (EtherType_0_0_0_0_0 (Buffer) >= 46 and then EtherType_0_0_0_0_0 (Buffer) <= 1500))) or (Valid_Payload_0_0_0_0_0_1 (Buffer) and then ((Buffer'Last + (-Buffer'First) + (-17)) >= 46 and then (Buffer'Last + (-Buffer'First) + (-17)) <= 1500))) or (Valid_Payload_0_0_0_1_0 (Buffer) and then (EtherType_0_0_0_1 (Buffer) >= 46 and then EtherType_0_0_0_1 (Buffer) <= 1500))));

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural)
     with
       Pre => Valid_Payload (Buffer),
       Post => (if Valid_Payload_0_0_0_0_0_0 (Buffer) then (First = Payload_0_0_0_0_0_0_First (Buffer) and then Last = Payload_0_0_0_0_0_0_Last (Buffer)) elsif Valid_Payload_0_0_0_0_0_1 (Buffer) then (First = Payload_0_0_0_0_0_1_First (Buffer) and then Last = Payload_0_0_0_0_0_1_Last (Buffer)) elsif Valid_Payload_0_0_0_1_0 (Buffer) then (First = Payload_0_0_0_1_0_First (Buffer) and then Last = Payload_0_0_0_1_0_Last (Buffer)) elsif Valid_Payload_0_0_0_1_1 (Buffer) then (First = Payload_0_0_0_1_1_First (Buffer) and then Last = Payload_0_0_0_1_1_Last (Buffer)) else Unreachable);

   function Is_Valid (Buffer : Bytes) return Boolean is
      (Valid_Payload (Buffer));

end Ethernet;
