package Ethernet.Frame
  with SPARK_Mode
is

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UINT48 return UINT48 is
      (UINT48'First)
     with
       Pre => False;

   function Unreachable_UINT16 return UINT16 is
      (UINT16'First)
     with
       Pre => False;

   function Unreachable_Natural return Natural is
      (Natural'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Is_Contained (Buffer : Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Initialize (Buffer : Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_Destination_0 (Buffer : Bytes) return Boolean is
      (Buffer'Length >= 6)
     with
       Pre => Is_Contained (Buffer);

   function Destination_0 (Buffer : Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer (Buffer'First .. (Buffer'First + 5))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination_0 (Buffer));

   function Valid_Destination (Buffer : Bytes) return Boolean is
      (Valid_Destination_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Destination (Buffer : Bytes) return UINT48 is
      ((if Valid_Destination_0 (Buffer) then Destination_0 (Buffer) else Unreachable_UINT48))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination (Buffer));

   function Valid_Source_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_Destination_0 (Buffer) and then Buffer'Length >= 12))
     with
       Pre => Is_Contained (Buffer);

   function Source_0_0 (Buffer : Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer ((Buffer'First + 6) .. (Buffer'First + 11))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_0_0 (Buffer));

   function Valid_Source (Buffer : Bytes) return Boolean is
      (Valid_Source_0_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Source (Buffer : Bytes) return UINT48 is
      ((if Valid_Source_0_0 (Buffer) then Source_0_0 (Buffer) else Unreachable_UINT48))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source (Buffer));

   function Valid_TPID_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_Source_0_0 (Buffer) and then Buffer'Length >= 14))
     with
       Pre => Is_Contained (Buffer);

   function TPID_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID_0_0_0 (Buffer));

   function Valid_TPID (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (TPID_0_0_0 (Buffer) /= 33024 or TPID_0_0_0 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function TPID (Buffer : Bytes) return UINT16 is
      ((if Valid_TPID_0_0_0 (Buffer) then TPID_0_0_0 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID (Buffer));

   function Valid_TCI_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (Buffer'Length >= 16 and then TPID_0_0_0 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function TCI_0_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 14) .. (Buffer'First + 15))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI_0_0_0_0 (Buffer));

   function Valid_TCI (Buffer : Bytes) return Boolean is
      (Valid_TCI_0_0_0_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function TCI (Buffer : Bytes) return UINT16 is
      ((if Valid_TCI_0_0_0_0 (Buffer) then TCI_0_0_0_0 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI (Buffer));

   function Valid_EtherType_0_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_TCI_0_0_0_0 (Buffer) and then Buffer'Length >= 18))
     with
       Pre => Is_Contained (Buffer);

   function EtherType_0_0_0_0_0 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 16) .. (Buffer'First + 17))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_0_0_0_0_0 (Buffer));

   function Valid_EtherType_0_0_0_1 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_0_0_0 (Buffer) and then (Buffer'Length >= 14 and then TPID_0_0_0 (Buffer) /= 33024)))
     with
       Pre => Is_Contained (Buffer);

   function EtherType_0_0_0_1 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_0_0_0_1 (Buffer));

   function Valid_EtherType (Buffer : Bytes) return Boolean is
      (((Valid_EtherType_0_0_0_1 (Buffer) and then (EtherType_0_0_0_1 (Buffer) >= 1536 or EtherType_0_0_0_1 (Buffer) <= 1500)) or (Valid_EtherType_0_0_0_0_0 (Buffer) and then (EtherType_0_0_0_0_0 (Buffer) >= 1536 or EtherType_0_0_0_0_0 (Buffer) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function EtherType (Buffer : Bytes) return UINT16 is
      ((if Valid_EtherType_0_0_0_0_0 (Buffer) then EtherType_0_0_0_0_0 (Buffer) elsif Valid_EtherType_0_0_0_1 (Buffer) then EtherType_0_0_0_1 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType (Buffer));

   function Valid_Payload_0_0_0_0_0_0 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_0_0 (Buffer) and then (Buffer'Length >= (Natural (EtherType_0_0_0_0_0 (Buffer)) + 18) and then EtherType_0_0_0_0_0 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_0_0_0_0_0_0_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then (Valid_Payload_0_0_0_0_0_0 (Buffer) and then Buffer'First <= (Natural'Last + (-18))));

   function Payload_0_0_0_0_0_0_Last (Buffer : Bytes) return Natural is
      ((Natural (EtherType_0_0_0_0_0 (Buffer)) + Buffer'First + 17))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0_0_0_0_0_0 (Buffer));

   function Valid_Payload_0_0_0_0_0_1 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_0_0 (Buffer) and then (Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then EtherType_0_0_0_0_0 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_0_0_0_0_0_1_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then (Valid_Payload_0_0_0_0_0_1 (Buffer) and then Buffer'First <= (Natural'Last + (-18))));

   function Payload_0_0_0_0_0_1_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0_0_0_0_0_1 (Buffer));

   function Valid_Payload_0_0_0_1_0 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_1 (Buffer) and then (Buffer'Length >= (Natural (EtherType_0_0_0_1 (Buffer)) + 14) and then EtherType_0_0_0_1 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_0_0_0_1_0_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then (Valid_Payload_0_0_0_1_0 (Buffer) and then Buffer'First <= (Natural'Last + (-14))));

   function Payload_0_0_0_1_0_Last (Buffer : Bytes) return Natural is
      ((Natural (EtherType_0_0_0_1 (Buffer)) + Buffer'First + 13))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0_0_0_1_0 (Buffer));

   function Valid_Payload_0_0_0_1_1 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0_0_0_1 (Buffer) and then (Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then EtherType_0_0_0_1 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_0_0_0_1_1_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then (Valid_Payload_0_0_0_1_1 (Buffer) and then Buffer'First <= (Natural'Last + (-14))));

   function Payload_0_0_0_1_1_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0_0_0_1_1 (Buffer));

   function Valid_Payload (Buffer : Bytes) return Boolean is
      (((((Valid_Payload_0_0_0_1_1 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500)) or (Valid_Payload_0_0_0_0_0_0 (Buffer) and then (EtherType_0_0_0_0_0 (Buffer) >= 46 and then EtherType_0_0_0_0_0 (Buffer) <= 1500))) or (Valid_Payload_0_0_0_0_0_1 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500))) or (Valid_Payload_0_0_0_1_0 (Buffer) and then (EtherType_0_0_0_1 (Buffer) >= 46 and then EtherType_0_0_0_1 (Buffer) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function Payload_First (Buffer : Bytes) return Natural is
      ((if Valid_Payload_0_0_0_0_0_0 (Buffer) then Payload_0_0_0_0_0_0_First (Buffer) elsif Valid_Payload_0_0_0_0_0_1 (Buffer) then Payload_0_0_0_0_0_1_First (Buffer) elsif Valid_Payload_0_0_0_1_0 (Buffer) then Payload_0_0_0_1_0_First (Buffer) elsif Valid_Payload_0_0_0_1_1 (Buffer) then Payload_0_0_0_1_1_First (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Payload_Last (Buffer : Bytes) return Natural is
      ((if Valid_Payload_0_0_0_0_0_0 (Buffer) then Payload_0_0_0_0_0_0_Last (Buffer) elsif Valid_Payload_0_0_0_0_0_1 (Buffer) then Payload_0_0_0_0_0_1_Last (Buffer) elsif Valid_Payload_0_0_0_1_0 (Buffer) then Payload_0_0_0_1_0_Last (Buffer) elsif Valid_Payload_0_0_0_1_1 (Buffer) then Payload_0_0_0_1_1_Last (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
       Post => (First = Payload_First (Buffer) and then Last = Payload_Last (Buffer));

   function Is_Valid (Buffer : Bytes) return Boolean is
      (Valid_Payload (Buffer))
     with
       Pre => Is_Contained (Buffer);

end Ethernet.Frame;
