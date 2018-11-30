package Ethernet.Frame
  with SPARK_Mode
is

   function Is_Contained (Buffer : Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Initialize (Buffer : Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_Destination_0 (Buffer : Bytes) return Boolean is
      ((Buffer'Length >= 6 and then Buffer'First <= (Natural'Last / 2)))
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

   function Valid_Source_00 (Buffer : Bytes) return Boolean is
      ((Valid_Destination_0 (Buffer) and then (Buffer'Length >= 12 and then Buffer'First <= (Natural'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Source_00 (Buffer : Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer ((Buffer'First + 6) .. (Buffer'First + 11))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_00 (Buffer));

   function Valid_Source (Buffer : Bytes) return Boolean is
      (Valid_Source_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Source (Buffer : Bytes) return UINT48 is
      ((if Valid_Source_00 (Buffer) then Source_00 (Buffer) else Unreachable_UINT48))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source (Buffer));

   function Valid_TPID_000 (Buffer : Bytes) return Boolean is
      ((Valid_Source_00 (Buffer) and then (Buffer'Length >= 14 and then Buffer'First <= (Natural'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function TPID_000 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID_000 (Buffer));

   function Valid_TPID (Buffer : Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then (TPID_000 (Buffer) /= 33024 or TPID_000 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function TPID (Buffer : Bytes) return UINT16 is
      ((if Valid_TPID_000 (Buffer) then TPID_000 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID (Buffer));

   function Valid_TCI_0000 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then ((Buffer'Length >= 16 and then Buffer'First <= (Natural'Last / 2)) and then TPID_000 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function TCI_0000 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 14) .. (Buffer'First + 15))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI_0000 (Buffer));

   function Valid_TCI (Buffer : Bytes) return Boolean is
      (Valid_TCI_0000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function TCI (Buffer : Bytes) return UINT16 is
      ((if Valid_TCI_0000 (Buffer) then TCI_0000 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI (Buffer));

   function Valid_EtherType_00000 (Buffer : Bytes) return Boolean is
      ((Valid_TCI_0000 (Buffer) and then (Buffer'Length >= 18 and then Buffer'First <= (Natural'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function EtherType_00000 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 16) .. (Buffer'First + 17))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_00000 (Buffer));

   function Valid_EtherType_0001 (Buffer : Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then ((Buffer'Length >= 14 and then Buffer'First <= (Natural'Last / 2)) and then TPID_000 (Buffer) /= 33024)))
     with
       Pre => Is_Contained (Buffer);

   function EtherType_0001 (Buffer : Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13))))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_0001 (Buffer));

   function Valid_EtherType (Buffer : Bytes) return Boolean is
      (((Valid_EtherType_0001 (Buffer) and then (EtherType_0001 (Buffer) >= 1536 or EtherType_0001 (Buffer) <= 1500)) or (Valid_EtherType_00000 (Buffer) and then (EtherType_00000 (Buffer) >= 1536 or EtherType_00000 (Buffer) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function EtherType (Buffer : Bytes) return UINT16 is
      ((if Valid_EtherType_00000 (Buffer) then EtherType_00000 (Buffer) elsif Valid_EtherType_0001 (Buffer) then EtherType_0001 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType (Buffer));

   function Valid_Payload_000000 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_00000 (Buffer) and then ((Buffer'Length >= (Natural (EtherType_00000 (Buffer)) + 18) and then Buffer'First <= (Natural'Last / 2)) and then EtherType_00000 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_000000_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000000 (Buffer));

   function Payload_000000_Last (Buffer : Bytes) return Natural is
      ((Natural (EtherType_00000 (Buffer)) + Buffer'First + 17))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000000 (Buffer));

   function Valid_Payload_000001 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_00000 (Buffer) and then ((Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then Buffer'First <= (Natural'Last / 2)) and then EtherType_00000 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_000001_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000001 (Buffer));

   function Payload_000001_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000001 (Buffer));

   function Valid_Payload_00010 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0001 (Buffer) and then ((Buffer'Length >= (Natural (EtherType_0001 (Buffer)) + 14) and then Buffer'First <= (Natural'Last / 2)) and then EtherType_0001 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_00010_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00010 (Buffer));

   function Payload_00010_Last (Buffer : Bytes) return Natural is
      ((Natural (EtherType_0001 (Buffer)) + Buffer'First + 13))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00010 (Buffer));

   function Valid_Payload_00011 (Buffer : Bytes) return Boolean is
      ((Valid_EtherType_0001 (Buffer) and then ((Buffer'Length >= (Buffer'Last + (-Buffer'First) + 1) and then Buffer'First <= (Natural'Last / 2)) and then EtherType_0001 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Payload_00011_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00011 (Buffer));

   function Payload_00011_Last (Buffer : Bytes) return Natural is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00011 (Buffer));

   function Valid_Payload (Buffer : Bytes) return Boolean is
      (((((Valid_Payload_00011 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500)) or (Valid_Payload_000000 (Buffer) and then (EtherType_00000 (Buffer) >= 46 and then EtherType_00000 (Buffer) <= 1500))) or (Valid_Payload_000001 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500))) or (Valid_Payload_00010 (Buffer) and then (EtherType_0001 (Buffer) >= 46 and then EtherType_0001 (Buffer) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function Payload_First (Buffer : Bytes) return Natural is
      ((if Valid_Payload_000000 (Buffer) then Payload_000000_First (Buffer) elsif Valid_Payload_000001 (Buffer) then Payload_000001_First (Buffer) elsif Valid_Payload_00010 (Buffer) then Payload_00010_First (Buffer) elsif Valid_Payload_00011 (Buffer) then Payload_00011_First (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Payload_Last (Buffer : Bytes) return Natural is
      ((if Valid_Payload_000000 (Buffer) then Payload_000000_Last (Buffer) elsif Valid_Payload_000001 (Buffer) then Payload_000001_Last (Buffer) elsif Valid_Payload_00010 (Buffer) then Payload_00010_Last (Buffer) elsif Valid_Payload_00011 (Buffer) then Payload_00011_Last (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
       Post => (First = Payload_First (Buffer) and then Last = Payload_Last (Buffer));

   function Is_Valid (Buffer : Bytes) return Boolean is
      (((((Valid_Payload_000000 (Buffer) and then (EtherType_00000 (Buffer) >= 46 and then EtherType_00000 (Buffer) <= 1500)) or (Valid_Payload_000001 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500))) or (Valid_Payload_00010 (Buffer) and then (EtherType_0001 (Buffer) >= 46 and then EtherType_0001 (Buffer) <= 1500))) or (Valid_Payload_00011 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

end Ethernet.Frame;
