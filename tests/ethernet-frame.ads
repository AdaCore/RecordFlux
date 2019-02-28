package Ethernet.Frame
  with SPARK_Mode
is

   function Is_Contained (Buffer : Types.Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Label (Buffer : Types.Bytes)
     with
       Ghost,
       Post => Is_Contained (Buffer);

   function Valid_Destination_0 (Buffer : Types.Bytes) return Boolean is
      ((Buffer'Length >= 6 and then Buffer'First <= (Types.Index_Type'Last / 2)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination_0 (Buffer : Types.Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer (Buffer'First .. (Buffer'First + 5)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination_0 (Buffer));

   function Valid_Destination (Buffer : Types.Bytes) return Boolean is
      (Valid_Destination_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination (Buffer : Types.Bytes) return UINT48 is
      ((if Valid_Destination_0 (Buffer) then Get_Destination_0 (Buffer) else Unreachable_UINT48))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination (Buffer));

   function Valid_Source_00 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Destination_0 (Buffer) and then (Buffer'Length >= 12 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source_00 (Buffer : Types.Bytes) return UINT48 is
      (Convert_To_UINT48 (Buffer ((Buffer'First + 6) .. (Buffer'First + 11)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_00 (Buffer));

   function Valid_Source (Buffer : Types.Bytes) return Boolean is
      (Valid_Source_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source (Buffer : Types.Bytes) return UINT48 is
      ((if Valid_Source_00 (Buffer) then Get_Source_00 (Buffer) else Unreachable_UINT48))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source (Buffer));

   function Valid_TPID_000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Source_00 (Buffer) and then (Buffer'Length >= 14 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_TPID_000 (Buffer : Types.Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID_000 (Buffer));

   function Valid_TPID (Buffer : Types.Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then (Get_TPID_000 (Buffer) /= 33024 or Get_TPID_000 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function Get_TPID (Buffer : Types.Bytes) return UINT16 is
      ((if Valid_TPID_000 (Buffer) then Get_TPID_000 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TPID (Buffer));

   function Valid_TCI_0000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then ((Buffer'Length >= 16 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_TPID_000 (Buffer) = 33024)))
     with
       Pre => Is_Contained (Buffer);

   function Get_TCI_0000 (Buffer : Types.Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 14) .. (Buffer'First + 15)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI_0000 (Buffer));

   function Valid_TCI (Buffer : Types.Bytes) return Boolean is
      (Valid_TCI_0000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_TCI (Buffer : Types.Bytes) return UINT16 is
      ((if Valid_TCI_0000 (Buffer) then Get_TCI_0000 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TCI (Buffer));

   function Valid_EtherType_00000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_TCI_0000 (Buffer) and then (Buffer'Length >= 18 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_EtherType_00000 (Buffer : Types.Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 16) .. (Buffer'First + 17)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_00000 (Buffer));

   function Valid_EtherType_0001 (Buffer : Types.Bytes) return Boolean is
      ((Valid_TPID_000 (Buffer) and then ((Buffer'Length >= 14 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_TPID_000 (Buffer) /= 33024)))
     with
       Pre => Is_Contained (Buffer);

   function Get_EtherType_0001 (Buffer : Types.Bytes) return UINT16 is
      (Convert_To_UINT16 (Buffer ((Buffer'First + 12) .. (Buffer'First + 13)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType_0001 (Buffer));

   function Valid_EtherType (Buffer : Types.Bytes) return Boolean is
      (((Valid_EtherType_0001 (Buffer) and then (Get_EtherType_0001 (Buffer) >= 1536 or Get_EtherType_0001 (Buffer) <= 1500)) or (Valid_EtherType_00000 (Buffer) and then (Get_EtherType_00000 (Buffer) >= 1536 or Get_EtherType_00000 (Buffer) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function Get_EtherType (Buffer : Types.Bytes) return UINT16 is
      ((if Valid_EtherType_00000 (Buffer) then Get_EtherType_00000 (Buffer) elsif Valid_EtherType_0001 (Buffer) then Get_EtherType_0001 (Buffer) else Unreachable_UINT16))
     with
       Pre => (Is_Contained (Buffer) and then Valid_EtherType (Buffer));

   function Valid_Payload_000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_EtherType_00000 (Buffer) and then ((Buffer'Length >= (Types.Length_Type (Get_EtherType_00000 (Buffer)) + 18) and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_EtherType_00000 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_000000_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000000 (Buffer));

   function Get_Payload_000000_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Types.Length_Type (Get_EtherType_00000 (Buffer)) + Buffer'First + 17))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000000 (Buffer));

   function Valid_Payload_000001 (Buffer : Types.Bytes) return Boolean is
      ((Valid_EtherType_00000 (Buffer) and then (Buffer'First <= (Types.Index_Type'Last / 2) and then Get_EtherType_00000 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_000001_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 18))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000001 (Buffer));

   function Get_Payload_000001_Last (Buffer : Types.Bytes) return Types.Index_Type is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_000001 (Buffer));

   function Valid_Payload_00010 (Buffer : Types.Bytes) return Boolean is
      ((Valid_EtherType_0001 (Buffer) and then ((Buffer'Length >= (Types.Length_Type (Get_EtherType_0001 (Buffer)) + 14) and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_EtherType_0001 (Buffer) <= 1500)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_00010_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00010 (Buffer));

   function Get_Payload_00010_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Types.Length_Type (Get_EtherType_0001 (Buffer)) + Buffer'First + 13))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00010 (Buffer));

   function Valid_Payload_00011 (Buffer : Types.Bytes) return Boolean is
      ((Valid_EtherType_0001 (Buffer) and then (Buffer'First <= (Types.Index_Type'Last / 2) and then Get_EtherType_0001 (Buffer) >= 1536)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_00011_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 14))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00011 (Buffer));

   function Get_Payload_00011_Last (Buffer : Types.Bytes) return Types.Index_Type is
      (Buffer'Last)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00011 (Buffer));

   function Valid_Payload (Buffer : Types.Bytes) return Boolean is
      (((((Valid_Payload_00011 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500)) or (Valid_Payload_000000 (Buffer) and then (Types.Length_Type (Get_EtherType_00000 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_00000 (Buffer)) <= 1500))) or (Valid_Payload_000001 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500))) or (Valid_Payload_00010 (Buffer) and then (Types.Length_Type (Get_EtherType_0001 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_0001 (Buffer)) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_000000 (Buffer) then Get_Payload_000000_First (Buffer) elsif Valid_Payload_000001 (Buffer) then Get_Payload_000001_First (Buffer) elsif Valid_Payload_00010 (Buffer) then Get_Payload_00010_First (Buffer) elsif Valid_Payload_00011 (Buffer) then Get_Payload_00011_First (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Get_Payload_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_000000 (Buffer) then Get_Payload_000000_Last (Buffer) elsif Valid_Payload_000001 (Buffer) then Get_Payload_000001_Last (Buffer) elsif Valid_Payload_00010 (Buffer) then Get_Payload_00010_Last (Buffer) elsif Valid_Payload_00011 (Buffer) then Get_Payload_00011_Last (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Get_Payload (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
       Post => (First = Get_Payload_First (Buffer) and then Last = Get_Payload_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
      (((((Valid_Payload_000000 (Buffer) and then (Types.Length_Type (Get_EtherType_00000 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_00000 (Buffer)) <= 1500)) or (Valid_Payload_000001 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500))) or (Valid_Payload_00010 (Buffer) and then (Types.Length_Type (Get_EtherType_0001 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_0001 (Buffer)) <= 1500))) or (Valid_Payload_00011 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500))))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
      ((if (Valid_Payload_000000 (Buffer) and then (Types.Length_Type (Get_EtherType_00000 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_00000 (Buffer)) <= 1500)) then (Types.Length_Type (Get_EtherType_00000 (Buffer)) + 18) elsif (Valid_Payload_000001 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-143) / 8)) <= 1500)) then (Buffer'Last + (-Buffer'First)) elsif (Valid_Payload_00010 (Buffer) and then (Types.Length_Type (Get_EtherType_0001 (Buffer)) >= 46 and then Types.Length_Type (Get_EtherType_0001 (Buffer)) <= 1500)) then (Types.Length_Type (Get_EtherType_0001 (Buffer)) + 14) elsif (Valid_Payload_00011 (Buffer) and then ((Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) >= 46 and then (Buffer'Last + ((-Buffer'First) / 8) + ((-111) / 8)) <= 1500)) then (Buffer'Last + (-Buffer'First)) else Unreachable_Types_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end Ethernet.Frame;
