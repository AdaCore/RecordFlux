package UDP.Datagram
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

   function Valid_Source_Port_0 (Buffer : Types.Bytes) return Boolean is
      ((Buffer'Length >= 2 and then Buffer'First <= (Types.Index_Type'Last / 2)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source_Port_0 (Buffer : Types.Bytes) return Port_Type is
      (Convert_To_Port_Type (Buffer (Buffer'First .. (Buffer'First + 1)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_Port_0 (Buffer));

   function Valid_Source_Port (Buffer : Types.Bytes) return Boolean is
      (Valid_Source_Port_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source_Port (Buffer : Types.Bytes) return Port_Type is
      ((if Valid_Source_Port_0 (Buffer) then Get_Source_Port_0 (Buffer) else Unreachable_Port_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_Port (Buffer));

   function Valid_Destination_Port_00 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Source_Port_0 (Buffer) and then (Buffer'Length >= 4 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination_Port_00 (Buffer : Types.Bytes) return Port_Type is
      (Convert_To_Port_Type (Buffer ((Buffer'First + 2) .. (Buffer'First + 3)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination_Port_00 (Buffer));

   function Valid_Destination_Port (Buffer : Types.Bytes) return Boolean is
      (Valid_Destination_Port_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination_Port (Buffer : Types.Bytes) return Port_Type is
      ((if Valid_Destination_Port_00 (Buffer) then Get_Destination_Port_00 (Buffer) else Unreachable_Port_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination_Port (Buffer));

   function Valid_Length_000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Destination_Port_00 (Buffer) and then ((Buffer'Length >= 6 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Convert_To_Length_Type_Base (Buffer ((Buffer'First + 4) .. (Buffer'First + 5)), 0) >= 8)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Length_000 (Buffer : Types.Bytes) return Length_Type is
      (Convert_To_Length_Type_Base (Buffer ((Buffer'First + 4) .. (Buffer'First + 5)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Length_000 (Buffer));

   function Valid_Length (Buffer : Types.Bytes) return Boolean is
      (Valid_Length_000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Length (Buffer : Types.Bytes) return Length_Type is
      ((if Valid_Length_000 (Buffer) then Get_Length_000 (Buffer) else Unreachable_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Length (Buffer));

   function Valid_Checksum_0000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Length_000 (Buffer) and then (Buffer'Length >= 8 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Checksum_0000 (Buffer : Types.Bytes) return Checksum_Type is
      (Convert_To_Checksum_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 7)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Checksum_0000 (Buffer));

   function Valid_Checksum (Buffer : Types.Bytes) return Boolean is
      (Valid_Checksum_0000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Checksum (Buffer : Types.Bytes) return Checksum_Type is
      ((if Valid_Checksum_0000 (Buffer) then Get_Checksum_0000 (Buffer) else Unreachable_Checksum_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Checksum (Buffer));

   function Valid_Payload_00000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Checksum_0000 (Buffer) and then (Buffer'Length >= Types.Length_Type (Get_Length_000 (Buffer)) and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_00000_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 8))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00000 (Buffer));

   function Get_Payload_00000_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + Types.Length_Type (Get_Length_000 (Buffer)) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00000 (Buffer));

   function Valid_Payload (Buffer : Types.Bytes) return Boolean is
      (Valid_Payload_00000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_00000 (Buffer) then Get_Payload_00000_First (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Get_Payload_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_00000 (Buffer) then Get_Payload_00000_Last (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Get_Payload (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
       Post => (First = Get_Payload_First (Buffer) and then Last = Get_Payload_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
      (Valid_Payload_00000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
      ((if Valid_Payload_00000 (Buffer) then Types.Length_Type (Get_Length_000 (Buffer)) else Unreachable_Types_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end UDP.Datagram;
