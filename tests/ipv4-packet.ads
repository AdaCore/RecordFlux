with IPv4.Options;

package IPv4.Packet
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

   function Valid_Version_0 (Buffer : Types.Bytes) return Boolean is
      (((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then (Convert_To_Version_Type_Base (Buffer (Buffer'First .. Buffer'First), 4) >= 4 and then Convert_To_Version_Type_Base (Buffer (Buffer'First .. Buffer'First), 4) <= 4)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Version_0 (Buffer : Types.Bytes) return Version_Type is
      (Convert_To_Version_Type_Base (Buffer (Buffer'First .. Buffer'First), 4))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Version_0 (Buffer));

   function Valid_Version (Buffer : Types.Bytes) return Boolean is
      (Valid_Version_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Version (Buffer : Types.Bytes) return Version_Type is
      ((if Valid_Version_0 (Buffer) then Get_Version_0 (Buffer) else Unreachable_Version_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Version (Buffer));

   function Valid_IHL_00 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Version_0 (Buffer) and then ((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Convert_To_IHL_Type_Base (Buffer (Buffer'First .. Buffer'First), 0) >= 5)))
     with
       Pre => Is_Contained (Buffer);

   function Get_IHL_00 (Buffer : Types.Bytes) return IHL_Type is
      (Convert_To_IHL_Type_Base (Buffer (Buffer'First .. Buffer'First), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_IHL_00 (Buffer));

   function Valid_IHL (Buffer : Types.Bytes) return Boolean is
      (Valid_IHL_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_IHL (Buffer : Types.Bytes) return IHL_Type is
      ((if Valid_IHL_00 (Buffer) then Get_IHL_00 (Buffer) else Unreachable_IHL_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_IHL (Buffer));

   function Valid_DSCP_000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_IHL_00 (Buffer) and then (Buffer'Length >= 2 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_DSCP_000 (Buffer : Types.Bytes) return DCSP_Type is
      (Convert_To_DCSP_Type (Buffer ((Buffer'First + 1) .. (Buffer'First + 1)), 2))
     with
       Pre => (Is_Contained (Buffer) and then Valid_DSCP_000 (Buffer));

   function Valid_DSCP (Buffer : Types.Bytes) return Boolean is
      (Valid_DSCP_000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_DSCP (Buffer : Types.Bytes) return DCSP_Type is
      ((if Valid_DSCP_000 (Buffer) then Get_DSCP_000 (Buffer) else Unreachable_DCSP_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_DSCP (Buffer));

   function Valid_ECN_0000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_DSCP_000 (Buffer) and then (Buffer'Length >= 2 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_ECN_0000 (Buffer : Types.Bytes) return ECN_Type is
      (Convert_To_ECN_Type (Buffer ((Buffer'First + 1) .. (Buffer'First + 1)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_ECN_0000 (Buffer));

   function Valid_ECN (Buffer : Types.Bytes) return Boolean is
      (Valid_ECN_0000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_ECN (Buffer : Types.Bytes) return ECN_Type is
      ((if Valid_ECN_0000 (Buffer) then Get_ECN_0000 (Buffer) else Unreachable_ECN_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_ECN (Buffer));

   function Valid_Total_Length_00000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_ECN_0000 (Buffer) and then ((Buffer'Length >= 4 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Convert_To_Total_Length_Type_Base (Buffer ((Buffer'First + 2) .. (Buffer'First + 3)), 0) >= 20)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Total_Length_00000 (Buffer : Types.Bytes) return Total_Length_Type is
      (Convert_To_Total_Length_Type_Base (Buffer ((Buffer'First + 2) .. (Buffer'First + 3)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Total_Length_00000 (Buffer));

   function Valid_Total_Length (Buffer : Types.Bytes) return Boolean is
      (Valid_Total_Length_00000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Total_Length (Buffer : Types.Bytes) return Total_Length_Type is
      ((if Valid_Total_Length_00000 (Buffer) then Get_Total_Length_00000 (Buffer) else Unreachable_Total_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Total_Length (Buffer));

   function Valid_Identification_000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Total_Length_00000 (Buffer) and then (Buffer'Length >= 6 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Identification_000000 (Buffer : Types.Bytes) return Identification_Type is
      (Convert_To_Identification_Type (Buffer ((Buffer'First + 4) .. (Buffer'First + 5)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Identification_000000 (Buffer));

   function Valid_Identification (Buffer : Types.Bytes) return Boolean is
      (Valid_Identification_000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Identification (Buffer : Types.Bytes) return Identification_Type is
      ((if Valid_Identification_000000 (Buffer) then Get_Identification_000000 (Buffer) else Unreachable_Identification_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Identification (Buffer));

   function Valid_Flag_R_0000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Identification_000000 (Buffer) and then ((Buffer'Length >= 7 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Valid_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 7))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_R_0000000 (Buffer : Types.Bytes) return Flag_Type is
      (Convert_To_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 7))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_R_0000000 (Buffer));

   function Valid_Flag_R (Buffer : Types.Bytes) return Boolean is
      ((Valid_Flag_R_0000000 (Buffer) and then Get_Flag_R_0000000 (Buffer) = Flag_False))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_R (Buffer : Types.Bytes) return Flag_Type is
      ((if Valid_Flag_R_0000000 (Buffer) then Get_Flag_R_0000000 (Buffer) else Unreachable_Flag_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_R (Buffer));

   function Valid_Flag_DF_00000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Flag_R_0000000 (Buffer) and then (((Buffer'Length >= 7 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_Flag_R_0000000 (Buffer) = Flag_False) and then Valid_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 6))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_DF_00000000 (Buffer : Types.Bytes) return Flag_Type is
      (Convert_To_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 6))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_DF_00000000 (Buffer));

   function Valid_Flag_DF (Buffer : Types.Bytes) return Boolean is
      (Valid_Flag_DF_00000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_DF (Buffer : Types.Bytes) return Flag_Type is
      ((if Valid_Flag_DF_00000000 (Buffer) then Get_Flag_DF_00000000 (Buffer) else Unreachable_Flag_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_DF (Buffer));

   function Valid_Flag_MF_000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Flag_DF_00000000 (Buffer) and then ((Buffer'Length >= 7 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Valid_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 5))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_MF_000000000 (Buffer : Types.Bytes) return Flag_Type is
      (Convert_To_Flag_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 6)), 5))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_MF_000000000 (Buffer));

   function Valid_Flag_MF (Buffer : Types.Bytes) return Boolean is
      (Valid_Flag_MF_000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Flag_MF (Buffer : Types.Bytes) return Flag_Type is
      ((if Valid_Flag_MF_000000000 (Buffer) then Get_Flag_MF_000000000 (Buffer) else Unreachable_Flag_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Flag_MF (Buffer));

   function Valid_Fragment_Offset_0000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Flag_MF_000000000 (Buffer) and then (Buffer'Length >= 8 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Fragment_Offset_0000000000 (Buffer : Types.Bytes) return Fragment_Offset_Type is
      (Convert_To_Fragment_Offset_Type (Buffer ((Buffer'First + 6) .. (Buffer'First + 7)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Fragment_Offset_0000000000 (Buffer));

   function Valid_Fragment_Offset (Buffer : Types.Bytes) return Boolean is
      (Valid_Fragment_Offset_0000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Fragment_Offset (Buffer : Types.Bytes) return Fragment_Offset_Type is
      ((if Valid_Fragment_Offset_0000000000 (Buffer) then Get_Fragment_Offset_0000000000 (Buffer) else Unreachable_Fragment_Offset_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Fragment_Offset (Buffer));

   function Valid_TTL_00000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Fragment_Offset_0000000000 (Buffer) and then (Buffer'Length >= 9 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_TTL_00000000000 (Buffer : Types.Bytes) return TTL_Type is
      (Convert_To_TTL_Type (Buffer ((Buffer'First + 8) .. (Buffer'First + 8)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TTL_00000000000 (Buffer));

   function Valid_TTL (Buffer : Types.Bytes) return Boolean is
      (Valid_TTL_00000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_TTL (Buffer : Types.Bytes) return TTL_Type is
      ((if Valid_TTL_00000000000 (Buffer) then Get_TTL_00000000000 (Buffer) else Unreachable_TTL_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_TTL (Buffer));

   function Valid_Protocol_000000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_TTL_00000000000 (Buffer) and then (Buffer'Length >= 10 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Protocol_000000000000 (Buffer : Types.Bytes) return Protocol_Type is
      (Convert_To_Protocol_Type (Buffer ((Buffer'First + 9) .. (Buffer'First + 9)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Protocol_000000000000 (Buffer));

   function Valid_Protocol (Buffer : Types.Bytes) return Boolean is
      (Valid_Protocol_000000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Protocol (Buffer : Types.Bytes) return Protocol_Type is
      ((if Valid_Protocol_000000000000 (Buffer) then Get_Protocol_000000000000 (Buffer) else Unreachable_Protocol_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Protocol (Buffer));

   function Valid_Header_Checksum_0000000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Protocol_000000000000 (Buffer) and then (Buffer'Length >= 12 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Header_Checksum_0000000000000 (Buffer : Types.Bytes) return Header_Checksum_Type is
      (Convert_To_Header_Checksum_Type (Buffer ((Buffer'First + 10) .. (Buffer'First + 11)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Header_Checksum_0000000000000 (Buffer));

   function Valid_Header_Checksum (Buffer : Types.Bytes) return Boolean is
      (Valid_Header_Checksum_0000000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Header_Checksum (Buffer : Types.Bytes) return Header_Checksum_Type is
      ((if Valid_Header_Checksum_0000000000000 (Buffer) then Get_Header_Checksum_0000000000000 (Buffer) else Unreachable_Header_Checksum_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Header_Checksum (Buffer));

   function Valid_Source_00000000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Header_Checksum_0000000000000 (Buffer) and then (Buffer'Length >= 16 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source_00000000000000 (Buffer : Types.Bytes) return Address_Type is
      (Convert_To_Address_Type (Buffer ((Buffer'First + 12) .. (Buffer'First + 15)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source_00000000000000 (Buffer));

   function Valid_Source (Buffer : Types.Bytes) return Boolean is
      (Valid_Source_00000000000000 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Source (Buffer : Types.Bytes) return Address_Type is
      ((if Valid_Source_00000000000000 (Buffer) then Get_Source_00000000000000 (Buffer) else Unreachable_Address_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Source (Buffer));

   function Valid_Destination_000000000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Source_00000000000000 (Buffer) and then (Buffer'Length >= 20 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination_000000000000000 (Buffer : Types.Bytes) return Address_Type is
      (Convert_To_Address_Type (Buffer ((Buffer'First + 16) .. (Buffer'First + 19)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination_000000000000000 (Buffer));

   function Valid_Destination (Buffer : Types.Bytes) return Boolean is
      ((Valid_Destination_000000000000000 (Buffer) and then (Get_IHL_00 (Buffer) > 5 or Get_IHL_00 (Buffer) = 5)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Destination (Buffer : Types.Bytes) return Address_Type is
      ((if Valid_Destination_000000000000000 (Buffer) then Get_Destination_000000000000000 (Buffer) else Unreachable_Address_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Destination (Buffer));

   function Valid_Options_0000000000000001 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Destination_000000000000000 (Buffer) and then ((Buffer'Length >= (Types.Length_Type (Get_IHL_00 (Buffer)) * 4) and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_IHL_00 (Buffer) > 5)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Options_0000000000000001_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 20))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Options_0000000000000001 (Buffer));

   function Get_Options_0000000000000001_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + (Types.Length_Type (Get_IHL_00 (Buffer)) * 4) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Options_0000000000000001 (Buffer));

   function Valid_Options (Buffer : Types.Bytes) return Boolean is
      (Valid_Options_0000000000000001 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Options_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Options_0000000000000001 (Buffer) then Get_Options_0000000000000001_First (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Options (Buffer));

   function Get_Options_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Options_0000000000000001 (Buffer) then Get_Options_0000000000000001_Last (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Options (Buffer));

   procedure Get_Options (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Options (Buffer)),
       Post => ((First = Get_Options_First (Buffer) and then Last = Get_Options_Last (Buffer)) and then IPv4.Options.Is_Contained (Buffer (First .. Last)));

   function Valid_Payload_0000000000000000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Destination_000000000000000 (Buffer) and then ((Buffer'Length >= Types.Length_Type (Get_Total_Length_00000 (Buffer)) and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_IHL_00 (Buffer) = 5)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_0000000000000000_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 20))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0000000000000000 (Buffer));

   function Get_Payload_0000000000000000_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + Types.Length_Type (Get_Total_Length_00000 (Buffer)) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_0000000000000000 (Buffer));

   function Valid_Payload_00000000000000010 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Options_0000000000000001 (Buffer) and then (Buffer'Length >= Types.Length_Type (Get_Total_Length_00000 (Buffer)) and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_00000000000000010_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + (Types.Length_Type (Get_IHL_00 (Buffer)) * 4)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00000000000000010 (Buffer));

   function Get_Payload_00000000000000010_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + Types.Length_Type (Get_Total_Length_00000 (Buffer)) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload_00000000000000010 (Buffer));

   function Valid_Payload (Buffer : Types.Bytes) return Boolean is
      ((Valid_Payload_00000000000000010 (Buffer) or Valid_Payload_0000000000000000 (Buffer)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Payload_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_0000000000000000 (Buffer) then Get_Payload_0000000000000000_First (Buffer) elsif Valid_Payload_00000000000000010 (Buffer) then Get_Payload_00000000000000010_First (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Get_Payload_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Payload_0000000000000000 (Buffer) then Get_Payload_0000000000000000_Last (Buffer) elsif Valid_Payload_00000000000000010 (Buffer) then Get_Payload_00000000000000010_Last (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Get_Payload (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
       Post => (First = Get_Payload_First (Buffer) and then Last = Get_Payload_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
      ((Valid_Payload_0000000000000000 (Buffer) or Valid_Payload_00000000000000010 (Buffer)))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
      ((if Valid_Payload_0000000000000000 (Buffer) then Types.Length_Type (Get_Total_Length_00000 (Buffer)) elsif Valid_Payload_00000000000000010 (Buffer) then Types.Length_Type (Get_Total_Length_00000 (Buffer)) else Unreachable_Types_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end IPv4.Packet;
