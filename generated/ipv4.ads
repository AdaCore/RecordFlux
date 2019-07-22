with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type, Types.Bit_Index_Type, Types.Bit_Length_Type;

package IPv4 with
  SPARK_Mode
is

   type Flag_Type_Base is mod (2**1);

   type Flag_Type is (Flag_False, Flag_True) with Size => 1;
   for Flag_Type use (Flag_False => 0, Flag_True => 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Flag_Type return Flag_Type is
     (Flag_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Flag_Type_Base is new Types.Convert_To_Mod (Flag_Type_Base);

   function Valid_Flag_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (case Convert_To_Flag_Type_Base (Buffer, Offset) is when 0 | 1 => True, when others => False)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Flag_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   function Convert_To_Flag_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Flag_Type is
     (case Convert_To_Flag_Type_Base (Buffer, Offset) is when 0 => Flag_False, when 1 => Flag_True, when others => Unreachable_Flag_Type)
    with
     Pre => (Buffer'Length = Types.Byte_Index ((Flag_Type_Base'Size + Types.Bit_Length_Type (Offset))) and then Valid_Flag_Type (Buffer, Offset));

   function Convert_To_Flag_Type_Base (Enum : Flag_Type) return Flag_Type_Base is
     (case Enum is when Flag_False => 0, when Flag_True => 1);

   type Option_Class_Type_Base is mod (2**2);

   type Option_Class_Type is (Control, Debugging_And_Measurement) with Size => 2;
   for Option_Class_Type use (Control => 0, Debugging_And_Measurement => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Class_Type return Option_Class_Type is
     (Option_Class_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Option_Class_Type_Base is new Types.Convert_To_Mod (Option_Class_Type_Base);

   function Valid_Option_Class_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (case Convert_To_Option_Class_Type_Base (Buffer, Offset) is when 0 | 2 => True, when others => False)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Option_Class_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   function Convert_To_Option_Class_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Option_Class_Type is
     (case Convert_To_Option_Class_Type_Base (Buffer, Offset) is when 0 => Control, when 2 => Debugging_And_Measurement, when others => Unreachable_Option_Class_Type)
    with
     Pre => (Buffer'Length = Types.Byte_Index ((Option_Class_Type_Base'Size + Types.Bit_Length_Type (Offset))) and then Valid_Option_Class_Type (Buffer, Offset));

   function Convert_To_Option_Class_Type_Base (Enum : Option_Class_Type) return Option_Class_Type_Base is
     (case Enum is when Control => 0, when Debugging_And_Measurement => 2);

   type Option_Number_Type is mod (2**5);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Number_Type return Option_Number_Type is
     (Option_Number_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Option_Number_Type is new Types.Convert_To_Mod (Option_Number_Type);

   function Valid_Option_Number_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Option_Number_Type'Size + Types.Bit_Length_Type (Offset)));

   type Option_Length_Type_Base is range 0 .. ((2**8) - 1) with Size => 8;

   subtype Option_Length_Type is Option_Length_Type_Base range 2 .. ((2**8) - 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Length_Type return Option_Length_Type is
     (Option_Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Option_Length_Type_Base is new Types.Convert_To_Int (Option_Length_Type_Base);

   function Valid_Option_Length_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (Convert_To_Option_Length_Type_Base (Buffer, Offset) >= 2)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Option_Length_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   type Version_Type_Base is range 0 .. ((2**4) - 1) with Size => 4;

   subtype Version_Type is Version_Type_Base range 4 .. 4;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Version_Type return Version_Type is
     (Version_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Version_Type_Base is new Types.Convert_To_Int (Version_Type_Base);

   function Valid_Version_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     ((Convert_To_Version_Type_Base (Buffer, Offset) >= 4 and then Convert_To_Version_Type_Base (Buffer, Offset) <= 4))
    with
     Pre => Buffer'Length = Types.Byte_Index ((Version_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   type IHL_Type_Base is range 0 .. ((2**4) - 1) with Size => 4;

   subtype IHL_Type is IHL_Type_Base range 5 .. 15;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IHL_Type return IHL_Type is
     (IHL_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_IHL_Type_Base is new Types.Convert_To_Int (IHL_Type_Base);

   function Valid_IHL_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (Convert_To_IHL_Type_Base (Buffer, Offset) >= 5)
    with
     Pre => Buffer'Length = Types.Byte_Index ((IHL_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   type DCSP_Type is mod (2**6);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_DCSP_Type return DCSP_Type is
     (DCSP_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_DCSP_Type is new Types.Convert_To_Mod (DCSP_Type);

   function Valid_DCSP_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((DCSP_Type'Size + Types.Bit_Length_Type (Offset)));

   type ECN_Type is mod (2**2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_ECN_Type return ECN_Type is
     (ECN_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_ECN_Type is new Types.Convert_To_Mod (ECN_Type);

   function Valid_ECN_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((ECN_Type'Size + Types.Bit_Length_Type (Offset)));

   type Total_Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;

   subtype Total_Length_Type is Total_Length_Type_Base range 20 .. ((2**16) - 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Total_Length_Type return Total_Length_Type is
     (Total_Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Total_Length_Type_Base is new Types.Convert_To_Int (Total_Length_Type_Base);

   function Valid_Total_Length_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (Convert_To_Total_Length_Type_Base (Buffer, Offset) >= 20)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Total_Length_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   type Identification_Type is mod (2**16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Identification_Type return Identification_Type is
     (Identification_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Identification_Type is new Types.Convert_To_Mod (Identification_Type);

   function Valid_Identification_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Identification_Type'Size + Types.Bit_Length_Type (Offset)));

   type Fragment_Offset_Type is mod (2**13);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Fragment_Offset_Type return Fragment_Offset_Type is
     (Fragment_Offset_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Fragment_Offset_Type is new Types.Convert_To_Mod (Fragment_Offset_Type);

   function Valid_Fragment_Offset_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Fragment_Offset_Type'Size + Types.Bit_Length_Type (Offset)));

   type TTL_Type is mod (2**8);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TTL_Type return TTL_Type is
     (TTL_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_TTL_Type is new Types.Convert_To_Mod (TTL_Type);

   function Valid_TTL_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((TTL_Type'Size + Types.Bit_Length_Type (Offset)));

   type Protocol_Type_Base is mod (2**8);

   type Protocol_Type_Enum is (PROTOCOL_UDP) with Size => 8;
   for Protocol_Type_Enum use (PROTOCOL_UDP => 17);

   type Protocol_Type (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Protocol_Type_Enum;
            when False =>
               Raw : Protocol_Type_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Type return Protocol_Type is
     ((False, Protocol_Type_Base'First))
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Protocol_Type_Base is new Types.Convert_To_Mod (Protocol_Type_Base);

   function Valid_Protocol_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Protocol_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   function Convert_To_Protocol_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Protocol_Type with
     Pre => (Buffer'Length = Types.Byte_Index ((Protocol_Type_Base'Size + Types.Bit_Length_Type (Offset))) and then Valid_Protocol_Type (Buffer, Offset));

   function Convert_To_Protocol_Type_Base (Enum : Protocol_Type_Enum) return Protocol_Type_Base is
     (case Enum is when PROTOCOL_UDP => 17);

   type Header_Checksum_Type is mod (2**16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Header_Checksum_Type return Header_Checksum_Type is
     (Header_Checksum_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Header_Checksum_Type is new Types.Convert_To_Mod (Header_Checksum_Type);

   function Valid_Header_Checksum_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Header_Checksum_Type'Size + Types.Bit_Length_Type (Offset)));

   type Address_Type is mod (2**32);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address_Type return Address_Type is
     (Address_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Address_Type is new Types.Convert_To_Mod (Address_Type);

   function Valid_Address_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Address_Type'Size + Types.Bit_Length_Type (Offset)));

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Index_Type return Types.Index_Type is
     (Types.Index_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Length_Type return Types.Length_Type is
     (Types.Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   pragma Warnings (On, "precondition is statically false");

end IPv4;
