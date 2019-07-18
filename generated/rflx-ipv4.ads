with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;

package RFLX.IPv4 with
  SPARK_Mode
is

   type Flag_Type_Base is mod 2**1;

   type Flag_Type is (Flag_False, Flag_True) with
     Size =>
       1;
   for Flag_Type use (Flag_False => 0, Flag_True => 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Flag_Type return Flag_Type is
     (Flag_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Flag_Type_Base);

   function Valid (Value : Flag_Type_Base) return Boolean is
     ((case Value is
         when 0 | 1 =>
            True,
         when others =>
            False));

   function Convert (Value : Flag_Type_Base) return Flag_Type is
     ((case Value is
         when 0 =>
            Flag_False,
         when 1 =>
            Flag_True,
         when others =>
            Unreachable_Flag_Type))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Flag_Type) return Flag_Type_Base is
     ((case Enum is
         when Flag_False =>
            0,
         when Flag_True =>
            1));

   type Option_Class_Type_Base is mod 2**2;

   type Option_Class_Type is (Control, Debugging_And_Measurement) with
     Size =>
       2;
   for Option_Class_Type use (Control => 0, Debugging_And_Measurement => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Class_Type return Option_Class_Type is
     (Option_Class_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Option_Class_Type_Base);

   function Valid (Value : Option_Class_Type_Base) return Boolean is
     ((case Value is
         when 0 | 2 =>
            True,
         when others =>
            False));

   function Convert (Value : Option_Class_Type_Base) return Option_Class_Type is
     ((case Value is
         when 0 =>
            Control,
         when 2 =>
            Debugging_And_Measurement,
         when others =>
            Unreachable_Option_Class_Type))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Option_Class_Type) return Option_Class_Type_Base is
     ((case Enum is
         when Control =>
            0,
         when Debugging_And_Measurement =>
            2));

   type Option_Number_Type is mod 2**5;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Number_Type return Option_Number_Type is
     (Option_Number_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Option_Number_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Option_Number_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Option_Number_Type) return Option_Number_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Option_Length_Type_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Option_Length_Type is Option_Length_Type_Base range 2 .. 2**8 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Length_Type return Option_Length_Type is
     (Option_Length_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (Option_Length_Type_Base);

   function Valid (Value : Option_Length_Type_Base) return Boolean is
     (Value >= 2);

   function Convert (Value : Option_Length_Type_Base) return Option_Length_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Version_Type_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype Version_Type is Version_Type_Base range 4 .. 4;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Version_Type return Version_Type is
     (Version_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (Version_Type_Base);

   function Valid (Value : Version_Type_Base) return Boolean is
     (Value >= 4
      and then Value <= 4);

   function Convert (Value : Version_Type_Base) return Version_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type IHL_Type_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype IHL_Type is IHL_Type_Base range 5 .. 15;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IHL_Type return IHL_Type is
     (IHL_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (IHL_Type_Base);

   function Valid (Value : IHL_Type_Base) return Boolean is
     (Value >= 5);

   function Convert (Value : IHL_Type_Base) return IHL_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type DCSP_Type is mod 2**6;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_DCSP_Type return DCSP_Type is
     (DCSP_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (DCSP_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : DCSP_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : DCSP_Type) return DCSP_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type ECN_Type is mod 2**2;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_ECN_Type return ECN_Type is
     (ECN_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (ECN_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : ECN_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : ECN_Type) return ECN_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Total_Length_Type_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Total_Length_Type is Total_Length_Type_Base range 20 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Total_Length_Type return Total_Length_Type is
     (Total_Length_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (Total_Length_Type_Base);

   function Valid (Value : Total_Length_Type_Base) return Boolean is
     (Value >= 20);

   function Convert (Value : Total_Length_Type_Base) return Total_Length_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Identification_Type is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Identification_Type return Identification_Type is
     (Identification_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Identification_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Identification_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Identification_Type) return Identification_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Fragment_Offset_Type is mod 2**13;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Fragment_Offset_Type return Fragment_Offset_Type is
     (Fragment_Offset_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Fragment_Offset_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Fragment_Offset_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Fragment_Offset_Type) return Fragment_Offset_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TTL_Type is mod 2**8;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TTL_Type return TTL_Type is
     (TTL_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (TTL_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : TTL_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : TTL_Type) return TTL_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Protocol_Type_Base is mod 2**8;

   type Protocol_Type_Enum is (PROTOCOL_UDP) with
     Size =>
       8;
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
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Protocol_Type_Base);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Protocol_Type_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Protocol_Type_Base) return Protocol_Type is
     ((case Value is
         when 17 =>
            (True, PROTOCOL_UDP),
         when others =>
            (False, Value)))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Protocol_Type_Enum) return Protocol_Type_Base is
     ((case Enum is
         when PROTOCOL_UDP =>
            17));

   type Header_Checksum_Type is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Header_Checksum_Type return Header_Checksum_Type is
     (Header_Checksum_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Header_Checksum_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Header_Checksum_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Header_Checksum_Type) return Header_Checksum_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Address_Type is mod 2**32;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address_Type return Address_Type is
     (Address_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Address_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Address_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Address_Type) return Address_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.IPv4;
