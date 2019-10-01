with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.IPv4 with
  SPARK_Mode
is

   type Flag_Base is mod 2**1;

   type Flag is (Flag_False, Flag_True) with
     Size =>
       1;
   for Flag use (Flag_False => 0, Flag_True => 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Flag return Flag is
     (Flag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Flag_Base);

   function Valid (Value : Flag_Base) return Boolean is
     ((case Value is
         when 0 | 1 =>
            True,
         when others =>
            False));

   function Convert (Value : Flag_Base) return Flag is
     ((case Value is
         when 0 =>
            Flag_False,
         when 1 =>
            Flag_True,
         when others =>
            Unreachable_Flag))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Flag) return Flag_Base is
     ((case Enum is
         when Flag_False =>
            0,
         when Flag_True =>
            1));

   type Option_Class_Base is mod 2**2;

   type Option_Class is (Control, Debugging_And_Measurement) with
     Size =>
       2;
   for Option_Class use (Control => 0, Debugging_And_Measurement => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Class return Option_Class is
     (Option_Class'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Option_Class_Base);

   function Valid (Value : Option_Class_Base) return Boolean is
     ((case Value is
         when 0 | 2 =>
            True,
         when others =>
            False));

   function Convert (Value : Option_Class_Base) return Option_Class is
     ((case Value is
         when 0 =>
            Control,
         when 2 =>
            Debugging_And_Measurement,
         when others =>
            Unreachable_Option_Class))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Option_Class) return Option_Class_Base is
     ((case Enum is
         when Control =>
            0,
         when Debugging_And_Measurement =>
            2));

   type Option_Number is mod 2**5;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Number return Option_Number is
     (Option_Number'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Option_Number);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Option_Number) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Option_Number) return Option_Number is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Option_Length_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Option_Length is Option_Length_Base range 2 .. 2**8 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Option_Length return Option_Length is
     (Option_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Option_Length_Base);

   function Valid (Value : Option_Length_Base) return Boolean is
     (Value >= 2);

   function Convert (Value : Option_Length_Base) return Option_Length is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Version_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype Version is Version_Base range 4 .. 4;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Version return Version is
     (Version'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Version_Base);

   function Valid (Value : Version_Base) return Boolean is
     (Value >= 4
      and then Value <= 4);

   function Convert (Value : Version_Base) return Version is
     (Value)
    with
     Pre =>
       Valid (Value);

   type IHL_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype IHL is IHL_Base range 5 .. 15;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IHL return IHL is
     (IHL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, IHL_Base);

   function Valid (Value : IHL_Base) return Boolean is
     (Value >= 5);

   function Convert (Value : IHL_Base) return IHL is
     (Value)
    with
     Pre =>
       Valid (Value);

   type DCSP is mod 2**6;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_DCSP return DCSP is
     (DCSP'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, DCSP);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : DCSP) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : DCSP) return DCSP is
     (Value)
    with
     Pre =>
       Valid (Value);

   type ECN is mod 2**2;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_ECN return ECN is
     (ECN'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, ECN);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : ECN) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : ECN) return ECN is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Total_Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Total_Length is Total_Length_Base range 20 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Total_Length return Total_Length is
     (Total_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Total_Length_Base);

   function Valid (Value : Total_Length_Base) return Boolean is
     (Value >= 20);

   function Convert (Value : Total_Length_Base) return Total_Length is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Identification is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Identification return Identification is
     (Identification'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Identification);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Identification) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Identification) return Identification is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Fragment_Offset is mod 2**13;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Fragment_Offset return Fragment_Offset is
     (Fragment_Offset'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Fragment_Offset);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Fragment_Offset) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Fragment_Offset) return Fragment_Offset is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TTL is mod 2**8;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TTL return TTL is
     (TTL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, TTL);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : TTL) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : TTL) return TTL is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Protocol_Base is mod 2**8;

   type Protocol_Enum is (PROTOCOL_UDP) with
     Size =>
       8;
   for Protocol_Enum use (PROTOCOL_UDP => 17);

   type Protocol (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Protocol_Enum;
            when False =>
               Raw : Protocol_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol return Protocol is
     ((False, Protocol_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Protocol_Base);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Protocol_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Protocol_Base) return Protocol is
     ((case Value is
         when 17 =>
            (True, PROTOCOL_UDP),
         when others =>
            (False, Value)))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Protocol_Enum) return Protocol_Base is
     ((case Enum is
         when PROTOCOL_UDP =>
            17));

   type Header_Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Header_Checksum return Header_Checksum is
     (Header_Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Header_Checksum);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Header_Checksum) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Header_Checksum) return Header_Checksum is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Address is mod 2**32;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address return Address is
     (Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Address);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Address) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Address) return Address is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.IPv4;
