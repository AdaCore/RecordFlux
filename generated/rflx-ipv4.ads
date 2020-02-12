package RFLX.IPv4 with
  SPARK_Mode
is

   type Flag_Base is mod 2**1;

   type Flag is (Flag_False, Flag_True) with
     Size =>
       1;
   for Flag use (Flag_False => 0, Flag_True => 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Flag return IPv4.Flag is
     (IPv4.Flag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.Flag_Base) return Boolean is
     ((case Val is
         when 0 | 1 =>
            True,
         when others =>
            False));

   function Convert (Enum : IPv4.Flag) return IPv4.Flag_Base is
     ((case Enum is
         when Flag_False =>
            0,
         when Flag_True =>
            1));

   function Convert (Val : IPv4.Flag_Base) return IPv4.Flag is
     ((case Val is
         when 0 =>
            Flag_False,
         when 1 =>
            Flag_True,
         when others =>
            Unreachable_IPv4_Flag))
    with
     Pre =>
       Valid (Val);

   type Option_Class_Base is mod 2**2;

   type Option_Class is (Control, Debugging_And_Measurement) with
     Size =>
       2;
   for Option_Class use (Control => 0, Debugging_And_Measurement => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Option_Class return IPv4.Option_Class is
     (IPv4.Option_Class'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.Option_Class_Base) return Boolean is
     ((case Val is
         when 0 | 2 =>
            True,
         when others =>
            False));

   function Convert (Enum : IPv4.Option_Class) return IPv4.Option_Class_Base is
     ((case Enum is
         when Control =>
            0,
         when Debugging_And_Measurement =>
            2));

   function Convert (Val : IPv4.Option_Class_Base) return IPv4.Option_Class is
     ((case Val is
         when 0 =>
            Control,
         when 2 =>
            Debugging_And_Measurement,
         when others =>
            Unreachable_IPv4_Option_Class))
    with
     Pre =>
       Valid (Val);

   type Option_Number is mod 2**5;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Option_Number return IPv4.Option_Number is
     (IPv4.Option_Number'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Option_Number) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.Option_Number) return IPv4.Option_Number is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Option_Length_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Option_Length is Option_Length_Base range 2 .. 2**8 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Option_Length return IPv4.Option_Length is
     (IPv4.Option_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.Option_Length_Base) return Boolean is
     (Val >= 2);

   function Convert (Val : IPv4.Option_Length_Base) return IPv4.Option_Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Version_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype Version is Version_Base range 4 .. 4;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Version return IPv4.Version is
     (IPv4.Version'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.Version_Base) return Boolean is
     (Val >= 4
      and Val <= 4);

   function Convert (Val : IPv4.Version_Base) return IPv4.Version is
     (Val)
    with
     Pre =>
       Valid (Val);

   type IHL_Base is range 0 .. 2**4 - 1 with
     Size =>
       4;

   subtype IHL is IHL_Base range 5 .. 15;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_IHL return IPv4.IHL is
     (IPv4.IHL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.IHL_Base) return Boolean is
     (Val >= 5);

   function Convert (Val : IPv4.IHL_Base) return IPv4.IHL is
     (Val)
    with
     Pre =>
       Valid (Val);

   type DCSP is mod 2**6;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_DCSP return IPv4.DCSP is
     (IPv4.DCSP'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.DCSP) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.DCSP) return IPv4.DCSP is
     (Val)
    with
     Pre =>
       Valid (Val);

   type ECN is mod 2**2;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_ECN return IPv4.ECN is
     (IPv4.ECN'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.ECN) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.ECN) return IPv4.ECN is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Total_Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Total_Length is Total_Length_Base range 20 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Total_Length return IPv4.Total_Length is
     (IPv4.Total_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : IPv4.Total_Length_Base) return Boolean is
     (Val >= 20);

   function Convert (Val : IPv4.Total_Length_Base) return IPv4.Total_Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Identification is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Identification return IPv4.Identification is
     (IPv4.Identification'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Identification) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.Identification) return IPv4.Identification is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Fragment_Offset is mod 2**13;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Fragment_Offset return IPv4.Fragment_Offset is
     (IPv4.Fragment_Offset'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Fragment_Offset) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.Fragment_Offset) return IPv4.Fragment_Offset is
     (Val)
    with
     Pre =>
       Valid (Val);

   type TTL is mod 2**8;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_TTL return IPv4.TTL is
     (IPv4.TTL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.TTL) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.TTL) return IPv4.TTL is
     (Val)
    with
     Pre =>
       Valid (Val);

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

   function Unreachable_IPv4_Protocol return IPv4.Protocol is
     ((False, IPv4.Protocol_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Protocol_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Enum : Protocol_Enum) return IPv4.Protocol_Base is
     ((case Enum is
         when PROTOCOL_UDP =>
            17));

   function Convert (Enum : Protocol_Enum) return IPv4.Protocol is
     ((True, Enum));

   function Convert (Val : IPv4.Protocol_Base) return IPv4.Protocol is
     ((case Val is
         when 17 =>
            (True, PROTOCOL_UDP),
         when others =>
            (False, Val)))
    with
     Pre =>
       Valid (Val);

   function Convert (Val : IPv4.Protocol) return IPv4.Protocol_Base is
     ((if Val.Known then
       Convert (Val.Enum)
    else
       Val.Raw));

   type Header_Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Header_Checksum return IPv4.Header_Checksum is
     (IPv4.Header_Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Header_Checksum) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.Header_Checksum) return IPv4.Header_Checksum is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Address is mod 2**32;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_IPv4_Address return IPv4.Address is
     (IPv4.Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : IPv4.Address) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : IPv4.Address) return IPv4.Address is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.IPv4;
