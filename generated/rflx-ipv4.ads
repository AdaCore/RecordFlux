pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package RFLX.IPv4 with
  SPARK_Mode
is

   type Version_Base is mod 2**4 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Version is range 4 .. 4 with
     Size =>
       4;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Version return RFLX.IPv4.Version is
     (RFLX.IPv4.Version'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.IPv4.Version_Base) return Boolean is
     (Val >= 4
      and Val <= 4);

   function To_Base (Val : RFLX.IPv4.Version) return RFLX.IPv4.Version_Base is
     (RFLX.IPv4.Version_Base (Val));

   function To_Actual (Val : RFLX.IPv4.Version_Base) return RFLX.IPv4.Version is
     (RFLX.IPv4.Version (Val))
    with
     Pre =>
       Valid (Val);

   type IHL_Base is mod 2**4 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type IHL is range 5 .. 15 with
     Size =>
       4;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_IHL return RFLX.IPv4.IHL is
     (RFLX.IPv4.IHL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.IPv4.IHL_Base) return Boolean is
     (Val >= 5);

   function To_Base (Val : RFLX.IPv4.IHL) return RFLX.IPv4.IHL_Base is
     (RFLX.IPv4.IHL_Base (Val));

   function To_Actual (Val : RFLX.IPv4.IHL_Base) return RFLX.IPv4.IHL is
     (RFLX.IPv4.IHL (Val))
    with
     Pre =>
       Valid (Val);

   type DCSP is mod 2**6;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_DCSP return RFLX.IPv4.DCSP is
     (RFLX.IPv4.DCSP'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.DCSP) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.DCSP) return RFLX.IPv4.DCSP is
     (Val);

   function To_Actual (Val : RFLX.IPv4.DCSP) return RFLX.IPv4.DCSP is
     (Val)
    with
     Pre =>
       Valid (Val);

   type ECN is mod 2**2;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_ECN return RFLX.IPv4.ECN is
     (RFLX.IPv4.ECN'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.ECN) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.ECN) return RFLX.IPv4.ECN is
     (Val);

   function To_Actual (Val : RFLX.IPv4.ECN) return RFLX.IPv4.ECN is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Total_Length_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Total_Length is range 20 .. 2**16 - 1 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Total_Length return RFLX.IPv4.Total_Length is
     (RFLX.IPv4.Total_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.IPv4.Total_Length_Base) return Boolean is
     (Val >= 20);

   function To_Base (Val : RFLX.IPv4.Total_Length) return RFLX.IPv4.Total_Length_Base is
     (RFLX.IPv4.Total_Length_Base (Val));

   function To_Actual (Val : RFLX.IPv4.Total_Length_Base) return RFLX.IPv4.Total_Length is
     (RFLX.IPv4.Total_Length (Val))
    with
     Pre =>
       Valid (Val);

   type Identification is mod 2**16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Identification return RFLX.IPv4.Identification is
     (RFLX.IPv4.Identification'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Identification) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Identification) return RFLX.IPv4.Identification is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Identification) return RFLX.IPv4.Identification is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Fragment_Offset is mod 2**13;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Fragment_Offset return RFLX.IPv4.Fragment_Offset is
     (RFLX.IPv4.Fragment_Offset'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Fragment_Offset) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Fragment_Offset) return RFLX.IPv4.Fragment_Offset is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Fragment_Offset) return RFLX.IPv4.Fragment_Offset is
     (Val)
    with
     Pre =>
       Valid (Val);

   type TTL is mod 2**8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_TTL return RFLX.IPv4.TTL is
     (RFLX.IPv4.TTL'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.TTL) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.TTL) return RFLX.IPv4.TTL is
     (Val);

   function To_Actual (Val : RFLX.IPv4.TTL) return RFLX.IPv4.TTL is
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

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Protocol return RFLX.IPv4.Protocol is
     ((False, RFLX.IPv4.Protocol_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Protocol_Base) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Enum : RFLX.IPv4.Protocol_Enum) return RFLX.IPv4.Protocol_Base is
     ((case Enum is
          when PROTOCOL_UDP =>
             17));

   function To_Actual (Enum : Protocol_Enum) return RFLX.IPv4.Protocol is
     ((True, Enum));

   function To_Actual (Val : RFLX.IPv4.Protocol_Base) return RFLX.IPv4.Protocol is
     ((case Val is
          when 17 =>
             (True, PROTOCOL_UDP),
          when others =>
             (False, Val)))
    with
     Pre =>
       Valid (Val);

   function To_Base (Val : RFLX.IPv4.Protocol) return RFLX.IPv4.Protocol_Base is
     ((if
          Val.Known
       then
          To_Base (Val.Enum)
       else
          Val.Raw));

   type Header_Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Header_Checksum return RFLX.IPv4.Header_Checksum is
     (RFLX.IPv4.Header_Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Header_Checksum) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Header_Checksum) return RFLX.IPv4.Header_Checksum is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Header_Checksum) return RFLX.IPv4.Header_Checksum is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Address is mod 2**32;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Address return RFLX.IPv4.Address is
     (RFLX.IPv4.Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Address) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Address) return RFLX.IPv4.Address is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Address) return RFLX.IPv4.Address is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Option_Class_Base is mod 2**2;

   type Option_Class is (Control, Debugging_And_Measurement) with
     Size =>
       2;
   for Option_Class use (Control => 0, Debugging_And_Measurement => 2);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Option_Class return RFLX.IPv4.Option_Class is
     (RFLX.IPv4.Option_Class'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.IPv4.Option_Class_Base) return Boolean is
     ((case Val is
          when 0 | 2 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.IPv4.Option_Class) return RFLX.IPv4.Option_Class_Base is
     ((case Enum is
          when Control =>
             0,
          when Debugging_And_Measurement =>
             2));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.IPv4.Option_Class_Base) return RFLX.IPv4.Option_Class is
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

   pragma Warnings (On, "unreachable branch");

   type Option_Number is mod 2**5;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Option_Number return RFLX.IPv4.Option_Number is
     (RFLX.IPv4.Option_Number'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Option_Number) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Option_Number) return RFLX.IPv4.Option_Number is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Option_Number) return RFLX.IPv4.Option_Number is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Option_Length_Base is mod 2**8 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Option_Length is range 2 .. 2**8 - 1 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_IPv4_Option_Length return RFLX.IPv4.Option_Length is
     (RFLX.IPv4.Option_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.IPv4.Option_Length_Base) return Boolean is
     (Val >= 2);

   function To_Base (Val : RFLX.IPv4.Option_Length) return RFLX.IPv4.Option_Length_Base is
     (RFLX.IPv4.Option_Length_Base (Val));

   function To_Actual (Val : RFLX.IPv4.Option_Length_Base) return RFLX.IPv4.Option_Length is
     (RFLX.IPv4.Option_Length (Val))
    with
     Pre =>
       Valid (Val);

end RFLX.IPv4;
