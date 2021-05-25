pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.IPv4 with
  SPARK_Mode
is

   type Version_Base is mod 2**4 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Version is range 4 .. 4 with
     Size =>
       4;

   function Valid (Val : RFLX.IPv4.Version_Base) return Boolean is
     (Val = 4);

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

   function Valid (Val : RFLX.IPv4.IHL_Base) return Boolean is
     (Val >= 5);

   function To_Base (Val : RFLX.IPv4.IHL) return RFLX.IPv4.IHL_Base is
     (RFLX.IPv4.IHL_Base (Val));

   function To_Actual (Val : RFLX.IPv4.IHL_Base) return RFLX.IPv4.IHL is
     (RFLX.IPv4.IHL (Val))
    with
     Pre =>
       Valid (Val);

   type DCSP is mod 2**6 with
     Size =>
       6;

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

   type ECN is mod 2**2 with
     Size =>
       2;

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

   type Total_Length is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Total_Length) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.IPv4.Total_Length) return RFLX.IPv4.Total_Length is
     (Val);

   function To_Actual (Val : RFLX.IPv4.Total_Length) return RFLX.IPv4.Total_Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Identification is mod 2**16 with
     Size =>
       16;

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

   type Fragment_Offset is mod 2**13 with
     Size =>
       13;

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

   type TTL is mod 2**8 with
     Size =>
       8;

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

   type Protocol_Enum is (P_ICMP, P_UDP) with
     Size =>
       8;
   for Protocol_Enum use (P_ICMP => 1, P_UDP => 17);

   type Protocol (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Protocol_Enum;
            when False =>
               Raw : Protocol_Base;
         end case;
      end record;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.IPv4.Protocol_Base) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Enum : RFLX.IPv4.Protocol_Enum) return RFLX.IPv4.Protocol_Base is
     ((case Enum is
          when P_ICMP =>
             1,
          when P_UDP =>
             17));

   function To_Actual (Enum : Protocol_Enum) return RFLX.IPv4.Protocol is
     ((True, Enum));

   function To_Actual (Val : RFLX.IPv4.Protocol_Base) return RFLX.IPv4.Protocol is
     ((case Val is
          when 1 =>
             (True, P_ICMP),
          when 17 =>
             (True, P_UDP),
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

   type Header_Checksum is mod 2**16 with
     Size =>
       16;

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

   type Address is mod 2**32 with
     Size =>
       32;

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
             raise Program_Error))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Option_Number is mod 2**5 with
     Size =>
       5;

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
