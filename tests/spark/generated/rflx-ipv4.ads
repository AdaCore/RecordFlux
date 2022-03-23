pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.IPv4 with
  SPARK_Mode
is

   type Version is range 4 .. 4 with
     Size =>
       4;

   use type RFLX.RFLX_Types.U64;

   function Valid_Version (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val = 4);

   function To_U64 (Val : RFLX.IPv4.Version) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Version is
     (RFLX.IPv4.Version (Val))
    with
     Pre =>
       Valid_Version (Val);

   type IHL is range 5 .. 15 with
     Size =>
       4;

   function Valid_IHL (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val >= 5
      and Val <= 15);

   function To_U64 (Val : RFLX.IPv4.IHL) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.IHL is
     (RFLX.IPv4.IHL (Val))
    with
     Pre =>
       Valid_IHL (Val);

   type DCSP is mod 2**6 with
     Size =>
       6;

   function Valid_DCSP (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 63);

   function To_U64 (Val : RFLX.IPv4.DCSP) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.DCSP is
     (RFLX.IPv4.DCSP (Val))
    with
     Pre =>
       Valid_DCSP (Val);

   type ECN is mod 2**2 with
     Size =>
       2;

   function Valid_ECN (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 3);

   function To_U64 (Val : RFLX.IPv4.ECN) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.ECN is
     (RFLX.IPv4.ECN (Val))
    with
     Pre =>
       Valid_ECN (Val);

   type Total_Length is mod 2**16 with
     Size =>
       16;

   function Valid_Total_Length (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 65535);

   function To_U64 (Val : RFLX.IPv4.Total_Length) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Total_Length is
     (RFLX.IPv4.Total_Length (Val))
    with
     Pre =>
       Valid_Total_Length (Val);

   type Identification is mod 2**16 with
     Size =>
       16;

   function Valid_Identification (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 65535);

   function To_U64 (Val : RFLX.IPv4.Identification) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Identification is
     (RFLX.IPv4.Identification (Val))
    with
     Pre =>
       Valid_Identification (Val);

   type Fragment_Offset is mod 2**13 with
     Size =>
       13;

   function Valid_Fragment_Offset (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 8191);

   function To_U64 (Val : RFLX.IPv4.Fragment_Offset) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Fragment_Offset is
     (RFLX.IPv4.Fragment_Offset (Val))
    with
     Pre =>
       Valid_Fragment_Offset (Val);

   type TTL is mod 2**8 with
     Size =>
       8;

   function Valid_TTL (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 255);

   function To_U64 (Val : RFLX.IPv4.TTL) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.TTL is
     (RFLX.IPv4.TTL (Val))
    with
     Pre =>
       Valid_TTL (Val);

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
               Raw : RFLX_Types.U64;
         end case;
      end record;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid_Protocol (Val : RFLX.RFLX_Types.U64) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_U64 (Enum : RFLX.IPv4.Protocol_Enum) return RFLX.RFLX_Types.U64 is
     ((case Enum is
          when P_ICMP =>
             1,
          when P_UDP =>
             17));

   function To_Actual (Enum : Protocol_Enum) return RFLX.IPv4.Protocol is
     ((True, Enum));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Protocol is
     ((case Val is
          when 1 =>
             (True, P_ICMP),
          when 17 =>
             (True, P_UDP),
          when others =>
             (False, Val)))
    with
     Pre =>
       Valid_Protocol (Val);

   function To_U64 (Val : RFLX.IPv4.Protocol) return RFLX.RFLX_Types.U64 is
     ((if Val.Known then To_U64 (Val.Enum) else Val.Raw));

   type Header_Checksum is mod 2**16 with
     Size =>
       16;

   function Valid_Header_Checksum (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 65535);

   function To_U64 (Val : RFLX.IPv4.Header_Checksum) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Header_Checksum is
     (RFLX.IPv4.Header_Checksum (Val))
    with
     Pre =>
       Valid_Header_Checksum (Val);

   type Address is mod 2**32 with
     Size =>
       32;

   function Valid_Address (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 4294967295);

   function To_U64 (Val : RFLX.IPv4.Address) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Address is
     (RFLX.IPv4.Address (Val))
    with
     Pre =>
       Valid_Address (Val);

   type Option_Class is (Control, Debugging_And_Measurement) with
     Size =>
       2;
   for Option_Class use (Control => 0, Debugging_And_Measurement => 2);

   function Valid_Option_Class (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val in 0 | 2);

   function To_U64 (Enum : RFLX.IPv4.Option_Class) return RFLX.RFLX_Types.U64 is
     ((case Enum is
          when Control =>
             0,
          when Debugging_And_Measurement =>
             2));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Option_Class is
     ((case Val is
          when 0 =>
             Control,
          when 2 =>
             Debugging_And_Measurement,
          when others =>
             RFLX.IPv4.Option_Class'Last))
    with
     Pre =>
       Valid_Option_Class (Val);

   pragma Warnings (On, "unreachable branch");

   type Option_Number is mod 2**5 with
     Size =>
       5;

   function Valid_Option_Number (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val <= 31);

   function To_U64 (Val : RFLX.IPv4.Option_Number) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Option_Number is
     (RFLX.IPv4.Option_Number (Val))
    with
     Pre =>
       Valid_Option_Number (Val);

   type Option_Length is range 2 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Option_Length (Val : RFLX.RFLX_Types.U64) return Boolean is
     (Val >= 2
      and Val <= 255);

   function To_U64 (Val : RFLX.IPv4.Option_Length) return RFLX.RFLX_Types.U64 is
     (RFLX.RFLX_Types.U64 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.U64) return RFLX.IPv4.Option_Length is
     (RFLX.IPv4.Option_Length (Val))
    with
     Pre =>
       Valid_Option_Length (Val);

end RFLX.IPv4;
