with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.Ethernet with
  SPARK_Mode
is

   type Address is mod 2**48;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Ethernet_Address return Ethernet.Address is
     (Ethernet.Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Ethernet.Address) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Ethernet.Address) return Ethernet.Address is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Type_Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Type_Length is Type_Length_Base range 46 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Ethernet_Type_Length return Ethernet.Type_Length is
     (Ethernet.Type_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Ethernet.Type_Length_Base) return Boolean is
     (Val >= 46);

   function Convert (Val : Ethernet.Type_Length_Base) return Ethernet.Type_Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type TPID_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype TPID is TPID_Base range 16#8100# .. 16#8100#;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Ethernet_TPID return Ethernet.TPID is
     (Ethernet.TPID'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Ethernet.TPID_Base) return Boolean is
     (Val >= 16#8100#
      and Val <= 16#8100#);

   function Convert (Val : Ethernet.TPID_Base) return Ethernet.TPID is
     (Val)
    with
     Pre =>
       Valid (Val);

   type TCI is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Ethernet_TCI return Ethernet.TCI is
     (Ethernet.TCI'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Ethernet.TCI) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Ethernet.TCI) return Ethernet.TCI is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.Ethernet;
