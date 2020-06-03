pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package RFLX.UDP with
  SPARK_Mode
is

   type Port is mod 2**16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_UDP_Port return RFLX.UDP.Port is
     (RFLX.UDP.Port'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.UDP.Port) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.UDP.Port) return RFLX.UDP.Port is
     (Val);

   function To_Actual (Val : RFLX.UDP.Port) return RFLX.UDP.Port is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Length_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Length is range 8 .. 2**16 - 1 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_UDP_Length return RFLX.UDP.Length is
     (RFLX.UDP.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.UDP.Length_Base) return Boolean is
     (Val >= 8);

   function To_Base (Val : RFLX.UDP.Length) return RFLX.UDP.Length_Base is
     (RFLX.UDP.Length_Base (Val));

   function To_Actual (Val : RFLX.UDP.Length_Base) return RFLX.UDP.Length is
     (RFLX.UDP.Length (Val))
    with
     Pre =>
       Valid (Val);

   type Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_UDP_Checksum return RFLX.UDP.Checksum is
     (RFLX.UDP.Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.UDP.Checksum) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.UDP.Checksum) return RFLX.UDP.Checksum is
     (Val);

   function To_Actual (Val : RFLX.UDP.Checksum) return RFLX.UDP.Checksum is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.UDP;
