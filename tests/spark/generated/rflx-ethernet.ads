pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Ethernet with
  SPARK_Mode
is

   type Address is mod 2**48 with
     Size =>
       48;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Ethernet.Address) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Ethernet.Address) return RFLX.Ethernet.Address is
     (Val);

   function To_Actual (Val : RFLX.Ethernet.Address) return RFLX.Ethernet.Address is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Type_Length_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Type_Length is range 46 .. 2**16 - 1 with
     Size =>
       16;

   function Valid (Val : RFLX.Ethernet.Type_Length_Base) return Boolean is
     (Val >= 46);

   function To_Base (Val : RFLX.Ethernet.Type_Length) return RFLX.Ethernet.Type_Length_Base is
     (RFLX.Ethernet.Type_Length_Base (Val));

   function To_Actual (Val : RFLX.Ethernet.Type_Length_Base) return RFLX.Ethernet.Type_Length is
     (RFLX.Ethernet.Type_Length (Val))
    with
     Pre =>
       Valid (Val);

   type TPID_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type TPID is range 16#8100# .. 16#8100# with
     Size =>
       16;

   function Valid (Val : RFLX.Ethernet.TPID_Base) return Boolean is
     (Val = 16#8100#);

   function To_Base (Val : RFLX.Ethernet.TPID) return RFLX.Ethernet.TPID_Base is
     (RFLX.Ethernet.TPID_Base (Val));

   function To_Actual (Val : RFLX.Ethernet.TPID_Base) return RFLX.Ethernet.TPID is
     (RFLX.Ethernet.TPID (Val))
    with
     Pre =>
       Valid (Val);

   type TCI is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Ethernet.TCI) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Ethernet.TCI) return RFLX.Ethernet.TCI is
     (Val);

   function To_Actual (Val : RFLX.Ethernet.TCI) return RFLX.Ethernet.TCI is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.Ethernet;
