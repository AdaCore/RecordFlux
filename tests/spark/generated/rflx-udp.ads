pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.UDP with
  SPARK_Mode
is

   type Port is mod 2**16 with
     Size =>
       16;

   use type RFLX.RFLX_Types.S63;

   function Valid_Port (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val <= 65535);

   function To_S63 (Val : RFLX.UDP.Port) return RFLX.RFLX_Types.S63 is
     (RFLX.RFLX_Types.S63 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.UDP.Port is
     (RFLX.UDP.Port (Val))
    with
     Pre =>
       Valid_Port (Val);

   type Length is range 8 .. 2**16 - 1 with
     Size =>
       16;

   function Valid_Length (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val >= 8
      and Val <= 65535);

   function To_S63 (Val : RFLX.UDP.Length) return RFLX.RFLX_Types.S63 is
     (RFLX.RFLX_Types.S63 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.UDP.Length is
     (RFLX.UDP.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

   type Checksum is mod 2**16 with
     Size =>
       16;

   function Valid_Checksum (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val <= 65535);

   function To_S63 (Val : RFLX.UDP.Checksum) return RFLX.RFLX_Types.S63 is
     (RFLX.RFLX_Types.S63 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.UDP.Checksum is
     (RFLX.UDP.Checksum (Val))
    with
     Pre =>
       Valid_Checksum (Val);

end RFLX.UDP;
