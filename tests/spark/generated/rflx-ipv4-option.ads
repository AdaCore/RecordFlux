pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.IPv4.Generic_Option;

package RFLX.IPv4.Option is new RFLX.IPv4.Generic_Option (RFLX.RFLX_Types);
