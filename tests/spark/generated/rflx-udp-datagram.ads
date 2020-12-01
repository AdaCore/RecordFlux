pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.UDP.Generic_Datagram;
with RFLX.RFLX_Types;

package RFLX.UDP.Datagram is new RFLX.UDP.Generic_Datagram (RFLX.RFLX_Types);
