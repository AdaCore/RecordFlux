pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Ethernet.Generic_Frame;

package RFLX.Ethernet.Frame is new RFLX.Ethernet.Generic_Frame (RFLX.RFLX_Types);
