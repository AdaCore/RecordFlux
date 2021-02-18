pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.ICMP.Generic_Message;

package RFLX.ICMP.Message is new RFLX.ICMP.Generic_Message (RFLX.RFLX_Types);
