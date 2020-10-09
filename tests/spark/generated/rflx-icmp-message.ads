pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.ICMP.Generic_Message;
with RFLX.RFLX_Types;

package RFLX.ICMP.Message is new RFLX.ICMP.Generic_Message (RFLX.RFLX_Types);
