pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.Ethernet.Generic_Frame;
with RFLX.RFLX_Types;

package RFLX.Ethernet.Frame is new RFLX.Ethernet.Generic_Frame (RFLX.RFLX_Types);
