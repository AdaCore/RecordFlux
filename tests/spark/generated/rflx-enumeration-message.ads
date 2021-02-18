pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Enumeration.Generic_Message;

package RFLX.Enumeration.Message is new RFLX.Enumeration.Generic_Message (RFLX.RFLX_Types);
