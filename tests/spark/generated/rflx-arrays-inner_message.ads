pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Arrays.Generic_Inner_Message;

package RFLX.Arrays.Inner_Message is new RFLX.Arrays.Generic_Inner_Message (RFLX.RFLX_Types);
