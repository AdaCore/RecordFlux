pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Sequence.Generic_Inner_Message;

package RFLX.Sequence.Inner_Message is new RFLX.Sequence.Generic_Inner_Message (RFLX.RFLX_Types);
