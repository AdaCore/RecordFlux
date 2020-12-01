pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.TLV.Generic_Message;
with RFLX.RFLX_Types;

package RFLX.Derivation.Message is new RFLX.TLV.Generic_Message (RFLX.RFLX_Types);
