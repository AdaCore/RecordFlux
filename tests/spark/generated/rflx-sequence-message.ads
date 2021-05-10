pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Sequence.Generic_Message;
with RFLX.Sequence.Modular_Vector;
with RFLX.Sequence.Range_Vector;
with RFLX.Sequence.Enumeration_Vector;
with RFLX.Sequence.AV_Enumeration_Vector;

package RFLX.Sequence.Message is new RFLX.Sequence.Generic_Message (RFLX.RFLX_Types, RFLX.Sequence.Modular_Vector, RFLX.Sequence.Range_Vector, RFLX.Sequence.Enumeration_Vector, RFLX.Sequence.AV_Enumeration_Vector);
