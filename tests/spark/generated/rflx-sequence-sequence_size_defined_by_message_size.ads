pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Sequence.Generic_Sequence_Size_Defined_By_Message_Size;
with RFLX.Sequence.Modular_Vector;

package RFLX.Sequence.Sequence_Size_Defined_By_Message_Size is new RFLX.Sequence.Generic_Sequence_Size_Defined_By_Message_Size (RFLX.RFLX_Types, RFLX.Sequence.Modular_Vector);
