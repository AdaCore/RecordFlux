pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.Arrays.Generic_Array_Size_Defined_By_Message_Size;
with RFLX.RFLX_Types;
with RFLX.Arrays.Modular_Vector;

package RFLX.Arrays.Array_Size_Defined_By_Message_Size is new RFLX.Arrays.Generic_Array_Size_Defined_By_Message_Size (RFLX.RFLX_Types, RFLX.Arrays.Modular_Vector);
