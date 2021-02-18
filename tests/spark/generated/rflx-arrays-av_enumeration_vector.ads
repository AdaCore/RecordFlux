pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.RFLX_Scalar_Sequence;
with RFLX.Arrays;

package RFLX.Arrays.AV_Enumeration_Vector is new RFLX.RFLX_Scalar_Sequence (RFLX.RFLX_Types, RFLX.Arrays.AV_Enumeration, RFLX.Arrays.AV_Enumeration_Base, RFLX.Arrays.Valid, RFLX.Arrays.To_Actual, RFLX.Arrays.To_Base);
