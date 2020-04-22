pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
with RFLX.Arrays;
with RFLX.RFLX_Types;

package RFLX.Arrays.Range_Vector is new RFLX.RFLX_Scalar_Sequence (RFLX.RFLX_Types, Range_Integer, Range_Integer_Base, Valid, To_Actual, To_Base);
