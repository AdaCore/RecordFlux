pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;
with RFLX.Types;

package RFLX.Arrays.Enumeration_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Enumeration, Enumeration_Base, Valid, To_Actual, To_Base);
