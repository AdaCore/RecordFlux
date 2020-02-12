pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;
with RFLX.Types;

package RFLX.Arrays.Range_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Range_Integer, Range_Integer_Base, Valid, Convert, Convert);
