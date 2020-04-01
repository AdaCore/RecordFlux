pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;
with RFLX.Types;

package RFLX.Arrays.Modular_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Modular_Integer, Modular_Integer, Valid, To_Actual, To_Base);
