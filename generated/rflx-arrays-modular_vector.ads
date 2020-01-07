pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;

package RFLX.Arrays.Modular_Vector is new Scalar_Sequence (Modular_Integer, Modular_Integer, Extract, Insert, Valid, Convert, Convert);
