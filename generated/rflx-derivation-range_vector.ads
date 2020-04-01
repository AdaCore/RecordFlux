pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;
with RFLX.Types;

package RFLX.Derivation.Range_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Range_Integer, Range_Integer_Base, Valid, To_Actual, To_Base);
