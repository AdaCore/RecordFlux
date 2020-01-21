pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.Range_Vector is new RFLX.Scalar_Sequence (Range_Integer, Range_Integer_Base, Extract, Insert, Valid, Convert, Convert);
