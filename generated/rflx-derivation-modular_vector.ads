pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.Modular_Vector is new RFLX.Scalar_Sequence (Modular_Integer, Modular_Integer, Extract, Insert, Valid, Convert, Convert);
