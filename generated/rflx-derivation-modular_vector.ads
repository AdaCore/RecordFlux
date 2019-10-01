pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.Modular_Vector is new Scalar_Sequence (Modular_Integer, Modular_Integer, Extract, Valid, Convert);
