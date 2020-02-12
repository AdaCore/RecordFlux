pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;
with RFLX.Types;

package RFLX.Derivation.Modular_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Modular_Integer, Modular_Integer, Valid, Convert, Convert);
