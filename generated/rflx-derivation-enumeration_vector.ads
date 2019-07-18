pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.Enumeration_Vector is new Scalar_Sequence (Enumeration, Enumeration_Base, Convert, Valid, Convert);
