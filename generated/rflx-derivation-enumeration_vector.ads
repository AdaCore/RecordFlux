pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.Enumeration_Vector is new RFLX.Scalar_Sequence (Enumeration, Enumeration_Base, Extract, Insert, Valid, Convert, Convert);
