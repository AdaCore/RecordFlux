pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;
with RFLX.Types;

package RFLX.Derivation.Enumeration_Vector is new RFLX.Scalar_Sequence (RFLX.Types, Enumeration, Enumeration_Base, Valid, Convert, Convert);
