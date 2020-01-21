pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Derivation;

package RFLX.Derivation.AV_Enumeration_Vector is new RFLX.Scalar_Sequence (AV_Enumeration, AV_Enumeration_Base, Extract, Insert, Valid, Convert, Convert);
