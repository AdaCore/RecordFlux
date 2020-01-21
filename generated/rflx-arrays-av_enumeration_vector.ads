pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;

package RFLX.Arrays.AV_Enumeration_Vector is new RFLX.Scalar_Sequence (AV_Enumeration, AV_Enumeration_Base, Extract, Insert, Valid, Convert, Convert);
