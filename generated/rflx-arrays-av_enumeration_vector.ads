pragma SPARK_Mode;
with RFLX.Scalar_Sequence;
with RFLX.Arrays;
with RFLX.Types;

package RFLX.Arrays.AV_Enumeration_Vector is new RFLX.Scalar_Sequence (RFLX.Types, AV_Enumeration, AV_Enumeration_Base, Valid, Convert, Convert);
