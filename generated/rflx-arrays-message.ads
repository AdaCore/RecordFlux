pragma SPARK_Mode;
with RFLX.Arrays.Generic_Message;
with RFLX.Arrays.Modular_Vector;
with RFLX.Arrays.Range_Vector;
with RFLX.Arrays.Enumeration_Vector;
with RFLX.Arrays.AV_Enumeration_Vector;

package RFLX.Arrays.Message is new RFLX.Arrays.Generic_Message (Modular_Vector, Range_Vector, Enumeration_Vector, AV_Enumeration_Vector);
