pragma SPARK_Mode;
with RFLX.Arrays.Generic_Message;
with RFLX.Types;
with RFLX.Arrays.Modular_Vector;
with RFLX.Arrays.Range_Vector;
with RFLX.Arrays.Enumeration_Vector;
with RFLX.Arrays.AV_Enumeration_Vector;

package RFLX.Arrays.Message is new RFLX.Arrays.Generic_Message (RFLX.Types, RFLX.Arrays.Modular_Vector, RFLX.Arrays.Range_Vector, RFLX.Arrays.Enumeration_Vector, RFLX.Arrays.AV_Enumeration_Vector);
