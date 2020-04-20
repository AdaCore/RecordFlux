pragma SPARK_Mode;
with RFLX.Arrays.Generic_Message;
with RFLX.RFLX_Types;
with RFLX.Arrays.Modular_Vector;
with RFLX.Arrays.Range_Vector;
with RFLX.Arrays.Enumeration_Vector;
with RFLX.Arrays.AV_Enumeration_Vector;

package RFLX.Derivation.Message is new RFLX.Arrays.Generic_Message (RFLX.RFLX_Types, RFLX.Arrays.Modular_Vector, RFLX.Arrays.Range_Vector, RFLX.Arrays.Enumeration_Vector, RFLX.Arrays.AV_Enumeration_Vector);
