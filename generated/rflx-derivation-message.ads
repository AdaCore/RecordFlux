pragma SPARK_Mode;
with RFLX.Arrays.Generic_Message;
with RFLX.Derivation.Modular_Vector;
with RFLX.Derivation.Range_Vector;
with RFLX.Derivation.Enumeration_Vector;
with RFLX.Derivation.AV_Enumeration_Vector;

package RFLX.Derivation.Message is new RFLX.Arrays.Generic_Message (Modular_Vector, Range_Vector, Enumeration_Vector, AV_Enumeration_Vector);
