pragma SPARK_Mode;
with Arrays.Generic_Message;
with Derivation.Modular_Vector;
with Derivation.Range_Vector;
with Derivation.Enumeration_Vector;
with Derivation.AV_Enumeration_Vector;

package Derivation.Message is new Arrays.Generic_Message (Modular_Vector, Range_Vector, Enumeration_Vector, AV_Enumeration_Vector);
