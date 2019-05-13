pragma SPARK_Mode;
with Arrays.Generic_Message;
with Arrays.Modular_Vector;
with Arrays.Range_Vector;
with Arrays.Enumeration_Vector;
with Arrays.AV_Enumeration_Vector;

package Arrays.Message is new Arrays.Generic_Message (Modular_Vector, Range_Vector, Enumeration_Vector, AV_Enumeration_Vector);
