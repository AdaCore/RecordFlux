pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Expression.Generic_Message;

package RFLX.Expression.Message is new RFLX.Expression.Generic_Message (RFLX.RFLX_Types);
