pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.Arrays.Generic_Messages_Message;
with RFLX.RFLX_Types;
with RFLX.Arrays.Inner_Messages;

package RFLX.Arrays.Messages_Message is new RFLX.Arrays.Generic_Messages_Message (RFLX.RFLX_Types, RFLX.Arrays.Inner_Messages);
