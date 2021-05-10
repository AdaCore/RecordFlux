pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.Sequence.Generic_Messages_Message;
with RFLX.Sequence.Inner_Messages;

package RFLX.Sequence.Messages_Message is new RFLX.Sequence.Generic_Messages_Message (RFLX.RFLX_Types, RFLX.Sequence.Inner_Messages);
