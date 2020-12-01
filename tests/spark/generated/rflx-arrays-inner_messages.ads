pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.Arrays.Inner_Message;
with RFLX.RFLX_Types;

package RFLX.Arrays.Inner_Messages is new RFLX.RFLX_Message_Sequence (RFLX.RFLX_Types, RFLX.Arrays.Inner_Message.Context, RFLX.Arrays.Inner_Message.Initialize, RFLX.Arrays.Inner_Message.Take_Buffer, RFLX.Arrays.Inner_Message.Has_Buffer, RFLX.Arrays.Inner_Message.Message_Last, RFLX.Arrays.Inner_Message.Initialized, RFLX.Arrays.Inner_Message.Structural_Valid_Message);
