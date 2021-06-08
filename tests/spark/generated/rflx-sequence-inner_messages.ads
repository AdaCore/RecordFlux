pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.Sequence.Inner_Message;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.Sequence.Inner_Messages is new RFLX.RFLX_Message_Sequence (RFLX.Sequence.Inner_Message.Context, RFLX.Sequence.Inner_Message.Initialize, RFLX.Sequence.Inner_Message.Take_Buffer, RFLX.Sequence.Inner_Message.Has_Buffer, RFLX.Sequence.Inner_Message.Message_Last, RFLX.Sequence.Inner_Message.Initialized, RFLX.Sequence.Inner_Message.Structural_Valid_Message);
