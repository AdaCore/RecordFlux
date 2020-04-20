pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.Arrays.Inner_Message;
with RFLX.RFLX_Types;

package RFLX.Arrays.Inner_Messages is new RFLX_Message_Sequence (RFLX.RFLX_Types, Inner_Message.Context, Inner_Message.Initialize, Inner_Message.Take_Buffer, Inner_Message.Has_Buffer, Inner_Message.Message_Last, Inner_Message.Initialized, Inner_Message.Structural_Valid_Message, Inner_Message.Valid_Context);
