pragma SPARK_Mode;
with RFLX.Message_Sequence;
with RFLX.Arrays.Inner_Message;

package RFLX.Arrays.Inner_Messages is new Message_Sequence (Inner_Message.Context, Inner_Message.Initialize, Inner_Message.Take_Buffer, Inner_Message.Has_Buffer, Inner_Message.Index, Inner_Message.Structural_Valid_Message, Inner_Message.Valid_Context);
