pragma SPARK_Mode;
with RFLX.Message_Sequence;
with RFLX.IPv4.Option;

package RFLX.IPv4.Options is new Message_Sequence (Option.Context_Type, Option.Initialize, Option.Take_Buffer, Option.Has_Buffer, Option.Index, Option.Structural_Valid_Message, Option.Valid_Context);
