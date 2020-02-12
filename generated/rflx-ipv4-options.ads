pragma SPARK_Mode;
with RFLX.Message_Sequence;
with RFLX.IPv4.Option;
with RFLX.Types;

package RFLX.IPv4.Options is new Message_Sequence (RFLX.Types, Option.Context, Option.Initialize, Option.Take_Buffer, Option.Has_Buffer, Option.Message_Last, Option.Initialized, Option.Structural_Valid_Message, Option.Valid_Context);
