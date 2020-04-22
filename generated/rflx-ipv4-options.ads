pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.IPv4.Option;
with RFLX.RFLX_Types;

package RFLX.IPv4.Options is new RFLX_Message_Sequence (RFLX.RFLX_Types, Option.Context, Option.Initialize, Option.Take_Buffer, Option.Has_Buffer, Option.Message_Last, Option.Initialized, Option.Structural_Valid_Message, Option.Valid_Context);
