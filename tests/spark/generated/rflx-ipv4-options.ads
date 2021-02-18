pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.RFLX_Message_Sequence;
with RFLX.IPv4.Option;

package RFLX.IPv4.Options is new RFLX.RFLX_Message_Sequence (RFLX.RFLX_Types, RFLX.IPv4.Option.Context, RFLX.IPv4.Option.Initialize, RFLX.IPv4.Option.Take_Buffer, RFLX.IPv4.Option.Has_Buffer, RFLX.IPv4.Option.Message_Last, RFLX.IPv4.Option.Initialized, RFLX.IPv4.Option.Structural_Valid_Message);
