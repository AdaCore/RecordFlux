pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.Universal.Option;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.Universal.Options is new RFLX.RFLX_Message_Sequence (RFLX.Universal.Option.Context, RFLX.Universal.Option.Initialize, RFLX.Universal.Option.Take_Buffer, RFLX.Universal.Option.Has_Buffer, RFLX.Universal.Option.Message_Last, RFLX.Universal.Option.Initialized, RFLX.Universal.Option.Structural_Valid_Message);
