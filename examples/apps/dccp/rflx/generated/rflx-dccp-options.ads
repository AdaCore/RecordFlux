pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.DCCP.Option;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.DCCP.Options is new RFLX.RFLX_Message_Sequence (RFLX.DCCP.Option.Context, RFLX.DCCP.Option.Initialize, RFLX.DCCP.Option.Take_Buffer, RFLX.DCCP.Option.Copy, RFLX.DCCP.Option.Has_Buffer, RFLX.DCCP.Option.Size, RFLX.DCCP.Option.Message_Last, RFLX.DCCP.Option.Initialized, RFLX.DCCP.Option.Well_Formed_Message);
