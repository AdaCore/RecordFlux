pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX_Template.RFLX_Generic_Types;
with RFLX_Template.RFLX_Builtin_Types;

package RFLX_Template.RFLX_Types is new RFLX_Template.RFLX_Generic_Types (RFLX_Builtin_Types.Index, RFLX_Builtin_Types.Byte, RFLX_Builtin_Types.Bytes, RFLX_Builtin_Types.Bytes_Ptr, RFLX_Builtin_Types.Length, RFLX_Builtin_Types.Bit_Length);
