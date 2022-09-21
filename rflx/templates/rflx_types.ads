pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma SPARK_Mode;
with {prefix}RFLX_Generic_Types;
with {prefix}RFLX_Builtin_Types;

package {prefix}RFLX_Types is new {prefix}RFLX_Generic_Types (RFLX_Builtin_Types.Index, RFLX_Builtin_Types.Byte, RFLX_Builtin_Types.Bytes, RFLX_Builtin_Types.Bytes_Ptr, RFLX_Builtin_Types.Length, RFLX_Builtin_Types.Bit_Length);
