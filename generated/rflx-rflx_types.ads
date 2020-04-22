pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.RFLX_Generic_Types;
with RFLX.RFLX_Builtin_Types;

package RFLX.RFLX_Types is new RFLX.RFLX_Generic_Types (RFLX_Builtin_Types.Index, RFLX_Builtin_Types.Byte, RFLX_Builtin_Types.Bytes, RFLX_Builtin_Types.Bytes_Ptr, RFLX_Builtin_Types.Length, RFLX_Builtin_Types.Bit_Length);
