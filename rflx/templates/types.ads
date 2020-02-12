pragma SPARK_Mode;
with {prefix}Generic_Types;
with {prefix}Builtin_Types;

package {prefix}Types is new {prefix}Generic_Types (Builtin_Types.Index, Builtin_Types.Byte, Builtin_Types.Bytes, Builtin_Types.Bytes_Ptr, Builtin_Types.Length, Builtin_Types.Bit_Length);
