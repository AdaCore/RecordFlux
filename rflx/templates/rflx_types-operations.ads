pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma SPARK_Mode;
with {prefix}RFLX_Types.Operators;
with {prefix}RFLX_Generic_Types.Generic_Operations;

package {prefix}RFLX_Types.Operations is new {prefix}RFLX_Types.Generic_Operations ({prefix}RFLX_Types.Operators);
