with RFLX.Builtin_Types; use RFLX.Builtin_Types;

package SPARK.File_IO
  with SPARK_Mode
is

   function Read_File (Name : String) return Bytes with
     Global => null;

   function Read_File_Ptr (Name : String) return Bytes_Ptr with
     Global => null,
     Post => Read_File_Ptr'Result /= null and Read_File_Ptr'Result'Last <= RFLX.Builtin_Types.Index'Last / 2 and Read_File_Ptr'Result'Length > 0;

end SPARK.File_IO;
