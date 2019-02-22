with Types; use Types;

package SPARK.File_IO
  with SPARK_Mode
is

   function Read_File (Name : String) return Bytes
     with Global => null;

end SPARK.File_IO;
