with Ada.Directories;
with Ada.Sequential_IO;

package body SPARK.File_IO
  with SPARK_Mode => Off
is

   function Read_File (Name : String) return Bytes is
      package Byte_IO is new Ada.Sequential_IO (Byte);
      Input_File : Byte_IO.File_Type;
      Buffer : Bytes (1 .. Index_Type (Ada.Directories.Size (Name)));
      Value : Byte := 0;
      I : Length_Type := 0;
   begin
      Byte_IO.Open (Input_File, Byte_IO.In_File, Name);
      while not Byte_IO.End_Of_File (Input_File) loop
         I := I + 1;
         Byte_IO.Read (Input_File, Value);
         Buffer (I) := Value;
      end loop;
      Byte_IO.Close (Input_File);
      return Buffer;
   end Read_File;

end SPARK.File_IO;
