with Ada.Directories;
with Ada.Sequential_IO;

package body SPARK.File_IO
  with SPARK_Mode => Off
is

   function Read_File (Name : String) return Bytes is
     (Read_File_Ptr (Name).all);

   function Read_File_Ptr (Name : String) return Bytes_Ptr is
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      package Byte_IO is new Ada.Sequential_IO (Byte);
      pragma Warnings (On, "this code can never be executed and has been deleted");
      Input_File : Byte_IO.File_Type;
      Buffer : constant Bytes_Ptr := new Bytes (1 .. Index (Ada.Directories.Size (Name)));
      Value : Byte := 0;
      I : Length := 0;
   begin
      Byte_IO.Open (Input_File, Byte_IO.In_File, Name);
      while not Byte_IO.End_Of_File (Input_File) loop
         I := I + 1;
         Byte_IO.Read (Input_File, Value);
         Buffer.all (I) := Value;
      end loop;
      Byte_IO.Close (Input_File);
      return Buffer;
   end Read_File_Ptr;

end SPARK.File_IO;
