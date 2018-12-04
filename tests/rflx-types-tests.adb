with SPARK.Assertions; use SPARK.Assertions;

package body RFLX.Types.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Types");
   end Name;

   procedure Test_Index_Calculations (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
   begin
      Assert (First_Bit_Index (Index'First)'Img, " 1", "Invalid first bit index for Index'First");
      Assert (First_Bit_Index (Index'Last)'Img, " 17179869169", "Invalid first bit index for Index'Last");

      Assert (Last_Bit_Index (Index'First)'Img, " 8", "Invalid last bit index for Index'First");
      Assert (Last_Bit_Index (Index'Last)'Img, " 17179869176", "Invalid last bit index for Index'Last");

      Assert (Byte_Index (First_Bit_Index (Index'First))'Img, " 1", "Invalid conversion between byte index and first bit index");
      Assert (Byte_Index (First_Bit_Index (Index'Last))'Img, " 2147483647", "Invalid conversion between byte index and first bit index");

      Assert (Byte_Index (Last_Bit_Index (Index'First))'Img, " 1", "Invalid conversion between byte index and last bit index");
      Assert (Byte_Index (Last_Bit_Index (Index'Last))'Img, " 2147483647", "Invalid conversion between byte index and last bit index");

      Assert (Byte_Index (Bit_Index'First)'Img, " 1", "Invalid byte index for Bit_Index'First");
      Assert (Byte_Index (Bit_Index'Last)'Img, " 2147483647", "Invalid byte index for Bit_Index'Last");
   end Test_Index_Calculations;

   procedure Test_Convert_To_Mod (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (255, 255, 0);

      type UINT13 is mod 2**13;
      function Convert_To_UINT13 is new Convert_To_Mod (UINT13);
      R13 : UINT13;

      type UINT8 is mod 2**8;
      function Convert_To_UINT8 is new Convert_To_Mod (UINT8);
      R8 : UINT8;
   begin
      R13 := Convert_To_UINT13 (Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (R13'Image, UINT13'Image (7936), "Invalid conversion with offset 0");
      R13 := Convert_To_UINT13 (Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (R13'Image, UINT13'Image (8064), "Invalid conversion with offset 1");
      R13 := Convert_To_UINT13 (Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (R13'Image, UINT13'Image (8128), "Invalid conversion with offset 2");
      R13 := Convert_To_UINT13 (Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (R13'Image, UINT13'Image (8160), "Invalid conversion with offset 3");
      R13 := Convert_To_UINT13 (Buffer, 4);
      Assert (R13'Image, UINT13'Image (8176), "Invalid conversion with offset 4");
      R13 := Convert_To_UINT13 (Buffer, 5);
      Assert (R13'Image, UINT13'Image (8184), "Invalid conversion with offset 5");
      R13 := Convert_To_UINT13 (Buffer, 6);
      Assert (R13'Image, UINT13'Image (8188), "Invalid conversion with offset 6");
      R13 := Convert_To_UINT13 (Buffer, 7);
      Assert (R13'Image, UINT13'Image (8190), "Invalid conversion with offset 7");

      R8 := Convert_To_UINT8 (Buffer (Buffer'Last .. Buffer'Last), 0);
      Assert (R8'Image, UINT8'Image (0), "Invalid conversion with offset 0");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (R8'Image, UINT8'Image (128), "Invalid conversion with offset 1");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (R8'Image, UINT8'Image (192), "Invalid conversion with offset 2");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (R8'Image, UINT8'Image (224), "Invalid conversion with offset 3");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 4);
      Assert (R8'Image, UINT8'Image (240), "Invalid conversion with offset 4");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 6);
      Assert (R8'Image, UINT8'Image (252), "Invalid conversion with offset 6");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First + 1 .. Buffer'Last), 7);
      Assert (R8'Image, UINT8'Image (254), "Invalid conversion with offset 7");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First .. Buffer'First), 0);
      Assert (R8'Image, UINT8'Image (255), "Invalid conversion with offset 0");
   end Test_Convert_To_Mod;

   procedure Test_Convert_To_Int (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (255, 0);

      type UINT13 is range 0 .. 8191 with Size => 13;
      function Convert_To_UINT13 is new Convert_To_Int (UINT13);
      R13 : UINT13;

      type UINT8 is range 0 .. 255 with Size => 8;
      function Convert_To_UINT8 is new Convert_To_Int (UINT8);
      R8 : UINT8;
   begin
      R13 := Convert_To_UINT13 (Buffer, 0);
      Assert (R13'Image, UINT13'Image (7936), "Invalid conversion with offset 0");
      R13 := Convert_To_UINT13 (Buffer, 1);
      Assert (R13'Image, UINT13'Image (8064), "Invalid conversion with offset 1");
      R13 := Convert_To_UINT13 (Buffer, 2);
      Assert (R13'Image, UINT13'Image (8128), "Invalid conversion with offset 2");

      R8 := Convert_To_UINT8 (Buffer (Buffer'Last .. Buffer'Last), 0);
      Assert (R8'Image, UINT8'Image (0), "Invalid conversion with offset 0");
      R8 := Convert_To_UINT8 (Buffer, 1);
      Assert (R8'Image, UINT8'Image (128), "Invalid conversion with offset 1");
      R8 := Convert_To_UINT8 (Buffer, 2);
      Assert (R8'Image, UINT8'Image (192), "Invalid conversion with offset 2");
      R8 := Convert_To_UINT8 (Buffer, 3);
      Assert (R8'Image, UINT8'Image (224), "Invalid conversion with offset 3");
      R8 := Convert_To_UINT8 (Buffer, 4);
      Assert (R8'Image, UINT8'Image (240), "Invalid conversion with offset 4");
      R8 := Convert_To_UINT8 (Buffer, 6);
      Assert (R8'Image, UINT8'Image (252), "Invalid conversion with offset 6");
      R8 := Convert_To_UINT8 (Buffer, 7);
      Assert (R8'Image, UINT8'Image (254), "Invalid conversion with offset 7");
      R8 := Convert_To_UINT8 (Buffer (Buffer'First .. Buffer'First), 0);
      Assert (R8'Image, UINT8'Image (255), "Invalid conversion with offset 0");
   end Test_Convert_To_Int;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Index_Calculations'Access, "Index calculations");
      Register_Routine (T, Test_Convert_To_Mod'Access, "Convert to modular integer");
      Register_Routine (T, Test_Convert_To_Int'Access, "Convert to range integer");
   end Register_Tests;

end RFLX.Types.Tests;
