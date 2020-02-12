with SPARK.Assertions; use SPARK.Assertions;

with RFLX.Types; use RFLX.Types;

package body RFLX.Builtin_Types.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Builtin_Types");
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

   generic
      type Offset_Type is (<>);
   function Identity (X : Offset_Type) return Offset_Type;

   function Identity (X : Offset_Type) return Offset_Type is
      (X);

   --  Simulate an offset value that is determined at runtime.
   --  This prevents the false assumption that the offset is statically determined at compile time,
   --  which could affect the ability to prove the precondition of the Extract function.
   function Dynamic_Offset is new Identity (Offset);

   procedure Test_Extract_Modular_Integer (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (255, 255, 0);

      type U13 is mod 2**13;
      function Extract_U13 is new Extract (U13);
      R13 : U13;

      type U8 is mod 2**8;
      function Extract_U8 is new Extract (U8);
      R8 : U8;
   begin
      R13 := Extract_U13 (Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (R13'Image, U13'Image (7936), "Invalid conversion with offset 0");
      R13 := Extract_U13 (Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (R13'Image, U13'Image (8064), "Invalid conversion with offset 1");
      R13 := Extract_U13 (Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (R13'Image, U13'Image (8128), "Invalid conversion with offset 2");
      R13 := Extract_U13 (Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (R13'Image, U13'Image (8160), "Invalid conversion with offset 3");
      R13 := Extract_U13 (Buffer, Dynamic_Offset (4));
      Assert (R13'Image, U13'Image (8176), "Invalid conversion with offset 4");
      R13 := Extract_U13 (Buffer, Dynamic_Offset (5));
      Assert (R13'Image, U13'Image (8184), "Invalid conversion with offset 5");
      R13 := Extract_U13 (Buffer, Dynamic_Offset (6));
      Assert (R13'Image, U13'Image (8188), "Invalid conversion with offset 6");
      R13 := Extract_U13 (Buffer, Dynamic_Offset (7));
      Assert (R13'Image, U13'Image (8190), "Invalid conversion with offset 7");

      R8 := Extract_U8 (Buffer (Buffer'Last .. Buffer'Last), 0);
      Assert (R8'Image, U8'Image (0), "Invalid conversion with offset 0");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (R8'Image, U8'Image (128), "Invalid conversion with offset 1");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (R8'Image, U8'Image (192), "Invalid conversion with offset 2");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (R8'Image, U8'Image (224), "Invalid conversion with offset 3");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 4);
      Assert (R8'Image, U8'Image (240), "Invalid conversion with offset 4");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 6);
      Assert (R8'Image, U8'Image (252), "Invalid conversion with offset 6");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), 7);
      Assert (R8'Image, U8'Image (254), "Invalid conversion with offset 7");
      R8 := Extract_U8 (Buffer (Buffer'First .. Buffer'First), 0);
      Assert (R8'Image, U8'Image (255), "Invalid conversion with offset 0");
   end Test_Extract_Modular_Integer;

   procedure Test_Extract_Range_Integer (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (255, 0);

      type U13 is range 0 .. 8191 with Size => 13;
      function Extract_U13 is new Extract (U13);
      R13 : U13;

      type U8 is range 0 .. 255 with Size => 8;
      function Extract_U8 is new Extract (U8);
      R8 : U8;
   begin
      R13 := Extract_U13 (Buffer, 0);
      Assert (R13'Image, U13'Image (7936), "Invalid conversion with offset 0");
      R13 := Extract_U13 (Buffer, 1);
      Assert (R13'Image, U13'Image (8064), "Invalid conversion with offset 1");
      R13 := Extract_U13 (Buffer, 2);
      Assert (R13'Image, U13'Image (8128), "Invalid conversion with offset 2");

      R8 := Extract_U8 (Buffer (Buffer'Last .. Buffer'Last), 0);
      Assert (R8'Image, U8'Image (0), "Invalid conversion with offset 0");
      R8 := Extract_U8 (Buffer, 1);
      Assert (R8'Image, U8'Image (128), "Invalid conversion with offset 1");
      R8 := Extract_U8 (Buffer, 2);
      Assert (R8'Image, U8'Image (192), "Invalid conversion with offset 2");
      R8 := Extract_U8 (Buffer, 3);
      Assert (R8'Image, U8'Image (224), "Invalid conversion with offset 3");
      R8 := Extract_U8 (Buffer, 4);
      Assert (R8'Image, U8'Image (240), "Invalid conversion with offset 4");
      R8 := Extract_U8 (Buffer, 6);
      Assert (R8'Image, U8'Image (252), "Invalid conversion with offset 6");
      R8 := Extract_U8 (Buffer, 7);
      Assert (R8'Image, U8'Image (254), "Invalid conversion with offset 7");
      R8 := Extract_U8 (Buffer (Buffer'First .. Buffer'First), 0);
      Assert (R8'Image, U8'Image (255), "Invalid conversion with offset 0");
   end Test_Extract_Range_Integer;

   procedure Test_Insert_Modular_Integer (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type U2 is mod 2**2;
      procedure Insert_U2 is new Insert (U2);

      type U13 is mod 2**13;
      procedure Insert_U13 is new Insert (U13);

      Buffer : Bytes (Index'First .. Index'First + 2);
   begin
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 0);
      Assert (Buffer, (1, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 1);
      Assert (Buffer, (2, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 2);
      Assert (Buffer, (4, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 3);
      Assert (Buffer, (8, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 4);
      Assert (Buffer, (16, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 5);
      Assert (Buffer, (32, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 6);
      Assert (Buffer, (64, 0, 0), "Invalid insertion of U2 in zero-initialized buffer with offset 6");

      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 0);
      Assert (Buffer, (254, 255, 255), "Invalid insertion of U2 in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 1);
      Assert (Buffer, (253, 255, 255), "Invalid insertion of U2 in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 2);
      Assert (Buffer, (251, 255, 255), "Invalid insertion of U2 in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 3);
      Assert (Buffer, (247, 255, 255), "Invalid insertion of U2 in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 4);
      Assert (Buffer, (239, 255, 255), "Invalid insertion of U2 in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 5);
      Assert (Buffer, (223, 255, 255), "Invalid insertion of U2 in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 6);
      Assert (Buffer, (191, 255, 255), "Invalid insertion of U2 in filled buffer with offset 6");

      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (0, 21, 85), "Invalid insertion of U13 in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (Buffer, (0, 42, 170), "Invalid insertion of U13 in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (Buffer, (0, 85, 84), "Invalid insertion of U13 in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (Buffer, (0, 170, 168), "Invalid insertion of U13 in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (1, 85, 80), "Invalid insertion of U13 in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (2, 170, 160), "Invalid insertion of U13 in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (5, 85, 64), "Invalid insertion of U13 in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (10, 170, 128), "Invalid insertion of U13 in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (255, 245, 85), "Invalid insertion of U13 in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (Buffer, (255, 234, 171), "Invalid insertion of U13 in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (Buffer, (255, 213, 87), "Invalid insertion of U13 in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (Buffer, (255, 170, 175), "Invalid insertion of U13 in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (255, 85, 95), "Invalid insertion of U13 in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (254, 170, 191), "Invalid insertion of U13 in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (253, 85, 127), "Invalid insertion of U13 in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (250, 170, 255), "Invalid insertion of U13 in filled buffer with offset 7");
   end Test_Insert_Modular_Integer;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Index_Calculations'Access, "Index calculations");
      Register_Routine (T, Test_Extract_Modular_Integer'Access, "Extract modular integer");
      Register_Routine (T, Test_Extract_Range_Integer'Access, "Extract range integer");
      Register_Routine (T, Test_Insert_Modular_Integer'Access, "Insert modular integer");
   end Register_Tests;

end RFLX.Builtin_Types.Tests;
