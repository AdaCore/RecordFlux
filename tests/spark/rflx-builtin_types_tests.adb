with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types; use RFLX.RFLX_Types;

package body RFLX.Builtin_Types_Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Builtin_Types");
   end Name;

   procedure Test_Index_Calculations (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
   begin
      Assert (First_Bit_Index (Index'First)'Img, " 1", "Invalid first bit index for Index'First");
      Assert (First_Bit_Index (Index'Last)'Img, " 17179869169", "Invalid first bit index for Index'Last");

      Assert (Last_Bit_Index (Index'First)'Img, " 8", "Invalid last bit index for Index'First");
      Assert (Last_Bit_Index (Index'Last)'Img, " 17179869176", "Invalid last bit index for Index'Last");

      Assert (Byte_Index (First_Bit_Index (Index'First))'Img, " 1",
              "Invalid conversion between byte index and first bit index");
      Assert (Byte_Index (First_Bit_Index (Index'Last))'Img, " 2147483647",
              "Invalid conversion between byte index and first bit index");

      Assert (Byte_Index (Last_Bit_Index (Index'First))'Img, " 1",
              "Invalid conversion between byte index and last bit index");
      Assert (Byte_Index (Last_Bit_Index (Index'Last))'Img, " 2147483647",
              "Invalid conversion between byte index and last bit index");

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

   procedure Test_Extract_Modular_Integer_1 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (Index'First => 170);

      type U1 is mod 2;
      function Extract_U1 is new Extract (U1);
      R1 : U1;
   begin
      R1 := Extract_U1 (Buffer, Dynamic_Offset (0));
      Assert (R1'Image, U1'Image (0), "Invalid conversion with offset 0");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (1));
      Assert (R1'Image, U1'Image (1), "Invalid conversion with offset 1");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (2));
      Assert (R1'Image, U1'Image (0), "Invalid conversion with offset 2");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (3));
      Assert (R1'Image, U1'Image (1), "Invalid conversion with offset 3");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (4));
      Assert (R1'Image, U1'Image (0), "Invalid conversion with offset 4");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (5));
      Assert (R1'Image, U1'Image (1), "Invalid conversion with offset 5");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (6));
      Assert (R1'Image, U1'Image (0), "Invalid conversion with offset 6");
      R1 := Extract_U1 (Buffer, Dynamic_Offset (7));
      Assert (R1'Image, U1'Image (1), "Invalid conversion with offset 7");
   end Test_Extract_Modular_Integer_1;

   procedure Test_Extract_Modular_Integer_8 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      Buffer : constant Bytes := (255, 255, 0);

      type U8 is mod 2**8;
      function Extract_U8 is new Extract (U8);
      R8 : U8;
   begin
      R8 := Extract_U8 (Buffer (Buffer'Last .. Buffer'Last), 0);
      Assert (R8'Image, U8'Image (0), "Invalid conversion with offset 0");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (1));
      Assert (R8'Image, U8'Image (128), "Invalid conversion with offset 1");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (2));
      Assert (R8'Image, U8'Image (192), "Invalid conversion with offset 2");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (3));
      Assert (R8'Image, U8'Image (224), "Invalid conversion with offset 3");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (4));
      Assert (R8'Image, U8'Image (240), "Invalid conversion with offset 4");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (5));
      Assert (R8'Image, U8'Image (248), "Invalid conversion with offset 5");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (6));
      Assert (R8'Image, U8'Image (252), "Invalid conversion with offset 6");
      R8 := Extract_U8 (Buffer (Buffer'First + 1 .. Buffer'Last), Dynamic_Offset (7));
      Assert (R8'Image, U8'Image (254), "Invalid conversion with offset 7");
      R8 := Extract_U8 (Buffer (Buffer'First .. Buffer'First), 0);
      Assert (R8'Image, U8'Image (255), "Invalid conversion with offset 0");
   end Test_Extract_Modular_Integer_8;

   procedure Test_Extract_Modular_Integer_13 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 0);

      type U13 is mod 2**13;
      function Extract_U13 is new Extract (U13);
      R13 : U13;
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
   end Test_Extract_Modular_Integer_13;

   procedure Test_Extract_Modular_Integer_62 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);

      type U62 is mod 2**62;
      function Extract_U62 is new Extract (U62);
      R62 : U62;
   begin
      R62 := Extract_U62 (Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (R62'Image, U62'Image (4611686018427387648), "Invalid conversion with offset 0");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (1));
      Assert (R62'Image, U62'Image (4611686018427387776), "Invalid conversion with offset 1");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (2));
      Assert (R62'Image, U62'Image (4611686018427387840), "Invalid conversion with offset 2");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (3));
      Assert (R62'Image, U62'Image (4611686018427387872), "Invalid conversion with offset 3");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (4));
      Assert (R62'Image, U62'Image (4611686018427387888), "Invalid conversion with offset 4");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (5));
      Assert (R62'Image, U62'Image (4611686018427387896), "Invalid conversion with offset 5");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (6));
      Assert (R62'Image, U62'Image (4611686018427387900), "Invalid conversion with offset 6");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (7));
      Assert (R62'Image, U62'Image (4611686018427387902), "Invalid conversion with offset 7");
      R62 := Extract_U62 (Buffer (Buffer'First .. Buffer'Last - 1), 0);
      Assert (R62'Image, U62'Image (4611686018427387903), "Invalid conversion with offset 0");
   end Test_Extract_Modular_Integer_62;

   procedure Test_Extract_Modular_Integer_64 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);

      type U64 is mod 2**64 with
        Annotate => (GNATprove, No_Wrap_Around);
      function Extract_U64 is new Extract (U64);
      R64 : U64;
   begin
      R64 := Extract_U64 (Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (R64'Image, U64'Image (18446744073709551360), "Invalid conversion with offset 0");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (1));
      Assert (R64'Image, U64'Image (18446744073709551488), "Invalid conversion with offset 1");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (2));
      Assert (R64'Image, U64'Image (18446744073709551552), "Invalid conversion with offset 2");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (3));
      Assert (R64'Image, U64'Image (18446744073709551584), "Invalid conversion with offset 3");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (4));
      Assert (R64'Image, U64'Image (18446744073709551600), "Invalid conversion with offset 4");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (5));
      Assert (R64'Image, U64'Image (18446744073709551608), "Invalid conversion with offset 5");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (6));
      Assert (R64'Image, U64'Image (18446744073709551612), "Invalid conversion with offset 6");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last), Dynamic_Offset (7));
      Assert (R64'Image, U64'Image (18446744073709551614), "Invalid conversion with offset 7");
      R64 := Extract_U64 (Buffer (Buffer'First .. Buffer'Last - 1), 0);
      Assert (R64'Image, U64'Image (18446744073709551615), "Invalid conversion with offset 0");
   end Test_Extract_Modular_Integer_64;

   procedure Test_Insert_Modular_Integer_1 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type U1 is mod 2;
      procedure Insert_U1 is new Insert (U1);

      Buffer : Bytes (Index'First .. Index'First + 2);
   begin
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (0));
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (1));
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (2));
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (3));
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (4));
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (5));
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (6));
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      Insert_U1 (1, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (7));
      Assert (Buffer, (128, 0, 0), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (0));
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (1));
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (2));
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (3));
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (4));
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (5));
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (6));
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      Insert_U1 (0, Buffer (Buffer'First .. Buffer'First), Dynamic_Offset (7));
      Assert (Buffer, (127, 255, 255), "Invalid insertion in filled buffer with offset 7");
   end Test_Insert_Modular_Integer_1;

   procedure Test_Insert_Modular_Integer_2 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type U2 is mod 2**2;
      procedure Insert_U2 is new Insert (U2);

      Buffer : Bytes (Index'First .. Index'First + 2);
   begin
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 0);
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 1);
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 2);
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 3);
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 4);
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 5);
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      Insert_U2 (1, Buffer (Buffer'First .. Buffer'First), 6);
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");

      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 0);
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 1);
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 2);
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 3);
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 4);
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 5);
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      Insert_U2 (2, Buffer (Buffer'First .. Buffer'First), 6);
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");
   end Test_Insert_Modular_Integer_2;

   procedure Test_Insert_Modular_Integer_13 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type U13 is mod 2**13;
      procedure Insert_U13 is new Insert (U13);

      Buffer : Bytes (Index'First .. Index'First + 2);
   begin
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (0, 21, 85), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (Buffer, (0, 42, 170), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (Buffer, (0, 85, 84), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (Buffer, (0, 170, 168), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (1, 85, 80), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (2, 170, 160), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (5, 85, 64), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      Insert_U13 (5461, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (10, 170, 128), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (255, 245, 85), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 1);
      Assert (Buffer, (255, 234, 171), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 2);
      Assert (Buffer, (255, 213, 87), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer (Buffer'First + 1 .. Buffer'Last), 3);
      Assert (Buffer, (255, 170, 175), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (255, 85, 95), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (254, 170, 191), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (253, 85, 127), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      Insert_U13 (5461, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (250, 170, 255), "Invalid insertion in filled buffer with offset 7");
   end Test_Insert_Modular_Integer_13;

   procedure Test_Insert_Modular_Integer_64 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type U64 is mod 2**64 with
        Annotate => (GNATprove, No_Wrap_Around);
      procedure Insert_U64 is new Insert (U64);

      Buffer : Bytes (Index'First .. Index'First + 8);
   begin
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (0, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (1));
      Assert (Buffer, (1, 85, 85, 85, 85, 85, 85, 85, 84),
              "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (2));
      Assert (Buffer, (2, 170, 170, 170, 170, 170, 170, 170, 168),
              "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (3));
      Assert (Buffer, (5, 85, 85, 85, 85, 85, 85, 85, 80),
              "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (10, 170, 170, 170, 170, 170, 170, 170, 160),
              "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (21, 85, 85, 85, 85, 85, 85, 85, 64),
              "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (42, 170, 170, 170, 170, 170, 170, 170, 128),
              "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (85, 85, 85, 85, 85, 85, 85, 85, 0),
              "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer (Buffer'First + 1 .. Buffer'Last), 0);
      Assert (Buffer, (255, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (1));
      Assert (Buffer, (255, 85, 85, 85, 85, 85, 85, 85, 85),
              "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (2));
      Assert (Buffer, (254, 170, 170, 170, 170, 170, 170, 170, 171),
              "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (3));
      Assert (Buffer, (253, 85, 85, 85, 85, 85, 85, 85, 87),
              "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (4));
      Assert (Buffer, (250, 170, 170, 170, 170, 170, 170, 170, 175),
              "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (5));
      Assert (Buffer, (245, 85, 85, 85, 85, 85, 85, 85, 95),
              "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (6));
      Assert (Buffer, (234, 170, 170, 170, 170, 170, 170, 170, 191),
              "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      Insert_U64 (12297829382473034410, Buffer, Dynamic_Offset (7));
      Assert (Buffer, (213, 85, 85, 85, 85, 85, 85, 85, 127),
              "Invalid insertion in filled buffer with offset 7");
   end Test_Insert_Modular_Integer_64;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Index_Calculations'Access, "Index calculations");
      Register_Routine (T, Test_Extract_Modular_Integer_1'Access, "Extract modular integer (1 bit)");
      Register_Routine (T, Test_Extract_Modular_Integer_8'Access, "Extract modular integer (8 bit)");
      Register_Routine (T, Test_Extract_Modular_Integer_13'Access, "Extract modular integer (13 bit)");
      Register_Routine (T, Test_Extract_Modular_Integer_62'Access, "Extract modular integer (62 bit)");
      Register_Routine (T, Test_Extract_Modular_Integer_64'Access, "Extract modular integer (64 bit)");
      Register_Routine (T, Test_Insert_Modular_Integer_1'Access, "Insert modular integer (1 bit)");
      Register_Routine (T, Test_Insert_Modular_Integer_2'Access, "Insert modular integer (2 bit)");
      Register_Routine (T, Test_Insert_Modular_Integer_13'Access, "Insert modular integer (13 bit)");
      Register_Routine (T, Test_Insert_Modular_Integer_64'Access, "Insert modular integer (64 bit)");
   end Register_Tests;

end RFLX.Builtin_Types_Tests;
