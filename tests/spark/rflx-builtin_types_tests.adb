with Ada.Numerics.Discrete_Random;
with GNAT.Byte_Swapping;

with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;
with RFLX.RFLX_Types.Operations;

package body RFLX.Builtin_Types_Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
   begin
      pragma Unreferenced (T);
      return AUnit.Format ("Builtin_Types");
   end Name;

   procedure Test_Index_Calculations (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
   begin
      Assert (RFLX_Types.To_First_Bit_Index (Index'First)'Img, " 1",
              "Invalid first bit index for Index'First");
      Assert (RFLX_Types.To_First_Bit_Index (Index'Last)'Img, " 17179869169",
              "Invalid first bit index for Index'Last");

      Assert (RFLX_Types.To_Last_Bit_Index (Index'First)'Img, " 8",
              "Invalid last bit index for Index'First");
      Assert (RFLX_Types.To_Last_Bit_Index (Index'Last)'Img, " 17179869176",
              "Invalid last bit index for Index'Last");

      Assert (RFLX_Types.To_Index (RFLX_Types.To_First_Bit_Index (Index'First))'Img, " 1",
              "Invalid conversion between byte index and first bit index");
      Assert (RFLX_Types.To_Index (RFLX_Types.To_First_Bit_Index (Index'Last))'Img, " 2147483647",
              "Invalid conversion between byte index and first bit index");

      Assert (RFLX_Types.To_Index (RFLX_Types.To_Last_Bit_Index (Index'First))'Img, " 1",
              "Invalid conversion between byte index and last bit index");
      Assert (RFLX_Types.To_Index (RFLX_Types.To_Last_Bit_Index (Index'Last))'Img, " 2147483647",
              "Invalid conversion between byte index and last bit index");

      Assert (RFLX_Types.To_Index (RFLX_Types.Bit_Index'First)'Img, " 1",
              "Invalid byte index for Bit_Index'First");
      Assert (RFLX_Types.To_Index (RFLX_Types.Bit_Index'Last)'Img, " 2147483647",
              "Invalid byte index for Bit_Index'Last");
   end Test_Index_Calculations;

   procedure Test_Length_Calculations (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
   begin
      Assert (RFLX_Types.To_Length (0)'Img, " 0", "Invalid conversion of 0");
      Assert (RFLX_Types.To_Length (1)'Img, " 1", "Invalid conversion of 1");
      Assert (RFLX_Types.To_Length (8)'Img, " 1", "Invalid conversion of 8");
      Assert (RFLX_Types.To_Length (9)'Img, " 2", "Invalid conversion of 9");
      Assert (RFLX_Types.To_Length (16)'Img, " 2", "Invalid conversion of 16");
   end Test_Length_Calculations;

   generic
      type Offset_Type is (<>);
   function Identity (X : Offset_Type) return Offset_Type;

   function Identity (X : Offset_Type) return Offset_Type is
      (X);

   -- Simulate an offset value that is determined at runtime.
   -- This prevents the false assumption that the offset is statically determined at compile time,
   -- which could affect the ability to prove the precondition of the RFLX_Types.Extract function.
   function Dynamic_Offset is new Identity (RFLX_Types.Offset);

   procedure Test_Extract_Modular_Integer_1 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (Index'First => 170);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (0), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 1,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 7");

   end Test_Extract_Modular_Integer_1;

   procedure Test_Extract_Modular_Integer_8 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'Last, Buffer'Last, 0, 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (1), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (128), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (2), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (192), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (3), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (224), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (4), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (240), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (5), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (248), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (6), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (252), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (7), 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (254), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'First, 0, 8,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (255), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_8;

   procedure Test_Extract_Modular_Integer_13 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (7936), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 1, 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8064), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 2, 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8128), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 3, 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8160), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8176), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8184), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8188), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 13,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (8190), "Invalid conversion with offset 7");

   end Test_Extract_Modular_Integer_13;

   procedure Test_Extract_Modular_Integer_62 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387648), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387776), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387840), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387872), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387888), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387896), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387900), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387902), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last - 1, 0, 62,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387903), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_62;

   procedure Test_Extract_Modular_Integer_64 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551360), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551488), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551552), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551584), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551600), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551608), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551612), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551614), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last - 1, 0, 64,
                                          RFLX_Types.High_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551615), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_64;

   procedure Test_Extract_Modular_Integer_1_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (Index'First => 170);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (0), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 1,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 7");

   end Test_Extract_Modular_Integer_1_LE;

   procedure Test_Extract_Modular_Integer_8_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'Last, Buffer'Last, 0, 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (0), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (1), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (2), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (3), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (3), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (7), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (4), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (15), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (5), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (31), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (6), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (63), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, Dynamic_Offset (7), 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (127), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'First, 0, 8,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (255), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_8_LE;

   procedure Test_Extract_Modular_Integer_13_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (31), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 1, 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (63), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 2, 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (127), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 3, 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (255), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (511), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1023), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (2047), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 13,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4095), "Invalid conversion with offset 7");

   end Test_Extract_Modular_Integer_13_LE;

   procedure Test_Extract_Modular_Integer_62_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18014398509481983), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (36028797018963967), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (72057594037927935), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (144115188075855871), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (288230376151711743), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (576460752303423487), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1152921504606846975), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (2305843009213693951), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last - 1, 0, 62,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387903), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_62_LE;

   procedure Test_Extract_Modular_Integer_64_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : constant Bytes := (255, 255, 255, 255, 255, 255, 255, 255, 0);
      R : RFLX_Types.U64;
   begin
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First + 1, Buffer'Last, 0, 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (72057594037927935), "Invalid conversion with offset 0");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (1), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (144115188075855871), "Invalid conversion with offset 1");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (2), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (288230376151711743), "Invalid conversion with offset 2");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (3), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (576460752303423487), "Invalid conversion with offset 3");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (4), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (1152921504606846975), "Invalid conversion with offset 4");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (5), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (2305843009213693951), "Invalid conversion with offset 5");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (6), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (4611686018427387903), "Invalid conversion with offset 6");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, Dynamic_Offset (7), 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (9223372036854775807), "Invalid conversion with offset 7");
      R := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last - 1, 0, 64,
                                          RFLX_Types.Low_Order_First);
      Assert (R'Image, RFLX_Types.U64'Image (18446744073709551615), "Invalid conversion with offset 0");

   end Test_Extract_Modular_Integer_64_LE;

   procedure Test_Insert_Modular_Integer_1 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (0), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (1), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (2), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (3), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (4), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (5), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (6), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (7), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (128, 0, 0), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (0), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (1), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (2), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (3), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (4), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (5), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (6), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (7), 1, RFLX_Types.High_Order_First);
      Assert (Buffer, (127, 255, 255), "Invalid insertion in filled buffer with offset 7");

   end Test_Insert_Modular_Integer_1;

   procedure Test_Insert_Modular_Integer_2 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 0, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 1, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 2, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 3, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 4, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 5, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First,
                                    Buffer'First, 6, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 0, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 1, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 2, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 3, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 4, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 5, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First,
                                    Buffer'First, 6, 2, RFLX_Types.High_Order_First);
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");

   end Test_Insert_Modular_Integer_2;

   procedure Test_Insert_Modular_Integer_13 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 0, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 21, 85), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 1, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 42, 170), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 2, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 85, 84), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 3, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 170, 168), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (4), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (1, 85, 80), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (5), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (2, 170, 160), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (6), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (5, 85, 64), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (7), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (10, 170, 128), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 0, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 245, 85), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 1, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 234, 171), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 2, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 213, 87), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1,
                                    Buffer'Last, 3, 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 170, 175), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (4), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 85, 95), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (5), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (254, 170, 191), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (6), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (253, 85, 127), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (7), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (250, 170, 255), "Invalid insertion in filled buffer with offset 7");

      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (0), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 255), "Invalid insertion of 0000011111111 with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (1), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 1, 254), "Invalid insertion of 0000011111111 with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (2), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 3, 252), "Invalid insertion of 0000011111111 with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (3), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 7, 248), "Invalid insertion of 0000011111111 with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (4), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 15, 240), "Invalid insertion of 0000011111111 with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (5), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 31, 224), "Invalid insertion of 0000011111111 with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (6), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 63, 192), "Invalid insertion of 0000011111111 with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First,
                                    Buffer'Last, Dynamic_Offset (7), 13, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 127, 128), "Invalid insertion of 0000011111111 with offset 7");

   end Test_Insert_Modular_Integer_13;

   procedure Test_Insert_Modular_Integer_64 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 8 => 0);
   begin
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer, Buffer'First + 1, Buffer'Last,
                                    0, 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (1), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (1, 85, 85, 85, 85, 85, 85, 85, 84),
              "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (2), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (2, 170, 170, 170, 170, 170, 170, 170, 168),
              "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (3), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (5, 85, 85, 85, 85, 85, 85, 85, 80),
              "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (4), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (10, 170, 170, 170, 170, 170, 170, 170, 160),
              "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (5), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (21, 85, 85, 85, 85, 85, 85, 85, 64),
              "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (6), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (42, 170, 170, 170, 170, 170, 170, 170, 128),
              "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (7), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (85, 85, 85, 85, 85, 85, 85, 85, 0),
              "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First + 1, Buffer'Last, 0, 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (1), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (255, 85, 85, 85, 85, 85, 85, 85, 85),
              "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (2), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (254, 170, 170, 170, 170, 170, 170, 170, 171),
              "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (3), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (253, 85, 85, 85, 85, 85, 85, 85, 87),
              "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (4), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (250, 170, 170, 170, 170, 170, 170, 170, 175),
              "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (5), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (245, 85, 85, 85, 85, 85, 85, 85, 95),
              "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (6), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (234, 170, 170, 170, 170, 170, 170, 170, 191),
              "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (7), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (213, 85, 85, 85, 85, 85, 85, 85, 127),
              "Invalid insertion in filled buffer with offset 7");

      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (0), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 0, 255, 255, 255, 255),
              "Invalid insertion off one-sided pattern with offset 0");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (1), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 1, 255, 255, 255, 254),
              "Invalid insertion off one-sided pattern with offset 1");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (2), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 3, 255, 255, 255, 252),
              "Invalid insertion off one-sided pattern with offset 2");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (3), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 7, 255, 255, 255, 248),
              "Invalid insertion off one-sided pattern with offset 3");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (4), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 15, 255, 255, 255, 240),
              "Invalid insertion off one-sided pattern with offset 4");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (5), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 31, 255, 255, 255, 224),
              "Invalid insertion off one-sided pattern with offset 5");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (6), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 63, 255, 255, 255, 192),
              "Invalid insertion off one-sided pattern with offset 6");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (7), 64, RFLX_Types.High_Order_First);
      Assert (Buffer, (0, 0, 0, 0, 127, 255, 255, 255, 128),
              "Invalid insertion off one-sided pattern with offset 7");

   end Test_Insert_Modular_Integer_64;

   procedure Test_Insert_Modular_Integer_1_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (0), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (1), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (2), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (3), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (4), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (5), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (6), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (7), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (128, 0, 0), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (0), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (1), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (2), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (3), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (4), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (5), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (6), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(0), Buffer, Buffer'First, Buffer'First,
                                    Dynamic_Offset (7), 1, RFLX_Types.Low_Order_First);
      Assert (Buffer, (127, 255, 255), "Invalid insertion in filled buffer with offset 7");

   end Test_Insert_Modular_Integer_1_LE;

   procedure Test_Insert_Modular_Integer_2_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 0, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 0, 0), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 1, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (2, 0, 0), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 2, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (4, 0, 0), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 3, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (8, 0, 0), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 4, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (16, 0, 0), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 5, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (32, 0, 0), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(1), Buffer, Buffer'First, Buffer'First, 6, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (64, 0, 0), "Invalid insertion in zero-initialized buffer with offset 6");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 0, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (254, 255, 255), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 1, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (253, 255, 255), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 2, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (251, 255, 255), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 3, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (247, 255, 255), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 4, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (239, 255, 255), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 5, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (223, 255, 255), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(2), Buffer, Buffer'First, Buffer'First, 6, 2,
                                    RFLX_Types.Low_Order_First);
      Assert (Buffer, (191, 255, 255), "Invalid insertion in filled buffer with offset 6");

   end Test_Insert_Modular_Integer_2_LE;

   procedure Test_Insert_Modular_Integer_13_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 2 => 0);
   begin
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    0, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 21, 170), "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    1, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 21, 170), "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    2, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 85, 168), "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    3, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 85, 168), "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (4), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 170, 160), "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (5), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 85, 160), "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (6), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (5, 170, 128), "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (7), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (5, 85, 128), "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    0, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 245, 170), "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    1, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 213, 171), "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    2, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 213, 171), "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First + 1, Buffer'Last,
                                    3, 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 85, 175), "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (4), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 170, 175), "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (5), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (253, 85, 191), "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (6), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (253, 170, 191), "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(5461), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (7), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (245, 85, 255), "Invalid insertion in filled buffer with offset 7");

      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (0), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 31, 7), "Invalid insertion of 0000011111111 with offset 0");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (1), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 63, 6), "Invalid insertion of 0000011111111 with offset 1");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (2), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 127, 4), "Invalid insertion of 0000011111111 with offset 2");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (3), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 255, 0), "Invalid insertion of 0000011111111 with offset 3");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (4), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 127, 0), "Invalid insertion of 0000011111111 with offset 4");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (5), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (3, 63, 0), "Invalid insertion of 0000011111111 with offset 5");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (6), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (7, 31, 0), "Invalid insertion of 0000011111111 with offset 6");
      Buffer := (0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(255), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (7), 13, RFLX_Types.Low_Order_First);
      Assert (Buffer, (15, 15, 0), "Invalid insertion of 0000011111111 with offset 7");

   end Test_Insert_Modular_Integer_13_LE;

   procedure Test_Insert_Modular_Integer_64_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : Bytes := (Index'First .. Index'First + 8 => 0);
   begin
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First + 1, Buffer'Last, 0, 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in zero-initialized buffer with offset 0");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (1), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 85, 85, 85, 85, 85, 85, 85, 170),
              "Invalid insertion in zero-initialized buffer with offset 1");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (2), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (2, 170, 170, 170, 170, 170, 170, 170, 168),
              "Invalid insertion in zero-initialized buffer with offset 2");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (3), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (2, 85, 85, 85, 85, 85, 85, 85, 168),
              "Invalid insertion in zero-initialized buffer with offset 3");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (4), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (10, 170, 170, 170, 170, 170, 170, 170, 160),
              "Invalid insertion in zero-initialized buffer with offset 4");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (5), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (10, 85, 85, 85, 85, 85, 85, 85, 160),
              "Invalid insertion in zero-initialized buffer with offset 5");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (6), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (42, 170, 170, 170, 170, 170, 170, 170, 128),
              "Invalid insertion in zero-initialized buffer with offset 6");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (7), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (42, 85, 85, 85, 85, 85, 85, 85, 128),
              "Invalid insertion in zero-initialized buffer with offset 7");

      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First + 1, Buffer'Last, 0, 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (255, 170, 170, 170, 170, 170, 170, 170, 170),
              "Invalid insertion in filled buffer with offset 0");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (1), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (254, 85, 85, 85, 85, 85, 85, 85, 171),
              "Invalid insertion in filled buffer with offset 1");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (2), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (254, 170, 170, 170, 170, 170, 170, 170, 171),
              "Invalid insertion in filled buffer with offset 2");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (3), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (250, 85, 85, 85, 85, 85, 85, 85, 175),
              "Invalid insertion in filled buffer with offset 3");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (4), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (250, 170, 170, 170, 170, 170, 170, 170, 175),
              "Invalid insertion in filled buffer with offset 4");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (5), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (234, 85, 85, 85, 85, 85, 85, 85, 191),
              "Invalid insertion in filled buffer with offset 5");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (6), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (234, 170, 170, 170, 170, 170, 170, 170, 191),
              "Invalid insertion in filled buffer with offset 6");
      Buffer := (255, 255, 255, 255, 255, 255, 255, 255, 255);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(12297829382473034410), Buffer,
                                    Buffer'First, Buffer'Last, Dynamic_Offset (7), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (170, 85, 85, 85, 85, 85, 85, 85, 255),
              "Invalid insertion in filled buffer with offset 7");

      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (0), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (0, 255, 255, 255, 255, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 0");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (1), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (1, 255, 255, 255, 127, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 1");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (2), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (3, 255, 255, 255, 63, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 2");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (3), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (7, 255, 255, 255, 31, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 3");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (4), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (15, 255, 255, 255, 15, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 4");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (5), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (31, 255, 255, 255, 7, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 5");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (6), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (63, 255, 255, 255, 3, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 6");
      Buffer := (0, 0, 0, 0, 0, 0, 0, 0, 0);
      RFLX_Types.Operations.Insert (RFLX.RFLX_Types.U64'(4294967295), Buffer, Buffer'First, Buffer'Last,
                                    Dynamic_Offset (7), 64, RFLX_Types.Low_Order_First);
      Assert (Buffer, (127, 255, 255, 255, 1, 0, 0, 0, 0),
              "Invalid insertion off one-sided pattern with offset 7");

   end Test_Insert_Modular_Integer_64_LE;

   procedure Test_Random_Insert_Extract (T : in out AUnit.Test_Cases.Test_Case'Class) with
        SPARK_Mode => Off, Pre => True
   is
      pragma Unreferenced (T);

      use type RFLX_Types.U64;

      package Rand is new Ada.Numerics.Discrete_Random (RFLX_Types.U64);

      function Swap is new GNAT.Byte_Swapping.Swapped8 (RFLX_Types.U64);

      Buffer : Bytes (Index'First .. Index'First + 7) := (others => 0);
      G : Rand.Generator;
      Num, Num2 : RFLX_Types.U64;
      Result : RFLX_Types.U64 with Address => Buffer'Address;

   begin
      for I in 1 .. 10000 loop
         Num := Rand.Random (G);
         RFLX_Types.Operations.Insert (Num, Buffer, Buffer'First, Buffer'Last, 0, 64, RFLX_Types.High_Order_First);
         Assert (Swap (Num) = Result, "invalid insertion of 64bit number");
         Num2 := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, 0, 64, RFLX_Types.High_Order_First);
         Assert (Num = Num2, "invalid extraction of 64bit number");
      end loop;

   end Test_Random_Insert_Extract;

   procedure Test_Random_Insert_Extract_LE (T : in out AUnit.Test_Cases.Test_Case'Class) with
        SPARK_Mode => Off, Pre => True
   is
      pragma Unreferenced (T);

      use type RFLX_Types.U64;

      package Rand is new Ada.Numerics.Discrete_Random (RFLX_Types.U64);

      Buffer : Bytes (Index'First .. Index'First + 7) := (others => 0);
      G : Rand.Generator;
      Num, Num2 : RFLX_Types.U64;
      Result : RFLX_Types.U64 with Address => Buffer'Address;

   begin
      for I in 1 .. 10000 loop
         Num := Rand.Random (G);
         RFLX_Types.Operations.Insert (Num, Buffer, Buffer'First, Buffer'Last, 0, 64,
                                       RFLX_Types.Low_Order_First);
         Assert (Num = Result, "invalid insertion of 64bit number");
         Num2 := RFLX_Types.Operations.Extract (Buffer, Buffer'First, Buffer'Last, 0, 64,
                                                RFLX_Types.Low_Order_First);
         Assert (Num = Num2, "invalid extraction of 64bit number");
      end loop;

   end Test_Random_Insert_Extract_LE;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Index_Calculations'Access, "Index calculations");
      Register_Routine (T, Test_Length_Calculations'Access, "Length calculations");
      Register_Routine (T, Test_Extract_Modular_Integer_1'Access, "Extract 1 bit");
      Register_Routine (T, Test_Extract_Modular_Integer_8'Access, "Extract 8 bit");
      Register_Routine (T, Test_Extract_Modular_Integer_13'Access, "Extract 13 bit");
      Register_Routine (T, Test_Extract_Modular_Integer_62'Access, "Extract 62 bit");
      Register_Routine (T, Test_Extract_Modular_Integer_64'Access, "Extract 64 bit");
      Register_Routine (T, Test_Extract_Modular_Integer_1_LE'Access, "Extract 1 bit little endian");
      Register_Routine (T, Test_Extract_Modular_Integer_8_LE'Access, "Extract 8 bit little endian");
      Register_Routine (T, Test_Extract_Modular_Integer_13_LE'Access, "Extract 13 bit little endian");
      Register_Routine (T, Test_Extract_Modular_Integer_62_LE'Access, "Extract 62 bit little endian");
      Register_Routine (T, Test_Extract_Modular_Integer_64_LE'Access, "Extract 64 bit little endian");
      Register_Routine (T, Test_Insert_Modular_Integer_1'Access, "Insert 1 bit");
      Register_Routine (T, Test_Insert_Modular_Integer_2'Access, "Insert 2 bit");
      Register_Routine (T, Test_Insert_Modular_Integer_13'Access, "Insert 13 bit");
      Register_Routine (T, Test_Insert_Modular_Integer_64'Access, "Insert 64 bit");
      Register_Routine (T, Test_Insert_Modular_Integer_1_LE'Access, "Insert 1 bit little endian");
      Register_Routine (T, Test_Insert_Modular_Integer_2_LE'Access, "Insert 2 bit little endian");
      Register_Routine (T, Test_Insert_Modular_Integer_13_LE'Access, "Insert 13 bit little endian");
      Register_Routine (T, Test_Insert_Modular_Integer_64_LE'Access, "Insert 64 bit little endian");
      Register_Routine (T, Test_Random_Insert_Extract'Access,
                        "Insert/extract random numbers");
      Register_Routine (T, Test_Random_Insert_Extract_LE'Access,
                        "Insert/extract random numbers little endian");

   end Register_Tests;

end RFLX.Builtin_Types_Tests;
