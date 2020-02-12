with SPARK.Assertions; use SPARK.Assertions;

with RFLX.Generic_Types;

package body RFLX.Custom_Types_Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Custom_Types");
   end Name;

   procedure Test_Index_6_Modular (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type Index is range 1 .. 2**6 - 1;
      type Byte is mod 2**8;
      type Bytes is array (Index range <>) of Byte;
      type Bytes_Ptr is access Bytes;
      type Length is range 0 .. 2**6 - 1;
      type Bit_Length is range 0 .. Length'Last * 8;

      package Types is new RFLX.Generic_Types (Index, Byte, Bytes, Bytes_Ptr, Length, Bit_Length);

      type Value is mod 2**14;

      function Extract is new Types.Extract (Value);
      procedure Insert is new Types.Insert (Value);
   begin
      null;
   end Test_Index_6_Modular;

   procedure Test_Index_18_Range (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type Index is range 1 .. 2**18 - 1;
      type Byte is range 0 .. 255;
      type Bytes is array (Index range <>) of Byte;
      type Bytes_Ptr is access Bytes;
      type Length is range 0 .. 2**18 - 1;
      type Bit_Length is range 0 .. Length'Last * 8;

      package Types is new RFLX.Generic_Types (Index, Byte, Bytes, Bytes_Ptr, Length, Bit_Length);

      type Value is range 0 .. 2**14 - 1;

      function Extract is new Types.Extract (Value);
      procedure Insert is new Types.Insert (Value);
   begin
      null;
   end Test_Index_18_Range;

   procedure Test_Index_60_Enum (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type Index is range 1 .. 2**60 - 1;
      type Characters is array (Index range <>) of Character;
      type Characters_Ptr is access Characters;
      type Length is range 0 .. 2**60 - 1;
      type Bit_Length is range 0 .. Length'Last * 8;

      package Types is new RFLX.Generic_Types (Index, Character, Characters, Characters_Ptr, Length, Bit_Length);

      type Value is (A, B, C, D, E, F, G, H);

      function Extract is new Types.Extract (Value);
      procedure Insert is new Types.Insert (Value);
   begin
      null;
   end Test_Index_60_Enum;

   procedure Test_String (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      type String_Ptr is access String;
      type Bit_Length is range 0 .. Natural'Last * 8;

      package Types is new RFLX.Generic_Types (Positive, Character, String, String_Ptr, Natural, Bit_Length);

      function Extract is new Types.Extract (Wide_Character);
      procedure Insert is new Types.Insert (Wide_Character);
   begin
      null;
   end Test_String;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Index_6_Modular'Access, "6-bit Index + Modular");
      Register_Routine (T, Test_Index_18_Range'Access, "18-bit Index + Range");
      Register_Routine (T, Test_Index_60_Enum'Access, "60-bit Index + Enum");
      Register_Routine (T, Test_String'Access, "String");
   end Register_Tests;

end RFLX.Custom_Types_Tests;
