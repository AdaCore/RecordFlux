with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with Arrays.Message;
with Arrays.Modular_Vector;
with Arrays.Range_Vector;
with Arrays.Enumeration_Vector;
with Arrays.AV_Enumeration_Vector;

package body Arrays.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Arrays");
   end Name;

   procedure Test_Arrays_Modular_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Length  : Arrays.Length_Type;
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Modular_Vector.Cursor_Type;
      Element : Arrays.Modular_Integer;
      Valid   : Boolean;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Length (Buffer);
      Assert (Valid, "Invalid Length");

      if Valid then
         Length := Arrays.Message.Get_Length (Buffer);
         Assert (Length'Image, Arrays.Length_Type'Image (4), "Unexpected Length");

         Valid := Arrays.Message.Valid_Modular_Vector (Buffer);
         Assert (Valid, "Invalid Modular_Vector");

         if Valid then
            Arrays.Message.Get_Modular_Vector (Buffer, First, Last);
            Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
            Assert (Last'Image, Types.Index_Type'Image (5), "Unexpected Modular_Vector'Last");

            Cursor := Arrays.Modular_Vector.First (Buffer (First .. Last));
            Assert (Cursor.First'Image, Types.Index_Type'Image (2), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (3), "Unexpected Cursor.Last");

            Valid := Arrays.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 1");

            if Valid then
               Element := Arrays.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Arrays.Modular_Integer'Image (1), "Invalid value of element 1");

               Arrays.Modular_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (4), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (5), "Unexpected Cursor.Last");

               Valid := Arrays.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (Valid, "Invalid element 2");

               if Valid then
                  Element := Arrays.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
                  Assert (Element'Image, Arrays.Modular_Integer'Image (2), "Invalid value of element 2");

                  Arrays.Modular_Vector.Next (Buffer (First .. Last), Cursor);
                  Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
                  Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

                  Valid := Arrays.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
                  Assert (not Valid, "Invalid acceptance of further element");
               end if;
            end if;
         end if;
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Modular_Sequential;

   procedure Test_Arrays_Modular_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Modular_Vector.Cursor_Type;
      Element : Arrays.Modular_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Modular_Vector (Buffer);
      Assert (Valid, "Invalid Modular_Vector");
      if Valid then
         Arrays.Message.Get_Modular_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (5), "Unexpected Modular_Vector'Last");

         Cursor := Arrays.Modular_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Arrays.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Arrays.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Arrays.Modular_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Modular_Loop;

   procedure Test_Arrays_Modular_Vector_With_Padding (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(3, 0, 1, 0, 0, 0, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Modular_Vector.Cursor_Type;
      Element : Arrays.Modular_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Modular_Vector (Buffer);
      Assert (Valid, "Invalid Modular_Vector");

      if Valid then
         Arrays.Message.Get_Modular_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (4), "Unexpected Modular_Vector'Last");

         Cursor := Arrays.Modular_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Arrays.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Arrays.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Arrays.Modular_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (2), "Unexpected number of elements");
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Modular_Vector_With_Padding;

   procedure Test_Arrays_Range_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Range_Vector.Cursor_Type;
      Element : Arrays.Range_Integer;
      Valid   : Boolean;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Range_Vector (Buffer);
      Assert (Valid, "Invalid Range_Vector");

      if Valid then
         Arrays.Message.Get_Range_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (6), "Unexpected Range_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (7), "Unexpected Range_Vector'Last");

         Cursor := Arrays.Range_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (6), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (6), "Unexpected Cursor.Last");

         Valid := Arrays.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Arrays.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Arrays.Range_Integer'Image (1), "Invalid value of element 1");

            Arrays.Range_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (7), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (7), "Unexpected Cursor.Last");

            Valid := Arrays.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Arrays.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Arrays.Range_Integer'Image (2), "Invalid value of element 2");

               Arrays.Range_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Arrays.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Range_Sequential;

   procedure Test_Arrays_Range_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Range_Vector.Cursor_Type;
      Element : Arrays.Range_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Range_Vector (Buffer);
      Assert (Valid, "Invalid Range_Vector");

      if Valid then
         Arrays.Message.Get_Range_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (6), "Unexpected Range_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (7), "Unexpected Range_Vector'Last");

         Cursor := Arrays.Range_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Arrays.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Arrays.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Arrays.Range_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Range_Loop;

   procedure Test_Arrays_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Enumeration_Vector.Cursor_Type;
      Element : Arrays.Enumeration;
      Valid   : Boolean;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid Enumeration_Vector");

      if Valid then
         Arrays.Message.Get_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (8), "Unexpected Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (9), "Unexpected Enumeration_Vector'Last");

         Cursor := Arrays.Enumeration_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (8), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (8), "Unexpected Cursor.Last");

         Valid := Arrays.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Arrays.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Arrays.ONE'Image, "Invalid value of element 1");

            Arrays.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (9), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (9), "Unexpected Cursor.Last");

            Valid := Arrays.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Arrays.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Arrays.TWO'Image, "Invalid value of element 2");

               Arrays.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Arrays.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Enumeration_Sequential;

   procedure Test_Arrays_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.Enumeration_Vector.Cursor_Type;
      Element : Arrays.Enumeration;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid Enumeration_Vector");

      if Valid then
         Arrays.Message.Get_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (8), "Unexpected Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (9), "Unexpected Enumeration_Vector'Last");

         Cursor := Arrays.Enumeration_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Arrays.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Arrays.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Arrays.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_Enumeration_Loop;

   procedure Test_Arrays_AV_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.AV_Enumeration_Vector.Cursor_Type;
      Element : Arrays.AV_Enumeration;
      Valid   : Boolean;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_AV_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid AV_Enumeration_Vector");

      if Valid then
         Arrays.Message.Get_AV_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (10), "Unexpected AV_Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (11), "Unexpected AV_Enumeration_Vector'Last");

         Cursor := Arrays.AV_Enumeration_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (10), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (10), "Unexpected Cursor.Last");

         Valid := Arrays.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Arrays.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            if Element.Known then
               Assert (Element.Enum'Image, Arrays.AV_ONE'Image, "Invalid value of element 1");
            else
               Assert (False, "Unknown value of element 1");
            end if;

            Arrays.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (11), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (11), "Unexpected Cursor.Last");

            Valid := Arrays.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Arrays.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
               if Element.Known then
                  Assert (Element.Enum'Image, Arrays.AV_TWO'Image, "Invalid value of element 2");
               else
                  Assert (False, "Unknown value of element 2");
               end if;

               Arrays.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Arrays.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_AV_Enumeration_Sequential;

   procedure Test_Arrays_AV_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Arrays.AV_Enumeration_Vector.Cursor_Type;
      Element : Arrays.AV_Enumeration;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Arrays.Message.Label (Buffer);

      Valid := Arrays.Message.Valid_AV_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid AV_Enumeration_Vector");

      if Valid then
         Arrays.Message.Get_AV_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (10), "Unexpected AV_Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (11), "Unexpected AV_Enumeration_Vector'Last");

         Cursor := Arrays.AV_Enumeration_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Arrays.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Arrays.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            if Element.Known then
               Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
            else
               Assert (False, "Unknown value of element " & I'Image);
            end if;

            Arrays.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Arrays.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Arrays_AV_Enumeration_Loop;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Arrays_Modular_Sequential'Access, "Modular Sequential");
      Register_Routine (T, Test_Arrays_Modular_Loop'Access, "Modular Loop");
      Register_Routine (T, Test_Arrays_Modular_Vector_With_Padding'Access, "Modular Padding");
      Register_Routine (T, Test_Arrays_Range_Sequential'Access, "Range Sequential");
      Register_Routine (T, Test_Arrays_Range_Loop'Access, "Range Loop");
      Register_Routine (T, Test_Arrays_Enumeration_Sequential'Access, "Enumeration Sequential");
      Register_Routine (T, Test_Arrays_Enumeration_Loop'Access, "Enumeration Loop");
      Register_Routine (T, Test_Arrays_AV_Enumeration_Sequential'Access, "AV_Enumeration Sequential");
   end Register_Tests;

end Arrays.Tests;
