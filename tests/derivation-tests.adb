with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with Derivation.Message;
with Derivation.Modular_Vector;
with Derivation.Range_Vector;
with Derivation.Enumeration_Vector;
with Derivation.AV_Enumeration_Vector;

package body Derivation.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Derivation");
   end Name;

   procedure Test_Derivation_Modular_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Length  : Derivation.Length_Type;
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Modular_Vector.Cursor_Type;
      Element : Derivation.Modular_Integer;
      Valid   : Boolean;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Length (Buffer);
      Assert (Valid, "Invalid Length");

      if Valid then
         Length := Derivation.Message.Get_Length (Buffer);
         Assert (Length'Image, Derivation.Length_Type'Image (4), "Unexpected Length");

         Valid := Derivation.Message.Valid_Modular_Vector (Buffer);
         Assert (Valid, "Invalid Modular_Vector");

         if Valid then
            Derivation.Message.Get_Modular_Vector (Buffer, First, Last);
            Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
            Assert (Last'Image, Types.Index_Type'Image (5), "Unexpected Modular_Vector'Last");

            Cursor := Derivation.Modular_Vector.First (Buffer (First .. Last));
            Assert (Cursor.First'Image, Types.Index_Type'Image (2), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (3), "Unexpected Cursor.Last");

            Valid := Derivation.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 1");

            if Valid then
               Element := Derivation.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Derivation.Modular_Integer'Image (1), "Invalid value of element 1");

               Derivation.Modular_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (4), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (5), "Unexpected Cursor.Last");

               Valid := Derivation.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (Valid, "Invalid element 2");

               if Valid then
                  Element := Derivation.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
                  Assert (Element'Image, Derivation.Modular_Integer'Image (2), "Invalid value of element 2");

                  Derivation.Modular_Vector.Next (Buffer (First .. Last), Cursor);
                  Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
                  Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

                  Valid := Derivation.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor);
                  Assert (not Valid, "Invalid acceptance of further element");
               end if;
            end if;
         end if;
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Modular_Sequential;

   procedure Test_Derivation_Modular_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Modular_Vector.Cursor_Type;
      Element : Derivation.Modular_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Modular_Vector (Buffer);
      Assert (Valid, "Invalid Modular_Vector");
      if Valid then
         Derivation.Message.Get_Modular_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (5), "Unexpected Modular_Vector'Last");

         Cursor := Derivation.Modular_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Derivation.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Derivation.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Derivation.Modular_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Modular_Loop;

   procedure Test_Derivation_Modular_Vector_With_Padding (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(3, 0, 1, 0, 0, 0, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Modular_Vector.Cursor_Type;
      Element : Derivation.Modular_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Modular_Vector (Buffer);
      Assert (Valid, "Invalid Modular_Vector");

      if Valid then
         Derivation.Message.Get_Modular_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (2), "Unexpected Modular_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (4), "Unexpected Modular_Vector'Last");

         Cursor := Derivation.Modular_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Derivation.Modular_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Derivation.Modular_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Derivation.Modular_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (2), "Unexpected number of elements");
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Modular_Vector_With_Padding;

   procedure Test_Derivation_Range_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Range_Vector.Cursor_Type;
      Element : Derivation.Range_Integer;
      Valid   : Boolean;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Range_Vector (Buffer);
      Assert (Valid, "Invalid Range_Vector");

      if Valid then
         Derivation.Message.Get_Range_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (6), "Unexpected Range_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (7), "Unexpected Range_Vector'Last");

         Cursor := Derivation.Range_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (6), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (6), "Unexpected Cursor.Last");

         Valid := Derivation.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Derivation.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Derivation.Range_Integer'Image (1), "Invalid value of element 1");

            Derivation.Range_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (7), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (7), "Unexpected Cursor.Last");

            Valid := Derivation.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Derivation.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Derivation.Range_Integer'Image (2), "Invalid value of element 2");

               Derivation.Range_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Derivation.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Range_Sequential;

   procedure Test_Derivation_Range_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Range_Vector.Cursor_Type;
      Element : Derivation.Range_Integer;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Range_Vector (Buffer);
      Assert (Valid, "Invalid Range_Vector");

      if Valid then
         Derivation.Message.Get_Range_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (6), "Unexpected Range_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (7), "Unexpected Range_Vector'Last");

         Cursor := Derivation.Range_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Derivation.Range_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Derivation.Range_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Derivation.Range_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Range_Loop;

   procedure Test_Derivation_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Enumeration_Vector.Cursor_Type;
      Element : Derivation.Enumeration;
      Valid   : Boolean;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid Enumeration_Vector");

      if Valid then
         Derivation.Message.Get_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (8), "Unexpected Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (9), "Unexpected Enumeration_Vector'Last");

         Cursor := Derivation.Enumeration_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (8), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (8), "Unexpected Cursor.Last");

         Valid := Derivation.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Derivation.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Element'Image, Derivation.ONE'Image, "Invalid value of element 1");

            Derivation.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (9), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (9), "Unexpected Cursor.Last");

            Valid := Derivation.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Derivation.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
               Assert (Element'Image, Derivation.TWO'Image, "Invalid value of element 2");

               Derivation.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Derivation.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Enumeration_Sequential;

   procedure Test_Derivation_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.Enumeration_Vector.Cursor_Type;
      Element : Derivation.Enumeration;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid Enumeration_Vector");

      if Valid then
         Derivation.Message.Get_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (8), "Unexpected Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (9), "Unexpected Enumeration_Vector'Last");

         Cursor := Derivation.Enumeration_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Derivation.Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Derivation.Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            Assert (Derivation.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            Derivation.Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_Enumeration_Loop;

   procedure Test_Derivation_AV_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.AV_Enumeration_Vector.Cursor_Type;
      Element : Derivation.AV_Enumeration;
      Valid   : Boolean;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_AV_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid AV_Enumeration_Vector");

      if Valid then
         Derivation.Message.Get_AV_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (10), "Unexpected AV_Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (11), "Unexpected AV_Enumeration_Vector'Last");

         Cursor := Derivation.AV_Enumeration_Vector.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (10), "Unexpected Cursor.First");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (10), "Unexpected Cursor.Last");

         Valid := Derivation.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid element 1");

         if Valid then
            Element := Derivation.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            if Element.Known then
               Assert (Element.Enum'Image, Derivation.AV_ONE'Image, "Invalid value of element 1");
            else
               Assert (False, "Unknown value of element 1");
            end if;

            Derivation.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (11), "Unexpected Cursor.First");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (11), "Unexpected Cursor.Last");

            Valid := Derivation.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element 2");

            if Valid then
               Element := Derivation.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
               if Element.Known then
                  Assert (Element.Enum'Image, Derivation.AV_TWO'Image, "Invalid value of element 2");
               else
                  Assert (False, "Unknown value of element 2");
               end if;

               Derivation.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (Types.Index_Type'Last), "Unexpected Cursor.First");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (Types.Index_Type'First), "Unexpected Cursor.Last");

               Valid := Derivation.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (not Valid, "Invalid acceptance of further element");
            end if;
         end if;
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_AV_Enumeration_Sequential;

   procedure Test_Derivation_AV_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes := Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      First   : Types.Index_Type;
      Last    : Types.Index_Type;
      Cursor  : Derivation.AV_Enumeration_Vector.Cursor_Type;
      Element : Derivation.AV_Enumeration;
      Valid   : Boolean;
      I       : Natural := 1;
   begin
      Derivation.Message.Label (Buffer);

      Valid := Derivation.Message.Valid_AV_Enumeration_Vector (Buffer);
      Assert (Valid, "Invalid AV_Enumeration_Vector");

      if Valid then
         Derivation.Message.Get_AV_Enumeration_Vector (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (10), "Unexpected AV_Enumeration_Vector'First");
         Assert (Last'Image, Types.Index_Type'Image (11), "Unexpected AV_Enumeration_Vector'Last");

         Cursor := Derivation.AV_Enumeration_Vector.First (Buffer (First .. Last));
         while I <= 10 and then Derivation.AV_Enumeration_Vector.Valid_Element (Buffer (First .. Last), Cursor) loop
            Element := Derivation.AV_Enumeration_Vector.Get_Element (Buffer (First .. Last), Cursor);
            if Element.Known then
               Assert (Derivation.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
            else
               Assert (False, "Unknown value of element " & I'Image);
            end if;

            Derivation.AV_Enumeration_Vector.Next (Buffer (First .. Last), Cursor);
            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      end if;

      Assert (Derivation.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Derivation_AV_Enumeration_Loop;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Derivation_Modular_Sequential'Access, "Modular Sequential");
      Register_Routine (T, Test_Derivation_Modular_Loop'Access, "Modular Loop");
      Register_Routine (T, Test_Derivation_Modular_Vector_With_Padding'Access, "Modular Padding");
      Register_Routine (T, Test_Derivation_Range_Sequential'Access, "Range Sequential");
      Register_Routine (T, Test_Derivation_Range_Loop'Access, "Range Loop");
      Register_Routine (T, Test_Derivation_Enumeration_Sequential'Access, "Enumeration Sequential");
      Register_Routine (T, Test_Derivation_Enumeration_Loop'Access, "Enumeration Loop");
      Register_Routine (T, Test_Derivation_AV_Enumeration_Sequential'Access, "AV_Enumeration Sequential");
   end Register_Tests;

end Derivation.Tests;
