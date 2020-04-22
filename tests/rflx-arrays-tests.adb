with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Bit_Length;
with RFLX.RFLX_Types;

with RFLX.Arrays.Message;
with RFLX.Arrays.Modular_Vector;
with RFLX.Arrays.Range_Vector;
with RFLX.Arrays.Enumeration_Vector;
with RFLX.Arrays.AV_Enumeration_Vector;
with RFLX.Arrays.Messages_Message;
with RFLX.Arrays.Inner_Message;
with RFLX.Arrays.Inner_Messages;

package body RFLX.Arrays.Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Arrays");
   end Name;

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 1) :=
     (others => 0);

   procedure Write_Data (Buffer : out RFLX_Builtin_Types.Bytes) is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
   pragma Warnings (Off, "unused assignment to ""Element_Context""");
   pragma Warnings (Off, "unused assignment to ""Modular_Vector_Context""");
   pragma Warnings (Off, "unused assignment to ""Range_Vector_Context""");
   pragma Warnings (Off, "unused assignment to ""Enumeration_Vector_Context""");
   pragma Warnings (Off, "unused assignment to ""AV_Enumeration_Vector_Context""");

   procedure Test_Parsing_Arrays_Modular_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Modular_Vector.Context := Arrays.Modular_Vector.Create;
      Element          : Arrays.Modular_Integer;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Modular_Vector) then
            Arrays.Message.Switch_To_Modular_Vector (Context, Sequence_Context);

            if Arrays.Modular_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Modular_Integer'Image (1), "Invalid value of element 1");

               Arrays.Modular_Vector.Next (Sequence_Context);
               if Arrays.Modular_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Modular_Integer'Image (2), "Invalid value of element 2");

                  Arrays.Modular_Vector.Next (Sequence_Context);
                  Assert (not Arrays.Modular_Vector.Valid_Element (Sequence_Context),
                          "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                          "Valid Modular_Vector before context update");
                  Arrays.Message.Update_Modular_Vector (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                          "Invalid Modular_Vector after context update");
               else
                  Assert (False, "Invalid element 2");
               end if;
            else
               Assert (False, "Invalid element 1");
            end if;

         else
            Assert (False, "Invalid Modular_Vector");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Modular_Sequential;

   procedure Test_Parsing_Arrays_Modular_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Modular_Vector.Context := Arrays.Modular_Vector.Create;
      Element          : Arrays.Modular_Integer;
      I                : Natural := 1;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Modular_Vector) then
            Arrays.Message.Switch_To_Modular_Vector (Context, Sequence_Context);

            loop
               --  WORKAROUND: Componolit/Workarounds#16
               exit when I > 10 or not Arrays.Modular_Vector.Valid_Element (Sequence_Context);
               pragma Loop_Invariant (I <= 10 and then Arrays.Modular_Vector.Valid_Element (Sequence_Context));

               pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
               pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
               pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

               Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Arrays.Modular_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                    "Valid Modular_Vector before context update");
            Arrays.Message.Update_Modular_Vector (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                    "Invalid Modular_Vector after context update");
         else
            Assert (False, "Invalid Modular_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Modular_Loop;

   procedure Test_Parsing_Arrays_Range_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Range_Vector.Context := Arrays.Range_Vector.Create;
      Element          : Arrays.Range_Integer;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Range_Vector) then
            Arrays.Message.Switch_To_Range_Vector (Context, Sequence_Context);

            if Arrays.Range_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Range_Integer'Image (1), "Invalid value of element 1");

               Arrays.Range_Vector.Next (Sequence_Context);
               if Arrays.Range_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Range_Integer'Image (2), "Invalid value of element 2");

                  Arrays.Range_Vector.Next (Sequence_Context);

                  Assert (not Arrays.Range_Vector.Valid_Element (Sequence_Context),
                          "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                          "Valid Range_Vector before context update");
                  Arrays.Message.Update_Range_Vector (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                          "Invalid Range_Vector after context update");
               else
                  Assert (False, "Invalid element 2");
               end if;
            else
               Assert (False, "Invalid element 1");
            end if;

         else
            Assert (False, "Invalid Range_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Range_Sequential;

   procedure Test_Parsing_Arrays_Range_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Range_Vector.Context := Arrays.Range_Vector.Create;
      Element          : Arrays.Range_Integer;
      I                : Natural := 1;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Range_Vector) then
            Arrays.Message.Switch_To_Range_Vector (Context, Sequence_Context);

            loop
               --  WORKAROUND: Componolit/Workarounds#16
               exit when I > 10 or not Arrays.Range_Vector.Valid_Element (Sequence_Context);
               pragma Loop_Invariant (I <= 10 and then Arrays.Range_Vector.Valid_Element (Sequence_Context));

               pragma Loop_Invariant (Arrays.Range_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
               pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
               pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

               Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Arrays.Range_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                    "Valid Range_Vector before context update");
            Arrays.Message.Update_Range_Vector (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                    "Invalid Range_Vector after context update");
         else
            Assert (False, "Invalid Range_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Range_Loop;

   procedure Test_Parsing_Arrays_Enumeration_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Enumeration_Vector.Context := Arrays.Enumeration_Vector.Create;
      Element          : Arrays.Enumeration;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Enumeration_Vector) then
            Arrays.Message.Switch_To_Enumeration_Vector (Context, Sequence_Context);

            if Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Enumeration'Image (Arrays.ONE), "Invalid value of element 1");

               Arrays.Enumeration_Vector.Next (Sequence_Context);
               if Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Enumeration'Image (Arrays.TWO), "Invalid value of element 2");

                  Arrays.Enumeration_Vector.Next (Sequence_Context);

                  Assert (not Arrays.Enumeration_Vector.Valid_Element (Sequence_Context),
                          "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                          "Valid Enumeration_Vector before context update");
                  Arrays.Message.Update_Enumeration_Vector (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                          "Invalid Enumeration_Vector after context update");
               else
                  Assert (False, "Invalid element 2");
               end if;
            else
               Assert (False, "Invalid element 1");
            end if;

         else
            Assert (False, "Invalid Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Enumeration_Sequential;

   procedure Test_Parsing_Arrays_Enumeration_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Enumeration_Vector.Context := Arrays.Enumeration_Vector.Create;
      Element          : Arrays.Enumeration;
      I                : Natural := 1;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_Enumeration_Vector) then
            Arrays.Message.Switch_To_Enumeration_Vector (Context, Sequence_Context);

            loop
               --  WORKAROUND: Componolit/Workarounds#16
               exit when I > 10 or not Arrays.Enumeration_Vector.Valid_Element (Sequence_Context);
               pragma Loop_Invariant (I <= 10 and then Arrays.Enumeration_Vector.Valid_Element (Sequence_Context));

               pragma Loop_Invariant (Arrays.Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
               pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
               pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

               Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I),
                       "Invalid value of element " & I'Image);

               Arrays.Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                    "Valid Enumeration_Vector before context update");
            Arrays.Message.Update_Enumeration_Vector (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                    "Invalid Enumeration_Vector after context update");
         else
            Assert (False, "Invalid Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_Enumeration_Loop;

   procedure Test_Parsing_Arrays_AV_Enumeration_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.AV_Enumeration_Vector.Context := Arrays.AV_Enumeration_Vector.Create;
      Element          : Arrays.AV_Enumeration;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
            Arrays.Message.Switch_To_AV_Enumeration_Vector (Context, Sequence_Context);

            if Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
               if Element.Known then
                  Assert (Element.Enum'Image, Arrays.AV_ONE'Image, "Invalid value of element 1");
               else
                  Assert (False, "Unknown value of element 1");
               end if;

               Arrays.AV_Enumeration_Vector.Next (Sequence_Context);
               if Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
                  if Element.Known then
                     Assert (Element.Enum'Image, Arrays.AV_TWO'Image, "Invalid value of element 2");
                  else
                     Assert (False, "Unknown value of element 2");
                  end if;

                  Arrays.AV_Enumeration_Vector.Next (Sequence_Context);

                  Assert (not Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context),
                          "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                          "Valid AV_Enumeration_Vector before context update");
                  Arrays.Message.Update_AV_Enumeration_Vector (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                          "Invalid AV_Enumeration_Vector after context update");
               else
                  Assert (False, "Invalid element 2");
               end if;
            else
               Assert (False, "Invalid element 1");
            end if;

         else
            Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_AV_Enumeration_Sequential;

   procedure Test_Parsing_Arrays_AV_Enumeration_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.AV_Enumeration_Vector.Context := Arrays.AV_Enumeration_Vector.Create;
      Element          : Arrays.AV_Enumeration;
      I                : Natural := 1;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
            Arrays.Message.Switch_To_AV_Enumeration_Vector (Context, Sequence_Context);

            loop
               --  WORKAROUND: Componolit/Workarounds#16
               exit when I > 10 or not Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context);
               pragma Loop_Invariant (I <= 10 and then Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context));

               pragma Loop_Invariant (Arrays.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
               pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
               pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

               Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
               if Element.Known then
                  Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I),
                          "Invalid value of element " & I'Image);
               else
                  Assert (False, "Unknown value of element " & I'Image);
               end if;

               Arrays.AV_Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.AV_Enumeration_Vector.Valid (Sequence_Context),
                    "Invalid AV_Enumeration_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                    "Valid AV_Enumeration_Vector before context update");
            Arrays.Message.Update_AV_Enumeration_Vector (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                    "Invalid AV_Enumeration_Vector after context update");
         else
            Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Parsing_Arrays_AV_Enumeration_Loop;

   procedure Test_Parsing_Arrays_Message (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Context : Arrays.Message.Context := Arrays.Message.Create;
      Length  : Arrays.Length;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Arrays.Modular_Vector.Context := Arrays.Modular_Vector.Create;
            Element          : Arrays.Modular_Integer;
            I                : Natural := 1;
         begin
            if Arrays.Message.Present (Context, Arrays.Message.F_Modular_Vector) then
               Arrays.Message.Switch_To_Modular_Vector (Context, Sequence_Context);

               loop
                  --  WORKAROUND: Componolit/Workarounds#16
                  exit when I > 10 or not Arrays.Modular_Vector.Valid_Element (Sequence_Context);
                  pragma Loop_Invariant (I <= 10 and then Arrays.Modular_Vector.Valid_Element (Sequence_Context));

                  pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
                  pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
                  pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

                  Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Arrays.Modular_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                       "Valid Modular_Vector before context update");
               Arrays.Message.Update_Modular_Vector (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
                       "Invalid Modular_Vector after context update");
            else
               Assert (False, "Invalid Modular_Vector or Buffer");
            end if;
         end;

         Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Arrays.Range_Vector.Context := Arrays.Range_Vector.Create;
            Element          : Arrays.Range_Integer;
            I                : Natural := 1;
         begin
            if Arrays.Message.Present (Context, Arrays.Message.F_Range_Vector) then
               Arrays.Message.Switch_To_Range_Vector (Context, Sequence_Context);

               loop
                  --  WORKAROUND: Componolit/Workarounds#16
                  exit when I > 10 or not Arrays.Range_Vector.Valid_Element (Sequence_Context);
                  pragma Loop_Invariant (I <= 10 and then Arrays.Range_Vector.Valid_Element (Sequence_Context));

                  pragma Loop_Invariant (Arrays.Range_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
                  pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
                  pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

                  Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Arrays.Range_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                       "Valid Range_Vector before context update");
               Arrays.Message.Update_Range_Vector (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
                       "Invalid Range_Vector after context update");
            else
               Assert (False, "Invalid Range_Vector or Buffer");
            end if;
         end;

         Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Arrays.Enumeration_Vector.Context := Arrays.Enumeration_Vector.Create;
            Element          : Arrays.Enumeration;
            I                : Natural := 1;
         begin
            if Arrays.Message.Present (Context, Arrays.Message.F_Enumeration_Vector) then
               Arrays.Message.Switch_To_Enumeration_Vector (Context, Sequence_Context);

               loop
                  --  WORKAROUND: Componolit/Workarounds#16
                  exit when I > 10 or not Arrays.Enumeration_Vector.Valid_Element (Sequence_Context);
                  pragma Loop_Invariant (I <= 10 and then Arrays.Enumeration_Vector.Valid_Element (Sequence_Context));

                  pragma Loop_Invariant (Arrays.Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
                  pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
                  pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

                  Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I),
                          "Invalid value of element " & I'Image);

                  Arrays.Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                       "Valid Enumeration_Vector before context update");
               Arrays.Message.Update_Enumeration_Vector (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
                       "Invalid Enumeration_Vector after context update");
            else
               Assert (False, "Invalid Enumeration_Vector or Buffer");
            end if;
         end;

         Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Arrays.AV_Enumeration_Vector.Context := Arrays.AV_Enumeration_Vector.Create;
            Element          : Arrays.AV_Enumeration;
            I                : Natural := 1;
         begin
            if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
               Arrays.Message.Switch_To_AV_Enumeration_Vector (Context, Sequence_Context);

               loop
                  --  WORKAROUND: Componolit/Workarounds#16
                  exit when I > 10 or not Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context);
                  pragma Loop_Invariant (I <= 10
                                         and then Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context));

                  pragma Loop_Invariant (Arrays.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
                  pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
                  pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

                  Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
                  if Element.Known then
                     Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I),
                             "Invalid value of element " & I'Image);
                  else
                     Assert (False, "Unknown value of element " & I'Image);
                  end if;

                  Arrays.AV_Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.AV_Enumeration_Vector.Valid (Sequence_Context),
                       "Invalid AV_Enumeration_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                       "Valid AV_Enumeration_Vector before context update");
               Arrays.Message.Update_AV_Enumeration_Vector (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
                       "Invalid AV_Enumeration_Vector after context update");
            else
               Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
            end if;
         end;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Arrays.Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Parsing_Arrays_Message;

   procedure Test_Parsing_Arrays_Messages_Message_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 0, 2, 0, 0);
      Context          : Arrays.Messages_Message.Context := Arrays.Messages_Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Inner_Messages.Context := Arrays.Inner_Messages.Create;
      Element_Context  : Arrays.Inner_Message.Context := Arrays.Inner_Message.Create;
   begin
      Arrays.Messages_Message.Initialize (Context, Buffer);

      Arrays.Messages_Message.Verify_Message (Context);
      if Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Length) then
         Length := Arrays.Messages_Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (5), "Unexpected Length");

         if Arrays.Messages_Message.Present (Context, Arrays.Messages_Message.F_Messages) then
            Arrays.Messages_Message.Switch_To_Messages (Context, Sequence_Context);

            if Arrays.Inner_Messages.Valid_Element (Sequence_Context) then
               Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

               Arrays.Inner_Message.Verify_Message (Element_Context);
               if Arrays.Inner_Message.Valid (Element_Context, Arrays.Inner_Message.F_Length) then
                  Length := Arrays.Inner_Message.Get_Length (Element_Context);
                  Assert (Length'Image, Arrays.Length'Image (1), "Unexpected Length of element 1");
               end if;
               Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 1");

               Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

               if Arrays.Inner_Messages.Valid_Element (Sequence_Context) then
                  Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

                  Arrays.Inner_Message.Verify_Message (Element_Context);
                  if Arrays.Inner_Message.Valid (Element_Context, Arrays.Inner_Message.F_Length) then
                     Length := Arrays.Inner_Message.Get_Length (Element_Context);
                     Assert (Length'Image, Arrays.Length'Image (2), "Unexpected Length of element 2");
                  end if;
                  Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context),
                          "Structural invalid element 2");

                  Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

                  Assert (not Arrays.Inner_Messages.Valid_Element (Sequence_Context),
                          "Invalid acceptance of further element");

                  Assert (not Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                          "Valid Messages before context update");
                  Arrays.Messages_Message.Update_Messages (Context, Sequence_Context);
                  Assert (Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                          "Invalid Messages after context update");
               else
                  Assert (False, "Invalid element 2");
               end if;
            else
               Assert (False, "Invalid element 1");
            end if;
         else
            Assert (False, "Invalid Messages or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Arrays.Messages_Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Parsing_Arrays_Messages_Message_Sequential;

   procedure Test_Parsing_Arrays_Messages_Message_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 0, 2, 0, 0);
      Context          : Arrays.Messages_Message.Context := Arrays.Messages_Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Inner_Messages.Context := Arrays.Inner_Messages.Create;
      Element_Context  : Arrays.Inner_Message.Context := Arrays.Inner_Message.Create;
      I                : Natural := 1;
   begin
      Arrays.Messages_Message.Initialize (Context, Buffer);

      Arrays.Messages_Message.Verify_Message (Context);
      if Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Length) then
         Length := Arrays.Messages_Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (5), "Unexpected Length");

         if Arrays.Messages_Message.Present (Context, Arrays.Messages_Message.F_Messages) then
            Arrays.Messages_Message.Switch_To_Messages (Context, Sequence_Context);

            loop
               --  WORKAROUND: Componolit/Workarounds#16
               exit when I > 10 or not Arrays.Inner_Messages.Valid_Element (Sequence_Context);
               pragma Loop_Invariant (I <= 10 and then Arrays.Inner_Messages.Valid_Element (Sequence_Context));

               pragma Loop_Invariant (Arrays.Inner_Messages.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
               pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
               pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

               Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

               Arrays.Inner_Message.Verify_Message (Element_Context);
               if Arrays.Inner_Message.Valid (Element_Context, Arrays.Inner_Message.F_Length) then
                  Length := Arrays.Inner_Message.Get_Length (Element_Context);
                  Assert (Length'Image, I'Image, "Unexpected Length of element " & I'Image);
               end if;
               Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context),
                       "Structural invalid element " & I'Image);

               Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (not Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                    "Valid Messages before context update");
            Arrays.Messages_Message.Update_Messages (Context, Sequence_Context);
            Assert (Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                    "Invalid Messages after context update");
         else
            Assert (False, "Invalid Messages or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Arrays.Messages_Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Parsing_Arrays_Messages_Message_Loop;

   procedure Test_Generating_Arrays_Message (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected                      : constant RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Buffer                        : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Context                       : Arrays.Message.Context := Arrays.Message.Create;
      Modular_Vector_Context        : Arrays.Modular_Vector.Context := Arrays.Modular_Vector.Create;
      Range_Vector_Context          : Arrays.Range_Vector.Context := Arrays.Range_Vector.Create;
      Enumeration_Vector_Context    : Arrays.Enumeration_Vector.Context := Arrays.Enumeration_Vector.Create;
      AV_Enumeration_Vector_Context : Arrays.AV_Enumeration_Vector.Context := Arrays.AV_Enumeration_Vector.Create;
   begin
      Arrays.Message.Initialize (Context, Buffer);
      Arrays.Message.Set_Length (Context, 4);

      Arrays.Message.Switch_To_Modular_Vector (Context, Modular_Vector_Context);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 1);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 2);
      Assert (not Arrays.Modular_Vector.Valid_Element (Modular_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
              "Valid Modular_Vector before context update");
      Arrays.Message.Update_Modular_Vector (Context, Modular_Vector_Context);
      Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector),
              "Invalid Modular_Vector after context update");
      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete generating");

      Arrays.Message.Switch_To_Range_Vector (Context, Range_Vector_Context);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 1);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 2);
      Assert (not Arrays.Range_Vector.Valid_Element (Range_Vector_Context), "Invalid acceptance of further element");
      Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
              "Valid Range_Vector before context update");
      Arrays.Message.Update_Range_Vector (Context, Range_Vector_Context);
      Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector),
              "Invalid Range_Vector after context update");
      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete generating");

      Arrays.Message.Switch_To_Enumeration_Vector (Context, Enumeration_Vector_Context);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.ONE);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.TWO);
      Assert (not Arrays.Enumeration_Vector.Valid_Element (Enumeration_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
              "Valid Enumeration_Vector before context update");
      Arrays.Message.Update_Enumeration_Vector (Context, Enumeration_Vector_Context);
      Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector),
              "Invalid Enumeration_Vector after context update");
      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete generating");

      Arrays.Message.Switch_To_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_ONE));
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_TWO));
      Assert (not Arrays.AV_Enumeration_Vector.Valid_Element (AV_Enumeration_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
              "Valid AV_Enumeration_Vector before context update");
      Arrays.Message.Update_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);
      Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector),
              "Invalid AV_Enumeration_Vector after context update");
      Assert (Arrays.Message.Valid_Message (Context), "Invalid Message after complete generating");

      Arrays.Message.Take_Buffer (Context, Buffer);
      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");
   end Test_Generating_Arrays_Message;

   procedure Test_Generating_Arrays_Messages_Message (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new Arrays.Inner_Message.Set_Payload (Write_Data);
      Expected         : constant RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 3, 2, 4, 6);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0);
      Context          : Arrays.Messages_Message.Context := Arrays.Messages_Message.Create;
      Sequence_Context : Arrays.Inner_Messages.Context := Arrays.Inner_Messages.Create;
      Element_Context  : Arrays.Inner_Message.Context := Arrays.Inner_Message.Create;
   begin
      Arrays.Messages_Message.Initialize (Context, Buffer);
      Arrays.Messages_Message.Set_Length (Context, 5);

      Arrays.Messages_Message.Switch_To_Messages (Context, Sequence_Context);

      if Arrays.Inner_Messages.Valid_Element (Sequence_Context) then
         Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
         Arrays.Inner_Message.Set_Length (Element_Context, 1);
         Data := (3, 0);
         Set_Payload (Element_Context);
         Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 1");
         Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

         if Arrays.Inner_Messages.Valid_Element (Sequence_Context) then
            Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
            Arrays.Inner_Message.Set_Length (Element_Context, 2);
            Data := (4, 6);
            Set_Payload (Element_Context);
            Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 2");
            Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

            Assert (not Arrays.Inner_Messages.Valid_Element (Sequence_Context),
                    "Invalid acceptance of further element");
            Assert (not Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                    "Valid Messages before context update");
            Arrays.Messages_Message.Update_Messages (Context, Sequence_Context);
            Assert (Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages),
                    "Invalid Messages after context update");

            Arrays.Messages_Message.Take_Buffer (Context, Buffer);
            Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
                    - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
            Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
                    Expected.all,
                    "Invalid binary representation");
         end if;
      end if;
   end Test_Generating_Arrays_Messages_Message;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Arrays_Modular_Sequential'Access, "Parsing Modular Sequential");
      Register_Routine (T, Test_Parsing_Arrays_Modular_Loop'Access, "Parsing Modular Loop");
      Register_Routine (T, Test_Parsing_Arrays_Range_Sequential'Access, "Parsing Range Sequential");
      Register_Routine (T, Test_Parsing_Arrays_Range_Loop'Access, "Parsing Range Loop");
      Register_Routine (T, Test_Parsing_Arrays_Enumeration_Sequential'Access, "Parsing Enumeration Sequential");
      Register_Routine (T, Test_Parsing_Arrays_Enumeration_Loop'Access, "Parsing Enumeration Loop");
      Register_Routine (T, Test_Parsing_Arrays_AV_Enumeration_Sequential'Access, "Parsing AV_Enumeration Sequential");
      Register_Routine (T, Test_Parsing_Arrays_AV_Enumeration_Loop'Access, "Parsing AV_Enumeration Loop");
      Register_Routine (T, Test_Parsing_Arrays_Message'Access, "Parsing Message");
      Register_Routine (T, Test_Parsing_Arrays_Messages_Message_Sequential'Access,
                        "Parsing Messages Message Sequential");
      Register_Routine (T, Test_Parsing_Arrays_Messages_Message_Loop'Access, "Parsing Messages Message Loop");
      Register_Routine (T, Test_Generating_Arrays_Message'Access, "Generating Message");
      Register_Routine (T, Test_Generating_Arrays_Messages_Message'Access, "Generating Messages Message");
   end Register_Tests;

end RFLX.Arrays.Tests;
