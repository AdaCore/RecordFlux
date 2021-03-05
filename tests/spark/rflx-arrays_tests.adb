with SPARK; use SPARK;
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
with RFLX.Arrays.Array_Size_Defined_By_Message_Size;

package body RFLX.Arrays_Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Arrays");
   end Name;

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 1) :=
     (others => 0);

   function Valid_Data_Length (L : RFLX_Builtin_Types.Length) return Boolean is
     (L <= Data'Length);

   procedure Write_Data (Buffer : out RFLX_Builtin_Types.Bytes) with
     SPARK_Mode,
     Pre => Valid_Data_Length (Buffer'Length)
   is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   procedure Test_Parsing_Scalar_Array_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Context : Arrays.Message.Context;
      Length  : Arrays.Length;
      package Message renames Arrays.Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Length), "Invalid Length");

      Length := Message.Get_Length (Context);

      Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");
      Assert (Message.Present (Context, Message.F_Modular_Vector), "Invalid Modular_Vector or Buffer");

      declare
         Sequence_Context : Arrays.Modular_Vector.Context;
         Element          : Arrays.Modular_Integer;
      begin
         Message.Switch_To_Modular_Vector (Context, Sequence_Context);

         Assert (Arrays.Modular_Vector.Has_Element (Sequence_Context), "Missing element 1");

         Arrays.Modular_Vector.Next (Sequence_Context);

         Assert (Arrays.Modular_Vector.Valid_Element (Sequence_Context), "Invalid element 2");

         Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.Modular_Integer'Image (1), "Invalid value of element 1");
         Assert (Arrays.Modular_Vector.Has_Element (Sequence_Context), "Missing element 2");

         Arrays.Modular_Vector.Next (Sequence_Context);

         Assert (Arrays.Modular_Vector.Valid_Element (Sequence_Context), "Invalid element 2");

         Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.Modular_Integer'Image (2), "Invalid value of element 2");
         Assert (not Arrays.Modular_Vector.Has_Element (Sequence_Context),
                 "Invalid acceptance of further element");
         Assert (not Message.Valid (Context, Message.F_Modular_Vector),
                 "Valid Modular_Vector before context update");
         Assert (Message.Complete_Modular_Vector (Context, Sequence_Context), "Incomplete Modular_Vector");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Modular_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Modular_Vector),
                 "Invalid Modular_Vector after context update");
      end;

      Assert (Message.Has_Buffer (Context) and then not Message.Valid_Message (Context),
              "Valid Message before complete parsing");
      Assert (Message.Present (Context, Message.F_Range_Vector), "Invalid Range_Vector or Buffer");

      declare
         Sequence_Context : Arrays.Range_Vector.Context;
         Element          : Arrays.Range_Integer;
      begin
         Message.Switch_To_Range_Vector (Context, Sequence_Context);

         Assert (Arrays.Range_Vector.Has_Element (Sequence_Context), "Missing element 1");

         Arrays.Range_Vector.Next (Sequence_Context);

         Assert (Arrays.Range_Vector.Valid_Element (Sequence_Context), "Invalid element 1");

         Element := Arrays.Range_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.Range_Integer'Image (1), "Invalid value of element 1");
         Assert (Arrays.Range_Vector.Has_Element (Sequence_Context), "Missing element 2");

         Arrays.Range_Vector.Next (Sequence_Context);

         Assert (Arrays.Range_Vector.Valid_Element (Sequence_Context), "Invalid element 2");

         Element := Arrays.Range_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.Range_Integer'Image (2), "Invalid value of element 2");
         Assert (not Arrays.Range_Vector.Has_Element (Sequence_Context),
                 "Invalid acceptance of further element");
         Assert (not Message.Valid (Context, Message.F_Range_Vector),
                 "Valid Range_Vector before context update");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Range_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Range_Vector),
                 "Invalid Range_Vector after context update");
      end;

      Assert (Message.Has_Buffer (Context) and then not Message.Valid_Message (Context),
              "Valid Message before complete parsing");
      Assert (Message.Present (Context, Message.F_Enumeration_Vector), "Invalid Enumeration_Vector or Buffer");

      declare
         Sequence_Context : Arrays.Enumeration_Vector.Context;
         Element          : Arrays.Enumeration;
      begin
         Message.Switch_To_Enumeration_Vector (Context, Sequence_Context);

         Assert (Arrays.Enumeration_Vector.Has_Element (Sequence_Context), "Missing element 1");

         Arrays.Enumeration_Vector.Next (Sequence_Context);

         Assert (Arrays.Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element 1");

         Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.One'Image, "Invalid value of element 1");
         Assert (Arrays.Enumeration_Vector.Has_Element (Sequence_Context), "Missing element 2");

         Arrays.Enumeration_Vector.Next (Sequence_Context);

         Assert (Arrays.Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element 2");

         Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Arrays.Two'Image, "Invalid value of element 2");
         Assert (not Arrays.Enumeration_Vector.Has_Element (Sequence_Context),
                 "Invalid acceptance of further element");
         Assert (not Message.Valid (Context, Message.F_Enumeration_Vector),
                 "Valid Enumeration_Vector before context update");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Enumeration_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Enumeration_Vector),
                 "Invalid Enumeration_Vector after context update");
      end;

      Assert (Message.Has_Buffer (Context) and then not Message.Valid_Message (Context),
              "Valid Message before complete parsing");
      Assert (Message.Present (Context, Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector or Buffer");

      declare
         Sequence_Context : Arrays.AV_Enumeration_Vector.Context;
         Element          : Arrays.AV_Enumeration;
      begin
         Message.Switch_To_AV_Enumeration_Vector (Context, Sequence_Context);

         Assert (Arrays.AV_Enumeration_Vector.Has_Element (Sequence_Context), "Missing element 1");

         Arrays.AV_Enumeration_Vector.Next (Sequence_Context);

         Assert (Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element 1");

         Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);

         Assert (Element.Known, "Unknown value of element 1");
         Assert (Element.Enum'Image, Arrays.AV_One'Image, "Invalid value of element 1");
         Assert (Arrays.AV_Enumeration_Vector.Has_Element (Sequence_Context), "Missing element 2");

         Arrays.AV_Enumeration_Vector.Next (Sequence_Context);

         Assert (Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element 2");

         Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);

         Assert (Element.Known, "Unknown value of element 2");
         Assert (Element.Enum'Image, Arrays.AV_Two'Image, "Invalid value of element 2");
         Assert (not Arrays.AV_Enumeration_Vector.Has_Element (Sequence_Context),
                 "Invalid acceptance of further element");
         Assert (not Message.Valid (Context, Message.F_AV_Enumeration_Vector),
                 "Valid AV_Enumeration_Vector before context update");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_AV_Enumeration_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_AV_Enumeration_Vector),
                 "Invalid AV_Enumeration_Vector after context update");
      end;

      Assert (Message.Valid_Message (Context), "Invalid Message after complete parsing");

      Message.Take_Buffer (Context, Buffer);
      Free_Bytes_Ptr (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (88)'Image, "Invalid Context.Last");
   end Test_Parsing_Scalar_Array_Sequential;

   procedure Test_Parsing_Scalar_Array_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Context : Arrays.Message.Context;
      Length  : Arrays.Length;
      package Message renames Arrays.Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Length), "Invalid Length");

      Length := Message.Get_Length (Context);

      Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete parsing");

      declare
         Sequence_Context : Arrays.Modular_Vector.Context;
         Element          : Arrays.Modular_Integer;
         I                : Natural := 1;
      begin
         Assert (Message.Present (Context, Message.F_Modular_Vector),
                 "Invalid Modular_Vector or Buffer");

         Message.Switch_To_Modular_Vector (Context, Sequence_Context);

         while Arrays.Modular_Vector.Has_Element (Sequence_Context) loop
            pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
            pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
            pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
            pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
            pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

            Assert (I <= 2, "Unexpected element");

            Arrays.Modular_Vector.Next (Sequence_Context);

            Assert (Arrays.Modular_Vector.Valid_Element (Sequence_Context), "Invalid element " & I'Image);

            Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);

            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
         Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");
         Assert (not Message.Valid (Context, Message.F_Modular_Vector),
                 "Valid Modular_Vector before context update");
         Assert (Message.Complete_Modular_Vector (Context, Sequence_Context), "Incomplete Modular_Vector");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Modular_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Modular_Vector),
                 "Invalid Modular_Vector after context update");
      end;

      Assert (not Message.Valid_Message (Context), "Valid Message before complete parsing");

      declare
         Sequence_Context : Arrays.Range_Vector.Context;
         Element          : Arrays.Range_Integer;
         I                : Natural := 1;
      begin
         Assert (Message.Present (Context, Message.F_Range_Vector),
                 "Invalid Range_Vector or Buffer");

         Message.Switch_To_Range_Vector (Context, Sequence_Context);

         while Arrays.Range_Vector.Has_Element (Sequence_Context) loop
            pragma Loop_Invariant (Arrays.Range_Vector.Has_Buffer (Sequence_Context));
            pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
            pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
            pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
            pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

            Assert (I <= 2, "Unexpected element");

            Arrays.Range_Vector.Next (Sequence_Context);

            Assert (Arrays.Range_Vector.Valid_Element (Sequence_Context), "Invalid element " & I'Image);

            Element := Arrays.Range_Vector.Get_Element (Sequence_Context);

            Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
         Assert (Arrays.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");
         Assert (not Message.Valid (Context, Message.F_Range_Vector),
                 "Valid Range_Vector before context update");
         Assert (Message.Complete_Range_Vector (Context, Sequence_Context), "Incomplete Range_Vector");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Range_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Range_Vector),
                 "Invalid Range_Vector after context update");
      end;

      Assert (not Message.Valid_Message (Context), "Valid Message before complete parsing");

      declare
         Sequence_Context : Arrays.Enumeration_Vector.Context;
         Element          : Arrays.Enumeration;
         I                : Natural := 1;
      begin
         Assert (Message.Present (Context, Message.F_Enumeration_Vector),
                 "Invalid Enumeration_Vector or Buffer");

         Message.Switch_To_Enumeration_Vector (Context, Sequence_Context);

         while Arrays.Enumeration_Vector.Has_Element (Sequence_Context) loop
            pragma Loop_Invariant (Arrays.Enumeration_Vector.Has_Buffer (Sequence_Context));
            pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
            pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
            pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
            pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

            Assert (I <= 2, "Unexpected element");

            Arrays.Enumeration_Vector.Next (Sequence_Context);

            Assert (Arrays.Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element " & I'Image);

            Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);

            Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I),
                    "Invalid value of element " & I'Image);

            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
         Assert (Arrays.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");
         Assert (not Message.Valid (Context, Message.F_Enumeration_Vector),
                 "Valid Enumeration_Vector before context update");
         Assert (Message.Complete_Enumeration_Vector (Context, Sequence_Context),
                 "Incomplete Enumeration_Vector");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_Enumeration_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_Enumeration_Vector),
                 "Invalid Enumeration_Vector after context update");
      end;

      Assert (not Message.Valid_Message (Context), "Valid Message before complete parsing");

      declare
         Sequence_Context : Arrays.AV_Enumeration_Vector.Context;
         Element          : Arrays.AV_Enumeration;
         I                : Natural := 1;
      begin
         Assert (Message.Present (Context, Message.F_AV_Enumeration_Vector),
                 "Invalid AV_Enumeration_Vector or Buffer");

         Message.Switch_To_AV_Enumeration_Vector (Context, Sequence_Context);

         while Arrays.AV_Enumeration_Vector.Has_Element (Sequence_Context) loop
            pragma Loop_Invariant (Arrays.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
            pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
            pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
            pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
            pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

            Assert (I <= 2, "Unexpected element");

            Arrays.AV_Enumeration_Vector.Next (Sequence_Context);

            Assert (Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid element " & I'Image);

            Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);

            Assert (Element.Known, "Unkown element " & I'Image);
            Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I),
                    "Invalid value of element " & I'Image);

            I := I + 1;
         end loop;

         Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
         Assert (Arrays.AV_Enumeration_Vector.Valid (Sequence_Context),
                 "Invalid AV_Enumeration_Vector after parsing");
         Assert (not Message.Valid (Context, Message.F_AV_Enumeration_Vector),
                 "Valid AV_Enumeration_Vector before context update");
         Assert (Message.Complete_AV_Enumeration_Vector (Context, Sequence_Context),
                 "Incomplete AV_Enumeration_Vector");

         --  WORKAROUND: Componolit/Workarounds#32
         pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
         pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
         Message.Update_AV_Enumeration_Vector (Context, Sequence_Context);
         pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Sequence_Context""");

         Assert (Message.Valid (Context, Message.F_AV_Enumeration_Vector),
                 "Invalid AV_Enumeration_Vector after context update");
      end;

      Assert (Message.Valid_Message (Context), "Invalid Message after complete parsing");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");
      Free_Bytes_Ptr (Buffer);
   end Test_Parsing_Scalar_Array_Loop;

   procedure Test_Generating_Scalar_Array (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected                      : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Buffer                        : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Context                       : Arrays.Message.Context;
      Modular_Vector_Context        : Arrays.Modular_Vector.Context;
      Range_Vector_Context          : Arrays.Range_Vector.Context;
      Enumeration_Vector_Context    : Arrays.Enumeration_Vector.Context;
      AV_Enumeration_Vector_Context : Arrays.AV_Enumeration_Vector.Context;
      package Message renames Arrays.Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 4);
      Message.Switch_To_Modular_Vector (Context, Modular_Vector_Context);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 1);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 2);

      Assert (not Arrays.Modular_Vector.Has_Element (Modular_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Modular_Vector),
              "Valid Modular_Vector before context update");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Modular_Vector_Context""");
      pragma Warnings (Off, """Modular_Vector_Context"" is set by ""*"" but not used after the call");
      Message.Update_Modular_Vector (Context, Modular_Vector_Context);
      pragma Warnings (On, """Modular_Vector_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Modular_Vector_Context""");

      Assert (Message.Valid (Context, Message.F_Modular_Vector),
              "Invalid Modular_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Switch_To_Range_Vector (Context, Range_Vector_Context);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 1);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 2);

      Assert (not Arrays.Range_Vector.Has_Element (Range_Vector_Context), "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Range_Vector),
              "Valid Range_Vector before context update");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Range_Vector_Context""");
      pragma Warnings (Off, """Range_Vector_Context"" is set by ""*"" but not used after the call");
      Message.Update_Range_Vector (Context, Range_Vector_Context);
      pragma Warnings (On, """Range_Vector_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Range_Vector_Context""");

      Assert (Message.Valid (Context, Message.F_Range_Vector),
              "Invalid Range_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Switch_To_Enumeration_Vector (Context, Enumeration_Vector_Context);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.One);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.Two);

      Assert (not Arrays.Enumeration_Vector.Has_Element (Enumeration_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Enumeration_Vector),
              "Valid Enumeration_Vector before context update");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Enumeration_Vector_Context""");
      pragma Warnings (Off, """Enumeration_Vector_Context"" is set by ""*"" but not used after the call");
      Message.Update_Enumeration_Vector (Context, Enumeration_Vector_Context);
      pragma Warnings (On, """Enumeration_Vector_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Enumeration_Vector_Context""");

      Assert (Message.Valid (Context, Message.F_Enumeration_Vector),
              "Invalid Enumeration_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Switch_To_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_One));
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_Two));

      Assert (not Arrays.AV_Enumeration_Vector.Has_Element (AV_Enumeration_Vector_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Valid AV_Enumeration_Vector before context update");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""AV_Enumeration_Vector_Context""");
      pragma Warnings (Off, """AV_Enumeration_Vector_Context"" is set by ""*"" but not used after the call");
      Message.Update_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);
      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""AV_Enumeration_Vector_Context""");
      pragma Warnings (Off, """AV_Enumeration_Vector_Context"" is set by ""*"" but not used after the call");

      Assert (Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Invalid AV_Enumeration_Vector after context update");
      Assert (Message.Valid_Message (Context), "Invalid Message after complete generating");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
   end Test_Generating_Scalar_Array;

   procedure Test_Generating_Scalar_Array_Independent (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected                      : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Buffer                        : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Modular_Vector_Buffer         : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0);
      Range_Vector_Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Enumeration_Vector_Buffer     : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      AV_Enumeration_Vector_Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context                       : Arrays.Message.Context;
      Modular_Vector_Context        : Arrays.Modular_Vector.Context;
      Range_Vector_Context          : Arrays.Range_Vector.Context;
      Enumeration_Vector_Context    : Arrays.Enumeration_Vector.Context;
      AV_Enumeration_Vector_Context : Arrays.AV_Enumeration_Vector.Context;
      package Message renames Arrays.Message;
   begin
      Arrays.Modular_Vector.Initialize (Modular_Vector_Context, Modular_Vector_Buffer);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 1);
      Arrays.Modular_Vector.Append_Element (Modular_Vector_Context, 2);

      Assert (not Arrays.Modular_Vector.Has_Element (Modular_Vector_Context),
              "Invalid acceptance of further element in Modular_Vector");
      Assert (not Message.Valid (Context, Message.F_Modular_Vector),
              "Valid Modular_Vector before context update");

      Arrays.Range_Vector.Initialize (Range_Vector_Context, Range_Vector_Buffer);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 1);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 2);

      Assert (not Arrays.Range_Vector.Has_Element (Range_Vector_Context),
              "Invalid acceptance of further element in Range_Vector");
      Assert (not Message.Valid (Context, Message.F_Range_Vector),
              "Valid Range_Vector before context update");

      Arrays.Enumeration_Vector.Initialize (Enumeration_Vector_Context, Enumeration_Vector_Buffer);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.One);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.Two);

      Assert (not Arrays.Enumeration_Vector.Has_Element (Enumeration_Vector_Context),
              "Invalid acceptance of further element in Enumeration_Vector");
      Assert (not Message.Valid (Context, Message.F_Enumeration_Vector),
              "Valid Enumeration_Vector before context update");

      Arrays.AV_Enumeration_Vector.Initialize (AV_Enumeration_Vector_Context, AV_Enumeration_Vector_Buffer);
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_One));
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_Two));

      Assert (not Arrays.AV_Enumeration_Vector.Has_Element (AV_Enumeration_Vector_Context),
              "Invalid acceptance of further element in AV_Enumeration_Vector");
      Assert (not Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Valid AV_Enumeration_Vector before context update");

      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 4);
      Message.Set_Modular_Vector (Context, Modular_Vector_Context);

      Assert (Message.Valid (Context, Message.F_Modular_Vector),
              "Invalid Modular_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_Range_Vector (Context, Range_Vector_Context);

      Assert (Message.Valid (Context, Message.F_Range_Vector),
              "Invalid Range_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_Enumeration_Vector (Context, Enumeration_Vector_Context);

      Assert (Message.Valid (Context, Message.F_Enumeration_Vector),
              "Invalid Enumeration_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);

      Assert (Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Invalid AV_Enumeration_Vector after context update");
      Assert (Message.Valid_Message (Context), "Invalid Message after complete generating");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""*_Context""");
      pragma Warnings (Off, """*_Context"" is set by ""*"" but not used after the call");
      Arrays.Modular_Vector.Take_Buffer (Modular_Vector_Context, Modular_Vector_Buffer);
      Arrays.Range_Vector.Take_Buffer (Range_Vector_Context, Range_Vector_Buffer);
      Arrays.Enumeration_Vector.Take_Buffer (Enumeration_Vector_Context, Enumeration_Vector_Buffer);
      Arrays.AV_Enumeration_Vector.Take_Buffer (AV_Enumeration_Vector_Context, AV_Enumeration_Vector_Buffer);
      pragma Warnings (On, "unused assignment to ""*_Context""");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Modular_Vector_Buffer);
      Free_Bytes_Ptr (Range_Vector_Buffer);
      Free_Bytes_Ptr (Enumeration_Vector_Buffer);
      Free_Bytes_Ptr (AV_Enumeration_Vector_Buffer);
   end Test_Generating_Scalar_Array_Independent;

   procedure Test_Generating_Scalar_Array_Independent_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected                      : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(0, 1, 2, 1, 2, 1, 2);
      Buffer                        : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0);
      Modular_Vector_Buffer         : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Range_Vector_Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Enumeration_Vector_Buffer     : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      AV_Enumeration_Vector_Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context                       : Arrays.Message.Context;
      Modular_Vector_Context        : Arrays.Modular_Vector.Context;
      Range_Vector_Context          : Arrays.Range_Vector.Context;
      Enumeration_Vector_Context    : Arrays.Enumeration_Vector.Context;
      AV_Enumeration_Vector_Context : Arrays.AV_Enumeration_Vector.Context;
      package Message renames Arrays.Message;
   begin
      Arrays.Modular_Vector.Initialize (Modular_Vector_Context, Modular_Vector_Buffer);
      Assert (Arrays.Modular_Vector.Has_Element (Modular_Vector_Context),
              "No acceptance of further element in Modular_Vector");

      Assert (not Message.Valid (Context, Message.F_Modular_Vector),
              "Valid Modular_Vector before context update");

      Arrays.Range_Vector.Initialize (Range_Vector_Context, Range_Vector_Buffer);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 1);
      Arrays.Range_Vector.Append_Element (Range_Vector_Context, 2);

      Assert (not Arrays.Range_Vector.Has_Element (Range_Vector_Context),
              "Invalid acceptance of further element in Range_Vector");
      Assert (not Message.Valid (Context, Message.F_Range_Vector),
              "Valid Range_Vector before context update");

      Arrays.Enumeration_Vector.Initialize (Enumeration_Vector_Context, Enumeration_Vector_Buffer);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.One);
      Arrays.Enumeration_Vector.Append_Element (Enumeration_Vector_Context, Arrays.Two);

      Assert (not Arrays.Enumeration_Vector.Has_Element (Enumeration_Vector_Context),
              "Invalid acceptance of further element in Enumeration_Vector");
      Assert (not Message.Valid (Context, Message.F_Enumeration_Vector),
              "Valid Enumeration_Vector before context update");

      Arrays.AV_Enumeration_Vector.Initialize (AV_Enumeration_Vector_Context, AV_Enumeration_Vector_Buffer);
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_One));
      Arrays.AV_Enumeration_Vector.Append_Element (AV_Enumeration_Vector_Context, Arrays.To_Actual (Arrays.AV_Two));

      Assert (not Arrays.AV_Enumeration_Vector.Has_Element (AV_Enumeration_Vector_Context),
              "Invalid acceptance of further element in AV_Enumeration_Vector");
      Assert (not Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Valid AV_Enumeration_Vector before context update");

      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 0);
      Message.Set_Modular_Vector (Context, Modular_Vector_Context);

      Assert (Message.Structural_Valid (Context, Message.F_Modular_Vector),
              "Invalid Modular_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_Range_Vector (Context, Range_Vector_Context);

      Assert (Message.Valid (Context, Message.F_Range_Vector),
              "Invalid Range_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_Enumeration_Vector (Context, Enumeration_Vector_Context);

      Assert (Message.Valid (Context, Message.F_Enumeration_Vector),
              "Invalid Enumeration_Vector after context update");
      Assert (not Message.Valid_Message (Context), "Valid Message before complete generating");

      Message.Set_AV_Enumeration_Vector (Context, AV_Enumeration_Vector_Context);

      Assert (Message.Valid (Context, Message.F_AV_Enumeration_Vector),
              "Invalid AV_Enumeration_Vector after context update");
      Assert (Message.Structural_Valid_Message (Context), "Invalid Message after complete generating");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""*_Context""");
      pragma Warnings (Off, """*_Context"" is set by ""*"" but not used after the call");
      Arrays.Modular_Vector.Take_Buffer (Modular_Vector_Context, Modular_Vector_Buffer);
      Arrays.Range_Vector.Take_Buffer (Range_Vector_Context, Range_Vector_Buffer);
      Arrays.Enumeration_Vector.Take_Buffer (Enumeration_Vector_Context, Enumeration_Vector_Buffer);
      Arrays.AV_Enumeration_Vector.Take_Buffer (AV_Enumeration_Vector_Context, AV_Enumeration_Vector_Buffer);
      pragma Warnings (On, "unused assignment to ""*_Context""");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Modular_Vector_Buffer);
      Free_Bytes_Ptr (Range_Vector_Buffer);
      Free_Bytes_Ptr (Enumeration_Vector_Buffer);
      Free_Bytes_Ptr (AV_Enumeration_Vector_Buffer);
   end Test_Generating_Scalar_Array_Independent_Empty;

   procedure Test_Parsing_Message_Array_Sequential (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 0, 2, 0, 0);
      Context          : Arrays.Messages_Message.Context;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Inner_Messages.Context;
      Element_Context  : Arrays.Inner_Message.Context;
      package Message renames Arrays.Messages_Message;
      package Inner_Message renames Arrays.Inner_Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Length), "Invalid Length");

      Length := Message.Get_Length (Context);

      Assert (Length'Image, Arrays.Length'Image (5), "Unexpected Length");
      Assert (Message.Present (Context, Message.F_Messages),
              "Invalid Messages or Buffer");

      Message.Switch_To_Messages (Context, Sequence_Context);

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Invalid element 1");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
      Inner_Message.Verify_Message (Element_Context);

      Assert (Inner_Message.Valid (Element_Context, Inner_Message.F_Length),
              "Invalid Length of Inner_Message 1");

      Length := Inner_Message.Get_Length (Element_Context);

      Assert (Length'Image, Arrays.Length'Image (1), "Unexpected Length of element 1");
      Assert (Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 1");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Invalid element 2");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

      Inner_Message.Verify_Message (Element_Context);

      Assert (Inner_Message.Valid (Element_Context, Inner_Message.F_Length),
              "Invalid Length of Inner_Message 2");

      Length := Inner_Message.Get_Length (Element_Context);

      Assert (Length'Image, Arrays.Length'Image (2), "Unexpected Length of element 2");
      Assert (Inner_Message.Structural_Valid_Message (Element_Context),
              "Structural invalid element 2");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (not Arrays.Inner_Messages.Has_Element (Sequence_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Messages),
              "Valid Messages before context update");
      Assert (Message.Complete_Messages (Context, Sequence_Context),
              "Invalid Messages_Message");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Message.Update_Messages (Context, Sequence_Context);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Assert (Message.Valid (Context, Message.F_Messages),
              "Invalid Messages after context update");
      Assert (Message.Has_Buffer (Context) and then Message.Valid_Message (Context),
              "Invalid Message after complete parsing");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");

      Free_Bytes_Ptr (Buffer);
   end Test_Parsing_Message_Array_Sequential;

   procedure Test_Parsing_Message_Array_Loop (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 0, 2, 0, 0);
      Context          : Arrays.Messages_Message.Context;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Inner_Messages.Context;
      Element_Context  : Arrays.Inner_Message.Context;
      I                : Natural := 1;
      package Message renames Arrays.Messages_Message;
      package Inner_Message renames Arrays.Inner_Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Length),  "Invalid Length");

      Length := Message.Get_Length (Context);

      Assert (Length'Image, Arrays.Length'Image (5), "Unexpected Length");
      Assert (Message.Present (Context, Message.F_Messages), "Invalid Messages or Buffer");

      Message.Switch_To_Messages (Context, Sequence_Context);

      while Arrays.Inner_Messages.Has_Element (Sequence_Context) loop
         pragma Loop_Invariant (Arrays.Inner_Messages.Has_Buffer (Sequence_Context));
         pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
         pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
         pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
         pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);
         pragma Loop_Invariant (not Inner_Message.Has_Buffer (Element_Context));

         Assert (I <= 2, "Unexpected element");

         Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

         Inner_Message.Verify_Message (Element_Context);

         Assert (Inner_Message.Valid (Element_Context, Inner_Message.F_Length),
                 "Invalid Length of Inner_Message " & I'Image);

         Length := Inner_Message.Get_Length (Element_Context);

         Assert (Length'Image, I'Image, "Unexpected Length of element " & I'Image);
         Assert (Inner_Message.Structural_Valid_Message (Element_Context),
                 "Structural invalid element " & I'Image);

         Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

         I := I + 1;
      end loop;

      Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      Assert (not Message.Valid (Context, Message.F_Messages),
              "Valid Messages before context update");
      Assert (Message.Complete_Messages (Context, Sequence_Context), "Invalid Messages_Message");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Message.Update_Messages (Context, Sequence_Context);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Assert (Message.Valid (Context, Message.F_Messages),
              "Invalid Messages after context update");

      Assert (Message.Valid_Message (Context), "Invalid Message after complete parsing");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");

      Free_Bytes_Ptr (Buffer);
   end Test_Parsing_Message_Array_Loop;

   procedure Test_Generating_Message_Array (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new Arrays.Inner_Message.Generic_Set_Payload (Write_Data, Valid_Data_Length);
      Expected         : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 3, 2, 4, 6);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0);
      Context          : Arrays.Messages_Message.Context;
      Sequence_Context : Arrays.Inner_Messages.Context;
      Element_Context  : Arrays.Inner_Message.Context;
      package Message renames Arrays.Messages_Message;
      package Inner_Message renames Arrays.Inner_Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 5);
      Message.Switch_To_Messages (Context, Sequence_Context);

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Missing element 1");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
      Inner_Message.Set_Length (Element_Context, 1);
      Data := (3, 0);
      Set_Payload (Element_Context);

      Assert (Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 1");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Missing element 2");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
      Inner_Message.Set_Length (Element_Context, 2);
      Data := (4, 6);
      Set_Payload (Element_Context);

      Assert (Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 2");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (not Arrays.Inner_Messages.Has_Element (Sequence_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Messages),
              "Valid Messages before context update");
      Assert (Message.Complete_Messages (Context, Sequence_Context),
              "Incomplete Messages");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Message.Update_Messages (Context, Sequence_Context);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Assert (Message.Valid (Context, Message.F_Messages),
              "Invalid Messages after context update");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
   end Test_Generating_Message_Array;

   procedure Test_Generating_Message_Array_Independent (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new Arrays.Inner_Message.Generic_Set_Payload (Write_Data, Valid_Data_Length);
      Expected         : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 1, 3, 2, 4, 6);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0);
      Sequence_Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0);
      Context          : Arrays.Messages_Message.Context;
      Sequence_Context : Arrays.Inner_Messages.Context;
      Element_Context  : Arrays.Inner_Message.Context;
      package Message renames Arrays.Messages_Message;
      package Inner_Message renames Arrays.Inner_Message;
   begin
      Arrays.Inner_Messages.Initialize (Sequence_Context, Sequence_Buffer);

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Invalid element 1");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
      Inner_Message.Set_Length (Element_Context, 1);
      Data := (3, 0);
      Set_Payload (Element_Context);

      Assert (Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 1");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (Arrays.Inner_Messages.Has_Element (Sequence_Context), "Invalid element 1");

      Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);
      Inner_Message.Set_Length (Element_Context, 2);
      Data := (4, 6);
      Set_Payload (Element_Context);

      Assert (Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 2");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Element_Context""");
      pragma Warnings (Off, """Element_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);
      pragma Warnings (On, """Element_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Element_Context""");

      Assert (not Arrays.Inner_Messages.Has_Element (Sequence_Context),
              "Invalid acceptance of further element");

      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 5);

      Assert (not Message.Valid (Context, Message.F_Messages),
              "Valid Messages before setting field");

      Message.Set_Messages (Context, Sequence_Context);

      Assert (Message.Valid (Context, Message.F_Messages),
              "Invalid Messages after setting field");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Take_Buffer (Sequence_Context, Sequence_Buffer);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Sequence_Buffer);
   end Test_Generating_Message_Array_Independent;

   procedure Test_Generating_Message_Array_Independent_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected         : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 0);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 1);
      Sequence_Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context          : Arrays.Messages_Message.Context;
      Sequence_Context : Arrays.Inner_Messages.Context;
      package Message renames Arrays.Messages_Message;
   begin
      Arrays.Inner_Messages.Initialize (Sequence_Context, Sequence_Buffer);

      Message.Initialize (Context, Buffer);
      Message.Set_Length (Context, 0);

      Assert (not Message.Valid (Context, Message.F_Messages),
              "Valid Messages before setting field");
      Message.Set_Messages (Context, Sequence_Context);
      Assert (Message.Structural_Valid (Context, Message.F_Messages),
              "Invalid Messages after setting field");

      Message.Take_Buffer (Context, Buffer);
      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Arrays.Inner_Messages.Take_Buffer (Sequence_Context, Sequence_Buffer);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Sequence_Buffer);
   end Test_Generating_Message_Array_Independent_Empty;

   procedure Test_Parsing_Array_Size_Defined_By_Message_Size (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 1, 0, 2);
      Context          : Arrays.Array_Size_Defined_By_Message_Size.Context;
      Sequence_Context : Arrays.Modular_Vector.Context;
      Header           : Arrays.Enumeration;
      Element          : Arrays.Modular_Integer;
      I                : Natural := 1;
      package Message renames Arrays.Array_Size_Defined_By_Message_Size;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Header), "Invalid Header");

      Header := Message.Get_Header (Context);

      Assert (Header'Image, Arrays.One'Image, "Invalid value of Header");
      Assert (Message.Present (Context, Message.F_Vector), "Invalid Vector or Buffer");

      Message.Switch_To_Vector (Context, Sequence_Context);

      while Arrays.Modular_Vector.Has_Element (Sequence_Context) loop
         pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
         pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
         pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
         pragma Loop_Invariant (Sequence_Context.First = Sequence_Context.First'Loop_Entry);
         pragma Loop_Invariant (Sequence_Context.Last = Sequence_Context.Last'Loop_Entry);

         Assert (I <= 2, "Unexpected element");

         Arrays.Modular_Vector.Next (Sequence_Context);

         Assert (Arrays.Modular_Vector.Valid_Element (Sequence_Context), "Invalid element " & I'Image);

         Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);

         Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

         I := I + 1;
      end loop;

      Assert (I'Image, Natural'Image (3), "Unexpected number of elements");
      Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Vector after parsing");
      Assert (not Message.Valid (Context, Message.F_Vector),
              "Valid Vector before context update");
      Assert (Message.Complete_Vector (Context, Sequence_Context), "Incomplete Vector");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Message.Update_Vector (Context, Sequence_Context);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Assert (Message.Valid (Context, Message.F_Vector),
              "Invalid Vector after context update");
      Assert (Message.Valid_Message (Context), "Invalid Message");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");

      Free_Bytes_Ptr (Buffer);
   end Test_Parsing_Array_Size_Defined_By_Message_Size;

   procedure Test_Parsing_Array_Size_Defined_By_Message_Size_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 1);
      Context          : Arrays.Array_Size_Defined_By_Message_Size.Context;
      Header           : Arrays.Enumeration;
      package Message renames Arrays.Array_Size_Defined_By_Message_Size;
   begin
      Message.Initialize (Context, Buffer);
      Message.Verify_Message (Context);

      Assert (Message.Valid (Context, Message.F_Header), "Invalid Header");

      Header := Message.Get_Header (Context);

      Assert (Header'Image, Arrays.One'Image, "Invalid value of Header");
      Assert (not Message.Present (Context, Message.F_Vector), "Present Vector");
      Assert (Message.Structural_Valid_Message (Context), "Invalid Message");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");

      Free_Bytes_Ptr (Buffer);
   end Test_Parsing_Array_Size_Defined_By_Message_Size_Empty;

   procedure Test_Generating_Array_Size_Defined_By_Message_Size (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected         : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 1, 0, 2);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0);
      Context          : Arrays.Array_Size_Defined_By_Message_Size.Context;
      Sequence_Context : Arrays.Modular_Vector.Context;
      package Message renames Arrays.Array_Size_Defined_By_Message_Size;
   begin
      Message.Initialize (Context, Buffer);

      Message.Set_Header (Context, Arrays.One);
      Message.Switch_To_Vector (Context, Sequence_Context);
      Arrays.Modular_Vector.Append_Element (Sequence_Context, 1);
      Arrays.Modular_Vector.Append_Element (Sequence_Context, 2);

      Assert (not Arrays.Modular_Vector.Has_Element (Sequence_Context),
              "Invalid acceptance of further element");
      Assert (not Message.Valid (Context, Message.F_Vector),
              "Valid Modular_Vector before context update");
      Assert (not Message.Valid (Context, Message.F_Vector),
              "Valid Modular_Vector before context update");

      --  WORKAROUND: Componolit/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
      pragma Warnings (Off, """Sequence_Context"" is set by ""*"" but not used after the call");
      Message.Update_Vector (Context, Sequence_Context);
      pragma Warnings (On, """Sequence_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Sequence_Context""");

      Assert (Message.Valid (Context, Message.F_Vector),
              "Invalid Modular_Vector after context update");
      Assert (Message.Valid_Message (Context), "Invalid Message");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Expected);
   end Test_Generating_Array_Size_Defined_By_Message_Size;

   procedure Test_Generating_Array_Size_Defined_By_Message_Size_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected         : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 1);
      Buffer           : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 0);
      Context          : Arrays.Array_Size_Defined_By_Message_Size.Context;
      package Message renames Arrays.Array_Size_Defined_By_Message_Size;
   begin
      Message.Initialize (Context, Buffer);
      Message.Set_Header (Context, Arrays.One);
      Message.Set_Vector_Empty (Context);

      Assert (Message.Structural_Valid (Context, Message.F_Vector), "Invalid Modular_Vector");
      Assert (Message.Structural_Valid_Message (Context), "Invalid Message");

      Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Buffer);
      Free_Bytes_Ptr (Expected);
   end Test_Generating_Array_Size_Defined_By_Message_Size_Empty;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Scalar_Array_Sequential'Access,
                        "Parsing scalar arrays (sequential)");
      Register_Routine (T, Test_Parsing_Scalar_Array_Loop'Access,
                        "Parsing scalar arrays (loop)");
      Register_Routine (T, Test_Generating_Scalar_Array'Access,
                        "Generating scalar arrays");
      Register_Routine (T, Test_Generating_Scalar_Array_Independent'Access,
                        "Generating scalar arrays (independent)");
      Register_Routine (T, Test_Generating_Scalar_Array_Independent_Empty'Access,
                        "Generating scalar arrays (independent, empty)");
      Register_Routine (T, Test_Parsing_Message_Array_Sequential'Access,
                        "Parsing message array (sequential)");
      Register_Routine (T, Test_Parsing_Message_Array_Loop'Access,
                        "Parsing message array (loop)");
      Register_Routine (T, Test_Generating_Message_Array'Access,
                        "Generating message array");
      Register_Routine (T, Test_Generating_Message_Array_Independent'Access,
                        "Generating message array (independent)");
      Register_Routine (T, Test_Generating_Message_Array_Independent_Empty'Access,
                        "Generating message array (independent, empty)");
      Register_Routine (T, Test_Parsing_Array_Size_Defined_By_Message_Size'Access,
                        "Parsing array with size defined by message size");
      Register_Routine (T, Test_Parsing_Array_Size_Defined_By_Message_Size_Empty'Access,
                        "Parsing array with size defined by message size (empty)");
      Register_Routine (T, Test_Generating_Array_Size_Defined_By_Message_Size'Access,
                        "Generating array with size defined by message size");
      Register_Routine (T, Test_Generating_Array_Size_Defined_By_Message_Size_Empty'Access,
                        "Generating array with size defined by message size (empty)");
   end Register_Tests;

end RFLX.Arrays_Tests;
