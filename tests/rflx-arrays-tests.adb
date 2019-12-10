with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Types; use type RFLX.Types.Integer_Address;

with RFLX.Arrays.Message;
with RFLX.Arrays.Modular_Vector;
with RFLX.Arrays.Range_Vector;
with RFLX.Arrays.Enumeration_Vector;
with RFLX.Arrays.AV_Enumeration_Vector;
with RFLX.Arrays.Messages_Message;
with RFLX.Arrays.Inner_Message;
with RFLX.Arrays.Inner_Messages;

package body RFLX.Arrays.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Arrays");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
   pragma Warnings (Off, "unused assignment to ""Element_Context""");

   procedure Test_Arrays_Modular_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            if Arrays.Modular_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Modular_Integer'Image (1), "Invalid value of element 1");

               Arrays.Modular_Vector.Next (Sequence_Context);
               if Arrays.Modular_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Modular_Integer'Image (2), "Invalid value of element 2");

                  Arrays.Modular_Vector.Next (Sequence_Context);
                  Assert (not Arrays.Modular_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
                  Arrays.Message.Update (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
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
   end Test_Arrays_Modular_Sequential;

   procedure Test_Arrays_Modular_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Arrays.Modular_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Arrays.Modular_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
            Arrays.Message.Update (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
         else
            Assert (False, "Invalid Modular_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Arrays_Modular_Loop;

   procedure Test_Arrays_Range_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            if Arrays.Range_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Range_Integer'Image (1), "Invalid value of element 1");

               Arrays.Range_Vector.Next (Sequence_Context);
               if Arrays.Range_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Range_Integer'Image (2), "Invalid value of element 2");

                  Arrays.Range_Vector.Next (Sequence_Context);

                  Assert (not Arrays.Range_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Valid Range_Vector before context update");
                  Arrays.Message.Update (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Invalid Range_Vector after context update");
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
   end Test_Arrays_Range_Sequential;

   procedure Test_Arrays_Range_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Arrays.Range_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Arrays.Range_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Arrays.Range_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Valid Range_Vector before context update");
            Arrays.Message.Update (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Invalid Range_Vector after context update");
         else
            Assert (False, "Invalid Range_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Arrays_Range_Loop;

   procedure Test_Arrays_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            if Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) then
               Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Arrays.Enumeration'Image (Arrays.ONE), "Invalid value of element 1");

               Arrays.Enumeration_Vector.Next (Sequence_Context);
               if Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) then
                  Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Arrays.Enumeration'Image (Arrays.TWO), "Invalid value of element 2");

                  Arrays.Enumeration_Vector.Next (Sequence_Context);

                  Assert (not Arrays.Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
                  Arrays.Message.Update (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
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
   end Test_Arrays_Enumeration_Sequential;

   procedure Test_Arrays_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
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
            Arrays.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Arrays.Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Arrays.Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
            Arrays.Message.Update (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
         else
            Assert (False, "Invalid Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Arrays_Enumeration_Loop;

   procedure Test_Arrays_AV_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Av_Enumeration_Vector.Context := Arrays.Av_Enumeration_Vector.Create;
      Element          : Arrays.AV_Enumeration;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
            Arrays.Message.Switch (Context, Sequence_Context);

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

                  Assert (not Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
                  Arrays.Message.Update (Context, Sequence_Context);
                  Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
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
   end Test_Arrays_AV_Enumeration_Sequential;

   procedure Test_Arrays_AV_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Arrays.Message.Context := Arrays.Message.Create;
      Length           : Arrays.Length;
      Sequence_Context : Arrays.Av_Enumeration_Vector.Context := Arrays.Av_Enumeration_Vector.Create;
      Element          : Arrays.AV_Enumeration;
      I                : Natural := 1;
   begin
      Arrays.Message.Initialize (Context, Buffer);

      Arrays.Message.Verify_Message (Context);
      if Arrays.Message.Valid (Context, Arrays.Message.F_Length) then
         Length := Arrays.Message.Get_Length (Context);
         Assert (Length'Image, Arrays.Length'Image (4), "Unexpected Length");

         if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
            Arrays.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Arrays.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
               if Element.Known then
                  Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
               else
                  Assert (False, "Unknown value of element " & I'Image);
               end if;

               Arrays.AV_Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Arrays.AV_Enumeration_Vector.Valid (Sequence_Context), "Invalid AV_Enumeration_Vector after parsing");

            Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
            Arrays.Message.Update (Context, Sequence_Context);
            Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
         else
            Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Arrays_AV_Enumeration_Loop;

   procedure Test_Arrays_Message_Verification (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
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
               Arrays.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Arrays.Modular_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Arrays.Modular_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Arrays.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Arrays.Modular_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
               Arrays.Message.Update (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
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
               Arrays.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Arrays.Range_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Arrays.Range_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Arrays.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Arrays.Range_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Valid Range_Vector before context update");
               Arrays.Message.Update (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Range_Vector), "Invalid Range_Vector after context update");
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
               Arrays.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Arrays.Enumeration_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Arrays.Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Arrays.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Arrays.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Arrays.Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
               Arrays.Message.Update (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
            else
               Assert (False, "Invalid Enumeration_Vector or Buffer");
            end if;
         end;

         Assert (not Arrays.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Arrays.Av_Enumeration_Vector.Context := Arrays.Av_Enumeration_Vector.Create;
            Element          : Arrays.AV_Enumeration;
            I                : Natural := 1;
         begin
            if Arrays.Message.Present (Context, Arrays.Message.F_AV_Enumeration_Vector) then
               Arrays.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Arrays.AV_Enumeration_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Arrays.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Arrays.AV_Enumeration_Vector.Get_Element (Sequence_Context);
                  if Element.Known then
                     Assert (Arrays.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
                  else
                     Assert (False, "Unknown value of element " & I'Image);
                  end if;

                  Arrays.AV_Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Arrays.AV_Enumeration_Vector.Valid (Sequence_Context), "Invalid AV_Enumeration_Vector after parsing");

               Assert (not Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
               Arrays.Message.Update (Context, Sequence_Context);
               Assert (Arrays.Message.Valid (Context, Arrays.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
            else
               Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
            end if;
         end;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Arrays.Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Arrays_Message_Verification;

   procedure Test_Arrays_Messages_Message_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(5, 1, 0, 2, 0, 0);
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
            Arrays.Messages_Message.Switch (Context, Sequence_Context);

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
                  Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element 2");

                  Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

                  Assert (not Arrays.Inner_Messages.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages), "Valid Messages before context update");
                  Arrays.Messages_Message.Update (Context, Sequence_Context);
                  Assert (Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages), "Invalid Messages after context update");
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
   end Test_Arrays_Messages_Message_Sequential;

   procedure Test_Arrays_Messages_Message_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(5, 1, 0, 2, 0, 0);
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
            Arrays.Messages_Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Arrays.Inner_Messages.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Arrays.Inner_Messages.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Arrays.Inner_Messages.Switch (Sequence_Context, Element_Context);

               Arrays.Inner_Message.Verify_Message (Element_Context);
               if Arrays.Inner_Message.Valid (Element_Context, Arrays.Inner_Message.F_Length) then
                  Length := Arrays.Inner_Message.Get_Length (Element_Context);
                  Assert (Length'Image, I'Image, "Unexpected Length of element " & I'Image);
               end if;
               Assert (Arrays.Inner_Message.Structural_Valid_Message (Element_Context), "Structural invalid element " & I'Image);

               Arrays.Inner_Messages.Update (Sequence_Context, Element_Context);

               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (not Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages), "Valid Messages before context update");
            Arrays.Messages_Message.Update (Context, Sequence_Context);
            Assert (Arrays.Messages_Message.Valid (Context, Arrays.Messages_Message.F_Messages), "Invalid Messages after context update");
         else
            Assert (False, "Invalid Messages or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Arrays.Messages_Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Arrays_Messages_Message_Loop;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Arrays_Modular_Sequential'Access, "Modular Sequential");
      Register_Routine (T, Test_Arrays_Modular_Loop'Access, "Modular Loop");
      Register_Routine (T, Test_Arrays_Range_Sequential'Access, "Range Sequential");
      Register_Routine (T, Test_Arrays_Range_Loop'Access, "Range Loop");
      Register_Routine (T, Test_Arrays_Enumeration_Sequential'Access, "Enumeration Sequential");
      Register_Routine (T, Test_Arrays_Enumeration_Loop'Access, "Enumeration Loop");
      Register_Routine (T, Test_Arrays_AV_Enumeration_Sequential'Access, "AV_Enumeration Sequential");
      Register_Routine (T, Test_Arrays_AV_Enumeration_Loop'Access, "AV_Enumeration Loop");
      Register_Routine (T, Test_Arrays_Message_Verification'Access, "Message Verification");
      Register_Routine (T, Test_Arrays_Messages_Message_Sequential'Access, "Messages Message Sequential");
      Register_Routine (T, Test_Arrays_Messages_Message_Loop'Access, "Messages Message Loop");
   end Register_Tests;

end RFLX.Arrays.Tests;
