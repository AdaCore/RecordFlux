with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Types; use type RFLX.Types.Integer_Address;

with RFLX.Derivation.Message;
with RFLX.Derivation.Modular_Vector;
with RFLX.Derivation.Range_Vector;
with RFLX.Derivation.Enumeration_Vector;
with RFLX.Derivation.AV_Enumeration_Vector;

package body RFLX.Derivation.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Derivation");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""Sequence_Context""");
   pragma Warnings (Off, "unused assignment to ""Element_Context""");

   procedure Test_Derivation_Modular_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Modular_Vector.Context := Derivation.Modular_Vector.Create;
      Element          : Derivation.Modular_Integer;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Modular_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            if Derivation.Modular_Vector.Valid_Element (Sequence_Context) then
               Element := Derivation.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Derivation.Modular_Integer'Image (1), "Invalid value of element 1");

               Derivation.Modular_Vector.Next (Sequence_Context);
               if Derivation.Modular_Vector.Valid_Element (Sequence_Context) then
                  Element := Derivation.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Derivation.Modular_Integer'Image (2), "Invalid value of element 2");

                  Derivation.Modular_Vector.Next (Sequence_Context);
                  Assert (not Derivation.Modular_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
                  Derivation.Message.Update (Context, Sequence_Context);
                  Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
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

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Modular_Sequential;

   procedure Test_Derivation_Modular_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Modular_Vector.Context := Derivation.Modular_Vector.Create;
      Element          : Derivation.Modular_Integer;
      I                : Natural := 1;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Modular_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Derivation.Modular_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Derivation.Modular_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Derivation.Modular_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Derivation.Modular_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Derivation.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

            Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
            Derivation.Message.Update (Context, Sequence_Context);
            Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
         else
            Assert (False, "Invalid Modular_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Modular_Loop;

   procedure Test_Derivation_Range_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Range_Vector.Context := Derivation.Range_Vector.Create;
      Element          : Derivation.Range_Integer;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Range_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            if Derivation.Range_Vector.Valid_Element (Sequence_Context) then
               Element := Derivation.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Derivation.Range_Integer'Image (1), "Invalid value of element 1");

               Derivation.Range_Vector.Next (Sequence_Context);
               if Derivation.Range_Vector.Valid_Element (Sequence_Context) then
                  Element := Derivation.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Derivation.Range_Integer'Image (2), "Invalid value of element 2");

                  Derivation.Range_Vector.Next (Sequence_Context);

                  Assert (not Derivation.Range_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Valid Range_Vector before context update");
                  Derivation.Message.Update (Context, Sequence_Context);
                  Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Invalid Range_Vector after context update");
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

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Range_Sequential;

   procedure Test_Derivation_Range_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Range_Vector.Context := Derivation.Range_Vector.Create;
      Element          : Derivation.Range_Integer;
      I                : Natural := 1;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Range_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Derivation.Range_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Derivation.Range_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Derivation.Range_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Derivation.Range_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Derivation.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

            Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Valid Range_Vector before context update");
            Derivation.Message.Update (Context, Sequence_Context);
            Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Invalid Range_Vector after context update");
         else
            Assert (False, "Invalid Range_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Range_Loop;

   procedure Test_Derivation_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Enumeration_Vector.Context := Derivation.Enumeration_Vector.Create;
      Element          : Derivation.Enumeration;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Enumeration_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            if Derivation.Enumeration_Vector.Valid_Element (Sequence_Context) then
               Element := Derivation.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Element'Image, Derivation.Enumeration'Image (Derivation.ONE), "Invalid value of element 1");

               Derivation.Enumeration_Vector.Next (Sequence_Context);
               if Derivation.Enumeration_Vector.Valid_Element (Sequence_Context) then
                  Element := Derivation.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Derivation.Enumeration'Image (Derivation.TWO), "Invalid value of element 2");

                  Derivation.Enumeration_Vector.Next (Sequence_Context);

                  Assert (not Derivation.Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
                  Derivation.Message.Update (Context, Sequence_Context);
                  Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
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

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Enumeration_Sequential;

   procedure Test_Derivation_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Enumeration_Vector.Context := Derivation.Enumeration_Vector.Create;
      Element          : Derivation.Enumeration;
      I                : Natural := 1;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_Enumeration_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Derivation.Enumeration_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Derivation.Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Derivation.Enumeration_Vector.Get_Element (Sequence_Context);
               Assert (Derivation.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

               Derivation.Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Derivation.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

            Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
            Derivation.Message.Update (Context, Sequence_Context);
            Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
         else
            Assert (False, "Invalid Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_Enumeration_Loop;

   procedure Test_Derivation_AV_Enumeration_Sequential (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Av_Enumeration_Vector.Context := Derivation.Av_Enumeration_Vector.Create;
      Element          : Derivation.AV_Enumeration;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_AV_Enumeration_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            if Derivation.AV_Enumeration_Vector.Valid_Element (Sequence_Context) then
               Element := Derivation.AV_Enumeration_Vector.Get_Element (Sequence_Context);
               if Element.Known then
                  Assert (Element.Enum'Image, Derivation.AV_ONE'Image, "Invalid value of element 1");
               else
                  Assert (False, "Unknown value of element 1");
               end if;

               Derivation.AV_Enumeration_Vector.Next (Sequence_Context);
               if Derivation.AV_Enumeration_Vector.Valid_Element (Sequence_Context) then
                  Element := Derivation.AV_Enumeration_Vector.Get_Element (Sequence_Context);
                  if Element.Known then
                     Assert (Element.Enum'Image, Derivation.AV_TWO'Image, "Invalid value of element 2");
                  else
                     Assert (False, "Unknown value of element 2");
                  end if;

                  Derivation.AV_Enumeration_Vector.Next (Sequence_Context);

                  Assert (not Derivation.AV_Enumeration_Vector.Valid_Element (Sequence_Context), "Invalid acceptance of further element");

                  Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
                  Derivation.Message.Update (Context, Sequence_Context);
                  Assert (Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
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

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_AV_Enumeration_Sequential;

   procedure Test_Derivation_AV_Enumeration_Loop (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer           : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2);
      Context          : Derivation.Message.Context := Derivation.Message.Create;
      Length           : Derivation.Length;
      Sequence_Context : Derivation.Av_Enumeration_Vector.Context := Derivation.Av_Enumeration_Vector.Create;
      Element          : Derivation.AV_Enumeration;
      I                : Natural := 1;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         if Derivation.Message.Present (Context, Derivation.Message.F_AV_Enumeration_Vector) then
            Derivation.Message.Switch (Context, Sequence_Context);

            while I <= 10 and then Derivation.AV_Enumeration_Vector.Valid_Element (Sequence_Context) loop
               pragma Loop_Invariant (Derivation.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
               pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
               pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

               Element := Derivation.AV_Enumeration_Vector.Get_Element (Sequence_Context);
               if Element.Known then
                  Assert (Derivation.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
               else
                  Assert (False, "Unknown value of element " & I'Image);
               end if;

               Derivation.AV_Enumeration_Vector.Next (Sequence_Context);
               I := I + 1;
            end loop;

            Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

            Assert (Derivation.AV_Enumeration_Vector.Valid (Sequence_Context), "Invalid AV_Enumeration_Vector after parsing");

            Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
            Derivation.Message.Update (Context, Sequence_Context);
            Assert (Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
         else
            Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
         end if;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");
   end Test_Derivation_AV_Enumeration_Loop;

   procedure Test_Derivation_Message_Verification (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(4, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2);
      Context : Derivation.Message.Context := Derivation.Message.Create;
      Length  : Derivation.Length;
   begin
      Derivation.Message.Initialize (Context, Buffer);

      Derivation.Message.Verify_Message (Context);
      if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
         Length := Derivation.Message.Get_Length (Context);
         Assert (Length'Image, Derivation.Length'Image (4), "Unexpected Length");

         Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Derivation.Modular_Vector.Context := Derivation.Modular_Vector.Create;
            Element          : Derivation.Modular_Integer;
            I                : Natural := 1;
         begin
            if Derivation.Message.Present (Context, Derivation.Message.F_Modular_Vector) then
               Derivation.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Derivation.Modular_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Derivation.Modular_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Derivation.Modular_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Derivation.Modular_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Derivation.Modular_Vector.Valid (Sequence_Context), "Invalid Modular_Vector after parsing");

               Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Valid Modular_Vector before context update");
               Derivation.Message.Update (Context, Sequence_Context);
               Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Modular_Vector), "Invalid Modular_Vector after context update");
            else
               Assert (False, "Invalid Modular_Vector or Buffer");
            end if;
         end;

         Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Derivation.Range_Vector.Context := Derivation.Range_Vector.Create;
            Element          : Derivation.Range_Integer;
            I                : Natural := 1;
         begin
            if Derivation.Message.Present (Context, Derivation.Message.F_Range_Vector) then
               Derivation.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Derivation.Range_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Derivation.Range_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Derivation.Range_Vector.Get_Element (Sequence_Context);
                  Assert (Element'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Derivation.Range_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Derivation.Range_Vector.Valid (Sequence_Context), "Invalid Range_Vector after parsing");

               Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Valid Range_Vector before context update");
               Derivation.Message.Update (Context, Sequence_Context);
               Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Range_Vector), "Invalid Range_Vector after context update");
            else
               Assert (False, "Invalid Range_Vector or Buffer");
            end if;
         end;

         Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Derivation.Enumeration_Vector.Context := Derivation.Enumeration_Vector.Create;
            Element          : Derivation.Enumeration;
            I                : Natural := 1;
         begin
            if Derivation.Message.Present (Context, Derivation.Message.F_Enumeration_Vector) then
               Derivation.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Derivation.Enumeration_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Derivation.Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Derivation.Enumeration_Vector.Get_Element (Sequence_Context);
                  Assert (Derivation.Enumeration'Pos (Element)'Image, Natural'Image (I), "Invalid value of element " & I'Image);

                  Derivation.Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Derivation.Enumeration_Vector.Valid (Sequence_Context), "Invalid Enumeration_Vector after parsing");

               Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Valid Enumeration_Vector before context update");
               Derivation.Message.Update (Context, Sequence_Context);
               Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Enumeration_Vector), "Invalid Enumeration_Vector after context update");
            else
               Assert (False, "Invalid Enumeration_Vector or Buffer");
            end if;
         end;

         Assert (not Derivation.Message.Valid_Message (Context), "Valid Message before complete parsing");

         declare
            Sequence_Context : Derivation.Av_Enumeration_Vector.Context := Derivation.Av_Enumeration_Vector.Create;
            Element          : Derivation.AV_Enumeration;
            I                : Natural := 1;
         begin
            if Derivation.Message.Present (Context, Derivation.Message.F_AV_Enumeration_Vector) then
               Derivation.Message.Switch (Context, Sequence_Context);

               while I <= 10 and then Derivation.AV_Enumeration_Vector.Valid_Element (Sequence_Context) loop
                  pragma Loop_Invariant (Derivation.AV_Enumeration_Vector.Has_Buffer (Sequence_Context));
                  pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
                  pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);

                  Element := Derivation.AV_Enumeration_Vector.Get_Element (Sequence_Context);
                  if Element.Known then
                     Assert (Derivation.AV_Enumeration_Enum'Pos (Element.Enum)'Image, Natural'Image (I), "Invalid value of element " & I'Image);
                  else
                     Assert (False, "Unknown value of element " & I'Image);
                  end if;

                  Derivation.AV_Enumeration_Vector.Next (Sequence_Context);
                  I := I + 1;
               end loop;

               Assert (I'Image, Natural'Image (3), "Unexpected number of elements");

               Assert (Derivation.AV_Enumeration_Vector.Valid (Sequence_Context), "Invalid AV_Enumeration_Vector after parsing");

               Assert (not Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Valid AV_Enumeration_Vector before context update");
               Derivation.Message.Update (Context, Sequence_Context);
               Assert (Derivation.Message.Valid (Context, Derivation.Message.F_AV_Enumeration_Vector), "Invalid AV_Enumeration_Vector after context update");
            else
               Assert (False, "Invalid AV_Enumeration_Vector or Buffer");
            end if;
         end;
      else
         Assert (False, "Invalid Length");
      end if;

      Assert (Derivation.Message.Valid_Message (Context), "Invalid Message after complete parsing");
   end Test_Derivation_Message_Verification;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Derivation_Modular_Sequential'Access, "Modular Sequential");
      Register_Routine (T, Test_Derivation_Modular_Loop'Access, "Modular Loop");
      Register_Routine (T, Test_Derivation_Range_Sequential'Access, "Range Sequential");
      Register_Routine (T, Test_Derivation_Range_Loop'Access, "Range Loop");
      Register_Routine (T, Test_Derivation_Enumeration_Sequential'Access, "Enumeration Sequential");
      Register_Routine (T, Test_Derivation_Enumeration_Loop'Access, "Enumeration Loop");
      Register_Routine (T, Test_Derivation_AV_Enumeration_Sequential'Access, "AV_Enumeration Sequential");
      Register_Routine (T, Test_Derivation_AV_Enumeration_Loop'Access, "AV_Enumeration Loop");
   end Register_Tests;

end RFLX.Derivation.Tests;
