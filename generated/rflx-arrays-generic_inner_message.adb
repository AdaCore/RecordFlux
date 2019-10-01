package body RFLX.Arrays.Generic_Inner_Message with
  SPARK_Mode
is

   function Create return Context is
     ((RFLX.Types.Index'First, RFLX.Types.Index'First, RFLX.Types.Bit_Index'First, RFLX.Types.Bit_Index'First, 0, null, RFLX.Types.Bit_Index'First, F_Initial, (others => (State => S_Invalid))));

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) is
      Buffer_First : constant RFLX.Types.Index := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index := Buffer'Last;
      Buffer_Address : constant RFLX.Types.Integer_Address := RFLX.Types.Bytes_Address (Buffer);
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, First, F_Initial, (others => (State => S_Invalid)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) is
   begin
      First := Ctx.Cursors (Fld).First;
      Last := Ctx.Cursors (Fld).Last;
   end Field_Range;

   function Index (Ctx : Context) return RFLX.Types.Bit_Index is
     (Ctx.Index);

   function Preliminary_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid
        or Ctx.Cursors (Fld).State = S_Preliminary)
      and then Ctx.Cursors (Fld).Value.Fld = Fld);

   function Preliminary_Valid_Predecessors (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial | F_Length =>
            True,
         when F_Payload =>
            Preliminary_Valid (Ctx, F_Length),
         when F_Final =>
            Preliminary_Valid (Ctx, F_Length)
               and then Preliminary_Valid (Ctx, F_Payload)));

   function Valid_Predecessors (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Length =>
            True,
         when F_Payload =>
            Present (Ctx, F_Length)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Ctx, Fld));

   function Valid_Target (Source_Field, Target_Field : Virtual_Field) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            Target_Field = F_Length,
         when F_Length =>
            Target_Field = F_Payload,
         when F_Payload =>
            Target_Field = F_Final,
         when F_Final =>
            False));

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
         when F_Length =>
            False,
         when F_Payload =>
            True));

   function Field_Condition (Ctx : Context; Source_Field, Target_Field : Virtual_Field) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            (case Target_Field is
                  when F_Length =>
                     True,
                  when others =>
                     False),
         when F_Length =>
            (case Target_Field is
                  when F_Payload =>
                     True,
                  when others =>
                     False),
         when F_Payload =>
            (case Target_Field is
                  when F_Final =>
                     True,
                  when others =>
                     False),
         when F_Final =>
            False))
    with
     Pre =>
       Valid_Target (Source_Field, Target_Field)
          and then Preliminary_Valid_Predecessors (Ctx, Target_Field);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length is
     ((case Ctx.Fld is
         when F_Initial =>
            (case Fld is
                  when F_Length =>
                     Length'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Length =>
            (case Fld is
                  when F_Payload =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Payload | F_Final =>
            0))
    with
     Pre =>
       Valid_Target (Ctx.Fld, Fld)
          and then Valid_Predecessors (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index is
     ((case Ctx.Fld is
         when F_Initial | F_Length | F_Payload | F_Final =>
            Ctx.Index))
    with
     Pre =>
       Valid_Target (Ctx.Fld, Fld)
          and then Valid_Predecessors (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Field_Postcondition (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Length =>
            Field_Condition (Ctx, Fld, F_Payload),
         when F_Payload =>
            Field_Condition (Ctx, Fld, F_Final)))
    with
     Pre =>
       Valid_Predecessors (Ctx, Fld)
          and then Preliminary_Valid (Ctx, Fld);

   function Valid_Context (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Target (Ctx.Fld, Fld)
      and then Valid_Predecessors (Ctx, Fld)
      and then Ctx.Buffer /= null);

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and then Ctx.First <= RFLX.Types.Bit_Index'Last / 2
      and then Field_First (Ctx, Fld) <= RFLX.Types.Bit_Index'Last / 2
      and then Field_Length (Ctx, Fld) >= 0
      and then Field_Length (Ctx, Fld) <= RFLX.Types.Bit_Length'Last / 2
      and then (Field_First (Ctx, Fld) + Field_Length (Ctx, Fld)) <= RFLX.Types.Bit_Length'Last / 2
      and then Ctx.First <= Field_First (Ctx, Fld)
      and then Ctx.Last >= ((Field_First (Ctx, Fld) + Field_Length (Ctx, Fld))) - 1)
    with
     Pre =>
       Valid_Context (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Valid_Context (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld)
          and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
      First : constant RFLX.Types.Bit_Index := Field_First (Ctx, Fld);
      Length : constant RFLX.Types.Bit_Length := Field_Length (Ctx, Fld);
      function Buffer_First return RFLX.Types.Index is
        (RFLX.Types.Byte_Index (First));
      function Buffer_Last return RFLX.Types.Index is
        (RFLX.Types.Byte_Index ((First + Length - 1)))
       with
        Pre =>
          Length >= 1;
      function Offset return RFLX.Types.Offset is
        (RFLX.Types.Offset ((8 - ((First + Length - 1)) mod 8) mod 8));
   begin
      return ((case Fld is
            when F_Length =>
               (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Payload =>
               (Fld => F_Payload)));
   end Get_Field_Value;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      First : RFLX.Types.Bit_Index;
      Last : RFLX.Types.Bit_Length;
      Value : Field_Dependent_Value;
   begin
      if Valid_Context (Ctx, Fld) then
         if Field_Condition (Ctx, Ctx.Fld, Fld) then
            if Sufficient_Buffer_Length (Ctx, Fld) then
               First := Field_First (Ctx, Fld);
               Last := ((First + Field_Length (Ctx, Fld))) - 1;
               Value := Get_Field_Value (Ctx, Fld);
               Ctx.Cursors (Fld) := (State => S_Preliminary, First => First, Last => Last, Value => Value);
               if Valid_Value (Value)
                  and then Field_Postcondition (Ctx, Fld) then
                  if Composite_Field (Fld) then
                     Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => First, Last => Last, Value => Value);
                  else
                     Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Value);
                  end if;
                  Ctx.Index := (Last + 1);
                  Ctx.Fld := Fld;
               else
                  Ctx.Cursors (Fld) := (State => S_Invalid);
               end if;
            else
               Ctx.Cursors (Fld) := (State => S_Incomplete);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Invalid);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Ctx : in out Context) is
   begin
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Payload);
   end Verify_Message;

   function Present (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid)
      and then Ctx.Cursors (Fld).Value.Fld = Fld
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).Value.Fld = Fld
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Length)
      and then Structural_Valid (Ctx, F_Payload));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Length)
      and then Valid (Ctx, F_Payload));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Payload));

   function Get_Length (Ctx : Context) return Length is
     (Ctx.Cursors (F_Length).Value.Length_Value);

   procedure Get_Payload (Ctx : Context) is
      First : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Process_Payload (Ctx.Buffer.all (First .. Last));
   end Get_Payload;

end RFLX.Arrays.Generic_Inner_Message;
