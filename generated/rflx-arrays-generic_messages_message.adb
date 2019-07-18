package body RFLX.Arrays.Generic_Messages_Message with
  SPARK_Mode
is

   function Create return Context_Type is
     ((RFLX.Types.Index_Type'First, RFLX.Types.Index_Type'First, RFLX.Types.Bit_Index_Type'First, RFLX.Types.Bit_Index_Type'First, 0, null, RFLX.Types.Bit_Index_Type'First, F_Initial, (others => (State => S_Invalid))));

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Context, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index_Type) is
      Buffer_First : constant RFLX.Types.Index_Type := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index_Type := Buffer'Last;
      Buffer_Address : constant RFLX.Types.Integer_Address := RFLX.Types.Bytes_Address (Buffer);
   begin
      Context := (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, First, F_Initial, (others => (State => S_Invalid)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out RFLX.Types.Bytes_Ptr) is
   begin
      Buffer := Context.Buffer;
      Context.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Context : Context_Type) return Boolean is
     (Context.Buffer /= null);

   procedure Field_Range (Context : Context_Type; Field : Field_Type; First : out RFLX.Types.Bit_Index_Type; Last : out RFLX.Types.Bit_Index_Type) is
   begin
      First := Context.Cursors (Field).First;
      Last := Context.Cursors (Field).Last;
   end Field_Range;

   function Index (Context : Context_Type) return RFLX.Types.Bit_Index_Type is
     (Context.Index);

   function Preliminary_Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid
        or Context.Cursors (Field).State = S_Preliminary)
      and then Context.Cursors (Field).Value.Field = Field);

   function Preliminary_Valid_Predecessors (Context : Context_Type; Field : All_Field_Type) return Boolean is
     ((case Field is
         when F_Initial | F_Length =>
            True,
         when F_Messages =>
            Preliminary_Valid (Context, F_Length),
         when F_Final =>
            Preliminary_Valid (Context, F_Length)
               and then Preliminary_Valid (Context, F_Messages)));

   function Valid_Predecessors (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Length =>
            True,
         when F_Messages =>
            Present (Context, F_Length)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Context, Field));

   function Valid_Target (Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            Target_Field = F_Length,
         when F_Length =>
            Target_Field = F_Messages,
         when F_Messages =>
            Target_Field = F_Final,
         when F_Final =>
            False));

   function Composite_Field (Field : Field_Type) return Boolean is
     ((case Field is
         when F_Length =>
            False,
         when F_Messages =>
            True));

   function Field_Condition (Context : Context_Type; Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            (case Target_Field is
                  when F_Length =>
                     True,
                  when others =>
                     False),
         when F_Length =>
            (case Target_Field is
                  when F_Messages =>
                     True,
                  when others =>
                     False),
         when F_Messages =>
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
          and then Preliminary_Valid_Predecessors (Context, Target_Field);

   function Field_Length (Context : Context_Type; Field : Field_Type) return RFLX.Types.Bit_Length_Type is
     ((case Context.Field is
         when F_Initial =>
            (case Field is
                  when F_Length =>
                     Length_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Length =>
            (case Field is
                  when F_Messages =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_Length).Value.Length_Value) * 8,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Messages | F_Final =>
            0))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_First (Context : Context_Type; Field : Field_Type) return RFLX.Types.Bit_Index_Type is
     ((case Context.Field is
         when F_Initial | F_Length | F_Messages | F_Final =>
            Context.Index))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_Postcondition (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Length =>
            Field_Condition (Context, Field, F_Messages),
         when F_Messages =>
            Field_Condition (Context, Field, F_Final)))
    with
     Pre =>
       Valid_Predecessors (Context, Field)
          and then Preliminary_Valid (Context, Field);

   function Valid_Context (Context : Context_Type; Field : Field_Type) return Boolean is
     (Valid_Target (Context.Field, Field)
      and then Valid_Predecessors (Context, Field)
      and then Context.Buffer /= null);

   function Sufficient_Buffer_Length (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Buffer /= null
      and then Context.First <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Field_First (Context, Field) <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Field_Length (Context, Field) >= 0
      and then Field_Length (Context, Field) <= RFLX.Types.Bit_Length_Type'Last / 2
      and then (Field_First (Context, Field) + Field_Length (Context, Field)) <= RFLX.Types.Bit_Length_Type'Last / 2
      and then Context.First <= Field_First (Context, Field)
      and then Context.Last >= ((Field_First (Context, Field) + Field_Length (Context, Field))) - 1)
    with
     Pre =>
       Valid_Context (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Get_Field_Value (Context : Context_Type; Field : Field_Type) return Result_Type with
     Pre =>
       Valid_Context (Context, Field)
          and then Field_Condition (Context, Context.Field, Field)
          and then Sufficient_Buffer_Length (Context, Field),
     Post =>
       Get_Field_Value'Result.Field = Field
   is
      First : constant RFLX.Types.Bit_Index_Type := Field_First (Context, Field);
      Length : constant RFLX.Types.Bit_Length_Type := Field_Length (Context, Field);
      function Buffer_First return RFLX.Types.Index_Type is
        (RFLX.Types.Byte_Index (First));
      function Buffer_Last return RFLX.Types.Index_Type is
        (RFLX.Types.Byte_Index ((First + Length - 1)))
       with
        Pre =>
          Length >= 1;
      function Offset return RFLX.Types.Offset_Type is
        (RFLX.Types.Offset_Type ((8 - ((First + Length - 1)) mod 8) mod 8));
   begin
      return ((case Field is
            when F_Length =>
               (Field => F_Length, Length_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Messages =>
               (Field => F_Messages)));
   end Get_Field_Value;

   procedure Verify (Context : in out Context_Type; Field : Field_Type) is
      First : RFLX.Types.Bit_Index_Type;
      Last : RFLX.Types.Bit_Length_Type;
      Value : Result_Type;
   begin
      if Valid_Context (Context, Field) then
         if Field_Condition (Context, Context.Field, Field) then
            if Sufficient_Buffer_Length (Context, Field) then
               First := Field_First (Context, Field);
               Last := ((First + Field_Length (Context, Field))) - 1;
               Value := Get_Field_Value (Context, Field);
               Context.Cursors (Field) := (State => S_Preliminary, First => First, Last => Last, Value => Value);
               if Valid_Type (Value)
                  and then Field_Postcondition (Context, Field) then
                  if Composite_Field (Field) then
                     Context.Cursors (Field) := (State => S_Structural_Valid, First => First, Last => Last, Value => Value);
                  else
                     Context.Cursors (Field) := (State => S_Valid, First => First, Last => Last, Value => Value);
                  end if;
                  Context.Index := (Last + 1);
                  Context.Field := Field;
               else
                  Context.Cursors (Field) := (State => S_Invalid);
               end if;
            else
               Context.Cursors (Field) := (State => S_Incomplete);
            end if;
         else
            Context.Cursors (Field) := (State => S_Invalid);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Context : in out Context_Type) is
   begin
      Verify (Context, F_Length);
      Verify (Context, F_Messages);
   end Verify_Message;

   function Present (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid)
      and then Context.Cursors (Field).Value.Field = Field
      and then Context.Cursors (Field).First < (Context.Cursors (Field).Last + 1));

   function Structural_Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid));

   function Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Cursors (Field).State = S_Valid
      and then Context.Cursors (Field).Value.Field = Field
      and then Context.Cursors (Field).First < (Context.Cursors (Field).Last + 1));

   function Incomplete (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Cursors (Field).State = S_Incomplete);

   function Structural_Valid_Message (Context : Context_Type) return Boolean is
     (Valid (Context, F_Length)
      and then Structural_Valid (Context, F_Messages));

   function Valid_Message (Context : Context_Type) return Boolean is
     (Valid (Context, F_Length)
      and then Valid (Context, F_Messages));

   function Incomplete_Message (Context : Context_Type) return Boolean is
     (Incomplete (Context, F_Length)
      or Incomplete (Context, F_Messages));

   function Get_Length (Context : Context_Type) return Length_Type is
     (Context.Cursors (F_Length).Value.Length_Value);

   procedure Get_Messages (Context : Context_Type) is
      First : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Messages).First);
      Last : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Messages).Last);
   begin
      Process_Messages (Context.Buffer.all (First .. Last));
   end Get_Messages;

   procedure Switch (Context : in out Context_Type; Sequence_Context : out Inner_Messages_Sequence.Context_Type) is
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Take_Buffer (Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Inner_Messages_Sequence.Initialize (Sequence_Context, Buffer, Context.Buffer_First, Context.Buffer_Last, Context.Cursors (F_Messages).First, Context.Cursors (F_Messages).Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Context : in out Context_Type; Sequence_Context : in out Inner_Messages_Sequence.Context_Type) is
      Valid_Sequence : constant Boolean := Inner_Messages_Sequence.Valid (Sequence_Context);
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Inner_Messages_Sequence.Take_Buffer (Sequence_Context, Buffer, Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last);
      Context.Buffer := Buffer;
      if Valid_Sequence then
         Context.Cursors (F_Messages) := (State => S_Valid, First => Context.Cursors (F_Messages).First, Last => Context.Cursors (F_Messages).Last, Value => Context.Cursors (F_Messages).Value);
      end if;
   end Update;

end RFLX.Arrays.Generic_Messages_Message;
