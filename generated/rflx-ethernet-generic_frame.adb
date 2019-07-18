package body RFLX.Ethernet.Generic_Frame with
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
         when F_Initial | F_Destination =>
            True,
         when F_Source =>
            Preliminary_Valid (Context, F_Destination),
         when F_Type_Length_TPID =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source),
         when F_TPID =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Type_Length_TPID),
         when F_TCI =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Type_Length_TPID)
               and then Preliminary_Valid (Context, F_TPID),
         when F_Type_Length =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Type_Length_TPID),
         when F_Payload =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Type_Length_TPID)
               and then Preliminary_Valid (Context, F_Type_Length),
         when F_Final =>
            Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Type_Length_TPID)
               and then Preliminary_Valid (Context, F_Type_Length)
               and then Preliminary_Valid (Context, F_Payload)));

   function Valid_Predecessors (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Destination =>
            True,
         when F_Source =>
            Present (Context, F_Destination),
         when F_Type_Length_TPID =>
            Present (Context, F_Destination)
               and then Present (Context, F_Source),
         when F_TPID =>
            Present (Context, F_Destination)
               and then Present (Context, F_Source)
               and then Present (Context, F_Type_Length_TPID),
         when F_TCI =>
            Present (Context, F_Destination)
               and then Present (Context, F_Source)
               and then Present (Context, F_Type_Length_TPID)
               and then Present (Context, F_TPID),
         when F_Type_Length =>
            Present (Context, F_Destination)
               and then Present (Context, F_Source)
               and then Present (Context, F_Type_Length_TPID),
         when F_Payload =>
            Present (Context, F_Destination)
               and then Present (Context, F_Source)
               and then Present (Context, F_Type_Length_TPID)
               and then Present (Context, F_Type_Length)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Context, Field));

   function Valid_Target (Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            Target_Field = F_Destination,
         when F_Destination =>
            Target_Field = F_Source,
         when F_Source =>
            Target_Field = F_Type_Length_TPID,
         when F_Type_Length_TPID =>
            Target_Field = F_TPID
               or Target_Field = F_Type_Length,
         when F_TPID =>
            Target_Field = F_TCI,
         when F_TCI =>
            Target_Field = F_Type_Length,
         when F_Type_Length =>
            Target_Field = F_Payload,
         when F_Payload =>
            Target_Field = F_Final,
         when F_Final =>
            False));

   function Composite_Field (Field : Field_Type) return Boolean is
     ((case Field is
         when F_Destination | F_Source | F_Type_Length_TPID | F_TPID | F_TCI | F_Type_Length =>
            False,
         when F_Payload =>
            True));

   function Field_Condition (Context : Context_Type; Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            (case Target_Field is
                  when F_Destination =>
                     True,
                  when others =>
                     False),
         when F_Destination =>
            (case Target_Field is
                  when F_Source =>
                     True,
                  when others =>
                     False),
         when F_Source =>
            (case Target_Field is
                  when F_Type_Length_TPID =>
                     True,
                  when others =>
                     False),
         when F_Type_Length_TPID =>
            (case Target_Field is
                  when F_TPID =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024,
                  when F_Type_Length =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024,
                  when others =>
                     False),
         when F_TPID =>
            (case Target_Field is
                  when F_TCI =>
                     True,
                  when others =>
                     False),
         when F_TCI =>
            (case Target_Field is
                  when F_Type_Length =>
                     True,
                  when others =>
                     False),
         when F_Type_Length =>
            (case Target_Field is
                  when F_Payload =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
                        or RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536,
                  when others =>
                     False),
         when F_Payload =>
            (case Target_Field is
                  when F_Final =>
                     ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
                        and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500,
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
                  when F_Destination =>
                     Address_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Destination =>
            (case Field is
                  when F_Source =>
                     Address_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Source =>
            (case Field is
                  when F_Type_Length_TPID =>
                     Type_Length_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Type_Length_TPID =>
            (case Field is
                  when F_TPID =>
                     TPID_Type_Base'Size,
                  when F_Type_Length =>
                     Type_Length_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_TPID =>
            (case Field is
                  when F_TCI =>
                     TCI_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_TCI =>
            (case Field is
                  when F_Type_Length =>
                     Type_Length_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Type_Length =>
            (case Field is
                  when F_Payload =>
                     (if RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) * 8 elsif RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then (Context.Last - Context.Cursors (F_Type_Length).Last) else RFLX.Types.Unreachable_Bit_Length_Type),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Payload | F_Final =>
            0))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_First (Context : Context_Type; Field : Field_Type) return RFLX.Types.Bit_Index_Type is
     ((case Context.Field is
         when F_Initial | F_Destination | F_Source =>
            Context.Index,
         when F_Type_Length_TPID =>
            (case Field is
                  when F_TPID | F_Type_Length =>
                     Context.Cursors (F_Type_Length_TPID).First,
                  when others =>
                     Context.Index),
         when F_TPID | F_TCI | F_Type_Length | F_Payload | F_Final =>
            Context.Index))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_Postcondition (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Destination =>
            Field_Condition (Context, Field, F_Source),
         when F_Source =>
            Field_Condition (Context, Field, F_Type_Length_TPID),
         when F_Type_Length_TPID =>
            Field_Condition (Context, Field, F_TPID)
               or Field_Condition (Context, Field, F_Type_Length),
         when F_TPID =>
            Field_Condition (Context, Field, F_TCI),
         when F_TCI =>
            Field_Condition (Context, Field, F_Type_Length),
         when F_Type_Length =>
            Field_Condition (Context, Field, F_Payload),
         when F_Payload =>
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
            when F_Destination =>
               (Field => F_Destination, Destination_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Source =>
               (Field => F_Source, Source_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Type_Length_TPID =>
               (Field => F_Type_Length_TPID, Type_Length_TPID_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TPID =>
               (Field => F_TPID, TPID_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TCI =>
               (Field => F_TCI, TCI_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Type_Length =>
               (Field => F_Type_Length, Type_Length_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Payload =>
               (Field => F_Payload)));
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
      Verify (Context, F_Destination);
      Verify (Context, F_Source);
      Verify (Context, F_Type_Length_TPID);
      Verify (Context, F_TPID);
      Verify (Context, F_TCI);
      Verify (Context, F_Type_Length);
      Verify (Context, F_Payload);
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
     (Valid (Context, F_Destination)
      and then Valid (Context, F_Source)
      and then Valid (Context, F_Type_Length_TPID)
      and then ((Valid (Context, F_TPID)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
          and then Valid (Context, F_TCI)
          and then Valid (Context, F_Type_Length)
          and then ((Structural_Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Structural_Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)))
        or (Valid (Context, F_Type_Length)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024
          and then ((Structural_Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Structural_Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)))));

   function Valid_Message (Context : Context_Type) return Boolean is
     (Valid (Context, F_Destination)
      and then Valid (Context, F_Source)
      and then Valid (Context, F_Type_Length_TPID)
      and then ((Valid (Context, F_TPID)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
          and then Valid (Context, F_TCI)
          and then Valid (Context, F_Type_Length)
          and then ((Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)))
        or (Valid (Context, F_Type_Length)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024
          and then ((Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Valid (Context, F_Payload)
              and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Context.Cursors (F_Payload).Last - Context.Cursors (F_Payload).First + 1)) / 8 <= 1500)))));

   function Incomplete_Message (Context : Context_Type) return Boolean is
     (Incomplete (Context, F_Destination)
      or Incomplete (Context, F_Source)
      or Incomplete (Context, F_Type_Length_TPID)
      or Incomplete (Context, F_TPID)
      or Incomplete (Context, F_TCI)
      or Incomplete (Context, F_Type_Length)
      or Incomplete (Context, F_Payload));

   function Get_Destination (Context : Context_Type) return Address_Type is
     (Context.Cursors (F_Destination).Value.Destination_Value);

   function Get_Source (Context : Context_Type) return Address_Type is
     (Context.Cursors (F_Source).Value.Source_Value);

   function Get_Type_Length_TPID (Context : Context_Type) return Type_Length_Type is
     (Context.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value);

   function Get_TPID (Context : Context_Type) return TPID_Type is
     (Context.Cursors (F_TPID).Value.TPID_Value);

   function Get_TCI (Context : Context_Type) return TCI_Type is
     (Context.Cursors (F_TCI).Value.TCI_Value);

   function Get_Type_Length (Context : Context_Type) return Type_Length_Type is
     (Context.Cursors (F_Type_Length).Value.Type_Length_Value);

   procedure Get_Payload (Context : Context_Type) is
      First : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Payload).First);
      Last : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Payload).Last);
   begin
      Process_Payload (Context.Buffer.all (First .. Last));
   end Get_Payload;

end RFLX.Ethernet.Generic_Frame;
