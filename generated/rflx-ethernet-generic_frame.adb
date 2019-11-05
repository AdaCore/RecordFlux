package body RFLX.Ethernet.Generic_Frame with
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
         when F_Initial | F_Destination =>
            True,
         when F_Source =>
            Preliminary_Valid (Ctx, F_Destination),
         when F_Type_Length_TPID =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source),
         when F_TPID =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Type_Length_TPID),
         when F_TCI =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Type_Length_TPID)
               and then Preliminary_Valid (Ctx, F_TPID),
         when F_Type_Length =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Type_Length_TPID),
         when F_Payload =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Type_Length_TPID)
               and then Preliminary_Valid (Ctx, F_Type_Length),
         when F_Final =>
            Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Type_Length_TPID)
               and then Preliminary_Valid (Ctx, F_Type_Length)
               and then Preliminary_Valid (Ctx, F_Payload)));

   function Valid_Predecessors (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Destination =>
            True,
         when F_Source =>
            Present (Ctx, F_Destination),
         when F_Type_Length_TPID =>
            Present (Ctx, F_Destination)
               and then Present (Ctx, F_Source),
         when F_TPID =>
            Present (Ctx, F_Destination)
               and then Present (Ctx, F_Source)
               and then Present (Ctx, F_Type_Length_TPID),
         when F_TCI =>
            Present (Ctx, F_Destination)
               and then Present (Ctx, F_Source)
               and then Present (Ctx, F_Type_Length_TPID)
               and then Present (Ctx, F_TPID),
         when F_Type_Length =>
            Present (Ctx, F_Destination)
               and then Present (Ctx, F_Source)
               and then Present (Ctx, F_Type_Length_TPID),
         when F_Payload =>
            Present (Ctx, F_Destination)
               and then Present (Ctx, F_Source)
               and then Present (Ctx, F_Type_Length_TPID)
               and then Present (Ctx, F_Type_Length)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Ctx, Fld));

   function Valid_Target (Source_Field, Target_Field : Virtual_Field) return Boolean is
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

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
         when F_Destination | F_Source | F_Type_Length_TPID | F_TPID | F_TCI | F_Type_Length =>
            False,
         when F_Payload =>
            True));

   function Field_Condition (Ctx : Context; Source_Field, Target_Field : Virtual_Field) return Boolean is
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
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100#,
                  when F_Type_Length =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100#,
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
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
                        or RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536,
                  when others =>
                     False),
         when F_Payload =>
            (case Target_Field is
                  when F_Final =>
                     ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
                        and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500,
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
                  when F_Destination =>
                     Address'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Destination =>
            (case Fld is
                  when F_Source =>
                     Address'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Source =>
            (case Fld is
                  when F_Type_Length_TPID =>
                     Type_Length_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Type_Length_TPID =>
            (case Fld is
                  when F_TPID =>
                     TPID_Base'Size,
                  when F_Type_Length =>
                     Type_Length_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_TPID =>
            (case Fld is
                  when F_TCI =>
                     TCI'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_TCI =>
            (case Fld is
                  when F_Type_Length =>
                     Type_Length_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Type_Length =>
            (case Fld is
                  when F_Payload =>
                     (if RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8 elsif RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then (Ctx.Last - Ctx.Cursors (F_Type_Length).Last) else RFLX.Types.Unreachable_Bit_Length),
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
         when F_Initial | F_Destination | F_Source =>
            Ctx.Index,
         when F_Type_Length_TPID =>
            (case Fld is
                  when F_TPID | F_Type_Length =>
                     Ctx.Cursors (F_Type_Length_TPID).First,
                  when others =>
                     Ctx.Index),
         when F_TPID | F_TCI | F_Type_Length | F_Payload | F_Final =>
            Ctx.Index))
    with
     Pre =>
       Valid_Target (Ctx.Fld, Fld)
          and then Valid_Predecessors (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Field_Postcondition (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Destination =>
            Field_Condition (Ctx, Fld, F_Source),
         when F_Source =>
            Field_Condition (Ctx, Fld, F_Type_Length_TPID),
         when F_Type_Length_TPID =>
            Field_Condition (Ctx, Fld, F_TPID)
               or Field_Condition (Ctx, Fld, F_Type_Length),
         when F_TPID =>
            Field_Condition (Ctx, Fld, F_TCI),
         when F_TCI =>
            Field_Condition (Ctx, Fld, F_Type_Length),
         when F_Type_Length =>
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
            when F_Destination =>
               (Fld => F_Destination, Destination_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Source =>
               (Fld => F_Source, Source_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Type_Length_TPID =>
               (Fld => F_Type_Length_TPID, Type_Length_TPID_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TPID =>
               (Fld => F_TPID, TPID_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TCI =>
               (Fld => F_TCI, TCI_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Type_Length =>
               (Fld => F_Type_Length, Type_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
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
      Verify (Ctx, F_Destination);
      Verify (Ctx, F_Source);
      Verify (Ctx, F_Type_Length_TPID);
      Verify (Ctx, F_TPID);
      Verify (Ctx, F_TCI);
      Verify (Ctx, F_Type_Length);
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
     (Valid (Ctx, F_Destination)
      and then Valid (Ctx, F_Source)
      and then Valid (Ctx, F_Type_Length_TPID)
      and then ((Valid (Ctx, F_TPID)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100#
          and then Valid (Ctx, F_TCI)
          and then Valid (Ctx, F_Type_Length)
          and then ((Structural_Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Structural_Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)))
        or (Valid (Ctx, F_Type_Length)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100#
          and then ((Structural_Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Structural_Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Destination)
      and then Valid (Ctx, F_Source)
      and then Valid (Ctx, F_Type_Length_TPID)
      and then ((Valid (Ctx, F_TPID)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100#
          and then Valid (Ctx, F_TCI)
          and then Valid (Ctx, F_Type_Length)
          and then ((Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)))
        or (Valid (Ctx, F_Type_Length)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100#
          and then ((Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)
            or (Valid (Ctx, F_Payload)
              and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
              and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500)))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Destination)
      or Incomplete (Ctx, F_Source)
      or Incomplete (Ctx, F_Type_Length_TPID)
      or Incomplete (Ctx, F_TPID)
      or Incomplete (Ctx, F_TCI)
      or Incomplete (Ctx, F_Type_Length)
      or Incomplete (Ctx, F_Payload));

   function Get_Destination (Ctx : Context) return Address is
     (Ctx.Cursors (F_Destination).Value.Destination_Value);

   function Get_Source (Ctx : Context) return Address is
     (Ctx.Cursors (F_Source).Value.Source_Value);

   function Get_Type_Length_TPID (Ctx : Context) return Type_Length is
     (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value);

   function Get_TPID (Ctx : Context) return TPID is
     (Ctx.Cursors (F_TPID).Value.TPID_Value);

   function Get_TCI (Ctx : Context) return TCI is
     (Ctx.Cursors (F_TCI).Value.TCI_Value);

   function Get_Type_Length (Ctx : Context) return Type_Length is
     (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value);

   procedure Get_Payload (Ctx : Context) is
      First : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Process_Payload (Ctx.Buffer.all (First .. Last));
   end Get_Payload;

end RFLX.Ethernet.Generic_Frame;
