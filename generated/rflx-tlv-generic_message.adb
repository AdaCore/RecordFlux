package body RFLX.TLV.Generic_Message with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   function Create return Context is
     ((RFLX.Types.Index'First, RFLX.Types.Index'First, RFLX.Types.Bit_Index'First, RFLX.Types.Bit_Index'First, null, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) is
      Buffer_First : constant RFLX.Types.Index := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Tag))
         and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Error)) then
       Ctx.Cursors (F_Tag).Last
    elsif Structural_Valid (Ctx.Cursors (F_Value)) then
       Ctx.Cursors (F_Value).Last
    else
       RFLX.Types.Unreachable_Bit_Length));

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) is
   begin
      First := Ctx.Cursors (Fld).First;
      Last := Ctx.Cursors (Fld).Last;
   end Field_Range;

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Tag =>
                     True,
                  when others =>
                     False),
         when F_Tag =>
            (case Fld is
                  when F_Length =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data)),
                  when others =>
                     False),
         when F_Length =>
            (case Fld is
                  when F_Value =>
                     True,
                  when others =>
                     False),
         when F_Value | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Initial =>
            True,
         when F_Tag =>
            RFLX.Types.Bit_Length (Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data))
               or RFLX.Types.Bit_Length (Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Error)),
         when F_Length | F_Value =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Tag =>
                     Tag_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Tag =>
            (case Fld is
                  when F_Length =>
                     Length'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Length =>
            (case Fld is
                  when F_Value =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Value | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index is
     ((case Fld is
         when F_Tag =>
            Ctx.First,
         when F_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_Value =>
            (if Ctx.Cursors (Fld).Predecessor = F_Length then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length)));

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index is
     ((Field_First (Ctx, Fld) + Field_Length (Ctx, Fld) - 1));

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
         when F_Initial =>
            F_Initial,
         when others =>
            Ctx.Cursors (Fld).Predecessor));

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
         when F_Tag =>
            (if RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data)) then
                F_Length
             elsif RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Error)) then
                F_Final
             else
                F_Initial),
         when F_Length =>
            F_Value,
         when F_Value =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and then Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Tag =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Length =>
            (Valid (Ctx.Cursors (F_Tag))
                 and then Ctx.Cursors (Fld).Predecessor = F_Tag),
         when F_Value =>
            (Valid (Ctx.Cursors (F_Length))
                 and then Ctx.Cursors (Fld).Predecessor = F_Length),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Tag))
                 and then Ctx.Cursors (Fld).Predecessor = F_Tag)
               or (Structural_Valid (Ctx.Cursors (F_Value))
                 and then Ctx.Cursors (Fld).Predecessor = F_Value)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Tag =>
            Invalid (Ctx.Cursors (F_Length)),
         when F_Length =>
            Invalid (Ctx.Cursors (F_Value)),
         when F_Value =>
            True));

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length is
     ((RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Field_First (Ctx, Fld) + 1));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld),
     Post =>
       Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld)
          and then Invalid (Ctx.Cursors (Fld))
          and then Invalid_Successor (Ctx, Fld)
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Ctx.Cursors (Fld).Predecessor = Ctx.Cursors (Fld).Predecessor'Old
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
          and then Field_Length (Ctx, Fld) = Field_Length (Ctx, Fld)'Old
          and then (if Structural_Valid (Ctx.Cursors (F_Tag)) then
             Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Length)) then
             Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Value)) then
             Ctx.Cursors (F_Value) = Ctx.Cursors (F_Value)'Old)
   is
      First : constant RFLX.Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant RFLX.Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and then Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Tag =>
            Ctx.Cursors (F_Value) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, Ctx.Cursors (F_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Length =>
            Ctx.Cursors (F_Value) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, Ctx.Cursors (F_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Value =>
            Ctx.Cursors (F_Value) := (S_Invalid, Ctx.Cursors (F_Value).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
      end case;
   end Reset_Dependent_Fields;

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and then Ctx.First <= RFLX.Types.Bit_Index'Last / 2
      and then Field_First (Ctx, Fld) <= RFLX.Types.Bit_Index'Last / 2
      and then Field_Length (Ctx, Fld) >= 0
      and then Field_Length (Ctx, Fld) <= RFLX.Types.Bit_Length'Last / 2
      and then (Field_First (Ctx, Fld) + Field_Length (Ctx, Fld)) <= RFLX.Types.Bit_Length'Last / 2
      and then Ctx.First <= Field_First (Ctx, Fld)
      and then Ctx.Last >= Field_Last (Ctx, Fld))
    with
     Pre =>
       Has_Buffer (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
         when F_Tag | F_Length =>
            False,
         when F_Value =>
            True));

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Has_Buffer (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld)
          and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
      First : constant RFLX.Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX.Types.Bit_Index := Field_Last (Ctx, Fld);
      function Buffer_First return RFLX.Types.Index is
        (RFLX.Types.Byte_Index (First));
      function Buffer_Last return RFLX.Types.Index is
        (RFLX.Types.Byte_Index (Last));
      function Offset return RFLX.Types.Offset is
        (RFLX.Types.Offset ((8 - Last mod 8) mod 8));
   begin
      return ((case Fld is
            when F_Tag =>
               (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Length =>
               (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Value =>
               (Fld => F_Value)));
   end Get_Field_Value;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : Field_Dependent_Value;
   begin
      if Has_Buffer (Ctx)
         and then Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld) then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := Get_Field_Value (Ctx, Fld);
            if Valid_Value (Value)
               and then Field_Condition (Ctx, Value) then
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Tag)) then
                   (Ctx.Cursors (F_Tag).Last - Ctx.Cursors (F_Tag).First + 1) = Tag_Base'Size
                     and then Ctx.Cursors (F_Tag).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Tag).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Length))
                          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data)) then
                        (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = Length'Size
                          and then Ctx.Cursors (F_Length).Predecessor = F_Tag
                          and then Ctx.Cursors (F_Length).First = (Ctx.Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Value)) then
                             (Ctx.Cursors (F_Value).Last - Ctx.Cursors (F_Value).First + 1) = RFLX.Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                               and then Ctx.Cursors (F_Value).Predecessor = F_Length
                               and then Ctx.Cursors (F_Value).First = (Ctx.Cursors (F_Length).Last + 1)))));
               if Fld = F_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Value then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               end if;
            else
               Ctx.Cursors (Fld) := (State => S_Invalid, Predecessor => F_Final);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Incomplete, Predecessor => F_Final);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Ctx : in out Context) is
   begin
      Verify (Ctx, F_Tag);
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Value);
   end Verify_Message;

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Structural_Valid (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Length)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data))
          and then Structural_Valid (Ctx, F_Value))
        or RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Error))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Length)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Data))
          and then Valid (Ctx, F_Value))
        or RFLX.Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX.Types.Bit_Length (Convert (Msg_Error))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Value));

   function Get_Tag (Ctx : Context) return Tag is
     (Convert (Ctx.Cursors (F_Tag).Value.Tag_Value));

   function Get_Length (Ctx : Context) return Length is
     (Ctx.Cursors (F_Length).Value.Length_Value);

   procedure Get_Value (Ctx : Context) is
      First : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Value).First);
      Last : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Value).Last);
   begin
      Process_Value (Ctx.Buffer.all (First .. Last));
   end Get_Value;

end RFLX.TLV.Generic_Message;
