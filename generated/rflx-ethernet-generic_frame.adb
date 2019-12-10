package body RFLX.Ethernet.Generic_Frame with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   function Create return Context is
     ((RFLX.Types.Index'First, RFLX.Types.Index'First, RFLX.Types.Bit_Index'First, RFLX.Types.Bit_Index'First, null, (F_Destination => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) is
      Buffer_First : constant RFLX.Types.Index := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Destination => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
     ((if Structural_Valid (Ctx.Cursors (F_Payload))
         and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
         and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500 then
       Ctx.Cursors (F_Payload).Last
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
                  when F_Destination =>
                     True,
                  when others =>
                     False),
         when F_Destination =>
            (case Fld is
                  when F_Source =>
                     True,
                  when others =>
                     False),
         when F_Source =>
            (case Fld is
                  when F_Type_Length_TPID =>
                     True,
                  when others =>
                     False),
         when F_Type_Length_TPID =>
            (case Fld is
                  when F_TPID =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100#,
                  when F_Type_Length =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100#,
                  when others =>
                     False),
         when F_TPID =>
            (case Fld is
                  when F_TCI =>
                     True,
                  when others =>
                     False),
         when F_TCI =>
            (case Fld is
                  when F_Type_Length =>
                     True,
                  when others =>
                     False),
         when F_Type_Length =>
            (case Fld is
                  when F_Payload =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
                        or RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536,
                  when others =>
                     False),
         when F_Payload | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value; Length : RFLX.Types.Bit_Length := 0) return Boolean is
     ((case Value.Fld is
         when F_Initial | F_Destination | F_Source =>
            True,
         when F_Type_Length_TPID =>
            RFLX.Types.Bit_Length (Value.Type_Length_TPID_Value) = 16#8100#
               or RFLX.Types.Bit_Length (Value.Type_Length_TPID_Value) /= 16#8100#,
         when F_TPID | F_TCI =>
            True,
         when F_Type_Length =>
            RFLX.Types.Bit_Length (Value.Type_Length_Value) <= 1500
               or RFLX.Types.Bit_Length (Value.Type_Length_Value) >= 1536,
         when F_Payload =>
            Length / 8 >= 46
               and then Length / 8 <= 1500,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
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
                     (if RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                         RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                      elsif RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                         (Ctx.Last - Ctx.Cursors (F_Type_Length).Last)
                      else
                         RFLX.Types.Unreachable_Bit_Length),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Payload | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index is
     ((case Fld is
         when F_Destination =>
            Ctx.First,
         when F_Source =>
            (if Ctx.Cursors (Fld).Predecessor = F_Destination then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_Type_Length_TPID =>
            (if Ctx.Cursors (Fld).Predecessor = F_Source then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_TPID =>
            (if Ctx.Cursors (Fld).Predecessor = F_Type_Length_TPID
                  and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100# then
                Ctx.Cursors (F_Type_Length_TPID).First
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_TCI =>
            (if Ctx.Cursors (Fld).Predecessor = F_TPID then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_Type_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Type_Length_TPID
                  and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100# then
                Ctx.Cursors (F_Type_Length_TPID).First
             elsif Ctx.Cursors (Fld).Predecessor = F_TCI then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                RFLX.Types.Unreachable_Bit_Length),
         when F_Payload =>
            (if Ctx.Cursors (Fld).Predecessor = F_Type_Length
                  and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Type_Length
                     and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
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
         when F_Destination =>
            F_Source,
         when F_Source =>
            F_Type_Length_TPID,
         when F_Type_Length_TPID =>
            (if RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100# then
                F_TPID
             elsif RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100# then
                F_Type_Length
             else
                F_Initial),
         when F_TPID =>
            F_TCI,
         when F_TCI =>
            F_Type_Length,
         when F_Type_Length =>
            (if RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                F_Payload
             elsif RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                F_Payload
             else
                F_Initial),
         when F_Payload =>
            (if ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 >= 46
                  and then ((Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1)) / 8 <= 1500 then
                F_Final
             else
                F_Initial)))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and then Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Destination =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Source =>
            (Valid (Ctx.Cursors (F_Destination))
                 and then Ctx.Cursors (Fld).Predecessor = F_Destination),
         when F_Type_Length_TPID =>
            (Valid (Ctx.Cursors (F_Source))
                 and then Ctx.Cursors (Fld).Predecessor = F_Source),
         when F_TPID =>
            (Valid (Ctx.Cursors (F_Type_Length_TPID))
                 and then Ctx.Cursors (Fld).Predecessor = F_Type_Length_TPID),
         when F_TCI =>
            (Valid (Ctx.Cursors (F_TPID))
                 and then Ctx.Cursors (Fld).Predecessor = F_TPID),
         when F_Type_Length =>
            (Valid (Ctx.Cursors (F_Type_Length_TPID))
                 and then Ctx.Cursors (Fld).Predecessor = F_Type_Length_TPID)
               or (Valid (Ctx.Cursors (F_TCI))
                 and then Ctx.Cursors (Fld).Predecessor = F_TCI),
         when F_Payload =>
            (Valid (Ctx.Cursors (F_Type_Length))
                 and then Ctx.Cursors (Fld).Predecessor = F_Type_Length),
         when F_Final =>
            (Structural_Valid (Ctx.Cursors (F_Payload))
                 and then Ctx.Cursors (Fld).Predecessor = F_Payload)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Destination =>
            Invalid (Ctx.Cursors (F_Source)),
         when F_Source =>
            Invalid (Ctx.Cursors (F_Type_Length_TPID)),
         when F_Type_Length_TPID =>
            Invalid (Ctx.Cursors (F_TPID))
               and then Invalid (Ctx.Cursors (F_Type_Length)),
         when F_TPID =>
            Invalid (Ctx.Cursors (F_TCI)),
         when F_TCI =>
            Invalid (Ctx.Cursors (F_Type_Length)),
         when F_Type_Length =>
            Invalid (Ctx.Cursors (F_Payload)),
         when F_Payload =>
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
          and then (if Structural_Valid (Ctx.Cursors (F_Destination)) then
             Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Source)) then
             Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Type_Length_TPID)) then
             Ctx.Cursors (F_Type_Length_TPID) = Ctx.Cursors (F_Type_Length_TPID)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_TPID)) then
             Ctx.Cursors (F_TPID) = Ctx.Cursors (F_TPID)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_TCI)) then
             Ctx.Cursors (F_TCI) = Ctx.Cursors (F_TCI)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Type_Length)) then
             Ctx.Cursors (F_Type_Length) = Ctx.Cursors (F_Type_Length)'Old)
          and then (if Structural_Valid (Ctx.Cursors (F_Payload)) then
             Ctx.Cursors (F_Payload) = Ctx.Cursors (F_Payload)'Old)
   is
      First : constant RFLX.Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant RFLX.Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and then Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Destination =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Source) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Destination) := (S_Invalid, Ctx.Cursors (F_Destination).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Source =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Source) := (S_Invalid, Ctx.Cursors (F_Source).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Type_Length_TPID =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, Ctx.Cursors (F_Type_Length_TPID).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_TPID =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, Ctx.Cursors (F_TPID).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_TCI =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, Ctx.Cursors (F_TCI).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Type_Length =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, Ctx.Cursors (F_Type_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and then Field_Length (Ctx, Fld) = Length);
         when F_Payload =>
            Ctx.Cursors (F_Payload) := (S_Invalid, Ctx.Cursors (F_Payload).Predecessor);
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
         when F_Destination | F_Source | F_Type_Length_TPID | F_TPID | F_TCI | F_Type_Length =>
            False,
         when F_Payload =>
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
      Value : Field_Dependent_Value;
   begin
      if Has_Buffer (Ctx)
         and then Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld) then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := Get_Field_Value (Ctx, Fld);
            if Valid_Value (Value)
               and then Field_Condition (Ctx, Value, Field_Length (Ctx, Fld)) then
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Destination)) then
                   (Ctx.Cursors (F_Destination).Last - Ctx.Cursors (F_Destination).First + 1) = Address'Size
                     and then Ctx.Cursors (F_Destination).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Destination).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Source)) then
                        (Ctx.Cursors (F_Source).Last - Ctx.Cursors (F_Source).First + 1) = Address'Size
                          and then Ctx.Cursors (F_Source).Predecessor = F_Destination
                          and then Ctx.Cursors (F_Source).First = (Ctx.Cursors (F_Destination).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Type_Length_TPID)) then
                             (Ctx.Cursors (F_Type_Length_TPID).Last - Ctx.Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size
                               and then Ctx.Cursors (F_Type_Length_TPID).Predecessor = F_Source
                               and then Ctx.Cursors (F_Type_Length_TPID).First = (Ctx.Cursors (F_Source).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_TPID))
                                    and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100# then
                                  (Ctx.Cursors (F_TPID).Last - Ctx.Cursors (F_TPID).First + 1) = TPID_Base'Size
                                    and then Ctx.Cursors (F_TPID).Predecessor = F_Type_Length_TPID
                                    and then Ctx.Cursors (F_TPID).First = Ctx.Cursors (F_Type_Length_TPID).First
                                    and then (if Structural_Valid (Ctx.Cursors (F_TCI)) then
                                       (Ctx.Cursors (F_TCI).Last - Ctx.Cursors (F_TCI).First + 1) = TCI'Size
                                         and then Ctx.Cursors (F_TCI).Predecessor = F_TPID
                                         and then Ctx.Cursors (F_TCI).First = (Ctx.Cursors (F_TPID).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Type_Length)) then
                                            (Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1) = Type_Length_Base'Size
                                              and then Ctx.Cursors (F_Type_Length).Predecessor = F_TCI
                                              and then Ctx.Cursors (F_Type_Length).First = (Ctx.Cursors (F_TCI).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Payload))
                                                   and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                                                 (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) = RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                                   and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                   and then Ctx.Cursors (F_Payload).First = (Ctx.Cursors (F_Type_Length).Last + 1))
                                              and then (if Structural_Valid (Ctx.Cursors (F_Payload))
                                                   and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                                                 (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) = (Ctx.Last - Ctx.Cursors (F_Type_Length).Last)
                                                   and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                   and then Ctx.Cursors (F_Payload).First = (Ctx.Cursors (F_Type_Length).Last + 1)))))
                               and then (if Structural_Valid (Ctx.Cursors (F_Type_Length))
                                    and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100# then
                                  (Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1) = Type_Length_Base'Size
                                    and then Ctx.Cursors (F_Type_Length).Predecessor = F_Type_Length_TPID
                                    and then Ctx.Cursors (F_Type_Length).First = Ctx.Cursors (F_Type_Length_TPID).First
                                    and then (if Structural_Valid (Ctx.Cursors (F_Payload))
                                         and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                                       (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) = RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                         and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                         and then Ctx.Cursors (F_Payload).First = (Ctx.Cursors (F_Type_Length).Last + 1))
                                    and then (if Structural_Valid (Ctx.Cursors (F_Payload))
                                         and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                                       (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) = (Ctx.Last - Ctx.Cursors (F_Type_Length).Last)
                                         and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                         and then Ctx.Cursors (F_Payload).First = (Ctx.Cursors (F_Type_Length).Last + 1)))))));
               if Fld = F_Destination then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Source then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Type_Length_TPID then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_TPID then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_TCI then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Type_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Payload then
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
      Verify (Ctx, F_Destination);
      Verify (Ctx, F_Source);
      Verify (Ctx, F_Type_Length_TPID);
      Verify (Ctx, F_TPID);
      Verify (Ctx, F_TCI);
      Verify (Ctx, F_Type_Length);
      Verify (Ctx, F_Payload);
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
