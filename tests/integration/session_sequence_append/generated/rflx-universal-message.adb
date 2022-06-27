pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Universal.Message with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; Written_Last : RFLX_Types.Bit_Length := 0) is
   begin
      Initialize (Ctx, Buffer, RFLX_Types.To_First_Bit_Index (Buffer'First), RFLX_Types.To_Last_Bit_Index (Buffer'Last), Written_Last);
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length := 0) is
      Buffer_First : constant RFLX_Types.Index := Buffer'First;
      Buffer_Last : constant RFLX_Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Message_Type => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Message_Type => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
   end Reset;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last));
      else
         Buffer := Ctx.Buffer.all (1 .. 0);
      end if;
   end Copy;

   function Read (Ctx : Context) return RFLX_Types.Bytes is
     (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last)));

   procedure Generic_Read (Ctx : Context) is
   begin
      Read (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last)));
   end Generic_Read;

   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) is
      Length : RFLX_Types.Length;
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last));
      Write (Ctx.Buffer.all (Ctx.Buffer'First + RFLX_Types.Index (Offset + 1) - 1 .. Ctx.Buffer'Last), Length, Ctx.Buffer'Length, Offset);
      pragma Assert (Length <= Ctx.Buffer.all'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Ctx.Written_Last := RFLX_Types.Bit_Index'Max (Ctx.Written_Last, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (Ctx.Buffer_First) + Offset + Length - 1));
   end Generic_Write;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
   begin
      Data := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last));
   end Data;

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Message_Type =>
             (if
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
              then
                 F_Data
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Null))
              then
                 F_Final
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                 and RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Null))
                 and RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
              then
                 F_Length
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
              then
                 F_Options
              else
                 F_Initial),
          when F_Length =>
             (if
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Data))
              then
                 F_Data
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Option_Types))
              then
                 F_Option_Types
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Options))
              then
                 F_Options
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Value))
                 and RFLX_Types.S63 (Ctx.Cursors (F_Length).Value) = Universal.Value'Size / 8
              then
                 F_Value
              elsif
                 RFLX_Types.S63 (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Values))
              then
                 F_Values
              else
                 F_Initial),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Message_Type =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Length))
             and Invalid (Ctx.Cursors (F_Options)),
          when F_Length =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Option_Types))
             and Invalid (Ctx.Cursors (F_Options))
             and Invalid (Ctx.Cursors (F_Value))
             and Invalid (Ctx.Cursors (F_Values)),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Data | F_Option_Types | F_Options | F_Values =>
                      Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld))) = Data,
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
       and Invalid (Ctx.Cursors (Fld))
       and Invalid_Successor (Ctx, Fld)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Cursors (Fld).Predecessor = Ctx.Cursors (Fld).Predecessor'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Old
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F) else Invalid (Ctx, F)))
   is
      First : constant RFLX_Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      for Fld_Loop in reverse Field'Succ (Fld) .. Field'Last loop
         Ctx.Cursors (Fld_Loop) := (S_Invalid, F_Final);
         pragma Loop_Invariant (Field_First (Ctx, Fld) = First
                                and Field_Size (Ctx, Fld) = Size);
         pragma Loop_Invariant ((for all F in Field =>
                                    (if F < Fld_Loop then Ctx.Cursors (F) = Ctx.Cursors'Loop_Entry (F) else Invalid (Ctx, F))));
      end loop;
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      Ctx.Cursors (Fld) := (S_Invalid, Ctx.Cursors (Fld).Predecessor);
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     (Fld in F_Data | F_Option_Types | F_Options | F_Values);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.S63 with
     Pre =>
       Has_Buffer (Ctx)
       and then Valid_Next (Ctx, Fld)
       and then Sufficient_Buffer_Length (Ctx, Fld)
       and then not Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Message_Type =>
             8,
          when F_Length =>
             16,
          when F_Value =>
             8,
          when others =>
             Positive'Last);
      Byte_Order : constant RFLX_Types.Byte_Order := RFLX_Types.High_Order_First;
   begin
      return RFLX_Types.Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, Size, Byte_Order);
   end Get;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : RFLX_Types.S63;
   begin
      if
         Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := (if Composite_Field (Fld) then 0 else Get (Ctx, Fld));
            if
               Valid_Value (Fld, Value)
               and then Field_Condition (Ctx, Fld, Value)
            then
               pragma Assert ((if
                                  Fld = F_Data
                                  or Fld = F_Message_Type
                                  or Fld = F_Option_Types
                                  or Fld = F_Options
                                  or Fld = F_Value
                                  or Fld = F_Values
                               then
                                  Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
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
      for F in Field loop
         Verify (Ctx, F);
      end loop;
   end Verify_Message;

   function Get_Data (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Data;

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Data;

   procedure Generic_Get_Data (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      Process_Data (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Data;

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.S63; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       Has_Buffer (Ctx)
       and then Valid_Next (Ctx, Fld)
       and then Valid_Value (Fld, Val)
       and then Valid_Size (Ctx, Fld, Size)
       and then Size <= Available_Space (Ctx, Fld)
       and then (if Composite_Field (Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
     Post =>
       Valid_Next (Ctx, Fld)
       and then Invalid_Successor (Ctx, Fld)
       and then Buffer_First = RFLX_Types.To_Index (Field_First (Ctx, Fld))
       and then Buffer_Last = RFLX_Types.To_Index (Field_First (Ctx, Fld) + Size - 1)
       and then Offset = RFLX_Types.Offset ((RFLX_Types.Byte'Size - (Field_First (Ctx, Fld) + Size - 1) mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size)
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and then Predecessor (Ctx, Fld) = Predecessor (Ctx, Fld)'Old
       and then Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld)
       and then (if State_Valid and Size > 0 then Valid (Ctx, Fld) else Structural_Valid (Ctx, Fld))
       and then (case Fld is
                    when F_Message_Type =>
                       Get_Message_Type (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
                            then
                               Predecessor (Ctx, F_Data) = F_Message_Type
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                               and RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Null))
                               and RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
                            then
                               Predecessor (Ctx, F_Length) = F_Message_Type
                               and Valid_Next (Ctx, F_Length))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                            then
                               Predecessor (Ctx, F_Options) = F_Message_Type
                               and Valid_Next (Ctx, F_Options))
                       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Length =>
                       Get_Length (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Data))
                            then
                               Predecessor (Ctx, F_Data) = F_Length
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Option_Types))
                            then
                               Predecessor (Ctx, F_Option_Types) = F_Length
                               and Valid_Next (Ctx, F_Option_Types))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Options))
                            then
                               Predecessor (Ctx, F_Options) = F_Length
                               and Valid_Next (Ctx, F_Options))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Value))
                               and RFLX_Types.S63 (Get_Length (Ctx)) = Universal.Value'Size / 8
                            then
                               Predecessor (Ctx, F_Value) = F_Length
                               and Valid_Next (Ctx, F_Value))
                       and (if
                               RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Values))
                            then
                               Predecessor (Ctx, F_Values) = F_Length
                               and Valid_Next (Ctx, F_Values)),
                    when F_Data | F_Option_Types | F_Options =>
                       (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Value =>
                       Get_Value (Ctx) = To_Actual (Val)
                       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Values =>
                       (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and then (for all F in Field =>
                    (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
   is
      First : RFLX_Types.Bit_Index;
      Last : RFLX_Types.Bit_Length;
   begin
      Reset_Dependent_Fields (Ctx, Fld);
      First := Field_First (Ctx, Fld);
      Last := Field_First (Ctx, Fld) + Size - 1;
      Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Buffer_First := RFLX_Types.To_Index (First);
      Buffer_Last := RFLX_Types.To_Index (Last);
      pragma Assert ((((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size, Written_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      pragma Assert (Size = (case Fld is
                         when F_Message_Type =>
                            8,
                         when F_Length =>
                            16,
                         when F_Data =>
                            (if
                                Ctx.Cursors (Fld).Predecessor = F_Length
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.Bit_Length (To_S63 (RFLX.Universal.MT_Data))
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value) * 8
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Message_Type
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.Bit_Length (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Last)
                             else
                                RFLX_Types.Unreachable),
                         when F_Option_Types =>
                            RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value) * 8,
                         when F_Options =>
                            (if
                                Ctx.Cursors (Fld).Predecessor = F_Length
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.Bit_Length (To_S63 (RFLX.Universal.MT_Options))
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value) * 8
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Message_Type
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Value) = RFLX_Types.Bit_Length (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Last)
                             else
                                RFLX_Types.Unreachable),
                         when F_Value =>
                            8,
                         when F_Values =>
                            RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value) * 8));
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      else
         Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      end if;
      Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
   end Set;

   procedure Set_Scalar (Ctx : in out Context; Fld : Field; Val : RFLX_Types.S63) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, Fld)
       and then Valid_Value (Fld, Val)
       and then Valid_Size (Ctx, Fld, Field_Size (Ctx, Fld))
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld)
       and then Field_Size (Ctx, Fld) in 1 .. RFLX_Types.S63'Size
       and then RFLX_Types.Fits_Into (Val, Natural (Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (case Fld is
               when F_Message_Type =>
                  Get_Message_Type (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
                       then
                          Predecessor (Ctx, F_Data) = F_Message_Type
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                          and RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Null))
                          and RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) /= RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Data))
                       then
                          Predecessor (Ctx, F_Length) = F_Message_Type
                          and Valid_Next (Ctx, F_Length))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Unconstrained_Options))
                       then
                          Predecessor (Ctx, F_Options) = F_Message_Type
                          and Valid_Next (Ctx, F_Options))
                  and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Length =>
                  Get_Length (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Data))
                       then
                          Predecessor (Ctx, F_Data) = F_Length
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Option_Types))
                       then
                          Predecessor (Ctx, F_Option_Types) = F_Length
                          and Valid_Next (Ctx, F_Option_Types))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Options))
                       then
                          Predecessor (Ctx, F_Options) = F_Length
                          and Valid_Next (Ctx, F_Options))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Value))
                          and RFLX_Types.S63 (Get_Length (Ctx)) = Universal.Value'Size / 8
                       then
                          Predecessor (Ctx, F_Value) = F_Length
                          and Valid_Next (Ctx, F_Value))
                  and (if
                          RFLX_Types.S63 (To_S63 (Get_Message_Type (Ctx))) = RFLX_Types.S63 (To_S63 (RFLX.Universal.MT_Values))
                       then
                          Predecessor (Ctx, F_Values) = F_Length
                          and Valid_Next (Ctx, F_Values)),
               when F_Data | F_Option_Types | F_Options =>
                  (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Value =>
                  Get_Value (Ctx) = To_Actual (Val)
                  and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Values =>
                  (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Predecessor (Ctx, Fld) = Predecessor (Ctx, Fld)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
   is
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld);
   begin
      Set (Ctx, Fld, Val, Size, True, Buffer_First, Buffer_Last, Offset);
      RFLX_Types.Lemma_Size (Val, Positive (Size));
      RFLX_Types.Insert (Val, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, Positive (Size), RFLX_Types.High_Order_First);
   end Set_Scalar;

   procedure Set_Message_Type (Ctx : in out Context; Val : RFLX.Universal.Message_Type) is
   begin
      Set_Scalar (Ctx, F_Message_Type, To_S63 (Val));
   end Set_Message_Type;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Universal.Length) is
   begin
      Set_Scalar (Ctx, F_Length, To_S63 (Val));
   end Set_Length;

   procedure Set_Value (Ctx : in out Context; Val : RFLX.Universal.Value) is
   begin
      Set_Scalar (Ctx, F_Value, To_S63 (Val));
   end Set_Value;

   procedure Set_Data_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Data, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Data_Empty;

   procedure Set_Option_Types_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Option_Types, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Option_Types_Empty;

   procedure Set_Options_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Options, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Options_Empty;

   procedure Set_Values_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Values, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Values_Empty;

   procedure Set_Option_Types (Ctx : in out Context; Seq_Ctx : Universal.Option_Types.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (Universal.Option_Types.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Option_Types, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      Universal.Option_Types.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Option_Types;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : Universal.Options.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (Universal.Options.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Options, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      Universal.Options.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Options;

   procedure Set_Values (Ctx : in out Context; Seq_Ctx : Universal.Values.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (Universal.Values.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Values, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      Universal.Values.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Values;

   procedure Initialize_Data_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Valid_Length (Ctx, F_Data, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Data)) >= Length
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and Field_Size (Ctx, F_Data) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Data)
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Data);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Initialize_Data_Private;

   procedure Initialize_Data (Ctx : in out Context; Length : RFLX_Types.Length) is
   begin
      Initialize_Data_Private (Ctx, Length);
   end Initialize_Data;

   procedure Initialize_Option_Types_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Types)
       and then Valid_Length (Ctx, F_Option_Types, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Option_Types)) >= Length
       and then Field_First (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Option_Types)
       and Field_Size (Ctx, F_Option_Types) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Types) = Predecessor (Ctx, F_Option_Types)'Old
       and Valid_Next (Ctx, F_Option_Types) = Valid_Next (Ctx, F_Option_Types)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Option_Types);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Types) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Types)) := (State => S_Invalid, Predecessor => F_Option_Types);
   end Initialize_Option_Types_Private;

   procedure Initialize_Option_Types (Ctx : in out Context) is
   begin
      Initialize_Option_Types_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Option_Types)));
   end Initialize_Option_Types;

   procedure Initialize_Options_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Valid_Length (Ctx, F_Options, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Options)) >= Length
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Options)
       and Field_Size (Ctx, F_Options) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
   end Initialize_Options_Private;

   procedure Initialize_Options (Ctx : in out Context; Length : RFLX_Types.Length) is
   begin
      Initialize_Options_Private (Ctx, Length);
   end Initialize_Options;

   procedure Initialize_Values_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Values)
       and then Valid_Length (Ctx, F_Values, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Values)) >= Length
       and then Field_First (Ctx, F_Values) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Values)
       and Field_Size (Ctx, F_Values) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Values) = Predecessor (Ctx, F_Values)'Old
       and Valid_Next (Ctx, F_Values) = Valid_Next (Ctx, F_Values)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Values);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Values) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Values).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Values)) := (State => S_Invalid, Predecessor => F_Values);
   end Initialize_Values_Private;

   procedure Initialize_Values (Ctx : in out Context) is
   begin
      Initialize_Values_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Values)));
   end Initialize_Values;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Data));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Data_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_Data)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_Data)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_Data))) = Data);
   end Set_Data;

   procedure Generic_Set_Data (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Data_Private (Ctx, Length);
   end Generic_Set_Data;

   procedure Switch_To_Option_Types (Ctx : in out Context; Seq_Ctx : out Universal.Option_Types.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Types);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Option_Types) then
         Reset_Dependent_Fields (Ctx, F_Option_Types);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Option_Types) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Option_Types)) := (State => S_Invalid, Predecessor => F_Option_Types);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Universal.Option_Types.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Option_Types;

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out Universal.Options.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Options);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Options) then
         Reset_Dependent_Fields (Ctx, F_Options);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Options).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Universal.Options.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Options;

   procedure Switch_To_Values (Ctx : in out Context; Seq_Ctx : out Universal.Values.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Values);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Values) then
         Reset_Dependent_Fields (Ctx, F_Values);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Values) := (State => S_Structural_Valid, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Values).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Values)) := (State => S_Invalid, Predecessor => F_Values);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Universal.Values.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Values;

   procedure Update_Option_Types (Ctx : in out Context; Seq_Ctx : in out Universal.Option_Types.Context) is
      Valid_Sequence : constant Boolean := Universal.Option_Types.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Universal.Option_Types.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Option_Types) := (State => S_Valid, First => Ctx.Cursors (F_Option_Types).First, Last => Ctx.Cursors (F_Option_Types).Last, Value => Ctx.Cursors (F_Option_Types).Value, Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
      end if;
   end Update_Option_Types;

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out Universal.Options.Context) is
      Valid_Sequence : constant Boolean := Universal.Options.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Universal.Options.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Options) := (State => S_Valid, First => Ctx.Cursors (F_Options).First, Last => Ctx.Cursors (F_Options).Last, Value => Ctx.Cursors (F_Options).Value, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      end if;
   end Update_Options;

   procedure Update_Values (Ctx : in out Context; Seq_Ctx : in out Universal.Values.Context) is
      Valid_Sequence : constant Boolean := Universal.Values.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Universal.Values.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Values) := (State => S_Valid, First => Ctx.Cursors (F_Values).First, Last => Ctx.Cursors (F_Values).Last, Value => Ctx.Cursors (F_Values).Value, Predecessor => Ctx.Cursors (F_Values).Predecessor);
      end if;
   end Update_Values;

end RFLX.Universal.Message;
