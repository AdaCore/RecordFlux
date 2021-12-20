pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Universal.Message with
  SPARK_Mode
is

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
      Read (Read (Ctx));
   end Generic_Read;

   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) is
      Length : RFLX_Types.Length;
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last));
      Write (Ctx.Buffer.all (Ctx.Buffer'First + RFLX_Types.Index (Offset + 1) - 1 .. Ctx.Buffer'Last), Length, Ctx.Buffer'Length, Offset);
      pragma Assert (Length <= Ctx.Buffer.all'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Ctx.Written_Last := RFLX_Types.Bit_Index'Max (Ctx.Written_Last, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (Ctx.Buffer_First) + Offset + Length - 1));
   end Generic_Write;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     ((if Ctx.Verified_Last = Ctx.First - 1 then 0 else Ctx.Verified_Last - Ctx.First + 1));

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     ((if
          Ctx.Verified_Last = Ctx.First - 1
       then
          0
       else
          RFLX_Types.Length (RFLX_Types.To_Index (Ctx.Verified_Last) - RFLX_Types.To_Index (Ctx.First) + 1)));

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Message_Type =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
              then
                 F_Data
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
              then
                 F_Final
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                 and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                 and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
              then
                 F_Length
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
              then
                 F_Options
              else
                 F_Initial),
          when F_Length =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data))
              then
                 F_Data
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))
              then
                 F_Option_Types
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options))
              then
                 F_Options
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                 and RFLX_Types.U64 (Ctx.Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8
              then
                 F_Value
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values))
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
     ((case Fld is
          when F_Message_Type | F_Length =>
             False,
          when F_Data | F_Option_Types | F_Options =>
             True,
          when F_Value =>
             False,
          when F_Values =>
             True));

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Has_Buffer (Ctx)
       and then Valid_Next (Ctx, Fld)
       and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      function Extract is new RFLX_Types.Extract (RFLX.Universal.Message_Type_Base);
      function Extract is new RFLX_Types.Extract (RFLX.Universal.Length_Base);
      function Extract is new RFLX_Types.Extract (RFLX.Universal.Value);
   begin
      return ((case Fld is
                  when F_Message_Type =>
                     (Fld => F_Message_Type, Message_Type_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Length =>
                     (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Data =>
                     (Fld => F_Data),
                  when F_Option_Types =>
                     (Fld => F_Option_Types),
                  when F_Options =>
                     (Fld => F_Options),
                  when F_Value =>
                     (Fld => F_Value, Value_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Values =>
                     (Fld => F_Values)));
   end Get_Field_Value;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : Field_Dependent_Value;
   begin
      if
         Has_Buffer (Ctx)
         and then Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := Get_Field_Value (Ctx, Fld);
            if
               Valid_Value (Value)
               and Field_Condition (Ctx, Value)
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
               case Fld is
                  when F_Message_Type =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Length =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Data =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Option_Types =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Options =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Value =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Values =>
                     Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  case Fld is
                     when F_Data =>
                        Ctx.Cursors (F_Data) := (State => S_Structural_Valid, First => Field_First (Ctx, F_Data), Last => Field_Last (Ctx, F_Data), Value => Value, Predecessor => Ctx.Cursors (F_Data).Predecessor);
                     when F_Option_Types =>
                        Ctx.Cursors (F_Option_Types) := (State => S_Structural_Valid, First => Field_First (Ctx, F_Option_Types), Last => Field_Last (Ctx, F_Option_Types), Value => Value, Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
                     when F_Options =>
                        Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => Field_First (Ctx, F_Options), Last => Field_Last (Ctx, F_Options), Value => Value, Predecessor => Ctx.Cursors (F_Options).Predecessor);
                     when F_Values =>
                        Ctx.Cursors (F_Values) := (State => S_Structural_Valid, First => Field_First (Ctx, F_Values), Last => Field_Last (Ctx, F_Values), Value => Value, Predecessor => Ctx.Cursors (F_Values).Predecessor);
                     when others =>
                        null;
                  end case;
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Message_Type then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Data then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Types then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Options then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Value then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Values then
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
      Verify (Ctx, F_Message_Type);
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Data);
      Verify (Ctx, F_Option_Types);
      Verify (Ctx, F_Options);
      Verify (Ctx, F_Value);
      Verify (Ctx, F_Values);
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

   procedure Set_Field_Value (Ctx : in out Context; Val : Field_Dependent_Value; Fst, Lst : out RFLX_Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Val.Fld in Field'Range
       and then Valid_Next (Ctx, Val.Fld)
       and then Available_Space (Ctx, Val.Fld) >= Field_Size (Ctx, Val.Fld)
       and then (for all F in Field'Range =>
                    (if Structural_Valid (Ctx.Cursors (F)) then Ctx.Cursors (F).Last <= Field_Last (Ctx, Val.Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Fst = Field_First (Ctx, Val.Fld)
       and Lst = Field_Last (Ctx, Val.Fld)
       and Fst >= Ctx.First
       and Fst <= Lst + 1
       and Lst <= Ctx.Last
       and (for all F in Field'Range =>
               (if Structural_Valid (Ctx.Cursors (F)) then Ctx.Cursors (F).Last <= Lst))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Cursors = Ctx.Cursors'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Val.Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Val.Fld);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.Universal.Message_Type_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.Universal.Length_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.Universal.Value);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Message_Type =>
            Insert (Val.Message_Type_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Data | F_Option_Types | F_Options =>
            null;
         when F_Value =>
            Insert (Val.Value_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Values | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Message_Type (Ctx : in out Context; Val : RFLX.Universal.Message_Type) is
      Field_Value : constant Field_Dependent_Value := (F_Message_Type, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Message_Type);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Message_Type) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Message_Type).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Message_Type)) := (State => S_Invalid, Predecessor => F_Message_Type);
   end Set_Message_Type;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Universal.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

   procedure Set_Value (Ctx : in out Context; Val : RFLX.Universal.Value) is
      Field_Value : constant Field_Dependent_Value := (F_Value, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Value);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Value) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Value).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Value)) := (State => S_Invalid, Predecessor => F_Value);
   end Set_Value;

   procedure Set_Data_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Data);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Data) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Set_Data_Empty;

   procedure Set_Option_Types_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Types);
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Types);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Types) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Option_Types), Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Types)) := (State => S_Invalid, Predecessor => F_Option_Types);
   end Set_Option_Types_Empty;

   procedure Set_Options_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Options);
   begin
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
   end Set_Options_Empty;

   procedure Set_Values_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Values);
   begin
      Reset_Dependent_Fields (Ctx, F_Values);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Values) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Values), Predecessor => Ctx.Cursors (F_Values).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Values)) := (State => S_Invalid, Predecessor => F_Values);
   end Set_Values_Empty;

   procedure Set_Option_Types (Ctx : in out Context; Seq_Ctx : Universal.Option_Types.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Types) + RFLX_Types.To_Bit_Length (Universal.Option_Types.Byte_Size (Seq_Ctx)) - 1;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Types);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Types) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Option_Types), Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Types)) := (State => S_Invalid, Predecessor => F_Option_Types);
      Universal.Option_Types.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Option_Types;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : Universal.Options.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options) + RFLX_Types.To_Bit_Length (Universal.Options.Byte_Size (Seq_Ctx)) - 1;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
      Universal.Options.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Options;

   procedure Set_Values (Ctx : in out Context; Seq_Ctx : Universal.Values.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Values) + RFLX_Types.To_Bit_Length (Universal.Values.Byte_Size (Seq_Ctx)) - 1;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Values);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Values) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Values), Predecessor => Ctx.Cursors (F_Values).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Values)) := (State => S_Invalid, Predecessor => F_Values);
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
      Reset_Dependent_Fields (Ctx, F_Data);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
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
      Reset_Dependent_Fields (Ctx, F_Option_Types);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Types) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Option_Types), Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
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
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
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
      Reset_Dependent_Fields (Ctx, F_Values);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Values) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Values), Predecessor => Ctx.Cursors (F_Values).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Values)) := (State => S_Invalid, Predecessor => F_Values);
   end Initialize_Values_Private;

   procedure Initialize_Values (Ctx : in out Context) is
   begin
      Initialize_Values_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Values)));
   end Initialize_Values;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Data_Private (Ctx, Data'Length);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Data;

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
         Ctx.Cursors (F_Option_Types) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Option_Types), Predecessor => Ctx.Cursors (F_Option_Types).Predecessor);
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
         Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
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
         Ctx.Cursors (F_Values) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Values), Predecessor => Ctx.Cursors (F_Values).Predecessor);
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
