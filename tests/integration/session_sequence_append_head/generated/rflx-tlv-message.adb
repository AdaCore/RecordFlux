pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.TLV.Message with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX_Types.To_First_Bit_Index (Buffer'First), RFLX_Types.To_Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
      Buffer_First : constant RFLX_Types.Index := Buffer'First;
      Buffer_Last : constant RFLX_Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, Ctx.Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
   end Reset;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Message_Last));
      else
         Buffer := Ctx.Buffer.all (RFLX_Types.Index'Last .. RFLX_Types.Index'First);
      end if;
   end Copy;

   function Read (Ctx : Context) return RFLX_Types.Bytes is
     (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Message_Last)));

   procedure Generic_Read (Ctx : Context) is
   begin
      Read (Read (Ctx));
   end Generic_Read;

   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) is
      Length : RFLX_Types.Length;
   begin
      Write (Ctx.Buffer.all (Ctx.Buffer'First + RFLX_Types.Index (Offset + 1) - 1 .. Ctx.Buffer'Last), Length, Ctx.Buffer'Length, Offset);
      pragma Assert (Length <= Ctx.Buffer.all'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First), RFLX_Types.Bit_Index'Max (Ctx.Last, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (Ctx.Buffer_First) + Offset + Length - 1)));
   end Generic_Write;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     ((if Ctx.Message_Last = Ctx.First - 1 then 0 else Ctx.Message_Last - Ctx.First + 1));

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     ((if
          Ctx.Message_Last = Ctx.First - 1
       then
          0
       else
          RFLX_Types.Length (RFLX_Types.To_Index (Ctx.Message_Last) - RFLX_Types.To_Index (Ctx.First) + 1)));

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Tag =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Error))
              then
                 F_Final
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
              then
                 F_Length
              else
                 F_Initial),
          when F_Length =>
             F_Value,
          when F_Value =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Tag =>
             Invalid (Ctx.Cursors (F_Length)),
          when F_Length =>
             Invalid (Ctx.Cursors (F_Value)),
          when F_Value =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Value =>
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
      Ctx.Cursors (Fld) := (S_Invalid, Ctx.Cursors (Fld).Predecessor);
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Tag | F_Length =>
             False,
          when F_Value =>
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
      function Extract is new RFLX_Types.Extract (RFLX.TLV.Tag_Base);
      function Extract is new RFLX_Types.Extract (RFLX.TLV.Length);
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
               pragma Assert ((if Fld = F_Tag or Fld = F_Value then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               case Fld is
                  when F_Tag =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Length =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Value =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
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

   function Get_Value (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Value;

   procedure Get_Value (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Value;

   procedure Generic_Get_Value (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Value).Last);
   begin
      Process_Value (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Value;

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
      procedure Insert is new RFLX_Types.Insert (RFLX.TLV.Tag_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.TLV.Length);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Tag =>
            Insert (Val.Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Value | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.TLV.Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Tag, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Tag)) := (State => S_Invalid, Predecessor => F_Tag);
   end Set_Tag;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.TLV.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

   procedure Set_Value_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Value);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Value);
   begin
      Reset_Dependent_Fields (Ctx, F_Value);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Value) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Value), Predecessor => Ctx.Cursors (F_Value).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Value)) := (State => S_Invalid, Predecessor => F_Value);
   end Set_Value_Empty;

   procedure Initialize_Value_Private (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value)
       and then Field_First (Ctx, F_Value) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Value)
       and Ctx.Message_Last = Field_Last (Ctx, F_Value)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Value);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Value);
   begin
      Reset_Dependent_Fields (Ctx, F_Value);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Value) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Value), Predecessor => Ctx.Cursors (F_Value).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Value)) := (State => S_Invalid, Predecessor => F_Value);
   end Initialize_Value_Private;

   procedure Initialize_Value (Ctx : in out Context) is
   begin
      Initialize_Value_Private (Ctx);
   end Initialize_Value;

   procedure Set_Value (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Value);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Value);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Value_Private (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Value;

   procedure Generic_Set_Value (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Value);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Value);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Value_Private (Ctx);
      Process_Value (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Value;

end RFLX.TLV.Message;
