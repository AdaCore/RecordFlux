pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.IPv4.Option with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, Buffer, (F_Copied => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Cursors := (F_Copied => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final));
      Ctx.Message_Last := Ctx.First - 1;
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, Ctx.Buffer, (F_Copied => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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

   procedure Read (Ctx : Context) is
   begin
      Read (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Message_Last)));
   end Read;

   procedure Write (Ctx : in out Context) is
      Length : RFLX_Types.Length;
   begin
      Write (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Last)), Length);
      pragma Assert (Length <= Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Last))'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Reset (Ctx, Ctx.First, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (RFLX_Types.To_Index (Ctx.First)) + Length - 1));
   end Write;

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
          when F_Copied =>
             F_Option_Class,
          when F_Option_Class =>
             F_Option_Number,
          when F_Option_Number =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX_Types.U64 (To_Base (RFLX.IPv4.Control))
                 and Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 1
              then
                 F_Final
              elsif
                 Ctx.Cursors (F_Option_Number).Value.Option_Number_Value > 1
              then
                 F_Option_Length
              else
                 F_Initial),
          when F_Option_Length =>
             (if
                 (RFLX_Types.U64 (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX_Types.U64 (To_Base (RFLX.IPv4.Debugging_And_Measurement))
                  and Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 4)
                 or (RFLX_Types.U64 (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX_Types.U64 (To_Base (RFLX.IPv4.Control))
                     and (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 9
                          or Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 3
                          or Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 7))
                 or (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value = 11
                     and RFLX_Types.U64 (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX_Types.U64 (To_Base (RFLX.IPv4.Control))
                     and Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 2)
                 or (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value = 4
                     and RFLX_Types.U64 (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX_Types.U64 (To_Base (RFLX.IPv4.Control))
                     and Ctx.Cursors (F_Option_Number).Value.Option_Number_Value = 8)
              then
                 F_Option_Data
              else
                 F_Initial),
          when F_Option_Data =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Copied =>
             Invalid (Ctx.Cursors (F_Option_Class)),
          when F_Option_Class =>
             Invalid (Ctx.Cursors (F_Option_Number)),
          when F_Option_Number =>
             Invalid (Ctx.Cursors (F_Option_Length)),
          when F_Option_Length =>
             Invalid (Ctx.Cursors (F_Option_Data)),
          when F_Option_Data =>
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
                   when F_Option_Data =>
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
       and (case Fld is
               when F_Copied =>
                  Invalid (Ctx, F_Copied)
                  and Invalid (Ctx, F_Option_Class)
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Class =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Invalid (Ctx, F_Option_Class)
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Number =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Length =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Ctx.Cursors (F_Option_Number) = Ctx.Cursors (F_Option_Number)'Old
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Data =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Ctx.Cursors (F_Option_Number) = Ctx.Cursors (F_Option_Number)'Old
                  and Ctx.Cursors (F_Option_Length) = Ctx.Cursors (F_Option_Length)'Old
                  and Invalid (Ctx, F_Option_Data))
   is
      First : constant RFLX_Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      case Fld is
         when F_Copied =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Class) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Copied) := (S_Invalid, Ctx.Cursors (F_Copied).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Option_Class =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Class) := (S_Invalid, Ctx.Cursors (F_Option_Class).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Option_Number =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, Ctx.Cursors (F_Option_Number).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Option_Length =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, Ctx.Cursors (F_Option_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Option_Data =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, Ctx.Cursors (F_Option_Data).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Copied | F_Option_Class | F_Option_Number | F_Option_Length =>
             False,
          when F_Option_Data =>
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
      function Extract is new RFLX_Types.Extract (RFLX.RFLX_Builtin_Types.Boolean_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Option_Class_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Option_Number);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Option_Length_Base);
   begin
      return ((case Fld is
                  when F_Copied =>
                     (Fld => F_Copied, Copied_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Class =>
                     (Fld => F_Option_Class, Option_Class_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Number =>
                     (Fld => F_Option_Number, Option_Number_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Length =>
                     (Fld => F_Option_Length, Option_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Data =>
                     (Fld => F_Option_Data)));
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
                                  Fld = F_Option_Data
                                  or Fld = F_Option_Number
                               then
                                  Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               case Fld is
                  when F_Copied =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Option_Class =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Option_Number =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Option_Length =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Option_Data =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Copied then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Class then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Number then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Data then
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
      Verify (Ctx, F_Copied);
      Verify (Ctx, F_Option_Class);
      Verify (Ctx, F_Option_Number);
      Verify (Ctx, F_Option_Length);
      Verify (Ctx, F_Option_Data);
   end Verify_Message;

   function Get_Option_Data (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Option_Data;

   procedure Get_Option_Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Option_Data;

   procedure Generic_Get_Option_Data (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Data).Last);
   begin
      Process_Option_Data (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Option_Data;

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
      procedure Insert is new RFLX_Types.Insert (RFLX.RFLX_Builtin_Types.Boolean_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Option_Class_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Option_Number);
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Option_Length_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Copied =>
            Insert (Val.Copied_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Class =>
            Insert (Val.Option_Class_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Number =>
            Insert (Val.Option_Number_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Length =>
            Insert (Val.Option_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Data | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Copied (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Copied, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Copied);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Copied) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Copied).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Copied)) := (State => S_Invalid, Predecessor => F_Copied);
   end Set_Copied;

   procedure Set_Option_Class (Ctx : in out Context; Val : RFLX.IPv4.Option_Class) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Class, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Class);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Option_Class) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Class).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Class)) := (State => S_Invalid, Predecessor => F_Option_Class);
   end Set_Option_Class;

   procedure Set_Option_Number (Ctx : in out Context; Val : RFLX.IPv4.Option_Number) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Number, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Number);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Option_Number) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Number).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Number)) := (State => S_Invalid, Predecessor => F_Option_Number);
   end Set_Option_Number;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.IPv4.Option_Length) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Length, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Option_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Length)) := (State => S_Invalid, Predecessor => F_Option_Length);
   end Set_Option_Length;

   procedure Set_Option_Data_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Data);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Option_Data) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Option_Data), Predecessor => Ctx.Cursors (F_Option_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Data)) := (State => S_Invalid, Predecessor => F_Option_Data);
   end Set_Option_Data_Empty;

   procedure Initialize_Option_Data_Private (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Data)
       and then Available_Space (Ctx, F_Option_Data) >= Field_Size (Ctx, F_Option_Data)
       and then Field_First (Ctx, F_Option_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Option_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Option_Data) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Option_Data)
       and Ctx.Message_Last = Field_Last (Ctx, F_Option_Data)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Data) = Predecessor (Ctx, F_Option_Data)'Old
       and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Data);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Option_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Option_Data), Predecessor => Ctx.Cursors (F_Option_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Data)) := (State => S_Invalid, Predecessor => F_Option_Data);
   end Initialize_Option_Data_Private;

   procedure Initialize_Option_Data (Ctx : in out Context) is
   begin
      Initialize_Option_Data_Private (Ctx);
   end Initialize_Option_Data;

   procedure Set_Option_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Option_Data_Private (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Option_Data;

   procedure Generic_Set_Option_Data (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Option_Data_Private (Ctx);
      Process_Option_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Option_Data;

end RFLX.IPv4.Option;
