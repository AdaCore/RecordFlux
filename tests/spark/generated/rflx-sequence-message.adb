pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Sequence.Message with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, Buffer, (F_Length => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Cursors := (F_Length => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final));
      Ctx.Message_Last := Ctx.First - 1;
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, Ctx.Buffer, (F_Length => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
      Reset (Ctx, Ctx.First, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (RFLX_Types.To_Index (Ctx.First)) + Length - 1));
   end Write;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     ((if
          Ctx.Message_Last = Ctx.First - 1
       then
          0
       else
          Ctx.Message_Last - Ctx.First + 1));

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
          when F_Length =>
             F_Modular_Vector,
          when F_Modular_Vector =>
             F_Range_Vector,
          when F_Range_Vector =>
             F_Enumeration_Vector,
          when F_Enumeration_Vector =>
             F_AV_Enumeration_Vector,
          when F_AV_Enumeration_Vector =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Length =>
             Invalid (Ctx.Cursors (F_Modular_Vector)),
          when F_Modular_Vector =>
             Invalid (Ctx.Cursors (F_Range_Vector)),
          when F_Range_Vector =>
             Invalid (Ctx.Cursors (F_Enumeration_Vector)),
          when F_Enumeration_Vector =>
             Invalid (Ctx.Cursors (F_AV_Enumeration_Vector)),
          when F_AV_Enumeration_Vector =>
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
                   when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
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
               when F_Length =>
                  Invalid (Ctx, F_Length)
                  and Invalid (Ctx, F_Modular_Vector)
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Modular_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Invalid (Ctx, F_Modular_Vector)
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Range_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Enumeration_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Ctx.Cursors (F_Range_Vector) = Ctx.Cursors (F_Range_Vector)'Old
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_AV_Enumeration_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Ctx.Cursors (F_Range_Vector) = Ctx.Cursors (F_Range_Vector)'Old
                  and Ctx.Cursors (F_Enumeration_Vector) = Ctx.Cursors (F_Enumeration_Vector)'Old
                  and Invalid (Ctx, F_AV_Enumeration_Vector))
   is
      First : constant RFLX_Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      case Fld is
         when F_Length =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Modular_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, Ctx.Cursors (F_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Modular_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Modular_Vector) := (S_Invalid, Ctx.Cursors (F_Modular_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Range_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, Ctx.Cursors (F_Range_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Enumeration_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, Ctx.Cursors (F_Enumeration_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_AV_Enumeration_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Length =>
             False,
          when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
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
      function Extract is new RFLX_Types.Extract (RFLX.Sequence.Length);
   begin
      return ((case Fld is
                  when F_Length =>
                     (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Modular_Vector =>
                     (Fld => F_Modular_Vector),
                  when F_Range_Vector =>
                     (Fld => F_Range_Vector),
                  when F_Enumeration_Vector =>
                     (Fld => F_Enumeration_Vector),
                  when F_AV_Enumeration_Vector =>
                     (Fld => F_AV_Enumeration_Vector)));
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
                                  Fld = F_AV_Enumeration_Vector
                               then
                                  Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               case Fld is
                  when F_Length =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Modular_Vector =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Range_Vector =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Enumeration_Vector =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_AV_Enumeration_Vector =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Modular_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Range_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Enumeration_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_AV_Enumeration_Vector then
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
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Modular_Vector);
      Verify (Ctx, F_Range_Vector);
      Verify (Ctx, F_Enumeration_Vector);
      Verify (Ctx, F_AV_Enumeration_Vector);
   end Verify_Message;

   procedure Set_Field_Value (Ctx : in out Context; Val : Field_Dependent_Value; Fst, Lst : out RFLX_Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Val.Fld in Field'Range
       and then Valid_Next (Ctx, Val.Fld)
       and then Available_Space (Ctx, Val.Fld) >= Field_Size (Ctx, Val.Fld)
       and then (for all F in Field'Range =>
                    (if
                        Structural_Valid (Ctx.Cursors (F))
                     then
                        Ctx.Cursors (F).Last <= Field_Last (Ctx, Val.Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Fst = Field_First (Ctx, Val.Fld)
       and Lst = Field_Last (Ctx, Val.Fld)
       and Fst >= Ctx.First
       and Fst <= Lst + 1
       and Lst <= Ctx.Last
       and (for all F in Field'Range =>
               (if
                   Structural_Valid (Ctx.Cursors (F))
                then
                   Ctx.Cursors (F).Last <= Lst))
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
      procedure Insert is new RFLX_Types.Insert (RFLX.Sequence.Length);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Sequence.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

   procedure Set_Modular_Vector_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Modular_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Modular_Vector);
   begin
      Reset_Dependent_Fields (Ctx, F_Modular_Vector);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Modular_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Modular_Vector), Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Modular_Vector)) := (State => S_Invalid, Predecessor => F_Modular_Vector);
   end Set_Modular_Vector_Empty;

   procedure Set_Modular_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Modular_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Modular_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Modular_Vector);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Modular_Vector);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Modular_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Modular_Vector), Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Modular_Vector)) := (State => S_Invalid, Predecessor => F_Modular_Vector);
      Sequence.Modular_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Modular_Vector;

   procedure Set_Range_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Range_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Range_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Range_Vector);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Range_Vector);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Range_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Range_Vector), Predecessor => Ctx.Cursors (F_Range_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Range_Vector)) := (State => S_Invalid, Predecessor => F_Range_Vector);
      Sequence.Range_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Range_Vector;

   procedure Set_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Enumeration_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Enumeration_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Enumeration_Vector);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Enumeration_Vector);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Enumeration_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Enumeration_Vector), Predecessor => Ctx.Cursors (F_Enumeration_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_Enumeration_Vector);
      Sequence.Enumeration_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Enumeration_Vector;

   procedure Set_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : Sequence.AV_Enumeration_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_AV_Enumeration_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_AV_Enumeration_Vector);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_AV_Enumeration_Vector);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_AV_Enumeration_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_AV_Enumeration_Vector), Predecessor => Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_AV_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_AV_Enumeration_Vector);
      Sequence.AV_Enumeration_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_AV_Enumeration_Vector;

   procedure Switch_To_Modular_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Modular_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Modular_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Modular_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Modular_Vector) then
         Reset_Dependent_Fields (Ctx, F_Modular_Vector);
         Ctx.Message_Last := Last;
         Ctx.Cursors (F_Modular_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Modular_Vector), Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Modular_Vector)) := (State => S_Invalid, Predecessor => F_Modular_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Sequence.Modular_Vector.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Modular_Vector;

   procedure Switch_To_Range_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Range_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Range_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Range_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Range_Vector) then
         Reset_Dependent_Fields (Ctx, F_Range_Vector);
         Ctx.Message_Last := Last;
         Ctx.Cursors (F_Range_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Range_Vector), Predecessor => Ctx.Cursors (F_Range_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Range_Vector)) := (State => S_Invalid, Predecessor => F_Range_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Sequence.Range_Vector.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Range_Vector;

   procedure Switch_To_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Enumeration_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Enumeration_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Enumeration_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Enumeration_Vector) then
         Reset_Dependent_Fields (Ctx, F_Enumeration_Vector);
         Ctx.Message_Last := Last;
         Ctx.Cursors (F_Enumeration_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Enumeration_Vector), Predecessor => Ctx.Cursors (F_Enumeration_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_Enumeration_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Sequence.Enumeration_Vector.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Enumeration_Vector;

   procedure Switch_To_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.AV_Enumeration_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_AV_Enumeration_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_AV_Enumeration_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_AV_Enumeration_Vector) then
         Reset_Dependent_Fields (Ctx, F_AV_Enumeration_Vector);
         Ctx.Message_Last := Last;
         Ctx.Cursors (F_AV_Enumeration_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_AV_Enumeration_Vector), Predecessor => Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_AV_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_AV_Enumeration_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Sequence.AV_Enumeration_Vector.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_AV_Enumeration_Vector;

   procedure Update_Modular_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Modular_Vector.Context) is
      Valid_Sequence : constant Boolean := Sequence.Modular_Vector.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Sequence.Modular_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Modular_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Modular_Vector).First, Last => Ctx.Cursors (F_Modular_Vector).Last, Value => Ctx.Cursors (F_Modular_Vector).Value, Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
      end if;
   end Update_Modular_Vector;

   procedure Update_Range_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Range_Vector.Context) is
      Valid_Sequence : constant Boolean := Sequence.Range_Vector.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Sequence.Range_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Range_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Range_Vector).First, Last => Ctx.Cursors (F_Range_Vector).Last, Value => Ctx.Cursors (F_Range_Vector).Value, Predecessor => Ctx.Cursors (F_Range_Vector).Predecessor);
      end if;
   end Update_Range_Vector;

   procedure Update_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Enumeration_Vector.Context) is
      Valid_Sequence : constant Boolean := Sequence.Enumeration_Vector.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Sequence.Enumeration_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Enumeration_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Enumeration_Vector).First, Last => Ctx.Cursors (F_Enumeration_Vector).Last, Value => Ctx.Cursors (F_Enumeration_Vector).Value, Predecessor => Ctx.Cursors (F_Enumeration_Vector).Predecessor);
      end if;
   end Update_Enumeration_Vector;

   procedure Update_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.AV_Enumeration_Vector.Context) is
      Valid_Sequence : constant Boolean := Sequence.AV_Enumeration_Vector.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Sequence.AV_Enumeration_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_AV_Enumeration_Vector) := (State => S_Valid, First => Ctx.Cursors (F_AV_Enumeration_Vector).First, Last => Ctx.Cursors (F_AV_Enumeration_Vector).Last, Value => Ctx.Cursors (F_AV_Enumeration_Vector).Value, Predecessor => Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
      end if;
   end Update_AV_Enumeration_Vector;

end RFLX.Sequence.Message;
