pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Sequence.Sequence_Size_Defined_By_Message_Size with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Header => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Header => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     ((if Ctx.Verified_Last = Ctx.First - 1 then 0 else Ctx.Verified_Last - Ctx.First + 1));

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     ((if
          Ctx.Verified_Last = Ctx.First - 1
       then
          0
       else
          RFLX_Types.Length (RFLX_Types.To_Index (Ctx.Verified_Last) - RFLX_Types.To_Index (Ctx.First) + 1)));

   procedure Message_Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
   begin
      Data := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last));
   end Message_Data;

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Header =>
             F_Vector,
          when F_Vector =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Header =>
             Invalid (Ctx.Cursors (F_Vector)),
          when F_Vector =>
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
                   when F_Vector =>
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
          when F_Header =>
             False,
          when F_Vector =>
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
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((8 - Last mod 8) mod 8);
      function Extract is new RFLX_Types.Extract (RFLX.Sequence.Enumeration_Base);
   begin
      return ((case Fld is
                  when F_Header =>
                     (Fld => F_Header, Header_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Vector =>
                     (Fld => F_Vector)));
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
               pragma Assert ((if Fld = F_Vector then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Header then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Vector then
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
      for F in Field loop
         Verify (Ctx, F);
      end loop;
   end Verify_Message;

   procedure Set_Header (Ctx : in out Context; Val : RFLX.Sequence.Enumeration) is
      Field_Value : constant Field_Dependent_Value := (F_Header, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.Sequence.Enumeration_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Header);
      First := Field_First (Ctx, F_Header);
      Last := Field_Last (Ctx, F_Header);
      Insert (Field_Value.Header_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Header) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Header).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Header)) := (State => S_Invalid, Predecessor => F_Header);
   end Set_Header;

   procedure Set_Vector_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Vector);
   begin
      Reset_Dependent_Fields (Ctx, F_Vector);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Vector), Predecessor => Ctx.Cursors (F_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, Predecessor => F_Vector);
   end Set_Vector_Empty;

   procedure Set_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Modular_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector) + RFLX_Types.To_Bit_Length (Sequence.Modular_Vector.Byte_Size (Seq_Ctx)) - 1;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Vector);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Vector) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Vector), Predecessor => Ctx.Cursors (F_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, Predecessor => F_Vector);
      Sequence.Modular_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Vector;

   procedure Initialize_Vector_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Vector)
       and then Valid_Length (Ctx, F_Vector, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Vector)) >= Length
       and then Field_First (Ctx, F_Vector) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Vector)
       and Field_Size (Ctx, F_Vector) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      Reset_Dependent_Fields (Ctx, F_Vector);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Vector), Predecessor => Ctx.Cursors (F_Vector).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, Predecessor => F_Vector);
   end Initialize_Vector_Private;

   procedure Initialize_Vector (Ctx : in out Context; Length : RFLX_Types.Length) is
   begin
      Initialize_Vector_Private (Ctx, Length);
   end Initialize_Vector;

   procedure Switch_To_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Modular_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Vector) then
         Reset_Dependent_Fields (Ctx, F_Vector);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Vector), Predecessor => Ctx.Cursors (F_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, Predecessor => F_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Sequence.Modular_Vector.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Vector;

   procedure Update_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Modular_Vector.Context) is
      Valid_Sequence : constant Boolean := Sequence.Modular_Vector.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Sequence.Modular_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Vector).First, Last => Ctx.Cursors (F_Vector).Last, Value => Ctx.Cursors (F_Vector).Value, Predecessor => Ctx.Cursors (F_Vector).Predecessor);
      end if;
   end Update_Vector;

end RFLX.Sequence.Sequence_Size_Defined_By_Message_Size;
