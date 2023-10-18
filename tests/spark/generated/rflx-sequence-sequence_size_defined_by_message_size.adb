pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operations;

package body RFLX.Sequence.Sequence_Size_Defined_By_Message_Size with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Header => (State => S_Invalid, others => <>), others => <>));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Header => (State => S_Invalid, others => <>), others => <>));
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
          when F_Header =>
             F_Vector,
          when F_Vector =>
             F_Final))
    with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed (Ctx, Fld)
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

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
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Vector =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld))))),
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Old
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F) else Invalid (Ctx, F)))
   is
   begin
      for Fld_Loop in reverse Fld .. Field'Last loop
         pragma Loop_Invariant (Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Loop_Entry
                                and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Loop_Entry);
         pragma Loop_Invariant ((for all F in Field =>
                                    (if F <= Fld_Loop then Ctx.Cursors (F) = Ctx.Cursors'Loop_Entry (F) else Invalid (Ctx, F))));
         Ctx.Cursors (Fld_Loop) := (State => S_Invalid, others => <>);
      end loop;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     (Fld in F_Vector);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.Base_Integer with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Sufficient_Buffer_Length (Ctx, Fld)
       and then not RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Header =>
             8,
          when others =>
             Positive'Last);
      Byte_Order : constant RFLX_Types.Byte_Order := RFLX_Types.High_Order_First;
   begin
      return RFLX_Types.Operations.Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, Size, Byte_Order);
   end Get;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : RFLX_Types.Base_Integer;
   begin
      if
         Invalid (Ctx.Cursors (Fld))
         and then Valid_Next (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := (if Composite_Field (Fld) then 0 else Get (Ctx, Fld));
            if
               Valid_Value (Fld, Value)
               and then Field_Condition (Ctx, Fld)
            then
               pragma Assert ((if Fld = F_Vector then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Well_Formed, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value);
               end if;
            else
               Ctx.Cursors (Fld) := (others => <>);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Incomplete, others => <>);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Ctx : in out Context) is
   begin
      for F in Field loop
         pragma Loop_Invariant (Has_Buffer (Ctx)
                                and Ctx.Buffer_First = Ctx.Buffer_First'Loop_Entry
                                and Ctx.Buffer_Last = Ctx.Buffer_Last'Loop_Entry
                                and Ctx.First = Ctx.First'Loop_Entry
                                and Ctx.Last = Ctx.Last'Loop_Entry);
         Verify (Ctx, F);
      end loop;
   end Verify_Message;

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Value (Fld, Val)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, Fld)
       and then (if
                    RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Composite_Field (Fld)
                 then
                    Size mod RFLX_Types.Byte'Size = 0
                 else
                    State_Valid),
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
       and then Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and then Sufficient_Space (Ctx, Fld)
       and then (if State_Valid and Size > 0 then Valid (Ctx, Fld) else Well_Formed (Ctx, Fld))
       and then (Ctx.Cursors (Fld).Value = Val
                 and then (if
                              Fld in F_Vector
                              and then Well_Formed_Message (Ctx)
                           then
                              Message_Last (Ctx) = Field_Last (Ctx, Fld)))
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
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val);
      else
         Ctx.Cursors (Fld) := (State => S_Well_Formed, First => First, Last => Last, Value => Val);
      end if;
      pragma Assert (Last = (Field_First (Ctx, Fld) + Size) - 1);
   end Set;

   procedure Set_Scalar (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld)
       and then Fld in F_Header
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Value (Fld, Val)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Size (Ctx, Fld, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, Fld))
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, Fld) >= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, Fld) in 1 .. RFLX_Types.Base_Integer'Size
       and then RFLX_Types.Fits_Into (Val, Natural (RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (Ctx.Cursors (Fld).Value = Val
            and then (if
                         Fld in F_Vector
                         and then Well_Formed_Message (Ctx)
                      then
                         Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
   is
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld);
   begin
      Set (Ctx, Fld, Val, Size, True, Buffer_First, Buffer_Last, Offset);
      RFLX_Types.Lemma_Size (Val, Positive (Size));
      RFLX_Types.Operations.Insert (Val, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, Positive (Size), RFLX_Types.High_Order_First);
   end Set_Scalar;

   procedure Set_Header (Ctx : in out Context; Val : RFLX.Sequence.Enumeration) is
   begin
      Set_Scalar (Ctx, F_Header, RFLX.Sequence.To_Base_Integer (Val));
   end Set_Header;

   procedure Set_Vector_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Vector, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Vector_Empty;

   procedure Set_Vector (Ctx : in out Context; Seq_Ctx : RFLX.Sequence.Integer_Vector.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (RFLX.Sequence.Integer_Vector.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Vector, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      RFLX.Sequence.Integer_Vector.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Vector;

   procedure Initialize_Vector_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Length (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector, Length)
       and then RFLX_Types.To_Length (RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)) >= Length
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_First (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and then Well_Formed (Ctx, F_Vector)
       and then Field_Size (Ctx, F_Vector) = RFLX_Types.To_Bit_Length (Length)
       and then Ctx.Verified_Last = Field_Last (Ctx, F_Vector)
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and then Get_Header (Ctx) = Get_Header (Ctx)'Old
       and then Field_First (Ctx, F_Vector) = Field_First (Ctx, F_Vector)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Vector);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Vector) := (State => S_Well_Formed, First => First, Last => Last, Value => 0);
      Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, others => <>);
   end Initialize_Vector_Private;

   procedure Initialize_Vector (Ctx : in out Context; Length : RFLX_Types.Length) is
   begin
      Initialize_Vector_Private (Ctx, Length);
   end Initialize_Vector;

   procedure Switch_To_Vector (Ctx : in out Context; Seq_Ctx : out RFLX.Sequence.Integer_Vector.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Vector);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Vector);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Vector) then
         Reset_Dependent_Fields (Ctx, F_Vector);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Vector) := (State => S_Well_Formed, First => First, Last => Last, Value => 0);
         Ctx.Cursors (Successor (Ctx, F_Vector)) := (State => S_Invalid, others => <>);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.Sequence.Integer_Vector.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Vector;

   procedure Update_Vector (Ctx : in out Context; Seq_Ctx : in out RFLX.Sequence.Integer_Vector.Context) is
      Valid_Sequence : constant Boolean := RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Complete_Vector (Ctx, Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.Sequence.Integer_Vector.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Vector).First, Last => Ctx.Cursors (F_Vector).Last, Value => Ctx.Cursors (F_Vector).Value);
      else
         Reset_Dependent_Fields (Ctx, F_Vector);
         Ctx.Cursors (F_Vector) := (State => S_Invalid, others => <>);
      end if;
   end Update_Vector;

end RFLX.Sequence.Sequence_Size_Defined_By_Message_Size;
