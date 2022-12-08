pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Message with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Data => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Data => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
          when F_Data =>
             F_Final))
    with
     Pre =>
       RFLX.Test.Message.Has_Buffer (Ctx)
       and RFLX.Test.Message.Well_Formed (Ctx, Fld)
       and RFLX.Test.Message.Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       RFLX.Test.Message.Has_Buffer (Ctx)
       and RFLX.Test.Message.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Data =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld)))))));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.Test.Message.Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
       and Invalid (Ctx.Cursors (Fld))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Cursors (Fld).Predecessor = Ctx.Cursors (Fld).Predecessor'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Old
       and Invalid (Ctx, F_Data)
   is
      First : constant RFLX_Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      Ctx.Cursors (Fld) := (S_Invalid, Ctx.Cursors (Fld).Predecessor);
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
   end Reset_Dependent_Fields;

   function Composite_Field (Unused_Fld : Field) return Boolean is
     (True);

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : RFLX_Types.Base_Integer;
   begin
      if
         Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := 0;
            if
               Valid_Value (Fld, Value)
               and then Field_Condition (Ctx, Fld)
            then
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               Ctx.Cursors (Fld) := (State => S_Well_Formed, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
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
         pragma Loop_Invariant (Has_Buffer (Ctx)
                                and Ctx.Buffer_First = Ctx.Buffer_First'Loop_Entry
                                and Ctx.Buffer_Last = Ctx.Buffer_Last'Loop_Entry
                                and Ctx.First = Ctx.First'Loop_Entry
                                and Ctx.Last = Ctx.Last'Loop_Entry);
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

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       RFLX.Test.Message.Has_Buffer (Ctx)
       and then RFLX.Test.Message.Valid_Next (Ctx, Fld)
       and then RFLX.Test.Message.Valid_Value (Fld, Val)
       and then RFLX.Test.Message.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.Test.Message.Available_Space (Ctx, Fld)
       and then (if RFLX.Test.Message.Composite_Field (Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
     Post =>
       Valid_Next (Ctx, Fld)
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
       and then Sufficient_Space (Ctx, Fld)
       and then (if State_Valid and Size > 0 then Valid (Ctx, Fld) else Well_Formed (Ctx, Fld))
       and then (case Fld is
                    when F_Data =>
                       (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)))
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
                         when F_Data =>
                            RFLX_Types.Bit_Length (Ctx.Written_Last - Ctx.First + 1)));
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      else
         Ctx.Cursors (Fld) := (State => S_Well_Formed, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      end if;
      Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
      pragma Assert (Last = (Field_First (Ctx, Fld) + Size) - 1);
   end Set;

   procedure Set_Data_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Data, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Data_Empty;

   procedure Initialize_Data_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Test.Message.Has_Buffer (Ctx)
       and then RFLX.Test.Message.Valid_Next (Ctx, RFLX.Test.Message.F_Data)
       and then RFLX.Test.Message.Valid_Length (Ctx, RFLX.Test.Message.F_Data, Length)
       and then RFLX_Types.To_Length (RFLX.Test.Message.Available_Space (Ctx, RFLX.Test.Message.F_Data)) >= Length
       and then RFLX.Test.Message.Field_First (Ctx, RFLX.Test.Message.F_Data) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and Field_Size (Ctx, F_Data) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Data)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Data);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Data) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Initialize_Data_Private;

   procedure Initialize_Data (Ctx : in out Context; Length : RFLX_Types.Length) is
   begin
      Initialize_Data_Private (Ctx, Length);
   end Initialize_Data;

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

end RFLX.Test.Message;