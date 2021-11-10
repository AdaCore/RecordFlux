pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Message with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; Length : Test.Length; Extended : Boolean) is
   begin
      Initialize (Ctx, Buffer, RFLX_Types.To_First_Bit_Index (Buffer'First), RFLX_Types.To_Last_Bit_Index (Buffer'Last), Length, Extended);
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Length : Test.Length; Extended : Boolean) is
      Buffer_First : constant RFLX_Types.Index := Buffer'First;
      Buffer_Last : constant RFLX_Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Length, Extended, First - 1, Buffer, (F_Data => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context; Length : Test.Length; Extended : Boolean) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last), Length, Extended);
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Length : Test.Length; Extended : Boolean) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, Length, Extended, First - 1, Ctx.Buffer, (F_Data => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
      Write (Ctx.Buffer.all, Length);
      pragma Assert (Length <= Ctx.Buffer.all'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First), RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (Ctx.Buffer_First) + Length - 1), Ctx.Length, Ctx.Extended);
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
          when F_Data =>
             (if
                 RFLX_Types.U64 (To_Base (Ctx.Extended)) = RFLX_Types.U64 (To_Base (True))
              then
                 F_Extension
              elsif
                 RFLX_Types.U64 (To_Base (Ctx.Extended)) = RFLX_Types.U64 (To_Base (False))
              then
                 F_Final
              else
                 F_Initial),
          when F_Extension =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Data =>
             Invalid (Ctx.Cursors (F_Extension)),
          when F_Extension =>
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
                   when F_Data | F_Extension =>
                      Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld))) = Data));

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
       and Ctx.Length = Ctx.Length'Old
       and Ctx.Extended = Ctx.Extended'Old
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

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Has_Buffer (Ctx)
       and then Valid_Next (Ctx, Fld)
       and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
   begin
      return ((case Fld is
                  when F_Data =>
                     (Fld => F_Data),
                  when F_Extension =>
                     (Fld => F_Extension)));
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
               pragma Assert ((if Fld = F_Data or Fld = F_Extension then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               case Fld is
                  when F_Data =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Extension =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               if Fld = F_Data then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Extension then
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
      Verify (Ctx, F_Data);
      Verify (Ctx, F_Extension);
   end Verify_Message;

   function Get_Data (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Data;

   function Get_Extension (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Extension;

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Data;

   procedure Get_Extension (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Extension;

   procedure Generic_Get_Data (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Data).Last);
   begin
      Process_Data (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Data;

   procedure Generic_Get_Extension (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Extension).Last);
   begin
      Process_Extension (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Extension;

   procedure Initialize_Data_Private (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and Ctx.Message_Last = Field_Last (Ctx, F_Data)
       and Invalid (Ctx, F_Extension)
       and (if
               RFLX_Types.U64 (To_Base (Ctx.Extended)) = RFLX_Types.U64 (To_Base (True))
            then
               Predecessor (Ctx, F_Extension) = F_Data
               and Valid_Next (Ctx, F_Extension))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Length = Ctx.Length'Old
       and Ctx.Extended = Ctx.Extended'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Data);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Initialize_Data_Private;

   procedure Initialize_Data (Ctx : in out Context) is
   begin
      Initialize_Data_Private (Ctx);
   end Initialize_Data;

   procedure Initialize_Extension_Private (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Extension)
       and then Available_Space (Ctx, F_Extension) >= Field_Size (Ctx, F_Extension)
       and then Field_First (Ctx, F_Extension) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Extension) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Extension) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Extension)
       and Ctx.Message_Last = Field_Last (Ctx, F_Extension)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Length = Ctx.Length'Old
       and Ctx.Extended = Ctx.Extended'Old
       and Predecessor (Ctx, F_Extension) = Predecessor (Ctx, F_Extension)'Old
       and Valid_Next (Ctx, F_Extension) = Valid_Next (Ctx, F_Extension)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Extension);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Extension);
   begin
      Reset_Dependent_Fields (Ctx, F_Extension);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Extension) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Extension), Predecessor => Ctx.Cursors (F_Extension).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Extension)) := (State => S_Invalid, Predecessor => F_Extension);
   end Initialize_Extension_Private;

   procedure Initialize_Extension (Ctx : in out Context) is
   begin
      Initialize_Extension_Private (Ctx);
   end Initialize_Extension;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Data);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Data_Private (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Data;

   procedure Set_Extension (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Extension);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Extension);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Extension_Private (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Extension;

   procedure Generic_Set_Data (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Data);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Data_Private (Ctx);
      Process_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Data;

   procedure Generic_Set_Extension (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Extension);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Extension);
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Initialize_Extension_Private (Ctx);
      Process_Extension (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Extension;

end RFLX.Test.Message;
