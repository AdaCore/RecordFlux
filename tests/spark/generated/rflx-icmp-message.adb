pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.ICMP.Message with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
          when F_Tag =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Destination_Unreachable))
              then
                 F_Code_Destination_Unreachable
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Redirect))
              then
                 F_Code_Redirect
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Time_Exceeded))
              then
                 F_Code_Time_Exceeded
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Parameter_Problem))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Source_Quench))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
              then
                 F_Code_Zero
              else
                 F_Initial),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             F_Checksum,
          when F_Checksum =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Redirect))
              then
                 F_Gateway_Internet_Address
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
              then
                 F_Identifier
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Parameter_Problem))
              then
                 F_Pointer
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Time_Exceeded))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Destination_Unreachable))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Source_Quench))
              then
                 F_Unused_32
              else
                 F_Initial),
          when F_Gateway_Internet_Address =>
             F_Data,
          when F_Identifier =>
             F_Sequence_Number,
          when F_Pointer =>
             F_Unused_24,
          when F_Unused_32 =>
             F_Data,
          when F_Sequence_Number =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
              then
                 F_Data
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Reply))
              then
                 F_Final
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
              then
                 F_Originate_Timestamp
              else
                 F_Initial),
          when F_Unused_24 =>
             F_Data,
          when F_Originate_Timestamp =>
             F_Receive_Timestamp,
          when F_Data =>
             F_Final,
          when F_Receive_Timestamp =>
             F_Transmit_Timestamp,
          when F_Transmit_Timestamp =>
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
             Invalid (Ctx.Cursors (F_Code_Destination_Unreachable))
             and Invalid (Ctx.Cursors (F_Code_Redirect))
             and Invalid (Ctx.Cursors (F_Code_Time_Exceeded))
             and Invalid (Ctx.Cursors (F_Code_Zero)),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             Invalid (Ctx.Cursors (F_Checksum)),
          when F_Checksum =>
             Invalid (Ctx.Cursors (F_Gateway_Internet_Address))
             and Invalid (Ctx.Cursors (F_Identifier))
             and Invalid (Ctx.Cursors (F_Pointer))
             and Invalid (Ctx.Cursors (F_Unused_32)),
          when F_Gateway_Internet_Address =>
             Invalid (Ctx.Cursors (F_Data)),
          when F_Identifier =>
             Invalid (Ctx.Cursors (F_Sequence_Number)),
          when F_Pointer =>
             Invalid (Ctx.Cursors (F_Unused_24)),
          when F_Unused_32 =>
             Invalid (Ctx.Cursors (F_Data)),
          when F_Sequence_Number =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Originate_Timestamp)),
          when F_Unused_24 =>
             Invalid (Ctx.Cursors (F_Data)),
          when F_Originate_Timestamp =>
             Invalid (Ctx.Cursors (F_Receive_Timestamp)),
          when F_Data =>
             True,
          when F_Receive_Timestamp =>
             Invalid (Ctx.Cursors (F_Transmit_Timestamp)),
          when F_Transmit_Timestamp =>
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
                   when F_Data =>
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
     (Fld in F_Data);

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
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Tag_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Code_Destination_Unreachable_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Code_Redirect_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Code_Time_Exceeded_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Code_Zero_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Checksum);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Gateway_Internet_Address);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Identifier);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Pointer);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Unused_32_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Sequence_Number);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Unused_24_Base);
      function Extract is new RFLX_Types.Extract (RFLX.ICMP.Timestamp);
   begin
      return ((case Fld is
                  when F_Tag =>
                     (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Code_Destination_Unreachable =>
                     (Fld => F_Code_Destination_Unreachable, Code_Destination_Unreachable_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Code_Redirect =>
                     (Fld => F_Code_Redirect, Code_Redirect_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Code_Time_Exceeded =>
                     (Fld => F_Code_Time_Exceeded, Code_Time_Exceeded_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Code_Zero =>
                     (Fld => F_Code_Zero, Code_Zero_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Checksum =>
                     (Fld => F_Checksum, Checksum_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Gateway_Internet_Address =>
                     (Fld => F_Gateway_Internet_Address, Gateway_Internet_Address_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Identifier =>
                     (Fld => F_Identifier, Identifier_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Pointer =>
                     (Fld => F_Pointer, Pointer_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Unused_32 =>
                     (Fld => F_Unused_32, Unused_32_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Sequence_Number =>
                     (Fld => F_Sequence_Number, Sequence_Number_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Unused_24 =>
                     (Fld => F_Unused_24, Unused_24_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Originate_Timestamp =>
                     (Fld => F_Originate_Timestamp, Originate_Timestamp_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Data =>
                     (Fld => F_Data),
                  when F_Receive_Timestamp =>
                     (Fld => F_Receive_Timestamp, Receive_Timestamp_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First)),
                  when F_Transmit_Timestamp =>
                     (Fld => F_Transmit_Timestamp, Transmit_Timestamp_Value => Extract (Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First))));
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
                                  or Fld = F_Sequence_Number
                                  or Fld = F_Transmit_Timestamp
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
               for F in Field loop
                  if Fld = F then
                     Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
                  end if;
               end loop;
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

   procedure Set (Ctx : in out Context; Val : Field_Dependent_Value; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       Has_Buffer (Ctx)
       and then Val.Fld in Field
       and then Valid_Next (Ctx, Val.Fld)
       and then Valid_Value (Val)
       and then Valid_Size (Ctx, Val.Fld, Size)
       and then Size <= Available_Space (Ctx, Val.Fld)
       and then (if Composite_Field (Val.Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
     Post =>
       Valid_Next (Ctx, Val.Fld)
       and Invalid_Successor (Ctx, Val.Fld)
       and Buffer_First = RFLX_Types.To_Index (Field_First (Ctx, Val.Fld))
       and Buffer_Last = RFLX_Types.To_Index (Field_First (Ctx, Val.Fld) + Size - 1)
       and Offset = RFLX_Types.Offset ((RFLX_Types.Byte'Size - (Field_First (Ctx, Val.Fld) + Size - 1) mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Predecessor (Ctx, Val.Fld) = Predecessor (Ctx, Val.Fld)'Old
       and Field_First (Ctx, Val.Fld) = Field_First (Ctx, Val.Fld)'Old
       and (if State_Valid and Size > 0 then Valid (Ctx, Val.Fld) else Structural_Valid (Ctx, Val.Fld))
       and (case Val.Fld is
               when F_Initial =>
                  (Predecessor (Ctx, F_Tag) = F_Initial
                   and Valid_Next (Ctx, F_Tag)),
               when F_Tag =>
                  Get_Tag (Ctx) = To_Actual (Val.Tag_Value)
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Destination_Unreachable))
                       then
                          Predecessor (Ctx, F_Code_Destination_Unreachable) = F_Tag
                          and Valid_Next (Ctx, F_Code_Destination_Unreachable))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Redirect))
                       then
                          Predecessor (Ctx, F_Code_Redirect) = F_Tag
                          and Valid_Next (Ctx, F_Code_Redirect))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Time_Exceeded))
                       then
                          Predecessor (Ctx, F_Code_Time_Exceeded) = F_Tag
                          and Valid_Next (Ctx, F_Code_Time_Exceeded))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Request))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Parameter_Problem))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Source_Quench))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
                       then
                          Predecessor (Ctx, F_Code_Zero) = F_Tag
                          and Valid_Next (Ctx, F_Code_Zero)),
               when F_Code_Destination_Unreachable =>
                  Get_Code_Destination_Unreachable (Ctx) = To_Actual (Val.Code_Destination_Unreachable_Value)
                  and (Predecessor (Ctx, F_Checksum) = F_Code_Destination_Unreachable
                       and Valid_Next (Ctx, F_Checksum)),
               when F_Code_Redirect =>
                  Get_Code_Redirect (Ctx) = To_Actual (Val.Code_Redirect_Value)
                  and (Predecessor (Ctx, F_Checksum) = F_Code_Redirect
                       and Valid_Next (Ctx, F_Checksum)),
               when F_Code_Time_Exceeded =>
                  Get_Code_Time_Exceeded (Ctx) = To_Actual (Val.Code_Time_Exceeded_Value)
                  and (Predecessor (Ctx, F_Checksum) = F_Code_Time_Exceeded
                       and Valid_Next (Ctx, F_Checksum)),
               when F_Code_Zero =>
                  (Predecessor (Ctx, F_Checksum) = F_Code_Zero
                   and Valid_Next (Ctx, F_Checksum)),
               when F_Checksum =>
                  Get_Checksum (Ctx) = To_Actual (Val.Checksum_Value)
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Redirect))
                       then
                          Predecessor (Ctx, F_Gateway_Internet_Address) = F_Checksum
                          and Valid_Next (Ctx, F_Gateway_Internet_Address))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Information_Request))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
                       then
                          Predecessor (Ctx, F_Identifier) = F_Checksum
                          and Valid_Next (Ctx, F_Identifier))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Parameter_Problem))
                       then
                          Predecessor (Ctx, F_Pointer) = F_Checksum
                          and Valid_Next (Ctx, F_Pointer))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Time_Exceeded))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Destination_Unreachable))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Source_Quench))
                       then
                          Predecessor (Ctx, F_Unused_32) = F_Checksum
                          and Valid_Next (Ctx, F_Unused_32)),
               when F_Gateway_Internet_Address =>
                  Get_Gateway_Internet_Address (Ctx) = To_Actual (Val.Gateway_Internet_Address_Value)
                  and (Predecessor (Ctx, F_Data) = F_Gateway_Internet_Address
                       and Valid_Next (Ctx, F_Data)),
               when F_Identifier =>
                  Get_Identifier (Ctx) = To_Actual (Val.Identifier_Value)
                  and (Predecessor (Ctx, F_Sequence_Number) = F_Identifier
                       and Valid_Next (Ctx, F_Sequence_Number)),
               when F_Pointer =>
                  Get_Pointer (Ctx) = To_Actual (Val.Pointer_Value)
                  and (Predecessor (Ctx, F_Unused_24) = F_Pointer
                       and Valid_Next (Ctx, F_Unused_24)),
               when F_Unused_32 =>
                  (Predecessor (Ctx, F_Data) = F_Unused_32
                   and Valid_Next (Ctx, F_Data)),
               when F_Sequence_Number =>
                  Get_Sequence_Number (Ctx) = To_Actual (Val.Sequence_Number_Value)
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Reply))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Echo_Request))
                       then
                          Predecessor (Ctx, F_Data) = F_Sequence_Number
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Msg))
                          or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.ICMP.Timestamp_Reply))
                       then
                          Predecessor (Ctx, F_Originate_Timestamp) = F_Sequence_Number
                          and Valid_Next (Ctx, F_Originate_Timestamp))
                  and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Val.Fld)),
               when F_Unused_24 =>
                  (Predecessor (Ctx, F_Data) = F_Unused_24
                   and Valid_Next (Ctx, F_Data)),
               when F_Originate_Timestamp =>
                  Get_Originate_Timestamp (Ctx) = To_Actual (Val.Originate_Timestamp_Value)
                  and (Predecessor (Ctx, F_Receive_Timestamp) = F_Originate_Timestamp
                       and Valid_Next (Ctx, F_Receive_Timestamp)),
               when F_Data =>
                  (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Val.Fld)),
               when F_Receive_Timestamp =>
                  Get_Receive_Timestamp (Ctx) = To_Actual (Val.Receive_Timestamp_Value)
                  and (Predecessor (Ctx, F_Transmit_Timestamp) = F_Receive_Timestamp
                       and Valid_Next (Ctx, F_Transmit_Timestamp)),
               when F_Transmit_Timestamp =>
                  Get_Transmit_Timestamp (Ctx) = To_Actual (Val.Transmit_Timestamp_Value)
                  and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Val.Fld)),
               when F_Final =>
                  True)
       and (for all F in Field =>
               (if F < Val.Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
   is
      First : RFLX_Types.Bit_Index;
      Last : RFLX_Types.Bit_Length;
   begin
      Reset_Dependent_Fields (Ctx, Val.Fld);
      First := Field_First (Ctx, Val.Fld);
      Last := Field_First (Ctx, Val.Fld) + Size - 1;
      Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Buffer_First := RFLX_Types.To_Index (First);
      Buffer_Last := RFLX_Types.To_Index (Last);
      pragma Assert ((((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size, Written_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      if State_Valid then
         Ctx.Cursors (Val.Fld) := (State => S_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Val.Fld).Predecessor);
      else
         Ctx.Cursors (Val.Fld) := (State => S_Structural_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Val.Fld).Predecessor);
      end if;
      Ctx.Cursors (Successor (Ctx, Val.Fld)) := (State => S_Invalid, Predecessor => Val.Fld);
   end Set;

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.ICMP.Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Tag, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Tag_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Tag), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Tag_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Tag;

   procedure Set_Code_Destination_Unreachable (Ctx : in out Context; Val : RFLX.ICMP.Code_Destination_Unreachable) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Destination_Unreachable, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Destination_Unreachable_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Code_Destination_Unreachable), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Code_Destination_Unreachable_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Code_Destination_Unreachable;

   procedure Set_Code_Redirect (Ctx : in out Context; Val : RFLX.ICMP.Code_Redirect) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Redirect, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Redirect_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Code_Redirect), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Code_Redirect_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Code_Redirect;

   procedure Set_Code_Time_Exceeded (Ctx : in out Context; Val : RFLX.ICMP.Code_Time_Exceeded) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Time_Exceeded, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Time_Exceeded_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Code_Time_Exceeded), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Code_Time_Exceeded_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Code_Time_Exceeded;

   procedure Set_Code_Zero (Ctx : in out Context; Val : RFLX.ICMP.Code_Zero) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Zero, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Zero_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Code_Zero), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Code_Zero_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Code_Zero;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.ICMP.Checksum) is
      Field_Value : constant Field_Dependent_Value := (F_Checksum, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Checksum);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Checksum), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Checksum_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Checksum;

   procedure Set_Gateway_Internet_Address (Ctx : in out Context; Val : RFLX.ICMP.Gateway_Internet_Address) is
      Field_Value : constant Field_Dependent_Value := (F_Gateway_Internet_Address, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Gateway_Internet_Address);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Gateway_Internet_Address), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Gateway_Internet_Address_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Gateway_Internet_Address;

   procedure Set_Identifier (Ctx : in out Context; Val : RFLX.ICMP.Identifier) is
      Field_Value : constant Field_Dependent_Value := (F_Identifier, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Identifier);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Identifier), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Identifier_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Identifier;

   procedure Set_Pointer (Ctx : in out Context; Val : RFLX.ICMP.Pointer) is
      Field_Value : constant Field_Dependent_Value := (F_Pointer, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Pointer);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Pointer), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Pointer_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Pointer;

   procedure Set_Unused_32 (Ctx : in out Context; Val : RFLX.ICMP.Unused_32) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_32, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Unused_32_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Unused_32), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Unused_32_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Unused_32;

   procedure Set_Sequence_Number (Ctx : in out Context; Val : RFLX.ICMP.Sequence_Number) is
      Field_Value : constant Field_Dependent_Value := (F_Sequence_Number, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Sequence_Number);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Sequence_Number), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Sequence_Number_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Sequence_Number;

   procedure Set_Unused_24 (Ctx : in out Context; Val : RFLX.ICMP.Unused_24) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_24, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Unused_24_Base);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Unused_24), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Unused_24_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Unused_24;

   procedure Set_Originate_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Originate_Timestamp, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Timestamp);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Originate_Timestamp), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Originate_Timestamp_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Originate_Timestamp;

   procedure Set_Receive_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Receive_Timestamp, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Timestamp);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Receive_Timestamp), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Receive_Timestamp_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Receive_Timestamp;

   procedure Set_Transmit_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Transmit_Timestamp, To_Base (Val));
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Timestamp);
   begin
      Set (Ctx, Field_Value, Field_Size (Ctx, F_Transmit_Timestamp), True, Buffer_First, Buffer_Last, Offset);
      Insert (Field_Value.Transmit_Timestamp_Value, Ctx.Buffer, Buffer_First, Buffer_Last, Offset, RFLX_Types.High_Order_First);
   end Set_Transmit_Timestamp;

   procedure Set_Data_Empty (Ctx : in out Context) is
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, (Fld => F_Data), 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Data_Empty;

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
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
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

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Data_Private (Ctx, Data'Length);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Data;

   procedure Generic_Set_Data (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Data_Private (Ctx, Length);
   end Generic_Set_Data;

end RFLX.ICMP.Message;
