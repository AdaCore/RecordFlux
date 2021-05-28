pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.ICMP.Message with
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
      Ctx.Cursors := (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final));
      Ctx.Message_Last := Ctx.First - 1;
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
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Destination_Unreachable))
              then
                 F_Code_Destination_Unreachable
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Redirect))
              then
                 F_Code_Redirect
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Time_Exceeded))
              then
                 F_Code_Time_Exceeded
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Parameter_Problem))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Source_Quench))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Request))
              then
                 F_Code_Zero
              else
                 F_Initial),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             F_Checksum,
          when F_Checksum =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Redirect))
              then
                 F_Gateway_Internet_Address
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Reply))
              then
                 F_Identifier
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Parameter_Problem))
              then
                 F_Pointer
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Time_Exceeded))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Destination_Unreachable))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Source_Quench))
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
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Reply))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Echo_Request))
              then
                 F_Data
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Request))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Information_Reply))
              then
                 F_Final
              elsif
                 RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Msg))
                 or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (ICMP.Timestamp_Reply))
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
      and Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld))
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
       and (case Fld is
               when F_Tag =>
                  Invalid (Ctx, F_Tag)
                  and Invalid (Ctx, F_Code_Destination_Unreachable)
                  and Invalid (Ctx, F_Code_Redirect)
                  and Invalid (Ctx, F_Code_Time_Exceeded)
                  and Invalid (Ctx, F_Code_Zero)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Code_Destination_Unreachable =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Invalid (Ctx, F_Code_Destination_Unreachable)
                  and Invalid (Ctx, F_Code_Redirect)
                  and Invalid (Ctx, F_Code_Time_Exceeded)
                  and Invalid (Ctx, F_Code_Zero)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Code_Redirect =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Invalid (Ctx, F_Code_Redirect)
                  and Invalid (Ctx, F_Code_Time_Exceeded)
                  and Invalid (Ctx, F_Code_Zero)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Code_Time_Exceeded =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Invalid (Ctx, F_Code_Time_Exceeded)
                  and Invalid (Ctx, F_Code_Zero)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Code_Zero =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Invalid (Ctx, F_Code_Zero)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Checksum =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Gateway_Internet_Address =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Invalid (Ctx, F_Gateway_Internet_Address)
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Identifier =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Invalid (Ctx, F_Identifier)
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Pointer =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Invalid (Ctx, F_Pointer)
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Unused_32 =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Invalid (Ctx, F_Unused_32)
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Sequence_Number =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Invalid (Ctx, F_Sequence_Number)
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Unused_24 =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Ctx.Cursors (F_Sequence_Number) = Ctx.Cursors (F_Sequence_Number)'Old
                  and Invalid (Ctx, F_Unused_24)
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Originate_Timestamp =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Ctx.Cursors (F_Sequence_Number) = Ctx.Cursors (F_Sequence_Number)'Old
                  and Ctx.Cursors (F_Unused_24) = Ctx.Cursors (F_Unused_24)'Old
                  and Invalid (Ctx, F_Originate_Timestamp)
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Data =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Ctx.Cursors (F_Sequence_Number) = Ctx.Cursors (F_Sequence_Number)'Old
                  and Ctx.Cursors (F_Unused_24) = Ctx.Cursors (F_Unused_24)'Old
                  and Ctx.Cursors (F_Originate_Timestamp) = Ctx.Cursors (F_Originate_Timestamp)'Old
                  and Invalid (Ctx, F_Data)
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Receive_Timestamp =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Ctx.Cursors (F_Sequence_Number) = Ctx.Cursors (F_Sequence_Number)'Old
                  and Ctx.Cursors (F_Unused_24) = Ctx.Cursors (F_Unused_24)'Old
                  and Ctx.Cursors (F_Originate_Timestamp) = Ctx.Cursors (F_Originate_Timestamp)'Old
                  and Ctx.Cursors (F_Data) = Ctx.Cursors (F_Data)'Old
                  and Invalid (Ctx, F_Receive_Timestamp)
                  and Invalid (Ctx, F_Transmit_Timestamp),
               when F_Transmit_Timestamp =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                  and Ctx.Cursors (F_Code_Destination_Unreachable) = Ctx.Cursors (F_Code_Destination_Unreachable)'Old
                  and Ctx.Cursors (F_Code_Redirect) = Ctx.Cursors (F_Code_Redirect)'Old
                  and Ctx.Cursors (F_Code_Time_Exceeded) = Ctx.Cursors (F_Code_Time_Exceeded)'Old
                  and Ctx.Cursors (F_Code_Zero) = Ctx.Cursors (F_Code_Zero)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
                  and Ctx.Cursors (F_Gateway_Internet_Address) = Ctx.Cursors (F_Gateway_Internet_Address)'Old
                  and Ctx.Cursors (F_Identifier) = Ctx.Cursors (F_Identifier)'Old
                  and Ctx.Cursors (F_Pointer) = Ctx.Cursors (F_Pointer)'Old
                  and Ctx.Cursors (F_Unused_32) = Ctx.Cursors (F_Unused_32)'Old
                  and Ctx.Cursors (F_Sequence_Number) = Ctx.Cursors (F_Sequence_Number)'Old
                  and Ctx.Cursors (F_Unused_24) = Ctx.Cursors (F_Unused_24)'Old
                  and Ctx.Cursors (F_Originate_Timestamp) = Ctx.Cursors (F_Originate_Timestamp)'Old
                  and Ctx.Cursors (F_Data) = Ctx.Cursors (F_Data)'Old
                  and Ctx.Cursors (F_Receive_Timestamp) = Ctx.Cursors (F_Receive_Timestamp)'Old
                  and Invalid (Ctx, F_Transmit_Timestamp))
   is
      First : constant RFLX_Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      case Fld is
         when F_Tag =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Zero) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Time_Exceeded) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Redirect) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Destination_Unreachable) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, Ctx.Cursors (F_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Code_Destination_Unreachable =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Zero) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Time_Exceeded) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Redirect) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Destination_Unreachable) := (S_Invalid, Ctx.Cursors (F_Code_Destination_Unreachable).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Code_Redirect =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Zero) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Time_Exceeded) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Redirect) := (S_Invalid, Ctx.Cursors (F_Code_Redirect).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Code_Time_Exceeded =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Zero) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Time_Exceeded) := (S_Invalid, Ctx.Cursors (F_Code_Time_Exceeded).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Code_Zero =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code_Zero) := (S_Invalid, Ctx.Cursors (F_Code_Zero).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Checksum =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, Ctx.Cursors (F_Checksum).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Gateway_Internet_Address =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Gateway_Internet_Address) := (S_Invalid, Ctx.Cursors (F_Gateway_Internet_Address).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Identifier =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Identifier) := (S_Invalid, Ctx.Cursors (F_Identifier).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Pointer =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Pointer) := (S_Invalid, Ctx.Cursors (F_Pointer).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Unused_32 =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_32) := (S_Invalid, Ctx.Cursors (F_Unused_32).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Sequence_Number =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Sequence_Number) := (S_Invalid, Ctx.Cursors (F_Sequence_Number).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Unused_24 =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_24) := (S_Invalid, Ctx.Cursors (F_Unused_24).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Originate_Timestamp =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Originate_Timestamp) := (S_Invalid, Ctx.Cursors (F_Originate_Timestamp).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Data =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Data) := (S_Invalid, Ctx.Cursors (F_Data).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Receive_Timestamp =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Receive_Timestamp) := (S_Invalid, Ctx.Cursors (F_Receive_Timestamp).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Transmit_Timestamp =>
            Ctx.Cursors (F_Transmit_Timestamp) := (S_Invalid, Ctx.Cursors (F_Transmit_Timestamp).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Tag | F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero | F_Checksum | F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 | F_Sequence_Number | F_Unused_24 | F_Originate_Timestamp =>
             False,
          when F_Data =>
             True,
          when F_Receive_Timestamp | F_Transmit_Timestamp =>
             False));

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
                     (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Code_Destination_Unreachable =>
                     (Fld => F_Code_Destination_Unreachable, Code_Destination_Unreachable_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Code_Redirect =>
                     (Fld => F_Code_Redirect, Code_Redirect_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Code_Time_Exceeded =>
                     (Fld => F_Code_Time_Exceeded, Code_Time_Exceeded_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Code_Zero =>
                     (Fld => F_Code_Zero, Code_Zero_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Checksum =>
                     (Fld => F_Checksum, Checksum_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Gateway_Internet_Address =>
                     (Fld => F_Gateway_Internet_Address, Gateway_Internet_Address_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Identifier =>
                     (Fld => F_Identifier, Identifier_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Pointer =>
                     (Fld => F_Pointer, Pointer_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Unused_32 =>
                     (Fld => F_Unused_32, Unused_32_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Sequence_Number =>
                     (Fld => F_Sequence_Number, Sequence_Number_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Unused_24 =>
                     (Fld => F_Unused_24, Unused_24_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Originate_Timestamp =>
                     (Fld => F_Originate_Timestamp, Originate_Timestamp_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Data =>
                     (Fld => F_Data),
                  when F_Receive_Timestamp =>
                     (Fld => F_Receive_Timestamp, Receive_Timestamp_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Transmit_Timestamp =>
                     (Fld => F_Transmit_Timestamp, Transmit_Timestamp_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
               case Fld is
                  when F_Tag =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Code_Destination_Unreachable =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Code_Redirect =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Code_Time_Exceeded =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Code_Zero =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Checksum =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Gateway_Internet_Address =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Identifier =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Pointer =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Unused_32 =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Sequence_Number =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Unused_24 =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Originate_Timestamp =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Data =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Receive_Timestamp =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
                  when F_Transmit_Timestamp =>
                     Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               end case;
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Code_Destination_Unreachable then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Code_Redirect then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Code_Time_Exceeded then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Code_Zero then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Checksum then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Gateway_Internet_Address then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Identifier then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Pointer then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Unused_32 then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Sequence_Number then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Unused_24 then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Originate_Timestamp then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Data then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Receive_Timestamp then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Transmit_Timestamp then
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
      Verify (Ctx, F_Code_Destination_Unreachable);
      Verify (Ctx, F_Code_Redirect);
      Verify (Ctx, F_Code_Time_Exceeded);
      Verify (Ctx, F_Code_Zero);
      Verify (Ctx, F_Checksum);
      Verify (Ctx, F_Gateway_Internet_Address);
      Verify (Ctx, F_Identifier);
      Verify (Ctx, F_Pointer);
      Verify (Ctx, F_Unused_32);
      Verify (Ctx, F_Sequence_Number);
      Verify (Ctx, F_Unused_24);
      Verify (Ctx, F_Originate_Timestamp);
      Verify (Ctx, F_Data);
      Verify (Ctx, F_Receive_Timestamp);
      Verify (Ctx, F_Transmit_Timestamp);
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
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Tag_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Destination_Unreachable_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Redirect_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Time_Exceeded_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Code_Zero_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Checksum);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Gateway_Internet_Address);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Identifier);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Pointer);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Unused_32_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Sequence_Number);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Unused_24_Base);
      procedure Insert is new RFLX_Types.Insert (RFLX.ICMP.Timestamp);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Tag =>
            Insert (Val.Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Code_Destination_Unreachable =>
            Insert (Val.Code_Destination_Unreachable_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Code_Redirect =>
            Insert (Val.Code_Redirect_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Code_Time_Exceeded =>
            Insert (Val.Code_Time_Exceeded_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Code_Zero =>
            Insert (Val.Code_Zero_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Checksum =>
            Insert (Val.Checksum_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Gateway_Internet_Address =>
            Insert (Val.Gateway_Internet_Address_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Identifier =>
            Insert (Val.Identifier_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Pointer =>
            Insert (Val.Pointer_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Unused_32 =>
            Insert (Val.Unused_32_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Sequence_Number =>
            Insert (Val.Sequence_Number_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Unused_24 =>
            Insert (Val.Unused_24_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Originate_Timestamp =>
            Insert (Val.Originate_Timestamp_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Data =>
            null;
         when F_Receive_Timestamp =>
            Insert (Val.Receive_Timestamp_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Transmit_Timestamp =>
            Insert (Val.Transmit_Timestamp_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.ICMP.Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Tag, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Tag)) := (State => S_Invalid, Predecessor => F_Tag);
   end Set_Tag;

   procedure Set_Code_Destination_Unreachable (Ctx : in out Context; Val : RFLX.ICMP.Code_Destination_Unreachable) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Destination_Unreachable, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Destination_Unreachable);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Code_Destination_Unreachable) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Destination_Unreachable).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Destination_Unreachable)) := (State => S_Invalid, Predecessor => F_Code_Destination_Unreachable);
   end Set_Code_Destination_Unreachable;

   procedure Set_Code_Redirect (Ctx : in out Context; Val : RFLX.ICMP.Code_Redirect) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Redirect, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Redirect);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Code_Redirect) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Redirect).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Redirect)) := (State => S_Invalid, Predecessor => F_Code_Redirect);
   end Set_Code_Redirect;

   procedure Set_Code_Time_Exceeded (Ctx : in out Context; Val : RFLX.ICMP.Code_Time_Exceeded) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Time_Exceeded, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Time_Exceeded);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Code_Time_Exceeded) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Time_Exceeded).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Time_Exceeded)) := (State => S_Invalid, Predecessor => F_Code_Time_Exceeded);
   end Set_Code_Time_Exceeded;

   procedure Set_Code_Zero (Ctx : in out Context; Val : RFLX.ICMP.Code_Zero) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Zero, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Zero);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Code_Zero) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Zero).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Zero)) := (State => S_Invalid, Predecessor => F_Code_Zero);
   end Set_Code_Zero;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.ICMP.Checksum) is
      Field_Value : constant Field_Dependent_Value := (F_Checksum, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Checksum);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Checksum) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Checksum).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Checksum)) := (State => S_Invalid, Predecessor => F_Checksum);
   end Set_Checksum;

   procedure Set_Gateway_Internet_Address (Ctx : in out Context; Val : RFLX.ICMP.Gateway_Internet_Address) is
      Field_Value : constant Field_Dependent_Value := (F_Gateway_Internet_Address, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Gateway_Internet_Address);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Gateway_Internet_Address) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Gateway_Internet_Address).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Gateway_Internet_Address)) := (State => S_Invalid, Predecessor => F_Gateway_Internet_Address);
   end Set_Gateway_Internet_Address;

   procedure Set_Identifier (Ctx : in out Context; Val : RFLX.ICMP.Identifier) is
      Field_Value : constant Field_Dependent_Value := (F_Identifier, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Identifier);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Identifier) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Identifier).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Identifier)) := (State => S_Invalid, Predecessor => F_Identifier);
   end Set_Identifier;

   procedure Set_Pointer (Ctx : in out Context; Val : RFLX.ICMP.Pointer) is
      Field_Value : constant Field_Dependent_Value := (F_Pointer, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Pointer);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Pointer) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Pointer).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Pointer)) := (State => S_Invalid, Predecessor => F_Pointer);
   end Set_Pointer;

   procedure Set_Unused_32 (Ctx : in out Context; Val : RFLX.ICMP.Unused_32) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_32, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Unused_32);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Unused_32) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Unused_32).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Unused_32)) := (State => S_Invalid, Predecessor => F_Unused_32);
   end Set_Unused_32;

   procedure Set_Sequence_Number (Ctx : in out Context; Val : RFLX.ICMP.Sequence_Number) is
      Field_Value : constant Field_Dependent_Value := (F_Sequence_Number, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Sequence_Number);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Sequence_Number) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Sequence_Number).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Sequence_Number)) := (State => S_Invalid, Predecessor => F_Sequence_Number);
   end Set_Sequence_Number;

   procedure Set_Unused_24 (Ctx : in out Context; Val : RFLX.ICMP.Unused_24) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_24, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Unused_24);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Unused_24) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Unused_24).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Unused_24)) := (State => S_Invalid, Predecessor => F_Unused_24);
   end Set_Unused_24;

   procedure Set_Originate_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Originate_Timestamp, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Originate_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Originate_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Originate_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Originate_Timestamp)) := (State => S_Invalid, Predecessor => F_Originate_Timestamp);
   end Set_Originate_Timestamp;

   procedure Set_Receive_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Receive_Timestamp, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Receive_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Receive_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Receive_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Receive_Timestamp)) := (State => S_Invalid, Predecessor => F_Receive_Timestamp);
   end Set_Receive_Timestamp;

   procedure Set_Transmit_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Transmit_Timestamp, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Transmit_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Transmit_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Transmit_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Transmit_Timestamp)) := (State => S_Invalid, Predecessor => F_Transmit_Timestamp);
   end Set_Transmit_Timestamp;

   procedure Set_Data_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Data);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Data) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Set_Data_Empty;

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

end RFLX.ICMP.Message;
