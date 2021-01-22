pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.ICMP.Generic_Message with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, First, Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Tag)
      and then Available_Space (Ctx, F_Tag) = Ctx.Last - Ctx.First + 1
      and then Invalid (Ctx, F_Tag)
      and then Invalid (Ctx, F_Code_Destination_Unreachable)
      and then Invalid (Ctx, F_Code_Redirect)
      and then Invalid (Ctx, F_Code_Time_Exceeded)
      and then Invalid (Ctx, F_Code_Zero)
      and then Invalid (Ctx, F_Checksum)
      and then Invalid (Ctx, F_Gateway_Internet_Address)
      and then Invalid (Ctx, F_Identifier)
      and then Invalid (Ctx, F_Pointer)
      and then Invalid (Ctx, F_Unused_32)
      and then Invalid (Ctx, F_Sequence_Number)
      and then Invalid (Ctx, F_Unused_24)
      and then Invalid (Ctx, F_Originate_Timestamp)
      and then Invalid (Ctx, F_Data)
      and then Invalid (Ctx, F_Receive_Timestamp)
      and then Invalid (Ctx, F_Transmit_Timestamp));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     (Ctx.Message_Last);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    True,
                 when others =>
                    False),
          when F_Tag =>
             (case Fld is
                 when F_Code_Destination_Unreachable =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable)),
                 when F_Code_Redirect =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect)),
                 when F_Code_Time_Exceeded =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded)),
                 when F_Code_Zero =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)),
                 when others =>
                    False),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (case Fld is
                 when F_Checksum =>
                    True,
                 when others =>
                    False),
          when F_Checksum =>
             (case Fld is
                 when F_Gateway_Internet_Address =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect)),
                 when F_Identifier =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)),
                 when F_Pointer =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem)),
                 when F_Unused_32 =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)),
                 when others =>
                    False),
          when F_Gateway_Internet_Address =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Identifier =>
             (case Fld is
                 when F_Sequence_Number =>
                    True,
                 when others =>
                    False),
          when F_Pointer =>
             (case Fld is
                 when F_Unused_24 =>
                    True,
                 when others =>
                    False),
          when F_Unused_32 =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Sequence_Number =>
             (case Fld is
                 when F_Data =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)),
                 when F_Originate_Timestamp =>
                    Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)),
                 when others =>
                    False),
          when F_Unused_24 =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Originate_Timestamp =>
             (case Fld is
                 when F_Receive_Timestamp =>
                    True,
                 when others =>
                    False),
          when F_Data =>
             False,
          when F_Receive_Timestamp =>
             (case Fld is
                 when F_Transmit_Timestamp =>
                    True,
                 when others =>
                    False),
          when F_Transmit_Timestamp | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial =>
             True,
          when F_Tag =>
             Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Redirect))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Information_Reply))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Information_Request))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Source_Quench))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
             or Types.U64 (Val.Tag_Value) = Types.U64 (To_Base (Echo_Request)),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             True,
          when F_Checksum =>
             Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             True,
          when F_Sequence_Number =>
             Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)),
          when F_Unused_24 | F_Originate_Timestamp | F_Data | F_Receive_Timestamp | F_Transmit_Timestamp =>
             True,
          when F_Final =>
             False));

   function Field_Size (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    RFLX.ICMP.Tag_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Tag =>
             (case Fld is
                 when F_Code_Destination_Unreachable =>
                    RFLX.ICMP.Code_Destination_Unreachable_Base'Size,
                 when F_Code_Redirect =>
                    RFLX.ICMP.Code_Redirect_Base'Size,
                 when F_Code_Time_Exceeded =>
                    RFLX.ICMP.Code_Time_Exceeded_Base'Size,
                 when F_Code_Zero =>
                    RFLX.ICMP.Code_Zero_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (case Fld is
                 when F_Checksum =>
                    RFLX.ICMP.Checksum'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Checksum =>
             (case Fld is
                 when F_Gateway_Internet_Address =>
                    RFLX.ICMP.Gateway_Internet_Address'Size,
                 when F_Identifier =>
                    RFLX.ICMP.Identifier'Size,
                 when F_Pointer =>
                    RFLX.ICMP.Pointer'Size,
                 when F_Unused_32 =>
                    RFLX.ICMP.Unused_32_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Gateway_Internet_Address =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Identifier =>
             (case Fld is
                 when F_Sequence_Number =>
                    RFLX.ICMP.Sequence_Number'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Pointer =>
             (case Fld is
                 when F_Unused_24 =>
                    RFLX.ICMP.Unused_24_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Unused_32 =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Sequence_Number =>
             (case Fld is
                 when F_Data =>
                    Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last),
                 when F_Originate_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Unused_24 =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Originate_Timestamp =>
             (case Fld is
                 when F_Receive_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Data =>
             0,
          when F_Receive_Timestamp =>
             (case Fld is
                 when F_Transmit_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Transmit_Timestamp | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
          when F_Tag =>
             Ctx.First,
          when F_Code_Destination_Unreachable =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Code_Redirect =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Code_Time_Exceeded =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Code_Zero =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Checksum =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Code_Destination_Unreachable
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Redirect
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Time_Exceeded
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Zero
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Gateway_Internet_Address =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Identifier =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Pointer =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Unused_32 =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Sequence_Number =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Identifier
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Unused_24 =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Pointer
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Originate_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number
                 and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Data =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Gateway_Internet_Address
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number
                 and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                           or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Unused_24
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Unused_32
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Receive_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Originate_Timestamp
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length),
          when F_Transmit_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Receive_Timestamp
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 Types.Unreachable_Bit_Length)));

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index is
     (Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
          when F_Initial =>
             F_Initial,
          when others =>
             Ctx.Cursors (Fld).Predecessor));

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Tag =>
             (if
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
              then
                 F_Code_Destination_Unreachable
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
              then
                 F_Code_Redirect
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
              then
                 F_Code_Time_Exceeded
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
              then
                 F_Code_Zero
              else
                 F_Initial),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             F_Checksum,
          when F_Checksum =>
             (if
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
              then
                 F_Gateway_Internet_Address
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
              then
                 F_Identifier
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
              then
                 F_Pointer
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
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
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
              then
                 F_Data
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
              then
                 F_Final
              elsif
                 Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                 or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
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

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
          when F_Initial =>
             True,
          when F_Tag =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (Valid (Ctx.Cursors (F_Tag))
              and Ctx.Cursors (Fld).Predecessor = F_Tag),
          when F_Checksum =>
             (Valid (Ctx.Cursors (F_Code_Destination_Unreachable))
              and Ctx.Cursors (Fld).Predecessor = F_Code_Destination_Unreachable)
             or (Valid (Ctx.Cursors (F_Code_Redirect))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Redirect)
             or (Valid (Ctx.Cursors (F_Code_Time_Exceeded))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Time_Exceeded)
             or (Valid (Ctx.Cursors (F_Code_Zero))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Zero),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             (Valid (Ctx.Cursors (F_Checksum))
              and Ctx.Cursors (Fld).Predecessor = F_Checksum),
          when F_Sequence_Number =>
             (Valid (Ctx.Cursors (F_Identifier))
              and Ctx.Cursors (Fld).Predecessor = F_Identifier),
          when F_Unused_24 =>
             (Valid (Ctx.Cursors (F_Pointer))
              and Ctx.Cursors (Fld).Predecessor = F_Pointer),
          when F_Originate_Timestamp =>
             (Valid (Ctx.Cursors (F_Sequence_Number))
              and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number),
          when F_Data =>
             (Valid (Ctx.Cursors (F_Gateway_Internet_Address))
              and Ctx.Cursors (Fld).Predecessor = F_Gateway_Internet_Address)
             or (Valid (Ctx.Cursors (F_Sequence_Number))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number)
             or (Valid (Ctx.Cursors (F_Unused_24))
                 and Ctx.Cursors (Fld).Predecessor = F_Unused_24)
             or (Valid (Ctx.Cursors (F_Unused_32))
                 and Ctx.Cursors (Fld).Predecessor = F_Unused_32),
          when F_Receive_Timestamp =>
             (Valid (Ctx.Cursors (F_Originate_Timestamp))
              and Ctx.Cursors (Fld).Predecessor = F_Originate_Timestamp),
          when F_Transmit_Timestamp =>
             (Valid (Ctx.Cursors (F_Receive_Timestamp))
              and Ctx.Cursors (Fld).Predecessor = F_Receive_Timestamp),
          when F_Final =>
             (Structural_Valid (Ctx.Cursors (F_Data))
              and Ctx.Cursors (Fld).Predecessor = F_Data)
             or (Valid (Ctx.Cursors (F_Sequence_Number))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number)
             or (Valid (Ctx.Cursors (F_Transmit_Timestamp))
                 and Ctx.Cursors (Fld).Predecessor = F_Transmit_Timestamp)));

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

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Predecessor (Ctx, Fld)
      and then Path_Condition (Ctx, Fld));

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length is
     (Ctx.Last - Field_First (Ctx, Fld) + 1);

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_Size (Ctx, Fld) >= 0
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Data =>
                      Ctx.Buffer.all (Types.Byte_Index (Field_First (Ctx, Fld)) .. Types.Byte_Index (Field_Last (Ctx, Fld))) = Data,
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
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant Types.Bit_Length := Field_Size (Ctx, Fld) with
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
      First : constant Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant Types.Bit_Index := Field_Last (Ctx, Fld);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
      function Offset return Types.Offset is
        (Types.Offset ((8 - Last mod 8) mod 8));
      function Extract is new Types.Extract (RFLX.ICMP.Tag_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Code_Destination_Unreachable_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Code_Redirect_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Code_Time_Exceeded_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Code_Zero_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Checksum);
      function Extract is new Types.Extract (RFLX.ICMP.Gateway_Internet_Address);
      function Extract is new Types.Extract (RFLX.ICMP.Identifier);
      function Extract is new Types.Extract (RFLX.ICMP.Pointer);
      function Extract is new Types.Extract (RFLX.ICMP.Unused_32_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Sequence_Number);
      function Extract is new Types.Extract (RFLX.ICMP.Unused_24_Base);
      function Extract is new Types.Extract (RFLX.ICMP.Timestamp);
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
               Ctx.Message_Last := Field_Last (Ctx, Fld);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if
                                  Structural_Valid (Ctx.Cursors (F_Tag))
                               then
                                  Ctx.Cursors (F_Tag).Last - Ctx.Cursors (F_Tag).First + 1 = RFLX.ICMP.Tag_Base'Size
                                  and then Ctx.Cursors (F_Tag).Predecessor = F_Initial
                                  and then Ctx.Cursors (F_Tag).First = Ctx.First
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Code_Destination_Unreachable))
                                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            then
                                               Ctx.Cursors (F_Code_Destination_Unreachable).Last - Ctx.Cursors (F_Code_Destination_Unreachable).First + 1 = RFLX.ICMP.Code_Destination_Unreachable_Base'Size
                                               and then Ctx.Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                                               and then Ctx.Cursors (F_Code_Destination_Unreachable).First = Ctx.Cursors (F_Tag).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Checksum))
                                                         then
                                                            Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                            and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable
                                                            and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Destination_Unreachable).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                                      then
                                                                         Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                                      then
                                                                         Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                         and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                                   then
                                                                                      Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                                      and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                                      and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                                and then (if
                                                                                                                             Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                          then
                                                                                                                             Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                                      then
                                                                         Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                         and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                                   then
                                                                                      Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                                      and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                                      and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                                      then
                                                                         Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                         and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Code_Redirect))
                                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                            then
                                               Ctx.Cursors (F_Code_Redirect).Last - Ctx.Cursors (F_Code_Redirect).First + 1 = RFLX.ICMP.Code_Redirect_Base'Size
                                               and then Ctx.Cursors (F_Code_Redirect).Predecessor = F_Tag
                                               and then Ctx.Cursors (F_Code_Redirect).First = Ctx.Cursors (F_Tag).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Checksum))
                                                         then
                                                            Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                            and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Redirect
                                                            and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Redirect).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                                      then
                                                                         Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                                      then
                                                                         Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                         and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                                   then
                                                                                      Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                                      and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                                      and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                                and then (if
                                                                                                                             Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                          then
                                                                                                                             Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                                      then
                                                                         Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                         and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                                   then
                                                                                      Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                                      and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                                      and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                                      then
                                                                         Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                         and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Code_Time_Exceeded))
                                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            then
                                               Ctx.Cursors (F_Code_Time_Exceeded).Last - Ctx.Cursors (F_Code_Time_Exceeded).First + 1 = RFLX.ICMP.Code_Time_Exceeded_Base'Size
                                               and then Ctx.Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                                               and then Ctx.Cursors (F_Code_Time_Exceeded).First = Ctx.Cursors (F_Tag).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Checksum))
                                                         then
                                                            Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                            and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded
                                                            and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Time_Exceeded).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                                      then
                                                                         Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                                      then
                                                                         Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                         and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                                   then
                                                                                      Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                                      and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                                      and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                                and then (if
                                                                                                                             Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                          then
                                                                                                                             Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                                      then
                                                                         Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                         and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                                   then
                                                                                      Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                                      and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                                      and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                                      then
                                                                         Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                         and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Code_Zero))
                                               and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                            then
                                               Ctx.Cursors (F_Code_Zero).Last - Ctx.Cursors (F_Code_Zero).First + 1 = RFLX.ICMP.Code_Zero_Base'Size
                                               and then Ctx.Cursors (F_Code_Zero).Predecessor = F_Tag
                                               and then Ctx.Cursors (F_Code_Zero).First = Ctx.Cursors (F_Tag).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Checksum))
                                                         then
                                                            Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                            and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Zero
                                                            and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Zero).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                                      then
                                                                         Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                                      then
                                                                         Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                         and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                                   then
                                                                                      Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                                      and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                                      and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                                   and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                             or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                                   and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                                and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                                and then (if
                                                                                                                             Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                          then
                                                                                                                             Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                             and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                         and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                                      then
                                                                         Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                         and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                                   then
                                                                                      Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                                      and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                                      and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Data))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                                   and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                                   and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                         and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                                   or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                                      then
                                                                         Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                         and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                         and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Data))
                                                                                   then
                                                                                      Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                      and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                                      and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))));
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

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Structural_Valid (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
       or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Invalid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Invalid
      or Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Code_Destination_Unreachable)
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                 and then Valid (Ctx, F_Checksum)
                 and then ((Valid (Ctx, F_Gateway_Internet_Address)
                            and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                            and then Structural_Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Identifier)
                               and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                               and then Valid (Ctx, F_Sequence_Number)
                               and then ((Structural_Valid (Ctx, F_Data)
                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                         or (Valid (Ctx, F_Originate_Timestamp)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                             and then Valid (Ctx, F_Receive_Timestamp)
                                             and then Valid (Ctx, F_Transmit_Timestamp))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                           or (Valid (Ctx, F_Pointer)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                               and then Valid (Ctx, F_Unused_24)
                               and then Structural_Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Unused_32)
                               and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                               and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Redirect)
                    and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Time_Exceeded)
                    and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Zero)
                    and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Code_Destination_Unreachable)
                 and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                 and then Valid (Ctx, F_Checksum)
                 and then ((Valid (Ctx, F_Gateway_Internet_Address)
                            and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                            and then Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Identifier)
                               and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                               and then Valid (Ctx, F_Sequence_Number)
                               and then ((Valid (Ctx, F_Data)
                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                         or (Valid (Ctx, F_Originate_Timestamp)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                             and then Valid (Ctx, F_Receive_Timestamp)
                                             and then Valid (Ctx, F_Transmit_Timestamp))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                           or (Valid (Ctx, F_Pointer)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                               and then Valid (Ctx, F_Unused_24)
                               and then Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Unused_32)
                               and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                         or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                               and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Redirect)
                    and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Time_Exceeded)
                    and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Zero)
                    and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                              or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                       or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                            or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Code_Destination_Unreachable)
      or Incomplete (Ctx, F_Code_Redirect)
      or Incomplete (Ctx, F_Code_Time_Exceeded)
      or Incomplete (Ctx, F_Code_Zero)
      or Incomplete (Ctx, F_Checksum)
      or Incomplete (Ctx, F_Gateway_Internet_Address)
      or Incomplete (Ctx, F_Identifier)
      or Incomplete (Ctx, F_Pointer)
      or Incomplete (Ctx, F_Unused_32)
      or Incomplete (Ctx, F_Sequence_Number)
      or Incomplete (Ctx, F_Unused_24)
      or Incomplete (Ctx, F_Originate_Timestamp)
      or Incomplete (Ctx, F_Data)
      or Incomplete (Ctx, F_Receive_Timestamp)
      or Incomplete (Ctx, F_Transmit_Timestamp));

   function Get_Tag (Ctx : Context) return RFLX.ICMP.Tag is
     (To_Actual (Ctx.Cursors (F_Tag).Value.Tag_Value));

   function Get_Code_Destination_Unreachable (Ctx : Context) return RFLX.ICMP.Code_Destination_Unreachable is
     (To_Actual (Ctx.Cursors (F_Code_Destination_Unreachable).Value.Code_Destination_Unreachable_Value));

   function Get_Code_Redirect (Ctx : Context) return RFLX.ICMP.Code_Redirect is
     (To_Actual (Ctx.Cursors (F_Code_Redirect).Value.Code_Redirect_Value));

   function Get_Code_Time_Exceeded (Ctx : Context) return RFLX.ICMP.Code_Time_Exceeded is
     (To_Actual (Ctx.Cursors (F_Code_Time_Exceeded).Value.Code_Time_Exceeded_Value));

   function Get_Code_Zero (Ctx : Context) return RFLX.ICMP.Code_Zero is
     (To_Actual (Ctx.Cursors (F_Code_Zero).Value.Code_Zero_Value));

   function Get_Checksum (Ctx : Context) return RFLX.ICMP.Checksum is
     (To_Actual (Ctx.Cursors (F_Checksum).Value.Checksum_Value));

   function Get_Gateway_Internet_Address (Ctx : Context) return RFLX.ICMP.Gateway_Internet_Address is
     (To_Actual (Ctx.Cursors (F_Gateway_Internet_Address).Value.Gateway_Internet_Address_Value));

   function Get_Identifier (Ctx : Context) return RFLX.ICMP.Identifier is
     (To_Actual (Ctx.Cursors (F_Identifier).Value.Identifier_Value));

   function Get_Pointer (Ctx : Context) return RFLX.ICMP.Pointer is
     (To_Actual (Ctx.Cursors (F_Pointer).Value.Pointer_Value));

   function Get_Unused_32 (Ctx : Context) return RFLX.ICMP.Unused_32 is
     (To_Actual (Ctx.Cursors (F_Unused_32).Value.Unused_32_Value));

   function Get_Sequence_Number (Ctx : Context) return RFLX.ICMP.Sequence_Number is
     (To_Actual (Ctx.Cursors (F_Sequence_Number).Value.Sequence_Number_Value));

   function Get_Unused_24 (Ctx : Context) return RFLX.ICMP.Unused_24 is
     (To_Actual (Ctx.Cursors (F_Unused_24).Value.Unused_24_Value));

   function Get_Originate_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Originate_Timestamp).Value.Originate_Timestamp_Value));

   function Get_Receive_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Receive_Timestamp).Value.Receive_Timestamp_Value));

   function Get_Transmit_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Transmit_Timestamp).Value.Transmit_Timestamp_Value));

   procedure Get_Data (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Data).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Data).Last);
   begin
      Process_Data (Ctx.Buffer.all (First .. Last));
   end Get_Data;

   procedure Set_Field_Value (Ctx : in out Context; Val : Field_Dependent_Value; Fst, Lst : out Types.Bit_Index) with
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
      First : constant Types.Bit_Index := Field_First (Ctx, Val.Fld);
      Last : constant Types.Bit_Index := Field_Last (Ctx, Val.Fld);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
      function Offset return Types.Offset is
        (Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new Types.Insert (RFLX.ICMP.Tag_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Code_Destination_Unreachable_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Code_Redirect_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Code_Time_Exceeded_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Code_Zero_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Checksum);
      procedure Insert is new Types.Insert (RFLX.ICMP.Gateway_Internet_Address);
      procedure Insert is new Types.Insert (RFLX.ICMP.Identifier);
      procedure Insert is new Types.Insert (RFLX.ICMP.Pointer);
      procedure Insert is new Types.Insert (RFLX.ICMP.Unused_32_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Sequence_Number);
      procedure Insert is new Types.Insert (RFLX.ICMP.Unused_24_Base);
      procedure Insert is new Types.Insert (RFLX.ICMP.Timestamp);
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
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Tag)) := (State => S_Invalid, Predecessor => F_Tag);
   end Set_Tag;

   procedure Set_Code_Destination_Unreachable (Ctx : in out Context; Val : RFLX.ICMP.Code_Destination_Unreachable) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Destination_Unreachable, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Destination_Unreachable);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Code_Destination_Unreachable) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Destination_Unreachable).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Destination_Unreachable)) := (State => S_Invalid, Predecessor => F_Code_Destination_Unreachable);
   end Set_Code_Destination_Unreachable;

   procedure Set_Code_Redirect (Ctx : in out Context; Val : RFLX.ICMP.Code_Redirect) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Redirect, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Redirect);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Code_Redirect) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Redirect).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Redirect)) := (State => S_Invalid, Predecessor => F_Code_Redirect);
   end Set_Code_Redirect;

   procedure Set_Code_Time_Exceeded (Ctx : in out Context; Val : RFLX.ICMP.Code_Time_Exceeded) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Time_Exceeded, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Time_Exceeded);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Code_Time_Exceeded) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Time_Exceeded).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Time_Exceeded)) := (State => S_Invalid, Predecessor => F_Code_Time_Exceeded);
   end Set_Code_Time_Exceeded;

   procedure Set_Code_Zero (Ctx : in out Context; Val : RFLX.ICMP.Code_Zero) is
      Field_Value : constant Field_Dependent_Value := (F_Code_Zero, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code_Zero);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Code_Zero) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code_Zero).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code_Zero)) := (State => S_Invalid, Predecessor => F_Code_Zero);
   end Set_Code_Zero;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.ICMP.Checksum) is
      Field_Value : constant Field_Dependent_Value := (F_Checksum, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Checksum);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Checksum) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Checksum).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Checksum)) := (State => S_Invalid, Predecessor => F_Checksum);
   end Set_Checksum;

   procedure Set_Gateway_Internet_Address (Ctx : in out Context; Val : RFLX.ICMP.Gateway_Internet_Address) is
      Field_Value : constant Field_Dependent_Value := (F_Gateway_Internet_Address, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Gateway_Internet_Address);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Gateway_Internet_Address) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Gateway_Internet_Address).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Gateway_Internet_Address)) := (State => S_Invalid, Predecessor => F_Gateway_Internet_Address);
   end Set_Gateway_Internet_Address;

   procedure Set_Identifier (Ctx : in out Context; Val : RFLX.ICMP.Identifier) is
      Field_Value : constant Field_Dependent_Value := (F_Identifier, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Identifier);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Identifier) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Identifier).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Identifier)) := (State => S_Invalid, Predecessor => F_Identifier);
   end Set_Identifier;

   procedure Set_Pointer (Ctx : in out Context; Val : RFLX.ICMP.Pointer) is
      Field_Value : constant Field_Dependent_Value := (F_Pointer, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Pointer);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Pointer) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Pointer).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Pointer)) := (State => S_Invalid, Predecessor => F_Pointer);
   end Set_Pointer;

   procedure Set_Unused_32 (Ctx : in out Context; Val : RFLX.ICMP.Unused_32) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_32, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Unused_32);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Unused_32) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Unused_32).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Unused_32)) := (State => S_Invalid, Predecessor => F_Unused_32);
   end Set_Unused_32;

   procedure Set_Sequence_Number (Ctx : in out Context; Val : RFLX.ICMP.Sequence_Number) is
      Field_Value : constant Field_Dependent_Value := (F_Sequence_Number, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Sequence_Number);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Sequence_Number) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Sequence_Number).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Sequence_Number)) := (State => S_Invalid, Predecessor => F_Sequence_Number);
   end Set_Sequence_Number;

   procedure Set_Unused_24 (Ctx : in out Context; Val : RFLX.ICMP.Unused_24) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_24, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Unused_24);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Unused_24) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Unused_24).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Unused_24)) := (State => S_Invalid, Predecessor => F_Unused_24);
   end Set_Unused_24;

   procedure Set_Originate_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Originate_Timestamp, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Originate_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Originate_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Originate_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Originate_Timestamp)) := (State => S_Invalid, Predecessor => F_Originate_Timestamp);
   end Set_Originate_Timestamp;

   procedure Set_Receive_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Receive_Timestamp, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Receive_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Receive_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Receive_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Receive_Timestamp)) := (State => S_Invalid, Predecessor => F_Receive_Timestamp);
   end Set_Receive_Timestamp;

   procedure Set_Transmit_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) is
      Field_Value : constant Field_Dependent_Value := (F_Transmit_Timestamp, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Transmit_Timestamp);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Transmit_Timestamp) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Transmit_Timestamp).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Transmit_Timestamp)) := (State => S_Invalid, Predecessor => F_Transmit_Timestamp);
   end Set_Transmit_Timestamp;

   procedure Set_Data_Empty (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Data);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Data) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Set_Data_Empty;

   procedure Set_Data (Ctx : in out Context; Value : Types.Bytes) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Data);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Data (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Value;
   end Set_Data;

   procedure Generic_Set_Data (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Data);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Data (Ctx);
      Process_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Data;

   procedure Initialize_Data (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Data);
      Ctx.Message_Last := Last;
      pragma Assert ((if
                         Structural_Valid (Ctx.Cursors (F_Tag))
                      then
                         Ctx.Cursors (F_Tag).Last - Ctx.Cursors (F_Tag).First + 1 = RFLX.ICMP.Tag_Base'Size
                         and then Ctx.Cursors (F_Tag).Predecessor = F_Initial
                         and then Ctx.Cursors (F_Tag).First = Ctx.First
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Code_Destination_Unreachable))
                                      and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                   then
                                      Ctx.Cursors (F_Code_Destination_Unreachable).Last - Ctx.Cursors (F_Code_Destination_Unreachable).First + 1 = RFLX.ICMP.Code_Destination_Unreachable_Base'Size
                                      and then Ctx.Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                                      and then Ctx.Cursors (F_Code_Destination_Unreachable).First = Ctx.Cursors (F_Tag).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Checksum))
                                                then
                                                   Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                   and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable
                                                   and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Destination_Unreachable).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                             then
                                                                Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                             then
                                                                Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                          then
                                                                             Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                             and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                             and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                       and then (if
                                                                                                                    Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                 then
                                                                                                                    Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                             then
                                                                Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                          then
                                                                             Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                             and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                             and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                             then
                                                                Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Code_Redirect))
                                      and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                   then
                                      Ctx.Cursors (F_Code_Redirect).Last - Ctx.Cursors (F_Code_Redirect).First + 1 = RFLX.ICMP.Code_Redirect_Base'Size
                                      and then Ctx.Cursors (F_Code_Redirect).Predecessor = F_Tag
                                      and then Ctx.Cursors (F_Code_Redirect).First = Ctx.Cursors (F_Tag).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Checksum))
                                                then
                                                   Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                   and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Redirect
                                                   and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Redirect).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                             then
                                                                Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                             then
                                                                Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                          then
                                                                             Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                             and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                             and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                       and then (if
                                                                                                                    Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                 then
                                                                                                                    Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                             then
                                                                Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                          then
                                                                             Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                             and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                             and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                             then
                                                                Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Code_Time_Exceeded))
                                      and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                   then
                                      Ctx.Cursors (F_Code_Time_Exceeded).Last - Ctx.Cursors (F_Code_Time_Exceeded).First + 1 = RFLX.ICMP.Code_Time_Exceeded_Base'Size
                                      and then Ctx.Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                                      and then Ctx.Cursors (F_Code_Time_Exceeded).First = Ctx.Cursors (F_Tag).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Checksum))
                                                then
                                                   Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                   and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded
                                                   and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Time_Exceeded).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                             then
                                                                Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                             then
                                                                Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                          then
                                                                             Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                             and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                             and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                       and then (if
                                                                                                                    Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                 then
                                                                                                                    Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                             then
                                                                Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                          then
                                                                             Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                             and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                             and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                             then
                                                                Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Code_Zero))
                                      and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                   then
                                      Ctx.Cursors (F_Code_Zero).Last - Ctx.Cursors (F_Code_Zero).First + 1 = RFLX.ICMP.Code_Zero_Base'Size
                                      and then Ctx.Cursors (F_Code_Zero).Predecessor = F_Tag
                                      and then Ctx.Cursors (F_Code_Zero).First = Ctx.Cursors (F_Tag).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Checksum))
                                                then
                                                   Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                                   and then Ctx.Cursors (F_Checksum).Predecessor = F_Code_Zero
                                                   and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Code_Zero).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Gateway_Internet_Address))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Redirect))
                                                             then
                                                                Ctx.Cursors (F_Gateway_Internet_Address).Last - Ctx.Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Gateway_Internet_Address).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Gateway_Internet_Address).Last + 1))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Identifier))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Information_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply)))
                                                             then
                                                                Ctx.Cursors (F_Identifier).Last - Ctx.Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                                and then Ctx.Cursors (F_Identifier).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Identifier).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Sequence_Number))
                                                                          then
                                                                             Ctx.Cursors (F_Sequence_Number).Last - Ctx.Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                             and then Ctx.Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                             and then Ctx.Cursors (F_Sequence_Number).First = Ctx.Cursors (F_Identifier).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Reply))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Echo_Request)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last)
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Sequence_Number).Last + 1)
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Originate_Timestamp))
                                                                                          and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Msg))
                                                                                                    or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Timestamp_Reply)))
                                                                                       then
                                                                                          Ctx.Cursors (F_Originate_Timestamp).Last - Ctx.Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                          and then Ctx.Cursors (F_Originate_Timestamp).First = Ctx.Cursors (F_Sequence_Number).Last + 1
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Receive_Timestamp))
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Receive_Timestamp).Last - Ctx.Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                       and then Ctx.Cursors (F_Receive_Timestamp).First = Ctx.Cursors (F_Originate_Timestamp).Last + 1
                                                                                                       and then (if
                                                                                                                    Structural_Valid (Ctx.Cursors (F_Transmit_Timestamp))
                                                                                                                 then
                                                                                                                    Ctx.Cursors (F_Transmit_Timestamp).Last - Ctx.Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                                    and then Ctx.Cursors (F_Transmit_Timestamp).First = Ctx.Cursors (F_Receive_Timestamp).Last + 1)))))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Pointer))
                                                                and then Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Parameter_Problem))
                                                             then
                                                                Ctx.Cursors (F_Pointer).Last - Ctx.Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                                and then Ctx.Cursors (F_Pointer).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Pointer).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Unused_24))
                                                                          then
                                                                             Ctx.Cursors (F_Unused_24).Last - Ctx.Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                             and then Ctx.Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                             and then Ctx.Cursors (F_Unused_24).First = Ctx.Cursors (F_Pointer).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Data))
                                                                                       then
                                                                                          Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                                          and then Ctx.Cursors (F_Data).Predecessor = F_Unused_24
                                                                                          and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_24).Last + 1)))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Unused_32))
                                                                and then (Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Time_Exceeded))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Destination_Unreachable))
                                                                          or Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.U64 (To_Base (Source_Quench)))
                                                             then
                                                                Ctx.Cursors (F_Unused_32).Last - Ctx.Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                                and then Ctx.Cursors (F_Unused_32).Predecessor = F_Checksum
                                                                and then Ctx.Cursors (F_Unused_32).First = Ctx.Cursors (F_Checksum).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Data))
                                                                          then
                                                                             Ctx.Cursors (F_Data).Last - Ctx.Cursors (F_Data).First + 1 = 224
                                                                             and then Ctx.Cursors (F_Data).Predecessor = F_Unused_32
                                                                             and then Ctx.Cursors (F_Data).First = Ctx.Cursors (F_Unused_32).Last + 1))))));
      Ctx.Cursors (F_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Data), Predecessor => Ctx.Cursors (F_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Data)) := (State => S_Invalid, Predecessor => F_Data);
   end Initialize_Data;

end RFLX.ICMP.Generic_Message;
