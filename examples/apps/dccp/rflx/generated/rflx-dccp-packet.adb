pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operations;

package body RFLX.DCCP.Packet with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Source_Port => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Source_Port => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
          when F_Source_Port =>
             F_Destination_Port,
          when F_Destination_Port =>
             F_Data_Offset,
          when F_Data_Offset =>
             F_CCVal,
          when F_CCVal =>
             F_CsCov,
          when F_CsCov =>
             F_Checksum,
          when F_Checksum =>
             F_Res_3,
          when F_Res_3 =>
             F_Packet_Type,
          when F_Packet_Type =>
             F_X,
          when F_X =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))
              then
                 F_Res_8
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED))
              then
                 F_Sequence_Number_Short
              else
                 F_Initial),
          when F_Res_8 =>
             F_Sequence_Number_Long,
          when F_Sequence_Number_Short =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
              then
                 F_Ack_Reserved_Short
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              else
                 F_Initial),
          when F_Sequence_Number_Long =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
              then
                 F_Ack_Reserved_Long
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
              then
                 F_Service_Code
              else
                 F_Initial),
          when F_Ack_Reserved_Short =>
             F_Ack_Number_Short,
          when F_Ack_Reserved_Long =>
             F_Ack_Number_Long,
          when F_Ack_Number_Short =>
             (if
                 (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              else
                 F_Initial),
          when F_Ack_Number_Long =>
             (if
                 (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))
              then
                 F_Reset_Code
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE))
              then
                 F_Service_Code
              else
                 F_Initial),
          when F_Reset_Code =>
             F_Data_1,
          when F_Service_Code =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              else
                 F_Initial),
          when F_Data_1 =>
             F_Data_2,
          when F_Data_2 =>
             F_Data_3,
          when F_Data_3 =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Data
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1
              then
                 F_Options
              else
                 F_Initial),
          when F_Options =>
             F_Data,
          when F_Data =>
             F_Final))
    with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and RFLX.DCCP.Packet.Well_Formed (Ctx, Fld)
       and RFLX.DCCP.Packet.Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Source_Port =>
             Invalid (Ctx.Cursors (F_Destination_Port)),
          when F_Destination_Port =>
             Invalid (Ctx.Cursors (F_Data_Offset)),
          when F_Data_Offset =>
             Invalid (Ctx.Cursors (F_CCVal)),
          when F_CCVal =>
             Invalid (Ctx.Cursors (F_CsCov)),
          when F_CsCov =>
             Invalid (Ctx.Cursors (F_Checksum)),
          when F_Checksum =>
             Invalid (Ctx.Cursors (F_Res_3)),
          when F_Res_3 =>
             Invalid (Ctx.Cursors (F_Packet_Type)),
          when F_Packet_Type =>
             Invalid (Ctx.Cursors (F_X)),
          when F_X =>
             Invalid (Ctx.Cursors (F_Res_8))
             and Invalid (Ctx.Cursors (F_Sequence_Number_Short)),
          when F_Res_8 =>
             Invalid (Ctx.Cursors (F_Sequence_Number_Long)),
          when F_Sequence_Number_Short =>
             Invalid (Ctx.Cursors (F_Ack_Reserved_Short))
             and Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options)),
          when F_Sequence_Number_Long =>
             Invalid (Ctx.Cursors (F_Ack_Reserved_Long))
             and Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options))
             and Invalid (Ctx.Cursors (F_Service_Code)),
          when F_Ack_Reserved_Short =>
             Invalid (Ctx.Cursors (F_Ack_Number_Short)),
          when F_Ack_Reserved_Long =>
             Invalid (Ctx.Cursors (F_Ack_Number_Long)),
          when F_Ack_Number_Short =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options)),
          when F_Ack_Number_Long =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options))
             and Invalid (Ctx.Cursors (F_Reset_Code))
             and Invalid (Ctx.Cursors (F_Service_Code)),
          when F_Reset_Code =>
             Invalid (Ctx.Cursors (F_Data_1)),
          when F_Service_Code =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options)),
          when F_Data_1 =>
             Invalid (Ctx.Cursors (F_Data_2)),
          when F_Data_2 =>
             Invalid (Ctx.Cursors (F_Data_3)),
          when F_Data_3 =>
             Invalid (Ctx.Cursors (F_Data))
             and Invalid (Ctx.Cursors (F_Options)),
          when F_Options =>
             Invalid (Ctx.Cursors (F_Data)),
          when F_Data =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and RFLX.DCCP.Packet.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Options | F_Data =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld))))),
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld),
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
     (Fld in F_Options | F_Data);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.Base_Integer with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Packet.Sufficient_Buffer_Length (Ctx, Fld)
       and then not RFLX.DCCP.Packet.Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Source_Port | F_Destination_Port =>
             16,
          when F_Data_Offset =>
             8,
          when F_CCVal | F_CsCov =>
             4,
          when F_Checksum =>
             16,
          when F_Res_3 =>
             3,
          when F_Packet_Type =>
             4,
          when F_X =>
             1,
          when F_Res_8 =>
             8,
          when F_Sequence_Number_Short =>
             24,
          when F_Sequence_Number_Long =>
             48,
          when F_Ack_Reserved_Short =>
             8,
          when F_Ack_Reserved_Long =>
             16,
          when F_Ack_Number_Short =>
             24,
          when F_Ack_Number_Long =>
             48,
          when F_Reset_Code =>
             8,
          when F_Service_Code =>
             32,
          when F_Data_1 | F_Data_2 | F_Data_3 =>
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
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := (if Composite_Field (Fld) then 0 else Get (Ctx, Fld));
            if
               Valid_Value (Fld, Value)
               and then Field_Condition (Ctx, Fld, Value)
            then
               pragma Assert ((if Fld = F_Data then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Well_Formed, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
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
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Packet.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Packet.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.DCCP.Packet.Available_Space (Ctx, Fld)
       and then (if RFLX.DCCP.Packet.Composite_Field (Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
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
       and then Predecessor (Ctx, Fld) = Predecessor (Ctx, Fld)'Old
       and then Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and then Sufficient_Space (Ctx, Fld)
       and then (if State_Valid and Size > 0 then Valid (Ctx, Fld) else Well_Formed (Ctx, Fld))
       and then (case Fld is
                    when F_Source_Port =>
                       Get_Source_Port (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Destination_Port) = F_Source_Port
                            and Valid_Next (Ctx, F_Destination_Port)),
                    when F_Destination_Port =>
                       Get_Destination_Port (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Data_Offset) = F_Destination_Port
                            and Valid_Next (Ctx, F_Data_Offset)),
                    when F_Data_Offset =>
                       Get_Data_Offset (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_CCVal) = F_Data_Offset
                            and Valid_Next (Ctx, F_CCVal)),
                    when F_CCVal =>
                       Get_CCVal (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_CsCov) = F_CCVal
                            and Valid_Next (Ctx, F_CsCov)),
                    when F_CsCov =>
                       Get_CsCov (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Checksum) = F_CsCov
                            and Valid_Next (Ctx, F_Checksum)),
                    when F_Checksum =>
                       Get_Checksum (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Res_3) = F_Checksum
                            and Valid_Next (Ctx, F_Res_3)),
                    when F_Res_3 =>
                       Get_Res_3 (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Packet_Type) = F_Res_3
                            and Valid_Next (Ctx, F_Packet_Type)),
                    when F_Packet_Type =>
                       Get_Packet_Type (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_X) = F_Packet_Type
                            and Valid_Next (Ctx, F_X)),
                    when F_X =>
                       Get_X (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))
                            then
                               Predecessor (Ctx, F_Res_8) = F_X
                               and Valid_Next (Ctx, F_Res_8))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED))
                            then
                               Predecessor (Ctx, F_Sequence_Number_Short) = F_X
                               and Valid_Next (Ctx, F_Sequence_Number_Short)),
                    when F_Res_8 =>
                       Get_Res_8 (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Sequence_Number_Long) = F_Res_8
                            and Valid_Next (Ctx, F_Sequence_Number_Long)),
                    when F_Sequence_Number_Short =>
                       Get_Sequence_Number_Short (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                            then
                               Predecessor (Ctx, F_Ack_Reserved_Short) = F_Sequence_Number_Short
                               and Valid_Next (Ctx, F_Ack_Reserved_Short))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Sequence_Number_Short
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Sequence_Number_Short
                               and Valid_Next (Ctx, F_Options)),
                    when F_Sequence_Number_Long =>
                       Get_Sequence_Number_Long (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                            then
                               Predecessor (Ctx, F_Ack_Reserved_Long) = F_Sequence_Number_Long
                               and Valid_Next (Ctx, F_Ack_Reserved_Long))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Sequence_Number_Long
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Sequence_Number_Long
                               and Valid_Next (Ctx, F_Options))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                            then
                               Predecessor (Ctx, F_Service_Code) = F_Sequence_Number_Long
                               and Valid_Next (Ctx, F_Service_Code)),
                    when F_Ack_Reserved_Short =>
                       Get_Ack_Reserved_Short (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Ack_Number_Short) = F_Ack_Reserved_Short
                            and Valid_Next (Ctx, F_Ack_Number_Short)),
                    when F_Ack_Reserved_Long =>
                       Get_Ack_Reserved_Long (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Ack_Number_Long) = F_Ack_Reserved_Long
                            and Valid_Next (Ctx, F_Ack_Number_Long)),
                    when F_Ack_Number_Short =>
                       Get_Ack_Number_Short (Ctx) = To_Actual (Val)
                       and (if
                               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Ack_Number_Short
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Ack_Number_Short
                               and Valid_Next (Ctx, F_Options)),
                    when F_Ack_Number_Long =>
                       Get_Ack_Number_Long (Ctx) = To_Actual (Val)
                       and (if
                               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Ack_Number_Long
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Ack_Number_Long
                               and Valid_Next (Ctx, F_Options))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))
                            then
                               Predecessor (Ctx, F_Reset_Code) = F_Ack_Number_Long
                               and Valid_Next (Ctx, F_Reset_Code))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE))
                            then
                               Predecessor (Ctx, F_Service_Code) = F_Ack_Number_Long
                               and Valid_Next (Ctx, F_Service_Code)),
                    when F_Reset_Code =>
                       Get_Reset_Code (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Data_1) = F_Reset_Code
                            and Valid_Next (Ctx, F_Data_1)),
                    when F_Service_Code =>
                       Get_Service_Code (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Service_Code
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Service_Code
                               and Valid_Next (Ctx, F_Options)),
                    when F_Data_1 =>
                       Get_Data_1 (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Data_2) = F_Data_1
                            and Valid_Next (Ctx, F_Data_2)),
                    when F_Data_2 =>
                       Get_Data_2 (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Data_3) = F_Data_2
                            and Valid_Next (Ctx, F_Data_3)),
                    when F_Data_3 =>
                       Get_Data_3 (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Data) = F_Data_3
                               and Valid_Next (Ctx, F_Data))
                       and (if
                               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                            then
                               Predecessor (Ctx, F_Options) = F_Data_3
                               and Valid_Next (Ctx, F_Options)),
                    when F_Options =>
                       (Predecessor (Ctx, F_Data) = F_Options
                        and Valid_Next (Ctx, F_Data)),
                    when F_Data =>
                       (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)))
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
      pragma Assert (Size = (case Fld is
                         when F_Source_Port | F_Destination_Port =>
                            16,
                         when F_Data_Offset =>
                            8,
                         when F_CCVal | F_CsCov =>
                            4,
                         when F_Checksum =>
                            16,
                         when F_Res_3 =>
                            3,
                         when F_Packet_Type =>
                            4,
                         when F_X =>
                            1,
                         when F_Res_8 =>
                            8,
                         when F_Sequence_Number_Short =>
                            24,
                         when F_Sequence_Number_Long =>
                            48,
                         when F_Ack_Reserved_Short =>
                            8,
                         when F_Ack_Reserved_Long =>
                            16,
                         when F_Ack_Number_Short =>
                            24,
                         when F_Ack_Number_Long =>
                            48,
                         when F_Reset_Code =>
                            8,
                         when F_Service_Code =>
                            32,
                         when F_Data_1 | F_Data_2 | F_Data_3 =>
                            8,
                         when F_Options =>
                            (if
                                Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long
                                and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short
                                and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Data_3
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long
                                and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short
                                and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Service_Code
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
                             else
                                RFLX_Types.Unreachable),
                         when F_Data =>
                            (if
                                Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long
                                and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short
                                and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                           or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Data_3
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Options
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Options).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long
                                and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short
                                and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last)
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Service_Code
                                and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
                             then
                                RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last)
                             else
                                RFLX_Types.Unreachable)));
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      else
         Ctx.Cursors (Fld) := (State => S_Well_Formed, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      end if;
      Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
      pragma Assert (Last = (Field_First (Ctx, Fld) + Size) - 1);
   end Set;

   procedure Set_Scalar (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, Fld)
       and then Fld in F_Source_Port | F_Destination_Port | F_Data_Offset | F_CCVal | F_CsCov | F_Checksum | F_Res_3 | F_Packet_Type | F_X | F_Res_8 | F_Sequence_Number_Short | F_Sequence_Number_Long | F_Ack_Reserved_Short | F_Ack_Reserved_Long | F_Ack_Number_Short | F_Ack_Number_Long | F_Reset_Code | F_Service_Code | F_Data_1 | F_Data_2 | F_Data_3
       and then RFLX.DCCP.Packet.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Packet.Valid_Size (Ctx, Fld, RFLX.DCCP.Packet.Field_Size (Ctx, Fld))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, Fld) >= RFLX.DCCP.Packet.Field_Size (Ctx, Fld)
       and then RFLX.DCCP.Packet.Field_Size (Ctx, Fld) in 1 .. RFLX_Types.Base_Integer'Size
       and then RFLX_Types.Fits_Into (Val, Natural (RFLX.DCCP.Packet.Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (case Fld is
               when F_Source_Port =>
                  Get_Source_Port (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Destination_Port) = F_Source_Port
                       and Valid_Next (Ctx, F_Destination_Port)),
               when F_Destination_Port =>
                  Get_Destination_Port (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Data_Offset) = F_Destination_Port
                       and Valid_Next (Ctx, F_Data_Offset)),
               when F_Data_Offset =>
                  Get_Data_Offset (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_CCVal) = F_Data_Offset
                       and Valid_Next (Ctx, F_CCVal)),
               when F_CCVal =>
                  Get_CCVal (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_CsCov) = F_CCVal
                       and Valid_Next (Ctx, F_CsCov)),
               when F_CsCov =>
                  Get_CsCov (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Checksum) = F_CsCov
                       and Valid_Next (Ctx, F_Checksum)),
               when F_Checksum =>
                  Get_Checksum (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Res_3) = F_Checksum
                       and Valid_Next (Ctx, F_Res_3)),
               when F_Res_3 =>
                  Get_Res_3 (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Packet_Type) = F_Res_3
                       and Valid_Next (Ctx, F_Packet_Type)),
               when F_Packet_Type =>
                  Get_Packet_Type (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_X) = F_Packet_Type
                       and Valid_Next (Ctx, F_X)),
               when F_X =>
                  Get_X (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))
                       then
                          Predecessor (Ctx, F_Res_8) = F_X
                          and Valid_Next (Ctx, F_Res_8))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED))
                       then
                          Predecessor (Ctx, F_Sequence_Number_Short) = F_X
                          and Valid_Next (Ctx, F_Sequence_Number_Short)),
               when F_Res_8 =>
                  Get_Res_8 (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Sequence_Number_Long) = F_Res_8
                       and Valid_Next (Ctx, F_Sequence_Number_Long)),
               when F_Sequence_Number_Short =>
                  Get_Sequence_Number_Short (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                       then
                          Predecessor (Ctx, F_Ack_Reserved_Short) = F_Sequence_Number_Short
                          and Valid_Next (Ctx, F_Ack_Reserved_Short))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Sequence_Number_Short
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Sequence_Number_Short
                          and Valid_Next (Ctx, F_Options)),
               when F_Sequence_Number_Long =>
                  Get_Sequence_Number_Long (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                       then
                          Predecessor (Ctx, F_Ack_Reserved_Long) = F_Sequence_Number_Long
                          and Valid_Next (Ctx, F_Ack_Reserved_Long))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Sequence_Number_Long
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Sequence_Number_Long
                          and Valid_Next (Ctx, F_Options))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                       then
                          Predecessor (Ctx, F_Service_Code) = F_Sequence_Number_Long
                          and Valid_Next (Ctx, F_Service_Code)),
               when F_Ack_Reserved_Short =>
                  Get_Ack_Reserved_Short (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Ack_Number_Short) = F_Ack_Reserved_Short
                       and Valid_Next (Ctx, F_Ack_Number_Short)),
               when F_Ack_Reserved_Long =>
                  Get_Ack_Reserved_Long (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Ack_Number_Long) = F_Ack_Reserved_Long
                       and Valid_Next (Ctx, F_Ack_Number_Long)),
               when F_Ack_Number_Short =>
                  Get_Ack_Number_Short (Ctx) = To_Actual (Val)
                  and (if
                          (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Ack_Number_Short
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Ack_Number_Short
                          and Valid_Next (Ctx, F_Options)),
               when F_Ack_Number_Long =>
                  Get_Ack_Number_Long (Ctx) = To_Actual (Val)
                  and (if
                          (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Ack_Number_Long
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                           or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                          and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Ack_Number_Long
                          and Valid_Next (Ctx, F_Options))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))
                       then
                          Predecessor (Ctx, F_Reset_Code) = F_Ack_Number_Long
                          and Valid_Next (Ctx, F_Reset_Code))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE))
                       then
                          Predecessor (Ctx, F_Service_Code) = F_Ack_Number_Long
                          and Valid_Next (Ctx, F_Service_Code)),
               when F_Reset_Code =>
                  Get_Reset_Code (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Data_1) = F_Reset_Code
                       and Valid_Next (Ctx, F_Data_1)),
               when F_Service_Code =>
                  Get_Service_Code (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Service_Code
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Service_Code
                          and Valid_Next (Ctx, F_Options)),
               when F_Data_1 =>
                  Get_Data_1 (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Data_2) = F_Data_1
                       and Valid_Next (Ctx, F_Data_2)),
               when F_Data_2 =>
                  Get_Data_2 (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Data_3) = F_Data_2
                       and Valid_Next (Ctx, F_Data_3)),
               when F_Data_3 =>
                  Get_Data_3 (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Data) = F_Data_3
                          and Valid_Next (Ctx, F_Data))
                  and (if
                          RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
                       then
                          Predecessor (Ctx, F_Options) = F_Data_3
                          and Valid_Next (Ctx, F_Options)),
               when F_Options =>
                  (Predecessor (Ctx, F_Data) = F_Options
                   and Valid_Next (Ctx, F_Data)),
               when F_Data =>
                  (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Predecessor (Ctx, Fld) = Predecessor (Ctx, Fld)'Old
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

   procedure Set_Source_Port (Ctx : in out Context; Val : RFLX.DCCP.Port_Type) is
   begin
      Set_Scalar (Ctx, F_Source_Port, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Source_Port;

   procedure Set_Destination_Port (Ctx : in out Context; Val : RFLX.DCCP.Port_Type) is
   begin
      Set_Scalar (Ctx, F_Destination_Port, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Destination_Port;

   procedure Set_Data_Offset (Ctx : in out Context; Val : RFLX.DCCP.Data_Offset_Type) is
   begin
      Set_Scalar (Ctx, F_Data_Offset, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Data_Offset;

   procedure Set_CCVal (Ctx : in out Context; Val : RFLX.DCCP.CCVal_Type) is
   begin
      Set_Scalar (Ctx, F_CCVal, RFLX.DCCP.To_Base_Integer (Val));
   end Set_CCVal;

   procedure Set_CsCov (Ctx : in out Context; Val : RFLX.DCCP.Checksum_Coverage_Type) is
   begin
      Set_Scalar (Ctx, F_CsCov, RFLX.DCCP.To_Base_Integer (Val));
   end Set_CsCov;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.DCCP.Checksum_Type) is
   begin
      Set_Scalar (Ctx, F_Checksum, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Checksum;

   procedure Set_Res_3 (Ctx : in out Context; Val : RFLX.DCCP.Reserved_3_Type) is
   begin
      Set_Scalar (Ctx, F_Res_3, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Res_3;

   procedure Set_Packet_Type (Ctx : in out Context; Val : RFLX.DCCP.Type_Field) is
   begin
      Set_Scalar (Ctx, F_Packet_Type, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Packet_Type;

   procedure Set_X (Ctx : in out Context; Val : RFLX.DCCP.Ext_Seq_Type) is
   begin
      Set_Scalar (Ctx, F_X, RFLX.DCCP.To_Base_Integer (Val));
   end Set_X;

   procedure Set_Res_8 (Ctx : in out Context; Val : RFLX.DCCP.Reserved_8_Type) is
   begin
      Set_Scalar (Ctx, F_Res_8, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Res_8;

   procedure Set_Sequence_Number_Short (Ctx : in out Context; Val : RFLX.DCCP.Sequence_Number_Short_Type) is
   begin
      Set_Scalar (Ctx, F_Sequence_Number_Short, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Sequence_Number_Short;

   procedure Set_Sequence_Number_Long (Ctx : in out Context; Val : RFLX.DCCP.Sequence_Number_Long_Type) is
   begin
      Set_Scalar (Ctx, F_Sequence_Number_Long, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Sequence_Number_Long;

   procedure Set_Ack_Reserved_Short (Ctx : in out Context; Val : RFLX.DCCP.Reserved_8_Type) is
   begin
      Set_Scalar (Ctx, F_Ack_Reserved_Short, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Ack_Reserved_Short;

   procedure Set_Ack_Reserved_Long (Ctx : in out Context; Val : RFLX.DCCP.Reserved_16_Type) is
   begin
      Set_Scalar (Ctx, F_Ack_Reserved_Long, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Ack_Reserved_Long;

   procedure Set_Ack_Number_Short (Ctx : in out Context; Val : RFLX.DCCP.Ack_Number_Short_Type) is
   begin
      Set_Scalar (Ctx, F_Ack_Number_Short, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Ack_Number_Short;

   procedure Set_Ack_Number_Long (Ctx : in out Context; Val : RFLX.DCCP.Ack_Number_Long_Type) is
   begin
      Set_Scalar (Ctx, F_Ack_Number_Long, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Ack_Number_Long;

   procedure Set_Reset_Code (Ctx : in out Context; Val : RFLX.DCCP.Reset_Code_Type) is
   begin
      Set_Scalar (Ctx, F_Reset_Code, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Reset_Code;

   procedure Set_Service_Code (Ctx : in out Context; Val : RFLX.DCCP.Service_Code_Type) is
   begin
      Set_Scalar (Ctx, F_Service_Code, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Service_Code;

   procedure Set_Data_1 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) is
   begin
      Set_Scalar (Ctx, F_Data_1, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Data_1;

   procedure Set_Data_2 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) is
   begin
      Set_Scalar (Ctx, F_Data_2, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Data_2;

   procedure Set_Data_3 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) is
   begin
      Set_Scalar (Ctx, F_Data_3, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Data_3;

   procedure Set_Data_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Data, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Data_Empty;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : RFLX.DCCP.Options.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (RFLX.DCCP.Options.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Options, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      RFLX.DCCP.Options.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Options;

   procedure Initialize_Options_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Options, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Options)) >= Length
       and then RFLX.DCCP.Packet.Field_First (Ctx, RFLX.DCCP.Packet.F_Options) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Field_Size (Ctx, F_Options) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data) = F_Options
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
   end Initialize_Options_Private;

   procedure Initialize_Options (Ctx : in out Context) is
   begin
      Initialize_Options_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Options)));
   end Initialize_Options;

   procedure Initialize_Data_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Data, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data)) >= Length
       and then RFLX.DCCP.Packet.Field_First (Ctx, RFLX.DCCP.Packet.F_Data) mod RFLX_Types.Byte'Size = 1,
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
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
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

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out RFLX.DCCP.Options.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Options);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Options) then
         Reset_Dependent_Fields (Ctx, F_Options);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Options) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Options).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.DCCP.Options.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Options;

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out RFLX.DCCP.Options.Context) is
      Valid_Sequence : constant Boolean := RFLX.DCCP.Packet.Complete_Options (Ctx, Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.DCCP.Options.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Options) := (State => S_Valid, First => Ctx.Cursors (F_Options).First, Last => Ctx.Cursors (F_Options).Last, Value => Ctx.Cursors (F_Options).Value, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      else
         Reset_Dependent_Fields (Ctx, F_Options);
         Ctx.Cursors (F_Options) := (State => S_Invalid, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      end if;
   end Update_Options;

end RFLX.DCCP.Packet;
