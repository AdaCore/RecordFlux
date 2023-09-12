pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operations;

package body RFLX.IPv4.Packet with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Version => (State => S_Invalid, Predecessor => F_Initial, others => <>), others => (State => S_Invalid, Predecessor => F_Final, others => <>)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Version => (State => S_Invalid, Predecessor => F_Initial, others => <>), others => (State => S_Invalid, Predecessor => F_Final, others => <>)));
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
          when F_Version =>
             F_IHL,
          when F_IHL =>
             F_DSCP,
          when F_DSCP =>
             F_ECN,
          when F_ECN =>
             F_Total_Length,
          when F_Total_Length =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Total_Length).Value) >= RFLX_Types.Base_Integer (Ctx.Cursors (F_IHL).Value) * 4
              then
                 F_Identification
              else
                 F_Initial),
          when F_Identification =>
             F_Flag_R,
          when F_Flag_R =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Flag_R).Value) = RFLX_Types.Base_Integer (To_Base_Integer (False))
              then
                 F_Flag_DF
              else
                 F_Initial),
          when F_Flag_DF =>
             F_Flag_MF,
          when F_Flag_MF =>
             F_Fragment_Offset,
          when F_Fragment_Offset =>
             F_TTL,
          when F_TTL =>
             F_Protocol,
          when F_Protocol =>
             F_Header_Checksum,
          when F_Header_Checksum =>
             F_Source,
          when F_Source =>
             F_Destination,
          when F_Destination =>
             F_Options,
          when F_Options =>
             F_Payload,
          when F_Payload =>
             F_Final))
    with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and RFLX.IPv4.Packet.Well_Formed (Ctx, Fld)
       and RFLX.IPv4.Packet.Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Version =>
             Invalid (Ctx.Cursors (F_IHL)),
          when F_IHL =>
             Invalid (Ctx.Cursors (F_DSCP)),
          when F_DSCP =>
             Invalid (Ctx.Cursors (F_ECN)),
          when F_ECN =>
             Invalid (Ctx.Cursors (F_Total_Length)),
          when F_Total_Length =>
             Invalid (Ctx.Cursors (F_Identification)),
          when F_Identification =>
             Invalid (Ctx.Cursors (F_Flag_R)),
          when F_Flag_R =>
             Invalid (Ctx.Cursors (F_Flag_DF)),
          when F_Flag_DF =>
             Invalid (Ctx.Cursors (F_Flag_MF)),
          when F_Flag_MF =>
             Invalid (Ctx.Cursors (F_Fragment_Offset)),
          when F_Fragment_Offset =>
             Invalid (Ctx.Cursors (F_TTL)),
          when F_TTL =>
             Invalid (Ctx.Cursors (F_Protocol)),
          when F_Protocol =>
             Invalid (Ctx.Cursors (F_Header_Checksum)),
          when F_Header_Checksum =>
             Invalid (Ctx.Cursors (F_Source)),
          when F_Source =>
             Invalid (Ctx.Cursors (F_Destination)),
          when F_Destination =>
             Invalid (Ctx.Cursors (F_Options)),
          when F_Options =>
             Invalid (Ctx.Cursors (F_Payload)),
          when F_Payload =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and RFLX.IPv4.Packet.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Options | F_Payload =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld))))),
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld),
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
         Ctx.Cursors (Fld_Loop) := (State => S_Invalid, Predecessor => F_Final, others => <>);
         pragma Loop_Invariant (Field_First (Ctx, Fld) = First
                                and Field_Size (Ctx, Fld) = Size);
         pragma Loop_Invariant ((for all F in Field =>
                                    (if F < Fld_Loop then Ctx.Cursors (F) = Ctx.Cursors'Loop_Entry (F) else Invalid (Ctx, F))));
      end loop;
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      Ctx.Cursors (Fld) := (State => S_Invalid, Predecessor => Ctx.Cursors (Fld).Predecessor, others => <>);
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     (Fld in F_Options | F_Payload);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.Base_Integer with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Packet.Sufficient_Buffer_Length (Ctx, Fld)
       and then not RFLX.IPv4.Packet.Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Version | F_IHL =>
             4,
          when F_DSCP =>
             6,
          when F_ECN =>
             2,
          when F_Total_Length | F_Identification =>
             16,
          when F_Flag_R | F_Flag_DF | F_Flag_MF =>
             1,
          when F_Fragment_Offset =>
             13,
          when F_TTL | F_Protocol =>
             8,
          when F_Header_Checksum =>
             16,
          when F_Source | F_Destination =>
             32,
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
               pragma Assert ((if Fld = F_Payload then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Well_Formed, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld, others => <>);
            else
               Ctx.Cursors (Fld) := (State => S_Invalid, Predecessor => F_Final, others => <>);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Incomplete, Predecessor => F_Final, others => <>);
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

   function Get_Payload (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Payload;

   procedure Get_Payload (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Payload;

   procedure Generic_Get_Payload (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Process_Payload (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Payload;

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Packet.Valid_Value (Fld, Val)
       and then RFLX.IPv4.Packet.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.IPv4.Packet.Available_Space (Ctx, Fld)
       and then (if RFLX.IPv4.Packet.Composite_Field (Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
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
                    when F_Version =>
                       (Predecessor (Ctx, F_IHL) = F_Version
                        and Valid_Next (Ctx, F_IHL)),
                    when F_IHL =>
                       Get_IHL (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_DSCP) = F_IHL
                            and Valid_Next (Ctx, F_DSCP)),
                    when F_DSCP =>
                       Get_DSCP (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_ECN) = F_DSCP
                            and Valid_Next (Ctx, F_ECN)),
                    when F_ECN =>
                       Get_ECN (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Total_Length) = F_ECN
                            and Valid_Next (Ctx, F_Total_Length)),
                    when F_Total_Length =>
                       Get_Total_Length (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (Get_Total_Length (Ctx)) >= RFLX_Types.Base_Integer (Get_IHL (Ctx)) * 4
                            then
                               Predecessor (Ctx, F_Identification) = F_Total_Length
                               and Valid_Next (Ctx, F_Identification)),
                    when F_Identification =>
                       Get_Identification (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Flag_R) = F_Identification
                            and Valid_Next (Ctx, F_Flag_R)),
                    when F_Flag_R =>
                       Get_Flag_R (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Flag_R (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (False))
                            then
                               Predecessor (Ctx, F_Flag_DF) = F_Flag_R
                               and Valid_Next (Ctx, F_Flag_DF)),
                    when F_Flag_DF =>
                       Get_Flag_DF (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Flag_MF) = F_Flag_DF
                            and Valid_Next (Ctx, F_Flag_MF)),
                    when F_Flag_MF =>
                       Get_Flag_MF (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Fragment_Offset) = F_Flag_MF
                            and Valid_Next (Ctx, F_Fragment_Offset)),
                    when F_Fragment_Offset =>
                       Get_Fragment_Offset (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_TTL) = F_Fragment_Offset
                            and Valid_Next (Ctx, F_TTL)),
                    when F_TTL =>
                       Get_TTL (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Protocol) = F_TTL
                            and Valid_Next (Ctx, F_Protocol)),
                    when F_Protocol =>
                       Get_Protocol (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Header_Checksum) = F_Protocol
                            and Valid_Next (Ctx, F_Header_Checksum)),
                    when F_Header_Checksum =>
                       Get_Header_Checksum (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Source) = F_Header_Checksum
                            and Valid_Next (Ctx, F_Source)),
                    when F_Source =>
                       Get_Source (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Destination) = F_Source
                            and Valid_Next (Ctx, F_Destination)),
                    when F_Destination =>
                       Get_Destination (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Options) = F_Destination
                            and Valid_Next (Ctx, F_Options)),
                    when F_Options =>
                       (Predecessor (Ctx, F_Payload) = F_Options
                        and Valid_Next (Ctx, F_Payload)),
                    when F_Payload =>
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
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      else
         Ctx.Cursors (Fld) := (State => S_Well_Formed, First => First, Last => Last, Value => Val, Predecessor => Ctx.Cursors (Fld).Predecessor);
      end if;
      Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld, others => <>);
      pragma Assert (Last = (Field_First (Ctx, Fld) + Size) - 1);
   end Set;

   procedure Set_Scalar (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, Fld)
       and then Fld in F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination
       and then RFLX.IPv4.Packet.Valid_Value (Fld, Val)
       and then RFLX.IPv4.Packet.Valid_Size (Ctx, Fld, RFLX.IPv4.Packet.Field_Size (Ctx, Fld))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, Fld) >= RFLX.IPv4.Packet.Field_Size (Ctx, Fld)
       and then RFLX.IPv4.Packet.Field_Size (Ctx, Fld) in 1 .. RFLX_Types.Base_Integer'Size
       and then RFLX_Types.Fits_Into (Val, Natural (RFLX.IPv4.Packet.Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (case Fld is
               when F_Version =>
                  (Predecessor (Ctx, F_IHL) = F_Version
                   and Valid_Next (Ctx, F_IHL)),
               when F_IHL =>
                  Get_IHL (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_DSCP) = F_IHL
                       and Valid_Next (Ctx, F_DSCP)),
               when F_DSCP =>
                  Get_DSCP (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_ECN) = F_DSCP
                       and Valid_Next (Ctx, F_ECN)),
               when F_ECN =>
                  Get_ECN (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Total_Length) = F_ECN
                       and Valid_Next (Ctx, F_Total_Length)),
               when F_Total_Length =>
                  Get_Total_Length (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (Get_Total_Length (Ctx)) >= RFLX_Types.Base_Integer (Get_IHL (Ctx)) * 4
                       then
                          Predecessor (Ctx, F_Identification) = F_Total_Length
                          and Valid_Next (Ctx, F_Identification)),
               when F_Identification =>
                  Get_Identification (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Flag_R) = F_Identification
                       and Valid_Next (Ctx, F_Flag_R)),
               when F_Flag_R =>
                  Get_Flag_R (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Flag_R (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (False))
                       then
                          Predecessor (Ctx, F_Flag_DF) = F_Flag_R
                          and Valid_Next (Ctx, F_Flag_DF)),
               when F_Flag_DF =>
                  Get_Flag_DF (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Flag_MF) = F_Flag_DF
                       and Valid_Next (Ctx, F_Flag_MF)),
               when F_Flag_MF =>
                  Get_Flag_MF (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Fragment_Offset) = F_Flag_MF
                       and Valid_Next (Ctx, F_Fragment_Offset)),
               when F_Fragment_Offset =>
                  Get_Fragment_Offset (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_TTL) = F_Fragment_Offset
                       and Valid_Next (Ctx, F_TTL)),
               when F_TTL =>
                  Get_TTL (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Protocol) = F_TTL
                       and Valid_Next (Ctx, F_Protocol)),
               when F_Protocol =>
                  Get_Protocol (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Header_Checksum) = F_Protocol
                       and Valid_Next (Ctx, F_Header_Checksum)),
               when F_Header_Checksum =>
                  Get_Header_Checksum (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Source) = F_Header_Checksum
                       and Valid_Next (Ctx, F_Source)),
               when F_Source =>
                  Get_Source (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Destination) = F_Source
                       and Valid_Next (Ctx, F_Destination)),
               when F_Destination =>
                  Get_Destination (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Options) = F_Destination
                       and Valid_Next (Ctx, F_Options)),
               when F_Options =>
                  (Predecessor (Ctx, F_Payload) = F_Options
                   and Valid_Next (Ctx, F_Payload)),
               when F_Payload =>
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

   procedure Set_Version (Ctx : in out Context; Val : RFLX.IPv4.Version) is
   begin
      Set_Scalar (Ctx, F_Version, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Version;

   procedure Set_IHL (Ctx : in out Context; Val : RFLX.IPv4.IHL) is
   begin
      Set_Scalar (Ctx, F_IHL, RFLX.IPv4.To_Base_Integer (Val));
   end Set_IHL;

   procedure Set_DSCP (Ctx : in out Context; Val : RFLX.IPv4.DCSP) is
   begin
      Set_Scalar (Ctx, F_DSCP, RFLX.IPv4.To_Base_Integer (Val));
   end Set_DSCP;

   procedure Set_ECN (Ctx : in out Context; Val : RFLX.IPv4.ECN) is
   begin
      Set_Scalar (Ctx, F_ECN, RFLX.IPv4.To_Base_Integer (Val));
   end Set_ECN;

   procedure Set_Total_Length (Ctx : in out Context; Val : RFLX.IPv4.Total_Length) is
   begin
      Set_Scalar (Ctx, F_Total_Length, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Total_Length;

   procedure Set_Identification (Ctx : in out Context; Val : RFLX.IPv4.Identification) is
   begin
      Set_Scalar (Ctx, F_Identification, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Identification;

   procedure Set_Flag_R (Ctx : in out Context; Val : Boolean) is
   begin
      Set_Scalar (Ctx, F_Flag_R, To_Base_Integer (Val));
   end Set_Flag_R;

   procedure Set_Flag_DF (Ctx : in out Context; Val : Boolean) is
   begin
      Set_Scalar (Ctx, F_Flag_DF, To_Base_Integer (Val));
   end Set_Flag_DF;

   procedure Set_Flag_MF (Ctx : in out Context; Val : Boolean) is
   begin
      Set_Scalar (Ctx, F_Flag_MF, To_Base_Integer (Val));
   end Set_Flag_MF;

   procedure Set_Fragment_Offset (Ctx : in out Context; Val : RFLX.IPv4.Fragment_Offset) is
   begin
      Set_Scalar (Ctx, F_Fragment_Offset, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Fragment_Offset;

   procedure Set_TTL (Ctx : in out Context; Val : RFLX.IPv4.TTL) is
   begin
      Set_Scalar (Ctx, F_TTL, RFLX.IPv4.To_Base_Integer (Val));
   end Set_TTL;

   procedure Set_Protocol (Ctx : in out Context; Val : RFLX.IPv4.Protocol_Enum) is
   begin
      Set_Scalar (Ctx, F_Protocol, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Protocol;

   procedure Set_Header_Checksum (Ctx : in out Context; Val : RFLX.IPv4.Header_Checksum) is
   begin
      Set_Scalar (Ctx, F_Header_Checksum, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Header_Checksum;

   procedure Set_Source (Ctx : in out Context; Val : RFLX.IPv4.Address) is
   begin
      Set_Scalar (Ctx, F_Source, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Source;

   procedure Set_Destination (Ctx : in out Context; Val : RFLX.IPv4.Address) is
   begin
      Set_Scalar (Ctx, F_Destination, RFLX.IPv4.To_Base_Integer (Val));
   end Set_Destination;

   procedure Set_Options_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Options, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Options_Empty;

   procedure Set_Payload_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Payload, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Payload_Empty;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : RFLX.IPv4.Options.Context) is
      Size : constant RFLX_Types.Bit_Length := RFLX_Types.To_Bit_Length (RFLX.IPv4.Options.Byte_Size (Seq_Ctx));
      Unused_First, Unused_Last : RFLX_Types.Bit_Index;
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Options, 0, Size, True, Buffer_First, Buffer_Last, Unused_Offset);
      RFLX.IPv4.Options.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Options;

   procedure Initialize_Options_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Valid_Length (Ctx, RFLX.IPv4.Packet.F_Options, Length)
       and then RFLX_Types.To_Length (RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Options)) >= Length
       and then RFLX.IPv4.Packet.Field_First (Ctx, RFLX.IPv4.Packet.F_Options) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Field_Size (Ctx, F_Options) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
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
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options, others => <>);
   end Initialize_Options_Private;

   procedure Initialize_Options (Ctx : in out Context) is
   begin
      Initialize_Options_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Options)));
   end Initialize_Options;

   procedure Initialize_Payload_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid_Length (Ctx, RFLX.IPv4.Packet.F_Payload, Length)
       and then RFLX_Types.To_Length (RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload)) >= Length
       and then RFLX.IPv4.Packet.Field_First (Ctx, RFLX.IPv4.Packet.F_Payload) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Payload)
       and Field_Size (Ctx, F_Payload) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Payload)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Payload) = Field_First (Ctx, F_Payload)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Payload);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Payload) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload, others => <>);
   end Initialize_Payload_Private;

   procedure Initialize_Payload (Ctx : in out Context) is
   begin
      Initialize_Payload_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Payload)));
   end Initialize_Payload;

   procedure Set_Payload (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Payload));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Payload_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_Payload)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_Payload)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_Payload))) = Data);
   end Set_Payload;

   procedure Generic_Set_Payload (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Payload (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Payload_Private (Ctx, Length);
   end Generic_Set_Payload;

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out RFLX.IPv4.Options.Context) is
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
         Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options, others => <>);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.IPv4.Options.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Options;

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out RFLX.IPv4.Options.Context) is
      Valid_Sequence : constant Boolean := RFLX.IPv4.Packet.Complete_Options (Ctx, Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.IPv4.Options.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Options) := (State => S_Valid, First => Ctx.Cursors (F_Options).First, Last => Ctx.Cursors (F_Options).Last, Value => Ctx.Cursors (F_Options).Value, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      else
         Reset_Dependent_Fields (Ctx, F_Options);
         Ctx.Cursors (F_Options) := (State => S_Invalid, Predecessor => Ctx.Cursors (F_Options).Predecessor, others => <>);
      end if;
   end Update_Options;

end RFLX.IPv4.Packet;
