pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.IPv4.Packet with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Version => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Version => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
                 RFLX_Types.U64 (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX_Types.U64 (Ctx.Cursors (F_IHL).Value.IHL_Value) * 4
              then
                 F_Identification
              else
                 F_Initial),
          when F_Identification =>
             F_Flag_R,
          when F_Flag_R =>
             (if
                 RFLX_Types.U64 (Ctx.Cursors (F_Flag_R).Value.Flag_R_Value) = RFLX_Types.U64 (To_Base (False))
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
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

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
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Options | F_Payload =>
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
          when F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination =>
             False,
          when F_Options | F_Payload =>
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
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Version_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.IHL_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.DCSP);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.ECN);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Total_Length);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Identification);
      function Extract is new RFLX_Types.Extract (RFLX.RFLX_Builtin_Types.Boolean_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Fragment_Offset);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.TTL);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Protocol_Base);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Header_Checksum);
      function Extract is new RFLX_Types.Extract (RFLX.IPv4.Address);
   begin
      return ((case Fld is
                  when F_Version =>
                     (Fld => F_Version, Version_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_IHL =>
                     (Fld => F_IHL, IHL_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_DSCP =>
                     (Fld => F_DSCP, DSCP_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_ECN =>
                     (Fld => F_ECN, ECN_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Total_Length =>
                     (Fld => F_Total_Length, Total_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Identification =>
                     (Fld => F_Identification, Identification_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Flag_R =>
                     (Fld => F_Flag_R, Flag_R_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Flag_DF =>
                     (Fld => F_Flag_DF, Flag_DF_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Flag_MF =>
                     (Fld => F_Flag_MF, Flag_MF_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Fragment_Offset =>
                     (Fld => F_Fragment_Offset, Fragment_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_TTL =>
                     (Fld => F_TTL, TTL_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Protocol =>
                     (Fld => F_Protocol, Protocol_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Header_Checksum =>
                     (Fld => F_Header_Checksum, Header_Checksum_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Source =>
                     (Fld => F_Source, Source_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Destination =>
                     (Fld => F_Destination, Destination_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First)),
                  when F_Options =>
                     (Fld => F_Options),
                  when F_Payload =>
                     (Fld => F_Payload)));
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
               pragma Assert ((if Fld = F_Payload then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               if Fld = F_Version then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_IHL then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_DSCP then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_ECN then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Total_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Identification then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Flag_R then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Flag_DF then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Flag_MF then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Fragment_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_TTL then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Protocol then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Header_Checksum then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Source then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Destination then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Options then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Payload then
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
      Verify (Ctx, F_Version);
      Verify (Ctx, F_IHL);
      Verify (Ctx, F_DSCP);
      Verify (Ctx, F_ECN);
      Verify (Ctx, F_Total_Length);
      Verify (Ctx, F_Identification);
      Verify (Ctx, F_Flag_R);
      Verify (Ctx, F_Flag_DF);
      Verify (Ctx, F_Flag_MF);
      Verify (Ctx, F_Fragment_Offset);
      Verify (Ctx, F_TTL);
      Verify (Ctx, F_Protocol);
      Verify (Ctx, F_Header_Checksum);
      Verify (Ctx, F_Source);
      Verify (Ctx, F_Destination);
      Verify (Ctx, F_Options);
      Verify (Ctx, F_Payload);
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

   procedure Set_Version (Ctx : in out Context; Val : RFLX.IPv4.Version) is
      Field_Value : constant Field_Dependent_Value := (F_Version, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Version_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Version);
      First := Field_First (Ctx, F_Version);
      Last := Field_Last (Ctx, F_Version);
      Insert (Field_Value.Version_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Version) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Version).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Version)) := (State => S_Invalid, Predecessor => F_Version);
   end Set_Version;

   procedure Set_IHL (Ctx : in out Context; Val : RFLX.IPv4.IHL) is
      Field_Value : constant Field_Dependent_Value := (F_IHL, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.IHL_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_IHL);
      First := Field_First (Ctx, F_IHL);
      Last := Field_Last (Ctx, F_IHL);
      Insert (Field_Value.IHL_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_IHL) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_IHL).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_IHL)) := (State => S_Invalid, Predecessor => F_IHL);
   end Set_IHL;

   procedure Set_DSCP (Ctx : in out Context; Val : RFLX.IPv4.DCSP) is
      Field_Value : constant Field_Dependent_Value := (F_DSCP, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.DCSP);
   begin
      Reset_Dependent_Fields (Ctx, F_DSCP);
      First := Field_First (Ctx, F_DSCP);
      Last := Field_Last (Ctx, F_DSCP);
      Insert (Field_Value.DSCP_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_DSCP) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_DSCP).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_DSCP)) := (State => S_Invalid, Predecessor => F_DSCP);
   end Set_DSCP;

   procedure Set_ECN (Ctx : in out Context; Val : RFLX.IPv4.ECN) is
      Field_Value : constant Field_Dependent_Value := (F_ECN, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.ECN);
   begin
      Reset_Dependent_Fields (Ctx, F_ECN);
      First := Field_First (Ctx, F_ECN);
      Last := Field_Last (Ctx, F_ECN);
      Insert (Field_Value.ECN_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_ECN) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_ECN).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_ECN)) := (State => S_Invalid, Predecessor => F_ECN);
   end Set_ECN;

   procedure Set_Total_Length (Ctx : in out Context; Val : RFLX.IPv4.Total_Length) is
      Field_Value : constant Field_Dependent_Value := (F_Total_Length, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Total_Length);
   begin
      Reset_Dependent_Fields (Ctx, F_Total_Length);
      First := Field_First (Ctx, F_Total_Length);
      Last := Field_Last (Ctx, F_Total_Length);
      Insert (Field_Value.Total_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Total_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Total_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Total_Length)) := (State => S_Invalid, Predecessor => F_Total_Length);
   end Set_Total_Length;

   procedure Set_Identification (Ctx : in out Context; Val : RFLX.IPv4.Identification) is
      Field_Value : constant Field_Dependent_Value := (F_Identification, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Identification);
   begin
      Reset_Dependent_Fields (Ctx, F_Identification);
      First := Field_First (Ctx, F_Identification);
      Last := Field_Last (Ctx, F_Identification);
      Insert (Field_Value.Identification_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Identification) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Identification).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Identification)) := (State => S_Invalid, Predecessor => F_Identification);
   end Set_Identification;

   procedure Set_Flag_R (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Flag_R, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.RFLX_Builtin_Types.Boolean_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Flag_R);
      First := Field_First (Ctx, F_Flag_R);
      Last := Field_Last (Ctx, F_Flag_R);
      Insert (Field_Value.Flag_R_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Flag_R) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Flag_R).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Flag_R)) := (State => S_Invalid, Predecessor => F_Flag_R);
   end Set_Flag_R;

   procedure Set_Flag_DF (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Flag_DF, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.RFLX_Builtin_Types.Boolean_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Flag_DF);
      First := Field_First (Ctx, F_Flag_DF);
      Last := Field_Last (Ctx, F_Flag_DF);
      Insert (Field_Value.Flag_DF_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Flag_DF) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Flag_DF).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Flag_DF)) := (State => S_Invalid, Predecessor => F_Flag_DF);
   end Set_Flag_DF;

   procedure Set_Flag_MF (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Flag_MF, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.RFLX_Builtin_Types.Boolean_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Flag_MF);
      First := Field_First (Ctx, F_Flag_MF);
      Last := Field_Last (Ctx, F_Flag_MF);
      Insert (Field_Value.Flag_MF_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Flag_MF) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Flag_MF).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Flag_MF)) := (State => S_Invalid, Predecessor => F_Flag_MF);
   end Set_Flag_MF;

   procedure Set_Fragment_Offset (Ctx : in out Context; Val : RFLX.IPv4.Fragment_Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Fragment_Offset, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Fragment_Offset);
   begin
      Reset_Dependent_Fields (Ctx, F_Fragment_Offset);
      First := Field_First (Ctx, F_Fragment_Offset);
      Last := Field_Last (Ctx, F_Fragment_Offset);
      Insert (Field_Value.Fragment_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Fragment_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Fragment_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Fragment_Offset)) := (State => S_Invalid, Predecessor => F_Fragment_Offset);
   end Set_Fragment_Offset;

   procedure Set_TTL (Ctx : in out Context; Val : RFLX.IPv4.TTL) is
      Field_Value : constant Field_Dependent_Value := (F_TTL, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.TTL);
   begin
      Reset_Dependent_Fields (Ctx, F_TTL);
      First := Field_First (Ctx, F_TTL);
      Last := Field_Last (Ctx, F_TTL);
      Insert (Field_Value.TTL_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_TTL) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_TTL).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_TTL)) := (State => S_Invalid, Predecessor => F_TTL);
   end Set_TTL;

   procedure Set_Protocol (Ctx : in out Context; Val : RFLX.IPv4.Protocol_Enum) is
      Field_Value : constant Field_Dependent_Value := (F_Protocol, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Protocol_Base);
   begin
      Reset_Dependent_Fields (Ctx, F_Protocol);
      First := Field_First (Ctx, F_Protocol);
      Last := Field_Last (Ctx, F_Protocol);
      Insert (Field_Value.Protocol_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Protocol) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Protocol).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Protocol)) := (State => S_Invalid, Predecessor => F_Protocol);
   end Set_Protocol;

   procedure Set_Header_Checksum (Ctx : in out Context; Val : RFLX.IPv4.Header_Checksum) is
      Field_Value : constant Field_Dependent_Value := (F_Header_Checksum, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Header_Checksum);
   begin
      Reset_Dependent_Fields (Ctx, F_Header_Checksum);
      First := Field_First (Ctx, F_Header_Checksum);
      Last := Field_Last (Ctx, F_Header_Checksum);
      Insert (Field_Value.Header_Checksum_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Header_Checksum) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Header_Checksum).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Header_Checksum)) := (State => S_Invalid, Predecessor => F_Header_Checksum);
   end Set_Header_Checksum;

   procedure Set_Source (Ctx : in out Context; Val : RFLX.IPv4.Address) is
      Field_Value : constant Field_Dependent_Value := (F_Source, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Address);
   begin
      Reset_Dependent_Fields (Ctx, F_Source);
      First := Field_First (Ctx, F_Source);
      Last := Field_Last (Ctx, F_Source);
      Insert (Field_Value.Source_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Source) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Source).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Source)) := (State => S_Invalid, Predecessor => F_Source);
   end Set_Source;

   procedure Set_Destination (Ctx : in out Context; Val : RFLX.IPv4.Address) is
      Field_Value : constant Field_Dependent_Value := (F_Destination, To_Base (Val));
      First, Last : RFLX_Types.Bit_Index;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
      function Offset return RFLX_Types.Offset is
        (RFLX_Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new RFLX_Types.Insert (RFLX.IPv4.Address);
   begin
      Reset_Dependent_Fields (Ctx, F_Destination);
      First := Field_First (Ctx, F_Destination);
      Last := Field_Last (Ctx, F_Destination);
      Insert (Field_Value.Destination_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset, RFLX_Types.High_Order_First);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Destination) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Destination).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Destination)) := (State => S_Invalid, Predecessor => F_Destination);
   end Set_Destination;

   procedure Set_Options_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Options);
   begin
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
   end Set_Options_Empty;

   procedure Set_Payload_Empty (Ctx : in out Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Payload);
   begin
      Reset_Dependent_Fields (Ctx, F_Payload);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Payload) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Payload), Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload);
   end Set_Payload_Empty;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : IPv4.Options.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options) + RFLX_Types.To_Bit_Length (IPv4.Options.Byte_Size (Seq_Ctx)) - 1;
      function Buffer_First return RFLX_Types.Index is
        (RFLX_Types.To_Index (First));
      function Buffer_Last return RFLX_Types.Index is
        (RFLX_Types.To_Index (Last));
   begin
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + 7) / 8) * 8, Written_Last => ((Last + 7) / 8) * 8);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
      IPv4.Options.Copy (Seq_Ctx, Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Options;

   procedure Initialize_Options_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Valid_Length (Ctx, F_Options, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Options)) >= Length
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Options)
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
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      Reset_Dependent_Fields (Ctx, F_Options);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
   end Initialize_Options_Private;

   procedure Initialize_Options (Ctx : in out Context) is
   begin
      Initialize_Options_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Options)));
   end Initialize_Options;

   procedure Initialize_Payload_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Valid_Length (Ctx, F_Payload, Length)
       and then RFLX_Types.To_Length (Available_Space (Ctx, F_Payload)) >= Length
       and then Field_First (Ctx, F_Payload) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Payload)
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
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      Reset_Dependent_Fields (Ctx, F_Payload);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Payload) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Payload), Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload);
   end Initialize_Payload_Private;

   procedure Initialize_Payload (Ctx : in out Context) is
   begin
      Initialize_Payload_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Payload)));
   end Initialize_Payload;

   procedure Set_Payload (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Payload_Private (Ctx, Data'Length);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
   end Set_Payload;

   procedure Generic_Set_Payload (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Payload);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Payload (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Payload_Private (Ctx, Length);
   end Generic_Set_Payload;

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out IPv4.Options.Context) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Options);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, F_Options);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Options) then
         Reset_Dependent_Fields (Ctx, F_Options);
         pragma Warnings (Off, "attribute Update is an obsolescent feature");
         Ctx := Ctx'Update (Verified_Last => Last, Written_Last => RFLX_Types.Bit_Length'Max (Ctx.Written_Last, Last));
         pragma Warnings (On, "attribute Update is an obsolescent feature");
         Ctx.Cursors (F_Options) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Options), Predecessor => Ctx.Cursors (F_Options).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Options)) := (State => S_Invalid, Predecessor => F_Options);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      IPv4.Options.Initialize (Seq_Ctx, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Options;

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out IPv4.Options.Context) is
      Valid_Sequence : constant Boolean := IPv4.Options.Valid (Seq_Ctx);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      IPv4.Options.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Options) := (State => S_Valid, First => Ctx.Cursors (F_Options).First, Last => Ctx.Cursors (F_Options).Last, Value => Ctx.Cursors (F_Options).Value, Predecessor => Ctx.Cursors (F_Options).Predecessor);
      end if;
   end Update_Options;

end RFLX.IPv4.Packet;
