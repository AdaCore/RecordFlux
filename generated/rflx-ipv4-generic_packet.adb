package body RFLX.IPv4.Generic_Packet with
  SPARK_Mode
is

   function Create return Context is
     ((RFLX.Types.Index'First, RFLX.Types.Index'First, RFLX.Types.Bit_Index'First, RFLX.Types.Bit_Index'First, 0, null, RFLX.Types.Bit_Index'First, F_Initial, (others => (State => S_Invalid))));

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) is
      Buffer_First : constant RFLX.Types.Index := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index := Buffer'Last;
      Buffer_Address : constant RFLX.Types.Integer_Address := RFLX.Types.Bytes_Address (Buffer);
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, First, F_Initial, (others => (State => S_Invalid)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) is
   begin
      First := Ctx.Cursors (Fld).First;
      Last := Ctx.Cursors (Fld).Last;
   end Field_Range;

   function Index (Ctx : Context) return RFLX.Types.Bit_Index is
     (Ctx.Index);

   function Preliminary_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid
        or Ctx.Cursors (Fld).State = S_Preliminary)
      and then Ctx.Cursors (Fld).Value.Fld = Fld);

   function Preliminary_Valid_Predecessors (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial | F_Version =>
            True,
         when F_IHL =>
            Preliminary_Valid (Ctx, F_Version),
         when F_DSCP =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL),
         when F_ECN =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP),
         when F_Total_Length =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN),
         when F_Identification =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length),
         when F_Flag_R =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification),
         when F_Flag_DF =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R),
         when F_Flag_MF =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF),
         when F_Fragment_Offset =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF),
         when F_TTL =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset),
         when F_Protocol =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL),
         when F_Header_Checksum =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL)
               and then Preliminary_Valid (Ctx, F_Protocol),
         when F_Source =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL)
               and then Preliminary_Valid (Ctx, F_Protocol)
               and then Preliminary_Valid (Ctx, F_Header_Checksum),
         when F_Destination =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL)
               and then Preliminary_Valid (Ctx, F_Protocol)
               and then Preliminary_Valid (Ctx, F_Header_Checksum)
               and then Preliminary_Valid (Ctx, F_Source),
         when F_Options | F_Payload =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL)
               and then Preliminary_Valid (Ctx, F_Protocol)
               and then Preliminary_Valid (Ctx, F_Header_Checksum)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Destination),
         when F_Final =>
            Preliminary_Valid (Ctx, F_Version)
               and then Preliminary_Valid (Ctx, F_IHL)
               and then Preliminary_Valid (Ctx, F_DSCP)
               and then Preliminary_Valid (Ctx, F_ECN)
               and then Preliminary_Valid (Ctx, F_Total_Length)
               and then Preliminary_Valid (Ctx, F_Identification)
               and then Preliminary_Valid (Ctx, F_Flag_R)
               and then Preliminary_Valid (Ctx, F_Flag_DF)
               and then Preliminary_Valid (Ctx, F_Flag_MF)
               and then Preliminary_Valid (Ctx, F_Fragment_Offset)
               and then Preliminary_Valid (Ctx, F_TTL)
               and then Preliminary_Valid (Ctx, F_Protocol)
               and then Preliminary_Valid (Ctx, F_Header_Checksum)
               and then Preliminary_Valid (Ctx, F_Source)
               and then Preliminary_Valid (Ctx, F_Destination)
               and then Preliminary_Valid (Ctx, F_Payload)));

   function Valid_Predecessors (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Version =>
            True,
         when F_IHL =>
            Present (Ctx, F_Version),
         when F_DSCP =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL),
         when F_ECN =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP),
         when F_Total_Length =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN),
         when F_Identification =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length),
         when F_Flag_R =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification),
         when F_Flag_DF =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R),
         when F_Flag_MF =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF),
         when F_Fragment_Offset =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF),
         when F_TTL =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset),
         when F_Protocol =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset)
               and then Present (Ctx, F_TTL),
         when F_Header_Checksum =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset)
               and then Present (Ctx, F_TTL)
               and then Present (Ctx, F_Protocol),
         when F_Source =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset)
               and then Present (Ctx, F_TTL)
               and then Present (Ctx, F_Protocol)
               and then Present (Ctx, F_Header_Checksum),
         when F_Destination =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset)
               and then Present (Ctx, F_TTL)
               and then Present (Ctx, F_Protocol)
               and then Present (Ctx, F_Header_Checksum)
               and then Present (Ctx, F_Source),
         when F_Options | F_Payload =>
            Present (Ctx, F_Version)
               and then Present (Ctx, F_IHL)
               and then Present (Ctx, F_DSCP)
               and then Present (Ctx, F_ECN)
               and then Present (Ctx, F_Total_Length)
               and then Present (Ctx, F_Identification)
               and then Present (Ctx, F_Flag_R)
               and then Present (Ctx, F_Flag_DF)
               and then Present (Ctx, F_Flag_MF)
               and then Present (Ctx, F_Fragment_Offset)
               and then Present (Ctx, F_TTL)
               and then Present (Ctx, F_Protocol)
               and then Present (Ctx, F_Header_Checksum)
               and then Present (Ctx, F_Source)
               and then Present (Ctx, F_Destination)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Ctx, Fld));

   function Valid_Target (Source_Field, Target_Field : Virtual_Field) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            Target_Field = F_Version,
         when F_Version =>
            Target_Field = F_IHL,
         when F_IHL =>
            Target_Field = F_DSCP,
         when F_DSCP =>
            Target_Field = F_ECN,
         when F_ECN =>
            Target_Field = F_Total_Length,
         when F_Total_Length =>
            Target_Field = F_Identification,
         when F_Identification =>
            Target_Field = F_Flag_R,
         when F_Flag_R =>
            Target_Field = F_Flag_DF,
         when F_Flag_DF =>
            Target_Field = F_Flag_MF,
         when F_Flag_MF =>
            Target_Field = F_Fragment_Offset,
         when F_Fragment_Offset =>
            Target_Field = F_TTL,
         when F_TTL =>
            Target_Field = F_Protocol,
         when F_Protocol =>
            Target_Field = F_Header_Checksum,
         when F_Header_Checksum =>
            Target_Field = F_Source,
         when F_Source =>
            Target_Field = F_Destination,
         when F_Destination =>
            Target_Field = F_Payload
               or Target_Field = F_Options,
         when F_Options =>
            Target_Field = F_Payload,
         when F_Payload =>
            Target_Field = F_Final,
         when F_Final =>
            False));

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
         when F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination =>
            False,
         when F_Options | F_Payload =>
            True));

   function Field_Condition (Ctx : Context; Source_Field, Target_Field : Virtual_Field) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            (case Target_Field is
                  when F_Version =>
                     True,
                  when others =>
                     False),
         when F_Version =>
            (case Target_Field is
                  when F_IHL =>
                     True,
                  when others =>
                     False),
         when F_IHL =>
            (case Target_Field is
                  when F_DSCP =>
                     True,
                  when others =>
                     False),
         when F_DSCP =>
            (case Target_Field is
                  when F_ECN =>
                     True,
                  when others =>
                     False),
         when F_ECN =>
            (case Target_Field is
                  when F_Total_Length =>
                     True,
                  when others =>
                     False),
         when F_Total_Length =>
            (case Target_Field is
                  when F_Identification =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) * 4,
                  when others =>
                     False),
         when F_Identification =>
            (case Target_Field is
                  when F_Flag_R =>
                     True,
                  when others =>
                     False),
         when F_Flag_R =>
            (case Target_Field is
                  when F_Flag_DF =>
                     Ctx.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False),
                  when others =>
                     False),
         when F_Flag_DF =>
            (case Target_Field is
                  when F_Flag_MF =>
                     True,
                  when others =>
                     False),
         when F_Flag_MF =>
            (case Target_Field is
                  when F_Fragment_Offset =>
                     True,
                  when others =>
                     False),
         when F_Fragment_Offset =>
            (case Target_Field is
                  when F_TTL =>
                     True,
                  when others =>
                     False),
         when F_TTL =>
            (case Target_Field is
                  when F_Protocol =>
                     True,
                  when others =>
                     False),
         when F_Protocol =>
            (case Target_Field is
                  when F_Header_Checksum =>
                     True,
                  when others =>
                     False),
         when F_Header_Checksum =>
            (case Target_Field is
                  when F_Source =>
                     True,
                  when others =>
                     False),
         when F_Source =>
            (case Target_Field is
                  when F_Destination =>
                     True,
                  when others =>
                     False),
         when F_Destination =>
            (case Target_Field is
                  when F_Payload =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) = 5,
                  when F_Options =>
                     RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) > 5,
                  when others =>
                     False),
         when F_Options =>
            (case Target_Field is
                  when F_Payload =>
                     True,
                  when others =>
                     False),
         when F_Payload =>
            (case Target_Field is
                  when F_Final =>
                     True,
                  when others =>
                     False),
         when F_Final =>
            False))
    with
     Pre =>
       Valid_Target (Source_Field, Target_Field)
          and then Preliminary_Valid_Predecessors (Ctx, Target_Field);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length is
     ((case Ctx.Fld is
         when F_Initial =>
            (case Fld is
                  when F_Version =>
                     Version_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Version =>
            (case Fld is
                  when F_IHL =>
                     IHL_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_IHL =>
            (case Fld is
                  when F_DSCP =>
                     DCSP'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_DSCP =>
            (case Fld is
                  when F_ECN =>
                     ECN'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_ECN =>
            (case Fld is
                  when F_Total_Length =>
                     Total_Length_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Total_Length =>
            (case Fld is
                  when F_Identification =>
                     Identification'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Identification =>
            (case Fld is
                  when F_Flag_R =>
                     Flag_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Flag_R =>
            (case Fld is
                  when F_Flag_DF =>
                     Flag_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Flag_DF =>
            (case Fld is
                  when F_Flag_MF =>
                     Flag_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Flag_MF =>
            (case Fld is
                  when F_Fragment_Offset =>
                     Fragment_Offset'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Fragment_Offset =>
            (case Fld is
                  when F_TTL =>
                     TTL'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_TTL =>
            (case Fld is
                  when F_Protocol =>
                     Protocol_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Protocol =>
            (case Fld is
                  when F_Header_Checksum =>
                     Header_Checksum'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Header_Checksum =>
            (case Fld is
                  when F_Source =>
                     Address'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Source =>
            (case Fld is
                  when F_Destination =>
                     Address'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Destination =>
            (case Fld is
                  when F_Payload =>
                     (RFLX.Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) * 8 - 160),
                  when F_Options =>
                     (RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) * 32 - 160),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Options =>
            (case Fld is
                  when F_Payload =>
                     (RFLX.Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) * 8 + RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) * (-32)),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length),
         when F_Payload | F_Final =>
            0))
    with
     Pre =>
       Valid_Target (Ctx.Fld, Fld)
          and then Valid_Predecessors (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index is
     ((case Ctx.Fld is
         when F_Initial | F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination | F_Options | F_Payload | F_Final =>
            Ctx.Index))
    with
     Pre =>
       Valid_Target (Ctx.Fld, Fld)
          and then Valid_Predecessors (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Field_Postcondition (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Version =>
            Field_Condition (Ctx, Fld, F_IHL),
         when F_IHL =>
            Field_Condition (Ctx, Fld, F_DSCP),
         when F_DSCP =>
            Field_Condition (Ctx, Fld, F_ECN),
         when F_ECN =>
            Field_Condition (Ctx, Fld, F_Total_Length),
         when F_Total_Length =>
            Field_Condition (Ctx, Fld, F_Identification),
         when F_Identification =>
            Field_Condition (Ctx, Fld, F_Flag_R),
         when F_Flag_R =>
            Field_Condition (Ctx, Fld, F_Flag_DF),
         when F_Flag_DF =>
            Field_Condition (Ctx, Fld, F_Flag_MF),
         when F_Flag_MF =>
            Field_Condition (Ctx, Fld, F_Fragment_Offset),
         when F_Fragment_Offset =>
            Field_Condition (Ctx, Fld, F_TTL),
         when F_TTL =>
            Field_Condition (Ctx, Fld, F_Protocol),
         when F_Protocol =>
            Field_Condition (Ctx, Fld, F_Header_Checksum),
         when F_Header_Checksum =>
            Field_Condition (Ctx, Fld, F_Source),
         when F_Source =>
            Field_Condition (Ctx, Fld, F_Destination),
         when F_Destination =>
            Field_Condition (Ctx, Fld, F_Payload)
               or Field_Condition (Ctx, Fld, F_Options),
         when F_Options =>
            Field_Condition (Ctx, Fld, F_Payload),
         when F_Payload =>
            Field_Condition (Ctx, Fld, F_Final)))
    with
     Pre =>
       Valid_Predecessors (Ctx, Fld)
          and then Preliminary_Valid (Ctx, Fld);

   function Valid_Context (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Target (Ctx.Fld, Fld)
      and then Valid_Predecessors (Ctx, Fld)
      and then Ctx.Buffer /= null);

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and then Ctx.First <= RFLX.Types.Bit_Index'Last / 2
      and then Field_First (Ctx, Fld) <= RFLX.Types.Bit_Index'Last / 2
      and then Field_Length (Ctx, Fld) >= 0
      and then Field_Length (Ctx, Fld) <= RFLX.Types.Bit_Length'Last / 2
      and then (Field_First (Ctx, Fld) + Field_Length (Ctx, Fld)) <= RFLX.Types.Bit_Length'Last / 2
      and then Ctx.First <= Field_First (Ctx, Fld)
      and then Ctx.Last >= ((Field_First (Ctx, Fld) + Field_Length (Ctx, Fld))) - 1)
    with
     Pre =>
       Valid_Context (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld);

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Valid_Context (Ctx, Fld)
          and then Field_Condition (Ctx, Ctx.Fld, Fld)
          and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
      First : constant RFLX.Types.Bit_Index := Field_First (Ctx, Fld);
      Length : constant RFLX.Types.Bit_Length := Field_Length (Ctx, Fld);
      function Buffer_First return RFLX.Types.Index is
        (RFLX.Types.Byte_Index (First));
      function Buffer_Last return RFLX.Types.Index is
        (RFLX.Types.Byte_Index ((First + Length - 1)))
       with
        Pre =>
          Length >= 1;
      function Offset return RFLX.Types.Offset is
        (RFLX.Types.Offset ((8 - ((First + Length - 1)) mod 8) mod 8));
   begin
      return ((case Fld is
            when F_Version =>
               (Fld => F_Version, Version_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_IHL =>
               (Fld => F_IHL, IHL_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_DSCP =>
               (Fld => F_DSCP, DSCP_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_ECN =>
               (Fld => F_ECN, ECN_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Total_Length =>
               (Fld => F_Total_Length, Total_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Identification =>
               (Fld => F_Identification, Identification_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_R =>
               (Fld => F_Flag_R, Flag_R_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_DF =>
               (Fld => F_Flag_DF, Flag_DF_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_MF =>
               (Fld => F_Flag_MF, Flag_MF_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Fragment_Offset =>
               (Fld => F_Fragment_Offset, Fragment_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TTL =>
               (Fld => F_TTL, TTL_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Protocol =>
               (Fld => F_Protocol, Protocol_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Header_Checksum =>
               (Fld => F_Header_Checksum, Header_Checksum_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Source =>
               (Fld => F_Source, Source_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Destination =>
               (Fld => F_Destination, Destination_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Options =>
               (Fld => F_Options),
            when F_Payload =>
               (Fld => F_Payload)));
   end Get_Field_Value;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      First : RFLX.Types.Bit_Index;
      Last : RFLX.Types.Bit_Length;
      Value : Field_Dependent_Value;
   begin
      if Valid_Context (Ctx, Fld) then
         if Field_Condition (Ctx, Ctx.Fld, Fld) then
            if Sufficient_Buffer_Length (Ctx, Fld) then
               First := Field_First (Ctx, Fld);
               Last := ((First + Field_Length (Ctx, Fld))) - 1;
               Value := Get_Field_Value (Ctx, Fld);
               Ctx.Cursors (Fld) := (State => S_Preliminary, First => First, Last => Last, Value => Value);
               if Valid_Value (Value)
                  and then Field_Postcondition (Ctx, Fld) then
                  if Composite_Field (Fld) then
                     Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => First, Last => Last, Value => Value);
                  else
                     Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Value);
                  end if;
                  Ctx.Index := (Last + 1);
                  Ctx.Fld := Fld;
               else
                  Ctx.Cursors (Fld) := (State => S_Invalid);
               end if;
            else
               Ctx.Cursors (Fld) := (State => S_Incomplete);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Invalid);
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

   function Present (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid)
      and then Ctx.Cursors (Fld).Value.Fld = Fld
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).Value.Fld = Fld
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Version)
      and then Valid (Ctx, F_IHL)
      and then Valid (Ctx, F_DSCP)
      and then Valid (Ctx, F_ECN)
      and then Valid (Ctx, F_Total_Length)
      and then Valid (Ctx, F_Identification)
      and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) * 4
      and then Valid (Ctx, F_Flag_R)
      and then Valid (Ctx, F_Flag_DF)
      and then Ctx.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
      and then Valid (Ctx, F_Flag_MF)
      and then Valid (Ctx, F_Fragment_Offset)
      and then Valid (Ctx, F_TTL)
      and then Valid (Ctx, F_Protocol)
      and then Valid (Ctx, F_Header_Checksum)
      and then Valid (Ctx, F_Source)
      and then Valid (Ctx, F_Destination)
      and then ((Structural_Valid (Ctx, F_Payload)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) = 5)
        or (Structural_Valid (Ctx, F_Options)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) > 5
          and then Structural_Valid (Ctx, F_Payload))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Version)
      and then Valid (Ctx, F_IHL)
      and then Valid (Ctx, F_DSCP)
      and then Valid (Ctx, F_ECN)
      and then Valid (Ctx, F_Total_Length)
      and then Valid (Ctx, F_Identification)
      and then RFLX.Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) * 4
      and then Valid (Ctx, F_Flag_R)
      and then Valid (Ctx, F_Flag_DF)
      and then Ctx.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
      and then Valid (Ctx, F_Flag_MF)
      and then Valid (Ctx, F_Fragment_Offset)
      and then Valid (Ctx, F_TTL)
      and then Valid (Ctx, F_Protocol)
      and then Valid (Ctx, F_Header_Checksum)
      and then Valid (Ctx, F_Source)
      and then Valid (Ctx, F_Destination)
      and then ((Valid (Ctx, F_Payload)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) = 5)
        or (Valid (Ctx, F_Options)
          and then RFLX.Types.Bit_Length (Ctx.Cursors (F_IHL).Value.IHL_Value) > 5
          and then Valid (Ctx, F_Payload))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Version)
      or Incomplete (Ctx, F_IHL)
      or Incomplete (Ctx, F_DSCP)
      or Incomplete (Ctx, F_ECN)
      or Incomplete (Ctx, F_Total_Length)
      or Incomplete (Ctx, F_Identification)
      or Incomplete (Ctx, F_Flag_R)
      or Incomplete (Ctx, F_Flag_DF)
      or Incomplete (Ctx, F_Flag_MF)
      or Incomplete (Ctx, F_Fragment_Offset)
      or Incomplete (Ctx, F_TTL)
      or Incomplete (Ctx, F_Protocol)
      or Incomplete (Ctx, F_Header_Checksum)
      or Incomplete (Ctx, F_Source)
      or Incomplete (Ctx, F_Destination)
      or Incomplete (Ctx, F_Options)
      or Incomplete (Ctx, F_Payload));

   function Get_Version (Ctx : Context) return Version is
     (Ctx.Cursors (F_Version).Value.Version_Value);

   function Get_IHL (Ctx : Context) return IHL is
     (Ctx.Cursors (F_IHL).Value.IHL_Value);

   function Get_DSCP (Ctx : Context) return DCSP is
     (Ctx.Cursors (F_DSCP).Value.DSCP_Value);

   function Get_ECN (Ctx : Context) return ECN is
     (Ctx.Cursors (F_ECN).Value.ECN_Value);

   function Get_Total_Length (Ctx : Context) return Total_Length is
     (Ctx.Cursors (F_Total_Length).Value.Total_Length_Value);

   function Get_Identification (Ctx : Context) return Identification is
     (Ctx.Cursors (F_Identification).Value.Identification_Value);

   function Get_Flag_R (Ctx : Context) return Flag is
     (Convert (Ctx.Cursors (F_Flag_R).Value.Flag_R_Value));

   function Get_Flag_DF (Ctx : Context) return Flag is
     (Convert (Ctx.Cursors (F_Flag_DF).Value.Flag_DF_Value));

   function Get_Flag_MF (Ctx : Context) return Flag is
     (Convert (Ctx.Cursors (F_Flag_MF).Value.Flag_MF_Value));

   function Get_Fragment_Offset (Ctx : Context) return Fragment_Offset is
     (Ctx.Cursors (F_Fragment_Offset).Value.Fragment_Offset_Value);

   function Get_TTL (Ctx : Context) return TTL is
     (Ctx.Cursors (F_TTL).Value.TTL_Value);

   function Get_Protocol (Ctx : Context) return Protocol is
     (Convert (Ctx.Cursors (F_Protocol).Value.Protocol_Value));

   function Get_Header_Checksum (Ctx : Context) return Header_Checksum is
     (Ctx.Cursors (F_Header_Checksum).Value.Header_Checksum_Value);

   function Get_Source (Ctx : Context) return Address is
     (Ctx.Cursors (F_Source).Value.Source_Value);

   function Get_Destination (Ctx : Context) return Address is
     (Ctx.Cursors (F_Destination).Value.Destination_Value);

   procedure Get_Options (Ctx : Context) is
      First : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Options).First);
      Last : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Options).Last);
   begin
      Process_Options (Ctx.Buffer.all (First .. Last));
   end Get_Options;

   procedure Get_Payload (Ctx : Context) is
      First : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).First);
      Last : constant RFLX.Types.Index := RFLX.Types.Byte_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Process_Payload (Ctx.Buffer.all (First .. Last));
   end Get_Payload;

   procedure Switch (Ctx : in out Context; Sequence_Context : out Options_Sequence.Context) is
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Options_Sequence.Initialize (Sequence_Context, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.Cursors (F_Options).First, Ctx.Cursors (F_Options).Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Ctx : in out Context; Sequence_Context : in out Options_Sequence.Context) is
      Valid_Sequence : constant Boolean := Options_Sequence.Valid (Sequence_Context);
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Options_Sequence.Take_Buffer (Sequence_Context, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Options) := (State => S_Valid, First => Ctx.Cursors (F_Options).First, Last => Ctx.Cursors (F_Options).Last, Value => Ctx.Cursors (F_Options).Value);
      end if;
   end Update;

end RFLX.IPv4.Generic_Packet;
