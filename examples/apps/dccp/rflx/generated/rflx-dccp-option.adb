pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operations;

package body RFLX.DCCP.Option with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Option_Type => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Option_Type => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
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
          when F_Option_Type =>
             (if
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                 or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                 or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))
              then
                 F_Final
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))
              then
                 F_Option_Length
              else
                 F_Initial),
          when F_Option_Length =>
             (if
                 Ctx.Cursors (F_Option_Length).Value >= 4
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME))
              then
                 F_Elapsed_Time_Opt
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))
              then
                 F_Loss_Event_Rate
              elsif
                 Ctx.Cursors (F_Option_Length).Value >= 3
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT))
              then
                 F_NDP_Count_Opt
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
                 or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                 or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
                 or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R))
              then
                 F_Option_Feature
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))
              then
                 F_Receive_Rate
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
              then
                 F_Timestamp_Echo_Opt
              elsif
                 RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))
              then
                 F_Timestamp_Option
              else
                 F_Initial),
          when F_Loss_Event_Rate | F_NDP_Count_Opt =>
             F_Final,
          when F_Option_Feature =>
             (if
                 (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R)))
                 and (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Feature).Value) < RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.FEATURE_RESERVED))
                      or Ctx.Cursors (F_Option_Feature).Value > 255)
              then
                 F_Final
              elsif
                 True
              then
                 F_Option_Value
              else
                 F_Initial),
          when F_Receive_Rate =>
             F_Final,
          when F_Timestamp_Echo_Opt =>
             (if
                 Ctx.Cursors (F_Option_Length).Value >= 8
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
              then
                 F_Elapsed_Time_Opt
              else
                 F_Initial),
          when F_Timestamp_Option | F_Option_Value | F_Elapsed_Time_Opt =>
             F_Final))
    with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Well_Formed (Ctx, Fld)
       and RFLX.DCCP.Option.Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Option_Type =>
             Invalid (Ctx.Cursors (F_Option_Length)),
          when F_Option_Length =>
             Invalid (Ctx.Cursors (F_Elapsed_Time_Opt))
             and Invalid (Ctx.Cursors (F_Loss_Event_Rate))
             and Invalid (Ctx.Cursors (F_NDP_Count_Opt))
             and Invalid (Ctx.Cursors (F_Option_Feature))
             and Invalid (Ctx.Cursors (F_Receive_Rate))
             and Invalid (Ctx.Cursors (F_Timestamp_Echo_Opt))
             and Invalid (Ctx.Cursors (F_Timestamp_Option)),
          when F_Loss_Event_Rate | F_NDP_Count_Opt =>
             True,
          when F_Option_Feature =>
             Invalid (Ctx.Cursors (F_Option_Value)),
          when F_Receive_Rate =>
             True,
          when F_Timestamp_Echo_Opt =>
             Invalid (Ctx.Cursors (F_Elapsed_Time_Opt)),
          when F_Timestamp_Option | F_Option_Value | F_Elapsed_Time_Opt =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_NDP_Count_Opt | F_Option_Value | F_Elapsed_Time_Opt =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld))))),
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld),
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
     (Fld in F_NDP_Count_Opt | F_Option_Value | F_Elapsed_Time_Opt);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.Base_Integer with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Option.Sufficient_Buffer_Length (Ctx, Fld)
       and then not RFLX.DCCP.Option.Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Option_Type | F_Option_Length =>
             8,
          when F_Loss_Event_Rate =>
             32,
          when F_Option_Feature =>
             8,
          when F_Receive_Rate | F_Timestamp_Echo_Opt | F_Timestamp_Option =>
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
               pragma Assert ((if
                                  Fld = F_Elapsed_Time_Opt
                                  or Fld = F_Loss_Event_Rate
                                  or Fld = F_NDP_Count_Opt
                                  or Fld = F_Option_Feature
                                  or Fld = F_Option_Type
                                  or Fld = F_Option_Value
                                  or Fld = F_Receive_Rate
                                  or Fld = F_Timestamp_Option
                               then
                                  Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
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

   function Get_NDP_Count_Opt (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_NDP_Count_Opt;

   function Get_Option_Value (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Option_Value;

   function Get_Elapsed_Time_Opt (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Elapsed_Time_Opt;

   procedure Get_NDP_Count_Opt (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_NDP_Count_Opt;

   procedure Get_Option_Value (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Option_Value;

   procedure Get_Elapsed_Time_Opt (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Elapsed_Time_Opt;

   procedure Generic_Get_NDP_Count_Opt (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_NDP_Count_Opt).Last);
   begin
      Process_NDP_Count_Opt (Ctx.Buffer.all (First .. Last));
   end Generic_Get_NDP_Count_Opt;

   procedure Generic_Get_Option_Value (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      Process_Option_Value (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Option_Value;

   procedure Generic_Get_Elapsed_Time_Opt (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Elapsed_Time_Opt).Last);
   begin
      Process_Elapsed_Time_Opt (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Elapsed_Time_Opt;

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Option.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Option.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.DCCP.Option.Available_Space (Ctx, Fld)
       and then (if RFLX.DCCP.Option.Composite_Field (Fld) then Size mod RFLX_Types.Byte'Size = 0 else State_Valid),
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
                    when F_Option_Type =>
                       Get_Option_Type (Ctx) = To_Actual (Val)
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))
                            then
                               Predecessor (Ctx, F_Option_Length) = F_Option_Type
                               and Valid_Next (Ctx, F_Option_Length))
                       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Option_Length =>
                       Get_Option_Length (Ctx) = To_Actual (Val)
                       and (if
                               Get_Option_Length (Ctx) >= 4
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME))
                            then
                               Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Option_Length
                               and Valid_Next (Ctx, F_Elapsed_Time_Opt))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))
                            then
                               Predecessor (Ctx, F_Loss_Event_Rate) = F_Option_Length
                               and Valid_Next (Ctx, F_Loss_Event_Rate))
                       and (if
                               Get_Option_Length (Ctx) >= 3
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT))
                            then
                               Predecessor (Ctx, F_NDP_Count_Opt) = F_Option_Length
                               and Valid_Next (Ctx, F_NDP_Count_Opt))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
                               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
                               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R))
                            then
                               Predecessor (Ctx, F_Option_Feature) = F_Option_Length
                               and Valid_Next (Ctx, F_Option_Feature))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))
                            then
                               Predecessor (Ctx, F_Receive_Rate) = F_Option_Length
                               and Valid_Next (Ctx, F_Receive_Rate))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
                            then
                               Predecessor (Ctx, F_Timestamp_Echo_Opt) = F_Option_Length
                               and Valid_Next (Ctx, F_Timestamp_Echo_Opt))
                       and (if
                               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))
                            then
                               Predecessor (Ctx, F_Timestamp_Option) = F_Option_Length
                               and Valid_Next (Ctx, F_Timestamp_Option)),
                    when F_Loss_Event_Rate =>
                       Get_Loss_Event_Rate (Ctx) = To_Actual (Val)
                       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_NDP_Count_Opt =>
                       (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Option_Feature =>
                       Get_Option_Feature (Ctx) = To_Actual (Val)
                       and (Predecessor (Ctx, F_Option_Value) = F_Option_Feature
                            and Valid_Next (Ctx, F_Option_Value))
                       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Receive_Rate =>
                       Get_Receive_Rate (Ctx) = To_Actual (Val)
                       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Timestamp_Echo_Opt =>
                       Get_Timestamp_Echo_Opt (Ctx) = To_Actual (Val)
                       and (if
                               Get_Option_Length (Ctx) >= 8
                               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
                            then
                               Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Timestamp_Echo_Opt
                               and Valid_Next (Ctx, F_Elapsed_Time_Opt)),
                    when F_Timestamp_Option =>
                       Get_Timestamp_Option (Ctx) = To_Actual (Val)
                       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
                    when F_Option_Value | F_Elapsed_Time_Opt =>
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
                         when F_Option_Type | F_Option_Length =>
                            8,
                         when F_Loss_Event_Rate =>
                            32,
                         when F_NDP_Count_Opt =>
                            RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 16,
                         when F_Option_Feature =>
                            8,
                         when F_Receive_Rate | F_Timestamp_Echo_Opt | F_Timestamp_Option =>
                            32,
                         when F_Option_Value =>
                            8,
                         when F_Elapsed_Time_Opt =>
                            (if
                                Ctx.Cursors (Fld).Predecessor = F_Option_Length
                                and then (Ctx.Cursors (F_Option_Length).Value >= 4
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME)))
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 16
                             elsif
                                Ctx.Cursors (Fld).Predecessor = F_Timestamp_Echo_Opt
                                and then (Ctx.Cursors (F_Option_Length).Value >= 8
                                          and RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO)))
                             then
                                RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 48
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
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, Fld)
       and then Fld in F_Option_Type | F_Option_Length | F_Loss_Event_Rate | F_Option_Feature | F_Receive_Rate | F_Timestamp_Echo_Opt | F_Timestamp_Option
       and then RFLX.DCCP.Option.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Option.Valid_Size (Ctx, Fld, RFLX.DCCP.Option.Field_Size (Ctx, Fld))
       and then RFLX.DCCP.Option.Available_Space (Ctx, Fld) >= RFLX.DCCP.Option.Field_Size (Ctx, Fld)
       and then RFLX.DCCP.Option.Field_Size (Ctx, Fld) in 1 .. RFLX_Types.Base_Integer'Size
       and then RFLX_Types.Fits_Into (Val, Natural (RFLX.DCCP.Option.Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (case Fld is
               when F_Option_Type =>
                  Get_Option_Type (Ctx) = To_Actual (Val)
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))
                       then
                          Predecessor (Ctx, F_Option_Length) = F_Option_Type
                          and Valid_Next (Ctx, F_Option_Length))
                  and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Option_Length =>
                  Get_Option_Length (Ctx) = To_Actual (Val)
                  and (if
                          Get_Option_Length (Ctx) >= 4
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME))
                       then
                          Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Option_Length
                          and Valid_Next (Ctx, F_Elapsed_Time_Opt))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))
                       then
                          Predecessor (Ctx, F_Loss_Event_Rate) = F_Option_Length
                          and Valid_Next (Ctx, F_Loss_Event_Rate))
                  and (if
                          Get_Option_Length (Ctx) >= 3
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT))
                       then
                          Predecessor (Ctx, F_NDP_Count_Opt) = F_Option_Length
                          and Valid_Next (Ctx, F_NDP_Count_Opt))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
                          or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                          or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
                          or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R))
                       then
                          Predecessor (Ctx, F_Option_Feature) = F_Option_Length
                          and Valid_Next (Ctx, F_Option_Feature))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))
                       then
                          Predecessor (Ctx, F_Receive_Rate) = F_Option_Length
                          and Valid_Next (Ctx, F_Receive_Rate))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
                       then
                          Predecessor (Ctx, F_Timestamp_Echo_Opt) = F_Option_Length
                          and Valid_Next (Ctx, F_Timestamp_Echo_Opt))
                  and (if
                          RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))
                       then
                          Predecessor (Ctx, F_Timestamp_Option) = F_Option_Length
                          and Valid_Next (Ctx, F_Timestamp_Option)),
               when F_Loss_Event_Rate =>
                  Get_Loss_Event_Rate (Ctx) = To_Actual (Val)
                  and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_NDP_Count_Opt =>
                  (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Option_Feature =>
                  Get_Option_Feature (Ctx) = To_Actual (Val)
                  and (Predecessor (Ctx, F_Option_Value) = F_Option_Feature
                       and Valid_Next (Ctx, F_Option_Value))
                  and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Receive_Rate =>
                  Get_Receive_Rate (Ctx) = To_Actual (Val)
                  and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Timestamp_Echo_Opt =>
                  Get_Timestamp_Echo_Opt (Ctx) = To_Actual (Val)
                  and (if
                          Get_Option_Length (Ctx) >= 8
                          and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
                       then
                          Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Timestamp_Echo_Opt
                          and Valid_Next (Ctx, F_Elapsed_Time_Opt)),
               when F_Timestamp_Option =>
                  Get_Timestamp_Option (Ctx) = To_Actual (Val)
                  and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, Fld)),
               when F_Option_Value | F_Elapsed_Time_Opt =>
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

   procedure Set_Option_Type (Ctx : in out Context; Val : RFLX.DCCP.Opt_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Type, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Option_Type;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.DCCP.Option_Length_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Length, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Option_Length;

   procedure Set_Loss_Event_Rate (Ctx : in out Context; Val : RFLX.DCCP.Loss_Rate_Type) is
   begin
      Set_Scalar (Ctx, F_Loss_Event_Rate, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Loss_Event_Rate;

   procedure Set_Option_Feature (Ctx : in out Context; Val : RFLX.DCCP.Option_Feature_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Feature, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Option_Feature;

   procedure Set_Receive_Rate (Ctx : in out Context; Val : RFLX.DCCP.Receive_Rate_Type) is
   begin
      Set_Scalar (Ctx, F_Receive_Rate, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Receive_Rate;

   procedure Set_Timestamp_Echo_Opt (Ctx : in out Context; Val : RFLX.DCCP.Timestamp_Echo_Option_Type) is
   begin
      Set_Scalar (Ctx, F_Timestamp_Echo_Opt, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Timestamp_Echo_Opt;

   procedure Set_Timestamp_Option (Ctx : in out Context; Val : RFLX.DCCP.Timestamp_Option_Type) is
   begin
      Set_Scalar (Ctx, F_Timestamp_Option, RFLX.DCCP.To_Base_Integer (Val));
   end Set_Timestamp_Option;

   procedure Initialize_NDP_Count_Opt_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)) >= Length
       and then RFLX.DCCP.Option.Field_First (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_NDP_Count_Opt)
       and Field_Size (Ctx, F_NDP_Count_Opt) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_NDP_Count_Opt)
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_NDP_Count_Opt) = Predecessor (Ctx, F_NDP_Count_Opt)'Old
       and Valid_Next (Ctx, F_NDP_Count_Opt) = Valid_Next (Ctx, F_NDP_Count_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_NDP_Count_Opt) = Field_First (Ctx, F_NDP_Count_Opt)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_NDP_Count_Opt);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_NDP_Count_Opt) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_NDP_Count_Opt);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_NDP_Count_Opt) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_NDP_Count_Opt).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_NDP_Count_Opt)) := (State => S_Invalid, Predecessor => F_NDP_Count_Opt);
   end Initialize_NDP_Count_Opt_Private;

   procedure Initialize_NDP_Count_Opt (Ctx : in out Context) is
   begin
      Initialize_NDP_Count_Opt_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_NDP_Count_Opt)));
   end Initialize_NDP_Count_Opt;

   procedure Initialize_Option_Value_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Option_Value, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value)) >= Length
       and then RFLX.DCCP.Option.Field_First (Ctx, RFLX.DCCP.Option.F_Option_Value) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Value)
       and Field_Size (Ctx, F_Option_Value) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Value) = Predecessor (Ctx, F_Option_Value)'Old
       and Valid_Next (Ctx, F_Option_Value) = Valid_Next (Ctx, F_Option_Value)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Get_Option_Feature (Ctx) = Get_Option_Feature (Ctx)'Old
       and Field_First (Ctx, F_Option_Value) = Field_First (Ctx, F_Option_Value)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Value);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Value) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Option_Value);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Value) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Option_Value).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Value)) := (State => S_Invalid, Predecessor => F_Option_Value);
   end Initialize_Option_Value_Private;

   procedure Initialize_Option_Value (Ctx : in out Context) is
   begin
      Initialize_Option_Value_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Option_Value)));
   end Initialize_Option_Value;

   procedure Initialize_Elapsed_Time_Opt_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)) >= Length
       and then RFLX.DCCP.Option.Field_First (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt) mod RFLX_Types.Byte'Size = 1,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Elapsed_Time_Opt)
       and Field_Size (Ctx, F_Elapsed_Time_Opt) = RFLX_Types.To_Bit_Length (Length)
       and Ctx.Verified_Last = Field_Last (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Elapsed_Time_Opt) = Predecessor (Ctx, F_Elapsed_Time_Opt)'Old
       and Valid_Next (Ctx, F_Elapsed_Time_Opt) = Valid_Next (Ctx, F_Elapsed_Time_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Elapsed_Time_Opt) = Field_First (Ctx, F_Elapsed_Time_Opt)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Elapsed_Time_Opt);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Elapsed_Time_Opt) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Elapsed_Time_Opt);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Elapsed_Time_Opt) := (State => S_Well_Formed, First => First, Last => Last, Value => 0, Predecessor => Ctx.Cursors (F_Elapsed_Time_Opt).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Elapsed_Time_Opt)) := (State => S_Invalid, Predecessor => F_Elapsed_Time_Opt);
   end Initialize_Elapsed_Time_Opt_Private;

   procedure Initialize_Elapsed_Time_Opt (Ctx : in out Context) is
   begin
      Initialize_Elapsed_Time_Opt_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Elapsed_Time_Opt)));
   end Initialize_Elapsed_Time_Opt;

   procedure Set_NDP_Count_Opt (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_NDP_Count_Opt));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_NDP_Count_Opt_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_NDP_Count_Opt)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_NDP_Count_Opt)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_NDP_Count_Opt))) = Data);
   end Set_NDP_Count_Opt;

   procedure Set_Option_Value (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Option_Value));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Option_Value_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_Option_Value)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_Option_Value)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_Option_Value))) = Data);
   end Set_Option_Value;

   procedure Set_Elapsed_Time_Opt (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Elapsed_Time_Opt));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Elapsed_Time_Opt_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_Elapsed_Time_Opt)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_Elapsed_Time_Opt)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_Elapsed_Time_Opt))) = Data);
   end Set_Elapsed_Time_Opt;

   procedure Generic_Set_NDP_Count_Opt (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_NDP_Count_Opt);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_NDP_Count_Opt (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_NDP_Count_Opt_Private (Ctx, Length);
   end Generic_Set_NDP_Count_Opt;

   procedure Generic_Set_Option_Value (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Value);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Option_Value (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Option_Value_Private (Ctx, Length);
   end Generic_Set_Option_Value;

   procedure Generic_Set_Elapsed_Time_Opt (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Elapsed_Time_Opt);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (First + RFLX_Types.To_Bit_Length (Length) - 1);
   begin
      Process_Elapsed_Time_Opt (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
      Initialize_Elapsed_Time_Opt_Private (Ctx, Length);
   end Generic_Set_Elapsed_Time_Opt;

end RFLX.DCCP.Option;
