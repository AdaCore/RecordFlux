pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Ethernet.Generic_Frame with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First, Buffer, (F_Destination => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (Types.Byte_Index (Ctx.First) .. Types.Byte_Index (Ctx.Message_Last));
      else
         Buffer := Ctx.Buffer.all (Types.Index'Last .. Types.Index'First);
      end if;
   end Copy;

   function Byte_Size (Ctx : Context) return Types.Length is
     ((if
          Ctx.Message_Last = Ctx.First - 1
       then
          0
       else
          Types.Length (Types.Byte_Index (Ctx.Message_Last) - Types.Byte_Index (Ctx.First) + 1)));

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     (Ctx.Message_Last);

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Destination =>
             F_Source,
          when F_Source =>
             F_Type_Length_TPID,
          when F_Type_Length_TPID =>
             (if
                 Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value = 16#8100#
              then
                 F_TPID
              elsif
                 Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value /= 16#8100#
              then
                 F_Type_Length
              else
                 F_Initial),
          when F_TPID =>
             F_TCI,
          when F_TCI =>
             F_Type_Length,
          when F_Type_Length =>
             (if
                 Ctx.Cursors (F_Type_Length).Value.Type_Length_Value <= 1500
              then
                 F_Payload
              elsif
                 Ctx.Cursors (F_Type_Length).Value.Type_Length_Value >= 1536
              then
                 F_Payload
              else
                 F_Initial),
          when F_Payload =>
             (if
                 Types.U64 (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) / 8 >= 46
                 and Types.U64 (Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1) / 8 <= 1500
              then
                 F_Final
              else
                 F_Initial)))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Destination =>
             Invalid (Ctx.Cursors (F_Source)),
          when F_Source =>
             Invalid (Ctx.Cursors (F_Type_Length_TPID)),
          when F_Type_Length_TPID =>
             Invalid (Ctx.Cursors (F_TPID))
             and Invalid (Ctx.Cursors (F_Type_Length)),
          when F_TPID =>
             Invalid (Ctx.Cursors (F_TCI)),
          when F_TCI =>
             Invalid (Ctx.Cursors (F_Type_Length)),
          when F_Type_Length =>
             Invalid (Ctx.Cursors (F_Payload)),
          when F_Payload =>
             True));

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
                   when F_Payload =>
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
               when F_Destination =>
                  Invalid (Ctx, F_Destination)
                  and Invalid (Ctx, F_Source)
                  and Invalid (Ctx, F_Type_Length_TPID)
                  and Invalid (Ctx, F_TPID)
                  and Invalid (Ctx, F_TCI)
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_Source =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Invalid (Ctx, F_Source)
                  and Invalid (Ctx, F_Type_Length_TPID)
                  and Invalid (Ctx, F_TPID)
                  and Invalid (Ctx, F_TCI)
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_Type_Length_TPID =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old
                  and Invalid (Ctx, F_Type_Length_TPID)
                  and Invalid (Ctx, F_TPID)
                  and Invalid (Ctx, F_TCI)
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_TPID =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old
                  and Ctx.Cursors (F_Type_Length_TPID) = Ctx.Cursors (F_Type_Length_TPID)'Old
                  and Invalid (Ctx, F_TPID)
                  and Invalid (Ctx, F_TCI)
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_TCI =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old
                  and Ctx.Cursors (F_Type_Length_TPID) = Ctx.Cursors (F_Type_Length_TPID)'Old
                  and Ctx.Cursors (F_TPID) = Ctx.Cursors (F_TPID)'Old
                  and Invalid (Ctx, F_TCI)
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_Type_Length =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old
                  and Ctx.Cursors (F_Type_Length_TPID) = Ctx.Cursors (F_Type_Length_TPID)'Old
                  and Ctx.Cursors (F_TPID) = Ctx.Cursors (F_TPID)'Old
                  and Ctx.Cursors (F_TCI) = Ctx.Cursors (F_TCI)'Old
                  and Invalid (Ctx, F_Type_Length)
                  and Invalid (Ctx, F_Payload),
               when F_Payload =>
                  Ctx.Cursors (F_Destination) = Ctx.Cursors (F_Destination)'Old
                  and Ctx.Cursors (F_Source) = Ctx.Cursors (F_Source)'Old
                  and Ctx.Cursors (F_Type_Length_TPID) = Ctx.Cursors (F_Type_Length_TPID)'Old
                  and Ctx.Cursors (F_TPID) = Ctx.Cursors (F_TPID)'Old
                  and Ctx.Cursors (F_TCI) = Ctx.Cursors (F_TCI)'Old
                  and Ctx.Cursors (F_Type_Length) = Ctx.Cursors (F_Type_Length)'Old
                  and Invalid (Ctx, F_Payload))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      case Fld is
         when F_Destination =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Source) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Destination) := (S_Invalid, Ctx.Cursors (F_Destination).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Source =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Source) := (S_Invalid, Ctx.Cursors (F_Source).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Type_Length_TPID =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length_TPID) := (S_Invalid, Ctx.Cursors (F_Type_Length_TPID).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_TPID =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TPID) := (S_Invalid, Ctx.Cursors (F_TPID).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_TCI =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_TCI) := (S_Invalid, Ctx.Cursors (F_TCI).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Type_Length =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Type_Length) := (S_Invalid, Ctx.Cursors (F_Type_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Payload =>
            Ctx.Cursors (F_Payload) := (S_Invalid, Ctx.Cursors (F_Payload).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Destination | F_Source | F_Type_Length_TPID | F_TPID | F_TCI | F_Type_Length =>
             False,
          when F_Payload =>
             True));

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
      function Extract is new Types.Extract (RFLX.Ethernet.Address);
      function Extract is new Types.Extract (RFLX.Ethernet.Type_Length_Base);
      function Extract is new Types.Extract (RFLX.Ethernet.TPID_Base);
      function Extract is new Types.Extract (RFLX.Ethernet.TCI);
   begin
      return ((case Fld is
                  when F_Destination =>
                     (Fld => F_Destination, Destination_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Source =>
                     (Fld => F_Source, Source_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Type_Length_TPID =>
                     (Fld => F_Type_Length_TPID, Type_Length_TPID_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_TPID =>
                     (Fld => F_TPID, TPID_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_TCI =>
                     (Fld => F_TCI, TCI_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Type_Length =>
                     (Fld => F_Type_Length, Type_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
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
              and Field_Condition (Ctx, Value, Field_Size (Ctx, Fld))
            then
               Ctx.Message_Last := Field_Last (Ctx, Fld);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if
                                  Structural_Valid (Ctx.Cursors (F_Destination))
                               then
                                  Ctx.Cursors (F_Destination).Last - Ctx.Cursors (F_Destination).First + 1 = RFLX.Ethernet.Address'Size
                                  and then Ctx.Cursors (F_Destination).Predecessor = F_Initial
                                  and then Ctx.Cursors (F_Destination).First = Ctx.First
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Source))
                                            then
                                               Ctx.Cursors (F_Source).Last - Ctx.Cursors (F_Source).First + 1 = RFLX.Ethernet.Address'Size
                                               and then Ctx.Cursors (F_Source).Predecessor = F_Destination
                                               and then Ctx.Cursors (F_Source).First = Ctx.Cursors (F_Destination).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Type_Length_TPID))
                                                         then
                                                            Ctx.Cursors (F_Type_Length_TPID).Last - Ctx.Cursors (F_Type_Length_TPID).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                            and then Ctx.Cursors (F_Type_Length_TPID).Predecessor = F_Source
                                                            and then Ctx.Cursors (F_Type_Length_TPID).First = Ctx.Cursors (F_Source).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_TPID))
                                                                         and then Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value = 16#8100#
                                                                      then
                                                                         Ctx.Cursors (F_TPID).Last - Ctx.Cursors (F_TPID).First + 1 = RFLX.Ethernet.TPID_Base'Size
                                                                         and then Ctx.Cursors (F_TPID).Predecessor = F_Type_Length_TPID
                                                                         and then Ctx.Cursors (F_TPID).First = Types.Bit_Index (Ctx.Cursors (F_Type_Length_TPID).First)
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_TCI))
                                                                                   then
                                                                                      Ctx.Cursors (F_TCI).Last - Ctx.Cursors (F_TCI).First + 1 = RFLX.Ethernet.TCI'Size
                                                                                      and then Ctx.Cursors (F_TCI).Predecessor = F_TPID
                                                                                      and then Ctx.Cursors (F_TCI).First = Ctx.Cursors (F_TPID).Last + 1
                                                                                      and then (if
                                                                                                   Structural_Valid (Ctx.Cursors (F_Type_Length))
                                                                                                then
                                                                                                   Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                                                                   and then Ctx.Cursors (F_Type_Length).Predecessor = F_TCI
                                                                                                   and then Ctx.Cursors (F_Type_Length).First = Ctx.Cursors (F_TCI).Last + 1
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                                                and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value <= 1500
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                                                                                                and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                                                and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1)
                                                                                                   and then (if
                                                                                                                Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                                                and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value >= 1536
                                                                                                             then
                                                                                                                Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Type_Length).Last)
                                                                                                                and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                                                and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1))))
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Type_Length))
                                                                         and then Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value /= 16#8100#
                                                                      then
                                                                         Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                                         and then Ctx.Cursors (F_Type_Length).Predecessor = F_Type_Length_TPID
                                                                         and then Ctx.Cursors (F_Type_Length).First = Types.Bit_Index (Ctx.Cursors (F_Type_Length_TPID).First)
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                      and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value <= 1500
                                                                                   then
                                                                                      Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                                                                      and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                      and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1)
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                      and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value >= 1536
                                                                                   then
                                                                                      Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Type_Length).Last)
                                                                                      and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                      and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1))))));
               if Fld = F_Destination then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Source then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Type_Length_TPID then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_TPID then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_TCI then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Type_Length then
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
      Verify (Ctx, F_Destination);
      Verify (Ctx, F_Source);
      Verify (Ctx, F_Type_Length_TPID);
      Verify (Ctx, F_TPID);
      Verify (Ctx, F_TCI);
      Verify (Ctx, F_Type_Length);
      Verify (Ctx, F_Payload);
   end Verify_Message;

   procedure Get_Payload (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Payload).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Payload).Last);
   begin
      Process_Payload (Ctx.Buffer.all (First .. Last));
   end Get_Payload;

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
      procedure Insert is new Types.Insert (RFLX.Ethernet.Address);
      procedure Insert is new Types.Insert (RFLX.Ethernet.Type_Length_Base);
      procedure Insert is new Types.Insert (RFLX.Ethernet.TPID_Base);
      procedure Insert is new Types.Insert (RFLX.Ethernet.TCI);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Destination =>
            Insert (Val.Destination_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Source =>
            Insert (Val.Source_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Type_Length_TPID =>
            Insert (Val.Type_Length_TPID_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_TPID =>
            Insert (Val.TPID_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_TCI =>
            Insert (Val.TCI_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Type_Length =>
            Insert (Val.Type_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Payload | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Destination (Ctx : in out Context; Val : RFLX.Ethernet.Address) is
      Field_Value : constant Field_Dependent_Value := (F_Destination, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Destination);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Destination) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Destination).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Destination)) := (State => S_Invalid, Predecessor => F_Destination);
   end Set_Destination;

   procedure Set_Source (Ctx : in out Context; Val : RFLX.Ethernet.Address) is
      Field_Value : constant Field_Dependent_Value := (F_Source, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Source);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Source) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Source).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Source)) := (State => S_Invalid, Predecessor => F_Source);
   end Set_Source;

   procedure Set_Type_Length_TPID (Ctx : in out Context; Val : RFLX.Ethernet.Type_Length) is
      Field_Value : constant Field_Dependent_Value := (F_Type_Length_TPID, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Type_Length_TPID);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Type_Length_TPID) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Type_Length_TPID).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Type_Length_TPID)) := (State => S_Invalid, Predecessor => F_Type_Length_TPID);
   end Set_Type_Length_TPID;

   procedure Set_TPID (Ctx : in out Context; Val : RFLX.Ethernet.TPID) is
      Field_Value : constant Field_Dependent_Value := (F_TPID, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_TPID);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_TPID) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_TPID).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_TPID)) := (State => S_Invalid, Predecessor => F_TPID);
   end Set_TPID;

   procedure Set_TCI (Ctx : in out Context; Val : RFLX.Ethernet.TCI) is
      Field_Value : constant Field_Dependent_Value := (F_TCI, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_TCI);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_TCI) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_TCI).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_TCI)) := (State => S_Invalid, Predecessor => F_TCI);
   end Set_TCI;

   procedure Set_Type_Length (Ctx : in out Context; Val : RFLX.Ethernet.Type_Length) is
      Field_Value : constant Field_Dependent_Value := (F_Type_Length, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Type_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Type_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Type_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Type_Length)) := (State => S_Invalid, Predecessor => F_Type_Length);
   end Set_Type_Length;

   procedure Set_Payload (Ctx : in out Context; Value : Types.Bytes) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Payload (Ctx);
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Value;
   end Set_Payload;

   procedure Generic_Set_Payload (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Payload (Ctx);
      Process_Payload (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Payload;

   procedure Initialize_Payload (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
   begin
      Reset_Dependent_Fields (Ctx, F_Payload);
      Ctx.Message_Last := Last;
      pragma Assert ((if
                         Structural_Valid (Ctx.Cursors (F_Destination))
                      then
                         Ctx.Cursors (F_Destination).Last - Ctx.Cursors (F_Destination).First + 1 = RFLX.Ethernet.Address'Size
                         and then Ctx.Cursors (F_Destination).Predecessor = F_Initial
                         and then Ctx.Cursors (F_Destination).First = Ctx.First
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Source))
                                   then
                                      Ctx.Cursors (F_Source).Last - Ctx.Cursors (F_Source).First + 1 = RFLX.Ethernet.Address'Size
                                      and then Ctx.Cursors (F_Source).Predecessor = F_Destination
                                      and then Ctx.Cursors (F_Source).First = Ctx.Cursors (F_Destination).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Type_Length_TPID))
                                                then
                                                   Ctx.Cursors (F_Type_Length_TPID).Last - Ctx.Cursors (F_Type_Length_TPID).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                   and then Ctx.Cursors (F_Type_Length_TPID).Predecessor = F_Source
                                                   and then Ctx.Cursors (F_Type_Length_TPID).First = Ctx.Cursors (F_Source).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_TPID))
                                                                and then Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value = 16#8100#
                                                             then
                                                                Ctx.Cursors (F_TPID).Last - Ctx.Cursors (F_TPID).First + 1 = RFLX.Ethernet.TPID_Base'Size
                                                                and then Ctx.Cursors (F_TPID).Predecessor = F_Type_Length_TPID
                                                                and then Ctx.Cursors (F_TPID).First = Types.Bit_Index (Ctx.Cursors (F_Type_Length_TPID).First)
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_TCI))
                                                                          then
                                                                             Ctx.Cursors (F_TCI).Last - Ctx.Cursors (F_TCI).First + 1 = RFLX.Ethernet.TCI'Size
                                                                             and then Ctx.Cursors (F_TCI).Predecessor = F_TPID
                                                                             and then Ctx.Cursors (F_TCI).First = Ctx.Cursors (F_TPID).Last + 1
                                                                             and then (if
                                                                                          Structural_Valid (Ctx.Cursors (F_Type_Length))
                                                                                       then
                                                                                          Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                                                          and then Ctx.Cursors (F_Type_Length).Predecessor = F_TCI
                                                                                          and then Ctx.Cursors (F_Type_Length).First = Ctx.Cursors (F_TCI).Last + 1
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                                       and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value <= 1500
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                                                                                       and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                                       and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1)
                                                                                          and then (if
                                                                                                       Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                                       and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value >= 1536
                                                                                                    then
                                                                                                       Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Type_Length).Last)
                                                                                                       and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                                                       and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1))))
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Type_Length))
                                                                and then Ctx.Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value /= 16#8100#
                                                             then
                                                                Ctx.Cursors (F_Type_Length).Last - Ctx.Cursors (F_Type_Length).First + 1 = RFLX.Ethernet.Type_Length_Base'Size
                                                                and then Ctx.Cursors (F_Type_Length).Predecessor = F_Type_Length_TPID
                                                                and then Ctx.Cursors (F_Type_Length).First = Types.Bit_Index (Ctx.Cursors (F_Type_Length_TPID).First)
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Payload))
                                                                             and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value <= 1500
                                                                          then
                                                                             Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                                                             and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                             and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1)
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Payload))
                                                                             and then Ctx.Cursors (F_Type_Length).Value.Type_Length_Value >= 1536
                                                                          then
                                                                             Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = Types.Bit_Length (Ctx.Last) - Types.Bit_Length (Ctx.Cursors (F_Type_Length).Last)
                                                                             and then Ctx.Cursors (F_Payload).Predecessor = F_Type_Length
                                                                             and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Type_Length).Last + 1))))));
      Ctx.Cursors (F_Payload) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Payload), Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload);
   end Initialize_Payload;

end RFLX.Ethernet.Generic_Frame;
