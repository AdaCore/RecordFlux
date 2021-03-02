pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.UDP.Generic_Datagram with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, Buffer, (F_Source_Port => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Cursors := (F_Source_Port => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final));
      Ctx.Message_Last := Ctx.First - 1;
   end Reset;

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

   procedure Read (Ctx : Context) is
   begin
      Read (Ctx.Buffer.all (Types.Byte_Index (Ctx.First) .. Types.Byte_Index (Ctx.Message_Last)));
   end Read;

   procedure Write (Ctx : in out Context) is
   begin
      Reset (Ctx);
      Write (Ctx.Buffer.all (Types.Byte_Index (Ctx.First) .. Types.Byte_Index (Ctx.Last)));
   end Write;

   function Byte_Size (Ctx : Context) return Types.Length is
     ((if
          Ctx.Message_Last = Ctx.First - 1
       then
          0
       else
          Types.Length (Types.Byte_Index (Ctx.Message_Last) - Types.Byte_Index (Ctx.First) + 1)));

   pragma Warnings (Off, "precondition is always False");

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Source_Port =>
             F_Destination_Port,
          when F_Destination_Port =>
             F_Length,
          when F_Length =>
             F_Checksum,
          when F_Checksum =>
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
          when F_Source_Port =>
             Invalid (Ctx.Cursors (F_Destination_Port)),
          when F_Destination_Port =>
             Invalid (Ctx.Cursors (F_Length)),
          when F_Length =>
             Invalid (Ctx.Cursors (F_Checksum)),
          when F_Checksum =>
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
               when F_Source_Port =>
                  Invalid (Ctx, F_Source_Port)
                  and Invalid (Ctx, F_Destination_Port)
                  and Invalid (Ctx, F_Length)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Payload),
               when F_Destination_Port =>
                  Ctx.Cursors (F_Source_Port) = Ctx.Cursors (F_Source_Port)'Old
                  and Invalid (Ctx, F_Destination_Port)
                  and Invalid (Ctx, F_Length)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Payload),
               when F_Length =>
                  Ctx.Cursors (F_Source_Port) = Ctx.Cursors (F_Source_Port)'Old
                  and Ctx.Cursors (F_Destination_Port) = Ctx.Cursors (F_Destination_Port)'Old
                  and Invalid (Ctx, F_Length)
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Payload),
               when F_Checksum =>
                  Ctx.Cursors (F_Source_Port) = Ctx.Cursors (F_Source_Port)'Old
                  and Ctx.Cursors (F_Destination_Port) = Ctx.Cursors (F_Destination_Port)'Old
                  and Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Invalid (Ctx, F_Checksum)
                  and Invalid (Ctx, F_Payload),
               when F_Payload =>
                  Ctx.Cursors (F_Source_Port) = Ctx.Cursors (F_Source_Port)'Old
                  and Ctx.Cursors (F_Destination_Port) = Ctx.Cursors (F_Destination_Port)'Old
                  and Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Checksum) = Ctx.Cursors (F_Checksum)'Old
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
         when F_Source_Port =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Destination_Port) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Source_Port) := (S_Invalid, Ctx.Cursors (F_Source_Port).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Destination_Port =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Destination_Port) := (S_Invalid, Ctx.Cursors (F_Destination_Port).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Length =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, Ctx.Cursors (F_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
         when F_Checksum =>
            Ctx.Cursors (F_Payload) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Checksum) := (S_Invalid, Ctx.Cursors (F_Checksum).Predecessor);
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
          when F_Source_Port | F_Destination_Port | F_Length | F_Checksum =>
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
      function Extract is new Types.Extract (RFLX.UDP.Port);
      function Extract is new Types.Extract (RFLX.UDP.Length_Base);
      function Extract is new Types.Extract (RFLX.UDP.Checksum);
   begin
      return ((case Fld is
                  when F_Source_Port =>
                     (Fld => F_Source_Port, Source_Port_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Destination_Port =>
                     (Fld => F_Destination_Port, Destination_Port_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Length =>
                     (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Checksum =>
                     (Fld => F_Checksum, Checksum_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
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
               pragma Assert ((if
                                  Fld = F_Payload
                               then
                                  Field_Last (Ctx, Fld) mod Types.Byte'Size = 0));
               Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if
                                  Structural_Valid (Ctx.Cursors (F_Source_Port))
                               then
                                  Ctx.Cursors (F_Source_Port).Last - Ctx.Cursors (F_Source_Port).First + 1 = RFLX.UDP.Port'Size
                                  and then Ctx.Cursors (F_Source_Port).Predecessor = F_Initial
                                  and then Ctx.Cursors (F_Source_Port).First = Ctx.First
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Destination_Port))
                                            then
                                               Ctx.Cursors (F_Destination_Port).Last - Ctx.Cursors (F_Destination_Port).First + 1 = RFLX.UDP.Port'Size
                                               and then Ctx.Cursors (F_Destination_Port).Predecessor = F_Source_Port
                                               and then Ctx.Cursors (F_Destination_Port).First = Ctx.Cursors (F_Source_Port).Last + 1
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Length))
                                                         then
                                                            Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1 = RFLX.UDP.Length_Base'Size
                                                            and then Ctx.Cursors (F_Length).Predecessor = F_Destination_Port
                                                            and then Ctx.Cursors (F_Length).First = Ctx.Cursors (F_Destination_Port).Last + 1
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Checksum))
                                                                      then
                                                                         Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.UDP.Checksum'Size
                                                                         and then Ctx.Cursors (F_Checksum).Predecessor = F_Length
                                                                         and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Length).Last + 1
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Payload))
                                                                                   then
                                                                                      Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = (Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) - 8) * 8
                                                                                      and then Ctx.Cursors (F_Payload).Predecessor = F_Checksum
                                                                                      and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Checksum).Last + 1))))));
               if Fld = F_Source_Port then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Destination_Port then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Checksum then
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
      Verify (Ctx, F_Source_Port);
      Verify (Ctx, F_Destination_Port);
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Checksum);
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
      procedure Insert is new Types.Insert (RFLX.UDP.Port);
      procedure Insert is new Types.Insert (RFLX.UDP.Length_Base);
      procedure Insert is new Types.Insert (RFLX.UDP.Checksum);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Source_Port =>
            Insert (Val.Source_Port_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Destination_Port =>
            Insert (Val.Destination_Port_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Checksum =>
            Insert (Val.Checksum_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Payload | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Source_Port (Ctx : in out Context; Val : RFLX.UDP.Port) is
      Field_Value : constant Field_Dependent_Value := (F_Source_Port, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Source_Port);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Source_Port) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Source_Port).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Source_Port)) := (State => S_Invalid, Predecessor => F_Source_Port);
   end Set_Source_Port;

   procedure Set_Destination_Port (Ctx : in out Context; Val : RFLX.UDP.Port) is
      Field_Value : constant Field_Dependent_Value := (F_Destination_Port, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Destination_Port);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Destination_Port) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Destination_Port).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Destination_Port)) := (State => S_Invalid, Predecessor => F_Destination_Port);
   end Set_Destination_Port;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.UDP.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.UDP.Checksum) is
      Field_Value : constant Field_Dependent_Value := (F_Checksum, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Checksum);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := ((Last + 7) / 8) * 8;
      Ctx.Cursors (F_Checksum) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Checksum).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Checksum)) := (State => S_Invalid, Predecessor => F_Checksum);
   end Set_Checksum;

   procedure Set_Payload_Empty (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
   begin
      Reset_Dependent_Fields (Ctx, F_Payload);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Payload) := (State => S_Valid, First => First, Last => Last, Value => (Fld => F_Payload), Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload);
   end Set_Payload_Empty;

   procedure Initialize_Payload_Private (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Size (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Payload) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Payload) mod Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Payload)
       and Ctx.Message_Last = Field_Last (Ctx, F_Payload)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
   is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
   begin
      Reset_Dependent_Fields (Ctx, F_Payload);
      Ctx.Message_Last := Last;
      pragma Assert ((if
                         Structural_Valid (Ctx.Cursors (F_Source_Port))
                      then
                         Ctx.Cursors (F_Source_Port).Last - Ctx.Cursors (F_Source_Port).First + 1 = RFLX.UDP.Port'Size
                         and then Ctx.Cursors (F_Source_Port).Predecessor = F_Initial
                         and then Ctx.Cursors (F_Source_Port).First = Ctx.First
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Destination_Port))
                                   then
                                      Ctx.Cursors (F_Destination_Port).Last - Ctx.Cursors (F_Destination_Port).First + 1 = RFLX.UDP.Port'Size
                                      and then Ctx.Cursors (F_Destination_Port).Predecessor = F_Source_Port
                                      and then Ctx.Cursors (F_Destination_Port).First = Ctx.Cursors (F_Source_Port).Last + 1
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Length))
                                                then
                                                   Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1 = RFLX.UDP.Length_Base'Size
                                                   and then Ctx.Cursors (F_Length).Predecessor = F_Destination_Port
                                                   and then Ctx.Cursors (F_Length).First = Ctx.Cursors (F_Destination_Port).Last + 1
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Checksum))
                                                             then
                                                                Ctx.Cursors (F_Checksum).Last - Ctx.Cursors (F_Checksum).First + 1 = RFLX.UDP.Checksum'Size
                                                                and then Ctx.Cursors (F_Checksum).Predecessor = F_Length
                                                                and then Ctx.Cursors (F_Checksum).First = Ctx.Cursors (F_Length).Last + 1
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Payload))
                                                                          then
                                                                             Ctx.Cursors (F_Payload).Last - Ctx.Cursors (F_Payload).First + 1 = (Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) - 8) * 8
                                                                             and then Ctx.Cursors (F_Payload).Predecessor = F_Checksum
                                                                             and then Ctx.Cursors (F_Payload).First = Ctx.Cursors (F_Checksum).Last + 1))))));
      Ctx.Cursors (F_Payload) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Payload), Predecessor => Ctx.Cursors (F_Payload).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Payload)) := (State => S_Invalid, Predecessor => F_Payload);
   end Initialize_Payload_Private;

   procedure Initialize_Payload (Ctx : in out Context) is
   begin
      Initialize_Payload_Private (Ctx);
   end Initialize_Payload;

   procedure Set_Payload (Ctx : in out Context; Value : Types.Bytes) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Payload);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Payload);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Payload_Private (Ctx);
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
      Initialize_Payload_Private (Ctx);
      Process_Payload (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Generic_Set_Payload;

end RFLX.UDP.Generic_Datagram;
