pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.IPv4.Generic_Option with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Copied => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Copied)
      and then Available_Space (Ctx, F_Copied) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Copied)
      and then Invalid (Ctx, F_Option_Class)
      and then Invalid (Ctx, F_Option_Number)
      and then Invalid (Ctx, F_Option_Length)
      and then Invalid (Ctx, F_Option_Data));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if
          Structural_Valid (Ctx.Cursors (F_Option_Number))
          and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
          and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 1
       then
          Ctx.Cursors (F_Option_Number).Last
       elsif
          Structural_Valid (Ctx.Cursors (F_Option_Data))
       then
          Ctx.Cursors (F_Option_Data).Last
       else
          Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Copied =>
                    True,
                 when others =>
                    False),
          when F_Copied =>
             (case Fld is
                 when F_Option_Class =>
                    True,
                 when others =>
                    False),
          when F_Option_Class =>
             (case Fld is
                 when F_Option_Number =>
                    True,
                 when others =>
                    False),
          when F_Option_Number =>
             (case Fld is
                 when F_Option_Length =>
                    Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1,
                 when others =>
                    False),
          when F_Option_Length =>
             (case Fld is
                 when F_Option_Data =>
                    (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                     and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                    or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                        and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                             or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                             or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                    or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                        and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                        and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                    or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                        and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                        and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8),
                 when others =>
                    False),
          when F_Option_Data | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial | F_Copied | F_Option_Class =>
             True,
          when F_Option_Number =>
             (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
              and Types.Bit_Length (Val.Option_Number_Value) = 1)
             or Types.Bit_Length (Val.Option_Number_Value) > 1,
          when F_Option_Length =>
             (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
              and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
             or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                      or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                      or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
             or (Types.Bit_Length (Val.Option_Length_Value) = 11
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
             or (Types.Bit_Length (Val.Option_Length_Value) = 4
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8),
          when F_Option_Data =>
             True,
          when F_Final =>
             False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Copied =>
                    RFLX.RFLX_Builtin_Types.Boolean_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Copied =>
             (case Fld is
                 when F_Option_Class =>
                    RFLX.IPv4.Option_Class_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Option_Class =>
             (case Fld is
                 when F_Option_Number =>
                    RFLX.IPv4.Option_Number'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Option_Number =>
             (case Fld is
                 when F_Option_Length =>
                    RFLX.IPv4.Option_Length_Base'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Option_Length =>
             (case Fld is
                 when F_Option_Data =>
                    ((Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) - 2)) * 8,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Option_Data | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
          when F_Copied =>
             Ctx.First,
          when F_Option_Class =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Copied
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_Option_Number =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Option_Class
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_Option_Length =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Option_Number
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_Option_Data =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Option_Length
                 and ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                       and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                      or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                          and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                               or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                               or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                      or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                          and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                          and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                      or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                          and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                          and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length)));

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((Field_First (Ctx, Fld) + Field_Length (Ctx, Fld) - 1));

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
          when F_Initial =>
             F_Initial,
          when others =>
             Ctx.Cursors (Fld).Predecessor));

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
          when F_Copied =>
             F_Option_Class,
          when F_Option_Class =>
             F_Option_Number,
          when F_Option_Number =>
             (if
                 Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 1
              then
                 F_Final
              elsif
                 Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
              then
                 F_Option_Length
              else
                 F_Initial),
          when F_Option_Length =>
             (if
                 (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                  and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                 or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                     and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                          or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                          or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                 or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                     and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                     and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                 or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                     and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                     and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8)
              then
                 F_Option_Data
              else
                 F_Initial),
          when F_Option_Data =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
          when F_Initial =>
             True,
          when F_Copied =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Option_Class =>
             (Valid (Ctx.Cursors (F_Copied))
              and Ctx.Cursors (Fld).Predecessor = F_Copied),
          when F_Option_Number =>
             (Valid (Ctx.Cursors (F_Option_Class))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Class),
          when F_Option_Length =>
             (Valid (Ctx.Cursors (F_Option_Number))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Number),
          when F_Option_Data =>
             (Valid (Ctx.Cursors (F_Option_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Length),
          when F_Final =>
             (Valid (Ctx.Cursors (F_Option_Number))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Number)
             or (Structural_Valid (Ctx.Cursors (F_Option_Data))
                 and Ctx.Cursors (Fld).Predecessor = F_Option_Data)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Copied =>
             Invalid (Ctx.Cursors (F_Option_Class)),
          when F_Option_Class =>
             Invalid (Ctx.Cursors (F_Option_Number)),
          when F_Option_Number =>
             Invalid (Ctx.Cursors (F_Option_Length)),
          when F_Option_Length =>
             Invalid (Ctx.Cursors (F_Option_Data)),
          when F_Option_Data =>
             True));

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Predecessor (Ctx, Fld)
      and then Path_Condition (Ctx, Fld));

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((Types.Last_Bit_Index (Ctx.Buffer_Last) - Field_First (Ctx, Fld) + 1));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Ctx.First <= Types.Bit_Index'Last / 2
      and Field_First (Ctx, Fld) <= Types.Bit_Index'Last / 2
      and Field_Length (Ctx, Fld) >= 0
      and Field_Length (Ctx, Fld) <= Types.Bit_Length'Last / 2
      and (Field_First (Ctx, Fld) + Field_Length (Ctx, Fld)) <= Types.Bit_Length'Last / 2
      and Ctx.First <= Field_First (Ctx, Fld)
      and Ctx.Last >= Field_Last (Ctx, Fld))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Option_Data =>
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
       and Field_Length (Ctx, Fld) = Field_Length (Ctx, Fld)'Old
       and (case Fld is
               when F_Copied =>
                  Invalid (Ctx, F_Copied)
                  and Invalid (Ctx, F_Option_Class)
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Class =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Invalid (Ctx, F_Option_Class)
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Number =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Invalid (Ctx, F_Option_Number)
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Length =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Ctx.Cursors (F_Option_Number) = Ctx.Cursors (F_Option_Number)'Old
                  and Invalid (Ctx, F_Option_Length)
                  and Invalid (Ctx, F_Option_Data),
               when F_Option_Data =>
                  Ctx.Cursors (F_Copied) = Ctx.Cursors (F_Copied)'Old
                  and Ctx.Cursors (F_Option_Class) = Ctx.Cursors (F_Option_Class)'Old
                  and Ctx.Cursors (F_Option_Number) = Ctx.Cursors (F_Option_Number)'Old
                  and Ctx.Cursors (F_Option_Length) = Ctx.Cursors (F_Option_Length)'Old
                  and Invalid (Ctx, F_Option_Data))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Copied =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Class) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Copied) := (S_Invalid, Ctx.Cursors (F_Copied).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Option_Class =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Class) := (S_Invalid, Ctx.Cursors (F_Option_Class).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Option_Number =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Number) := (S_Invalid, Ctx.Cursors (F_Option_Number).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Option_Length =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Option_Length) := (S_Invalid, Ctx.Cursors (F_Option_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Option_Data =>
            Ctx.Cursors (F_Option_Data) := (S_Invalid, Ctx.Cursors (F_Option_Data).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Copied | F_Option_Class | F_Option_Number | F_Option_Length =>
             False,
          when F_Option_Data =>
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
      function Extract is new Types.Extract (RFLX.RFLX_Builtin_Types.Boolean_Base);
      function Extract is new Types.Extract (RFLX.IPv4.Option_Class_Base);
      function Extract is new Types.Extract (RFLX.IPv4.Option_Number);
      function Extract is new Types.Extract (RFLX.IPv4.Option_Length_Base);
   begin
      return ((case Fld is
                  when F_Copied =>
                     (Fld => F_Copied, Copied_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Class =>
                     (Fld => F_Option_Class, Option_Class_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Number =>
                     (Fld => F_Option_Number, Option_Number_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Length =>
                     (Fld => F_Option_Length, Option_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Option_Data =>
                     (Fld => F_Option_Data)));
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
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if
                                  Structural_Valid (Ctx.Cursors (F_Copied))
                               then
                                  (Ctx.Cursors (F_Copied).Last - Ctx.Cursors (F_Copied).First + 1) = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
                                  and then Ctx.Cursors (F_Copied).Predecessor = F_Initial
                                  and then Ctx.Cursors (F_Copied).First = Ctx.First
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Option_Class))
                                            then
                                               (Ctx.Cursors (F_Option_Class).Last - Ctx.Cursors (F_Option_Class).First + 1) = RFLX.IPv4.Option_Class_Base'Size
                                               and then Ctx.Cursors (F_Option_Class).Predecessor = F_Copied
                                               and then Ctx.Cursors (F_Option_Class).First = (Ctx.Cursors (F_Copied).Last + 1)
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Option_Number))
                                                         then
                                                            (Ctx.Cursors (F_Option_Number).Last - Ctx.Cursors (F_Option_Number).First + 1) = RFLX.IPv4.Option_Number'Size
                                                            and then Ctx.Cursors (F_Option_Number).Predecessor = F_Option_Class
                                                            and then Ctx.Cursors (F_Option_Number).First = (Ctx.Cursors (F_Option_Class).Last + 1)
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Option_Length))
                                                                         and then Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
                                                                      then
                                                                         (Ctx.Cursors (F_Option_Length).Last - Ctx.Cursors (F_Option_Length).First + 1) = RFLX.IPv4.Option_Length_Base'Size
                                                                         and then Ctx.Cursors (F_Option_Length).Predecessor = F_Option_Number
                                                                         and then Ctx.Cursors (F_Option_Length).First = (Ctx.Cursors (F_Option_Number).Last + 1)
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_Option_Data))
                                                                                      and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                                                                                                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                                                                                                or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                                    and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                                                                                         or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                                                                                         or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                                                                                                or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                                                                                    and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                                    and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                                                                                                or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                                                                                    and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                                    and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                                                                                   then
                                                                                      (Ctx.Cursors (F_Option_Data).Last - Ctx.Cursors (F_Option_Data).First + 1) = ((Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) - 2)) * 8
                                                                                      and then Ctx.Cursors (F_Option_Data).Predecessor = F_Option_Length
                                                                                      and then Ctx.Cursors (F_Option_Data).First = (Ctx.Cursors (F_Option_Length).Last + 1)))))));
               if Fld = F_Copied then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Class then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Number then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Option_Data then
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
      Verify (Ctx, F_Copied);
      Verify (Ctx, F_Option_Class);
      Verify (Ctx, F_Option_Number);
      Verify (Ctx, F_Option_Length);
      Verify (Ctx, F_Option_Data);
   end Verify_Message;

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Structural_Valid (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
       or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Invalid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Invalid
      or Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Copied)
      and then Valid (Ctx, F_Option_Class)
      and then Valid (Ctx, F_Option_Number)
      and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                or (Valid (Ctx, F_Option_Length)
                    and then Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
                    and then Structural_Valid (Ctx, F_Option_Data)
                    and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                               and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                       or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                       or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8)))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Copied)
      and then Valid (Ctx, F_Option_Class)
      and then Valid (Ctx, F_Option_Number)
      and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                or (Valid (Ctx, F_Option_Length)
                    and then Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
                    and then Valid (Ctx, F_Option_Data)
                    and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                               and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                       or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                       or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                              or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                  and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8)))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Copied)
      or Incomplete (Ctx, F_Option_Class)
      or Incomplete (Ctx, F_Option_Number)
      or Incomplete (Ctx, F_Option_Length)
      or Incomplete (Ctx, F_Option_Data));

   function Get_Copied (Ctx : Context) return Boolean is
     (To_Actual (Ctx.Cursors (F_Copied).Value.Copied_Value));

   function Get_Option_Class (Ctx : Context) return RFLX.IPv4.Option_Class is
     (To_Actual (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value));

   function Get_Option_Number (Ctx : Context) return RFLX.IPv4.Option_Number is
     (To_Actual (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value));

   function Get_Option_Length (Ctx : Context) return RFLX.IPv4.Option_Length is
     (To_Actual (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value));

   procedure Get_Option_Data (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Option_Data).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Option_Data).Last);
   begin
      Process_Option_Data (Ctx.Buffer.all (First .. Last));
   end Get_Option_Data;

   procedure Set_Field_Value (Ctx : in out Context; Val : Field_Dependent_Value; Fst, Lst : out Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Val.Fld in Field'Range
       and then Valid_Next (Ctx, Val.Fld)
       and then Available_Space (Ctx, Val.Fld) >= Field_Length (Ctx, Val.Fld)
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
       and Fst <= (Lst + 1)
       and Types.Byte_Index (Lst) <= Ctx.Buffer_Last
       and (for all F in Field'Range =>
               (if
                   Structural_Valid (Ctx.Cursors (F))
                then
                   Ctx.Cursors (F).Last <= Lst))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
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
      procedure Insert is new Types.Insert (RFLX.RFLX_Builtin_Types.Boolean_Base);
      procedure Insert is new Types.Insert (RFLX.IPv4.Option_Class_Base);
      procedure Insert is new Types.Insert (RFLX.IPv4.Option_Number);
      procedure Insert is new Types.Insert (RFLX.IPv4.Option_Length_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Copied =>
            Insert (Val.Copied_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Class =>
            Insert (Val.Option_Class_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Number =>
            Insert (Val.Option_Number_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Length =>
            Insert (Val.Option_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Option_Data | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Copied (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Copied, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Copied);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Copied) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Copied).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Copied)) := (State => S_Invalid, Predecessor => F_Copied);
   end Set_Copied;

   procedure Set_Option_Class (Ctx : in out Context; Val : RFLX.IPv4.Option_Class) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Class, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Class);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Option_Class) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Class).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Class)) := (State => S_Invalid, Predecessor => F_Option_Class);
   end Set_Option_Class;

   procedure Set_Option_Number (Ctx : in out Context; Val : RFLX.IPv4.Option_Number) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Number, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Number);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Option_Number) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Number).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Number)) := (State => S_Invalid, Predecessor => F_Option_Number);
   end Set_Option_Number;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.IPv4.Option_Length) is
      Field_Value : constant Field_Dependent_Value := (F_Option_Length, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Option_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Option_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Length)) := (State => S_Invalid, Predecessor => F_Option_Length);
   end Set_Option_Length;

   procedure Set_Option_Data (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Option_Data (Ctx);
      Process_Option_Data (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Option_Data;

   procedure Initialize_Option_Data (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Option_Data);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Option_Data);
   begin
      Reset_Dependent_Fields (Ctx, F_Option_Data);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      pragma Assert ((if
                         Structural_Valid (Ctx.Cursors (F_Copied))
                      then
                         (Ctx.Cursors (F_Copied).Last - Ctx.Cursors (F_Copied).First + 1) = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
                         and then Ctx.Cursors (F_Copied).Predecessor = F_Initial
                         and then Ctx.Cursors (F_Copied).First = Ctx.First
                         and then (if
                                      Structural_Valid (Ctx.Cursors (F_Option_Class))
                                   then
                                      (Ctx.Cursors (F_Option_Class).Last - Ctx.Cursors (F_Option_Class).First + 1) = RFLX.IPv4.Option_Class_Base'Size
                                      and then Ctx.Cursors (F_Option_Class).Predecessor = F_Copied
                                      and then Ctx.Cursors (F_Option_Class).First = (Ctx.Cursors (F_Copied).Last + 1)
                                      and then (if
                                                   Structural_Valid (Ctx.Cursors (F_Option_Number))
                                                then
                                                   (Ctx.Cursors (F_Option_Number).Last - Ctx.Cursors (F_Option_Number).First + 1) = RFLX.IPv4.Option_Number'Size
                                                   and then Ctx.Cursors (F_Option_Number).Predecessor = F_Option_Class
                                                   and then Ctx.Cursors (F_Option_Number).First = (Ctx.Cursors (F_Option_Class).Last + 1)
                                                   and then (if
                                                                Structural_Valid (Ctx.Cursors (F_Option_Length))
                                                                and then Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) > 1
                                                             then
                                                                (Ctx.Cursors (F_Option_Length).Last - Ctx.Cursors (F_Option_Length).First + 1) = RFLX.IPv4.Option_Length_Base'Size
                                                                and then Ctx.Cursors (F_Option_Length).Predecessor = F_Option_Number
                                                                and then Ctx.Cursors (F_Option_Length).First = (Ctx.Cursors (F_Option_Number).Last + 1)
                                                                and then (if
                                                                             Structural_Valid (Ctx.Cursors (F_Option_Data))
                                                                             and then ((Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                                                                                        and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                                                                                       or (Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                           and (Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                                                                                or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                                                                                or Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                                                                                       or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                                                                           and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                           and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                                                                                       or (Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                                                                           and Types.Bit_Length (Ctx.Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                                                                           and Types.Bit_Length (Ctx.Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                                                                          then
                                                                             (Ctx.Cursors (F_Option_Data).Last - Ctx.Cursors (F_Option_Data).First + 1) = ((Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value.Option_Length_Value) - 2)) * 8
                                                                             and then Ctx.Cursors (F_Option_Data).Predecessor = F_Option_Length
                                                                             and then Ctx.Cursors (F_Option_Data).First = (Ctx.Cursors (F_Option_Length).Last + 1)))))));
      Ctx.Cursors (F_Option_Data) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Option_Data), Predecessor => Ctx.Cursors (F_Option_Data).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Option_Data)) := (State => S_Invalid, Predecessor => F_Option_Data);
   end Initialize_Option_Data;

end RFLX.IPv4.Generic_Option;
