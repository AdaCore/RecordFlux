pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.Arrays.Generic_Message with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Length => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Length)
      and then Available_Space (Ctx, F_Length) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Length)
      and then Invalid (Ctx, F_Modular_Vector)
      and then Invalid (Ctx, F_Range_Vector)
      and then Invalid (Ctx, F_Enumeration_Vector)
      and then Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if
          Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
       then
          Ctx.Cursors (F_AV_Enumeration_Vector).Last
       else
          Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Length =>
                    True,
                 when others =>
                    False),
          when F_Length =>
             (case Fld is
                 when F_Modular_Vector =>
                    True,
                 when others =>
                    False),
          when F_Modular_Vector =>
             (case Fld is
                 when F_Range_Vector =>
                    True,
                 when others =>
                    False),
          when F_Range_Vector =>
             (case Fld is
                 when F_Enumeration_Vector =>
                    True,
                 when others =>
                    False),
          when F_Enumeration_Vector =>
             (case Fld is
                 when F_AV_Enumeration_Vector =>
                    True,
                 when others =>
                    False),
          when F_AV_Enumeration_Vector | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial | F_Length | F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
             True,
          when F_Final =>
             False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Length =>
                    RFLX.Arrays.Length'Size,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Length =>
             (case Fld is
                 when F_Modular_Vector =>
                    Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Modular_Vector =>
             (case Fld is
                 when F_Range_Vector =>
                    16,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Range_Vector =>
             (case Fld is
                 when F_Enumeration_Vector =>
                    16,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_Enumeration_Vector =>
             (case Fld is
                 when F_AV_Enumeration_Vector =>
                    16,
                 when others =>
                    Types.Unreachable_Bit_Length),
          when F_AV_Enumeration_Vector | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
          when F_Length =>
             Ctx.First,
          when F_Modular_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_Range_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Modular_Vector
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_Enumeration_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Range_Vector
              then
                 (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
              else
                 Types.Unreachable_Bit_Length),
          when F_AV_Enumeration_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Enumeration_Vector
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
          when F_Length =>
             F_Modular_Vector,
          when F_Modular_Vector =>
             F_Range_Vector,
          when F_Range_Vector =>
             F_Enumeration_Vector,
          when F_Enumeration_Vector =>
             F_AV_Enumeration_Vector,
          when F_AV_Enumeration_Vector =>
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
          when F_Length =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Modular_Vector =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length),
          when F_Range_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Modular_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Modular_Vector),
          when F_Enumeration_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Range_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Range_Vector),
          when F_AV_Enumeration_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Enumeration_Vector),
          when F_Final =>
             (Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_AV_Enumeration_Vector)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Length =>
             Invalid (Ctx.Cursors (F_Modular_Vector)),
          when F_Modular_Vector =>
             Invalid (Ctx.Cursors (F_Range_Vector)),
          when F_Range_Vector =>
             Invalid (Ctx.Cursors (F_Enumeration_Vector)),
          when F_Enumeration_Vector =>
             Invalid (Ctx.Cursors (F_AV_Enumeration_Vector)),
          when F_AV_Enumeration_Vector =>
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
                   when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
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
               when F_Length =>
                  Invalid (Ctx, F_Length)
                  and Invalid (Ctx, F_Modular_Vector)
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Modular_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Invalid (Ctx, F_Modular_Vector)
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Range_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Invalid (Ctx, F_Range_Vector)
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_Enumeration_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Ctx.Cursors (F_Range_Vector) = Ctx.Cursors (F_Range_Vector)'Old
                  and Invalid (Ctx, F_Enumeration_Vector)
                  and Invalid (Ctx, F_AV_Enumeration_Vector),
               when F_AV_Enumeration_Vector =>
                  Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                  and Ctx.Cursors (F_Modular_Vector) = Ctx.Cursors (F_Modular_Vector)'Old
                  and Ctx.Cursors (F_Range_Vector) = Ctx.Cursors (F_Range_Vector)'Old
                  and Ctx.Cursors (F_Enumeration_Vector) = Ctx.Cursors (F_Enumeration_Vector)'Old
                  and Invalid (Ctx, F_AV_Enumeration_Vector))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Length =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Modular_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, Ctx.Cursors (F_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Modular_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Modular_Vector) := (S_Invalid, Ctx.Cursors (F_Modular_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Range_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Range_Vector) := (S_Invalid, Ctx.Cursors (F_Range_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_Enumeration_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Enumeration_Vector) := (S_Invalid, Ctx.Cursors (F_Enumeration_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
         when F_AV_Enumeration_Vector =>
            Ctx.Cursors (F_AV_Enumeration_Vector) := (S_Invalid, Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Length (Ctx, Fld) = Length);
      end case;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
          when F_Length =>
             False,
          when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
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
      function Extract is new Types.Extract (RFLX.Arrays.Length);
   begin
      return ((case Fld is
                  when F_Length =>
                     (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
                  when F_Modular_Vector =>
                     (Fld => F_Modular_Vector),
                  when F_Range_Vector =>
                     (Fld => F_Range_Vector),
                  when F_Enumeration_Vector =>
                     (Fld => F_Enumeration_Vector),
                  when F_AV_Enumeration_Vector =>
                     (Fld => F_AV_Enumeration_Vector)));
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
                                  Structural_Valid (Ctx.Cursors (F_Length))
                               then
                                  (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = RFLX.Arrays.Length'Size
                                  and then Ctx.Cursors (F_Length).Predecessor = F_Initial
                                  and then Ctx.Cursors (F_Length).First = Ctx.First
                                  and then (if
                                               Structural_Valid (Ctx.Cursors (F_Modular_Vector))
                                            then
                                               (Ctx.Cursors (F_Modular_Vector).Last - Ctx.Cursors (F_Modular_Vector).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                                               and then Ctx.Cursors (F_Modular_Vector).Predecessor = F_Length
                                               and then Ctx.Cursors (F_Modular_Vector).First = (Ctx.Cursors (F_Length).Last + 1)
                                               and then (if
                                                            Structural_Valid (Ctx.Cursors (F_Range_Vector))
                                                         then
                                                            (Ctx.Cursors (F_Range_Vector).Last - Ctx.Cursors (F_Range_Vector).First + 1) = 16
                                                            and then Ctx.Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                                            and then Ctx.Cursors (F_Range_Vector).First = (Ctx.Cursors (F_Modular_Vector).Last + 1)
                                                            and then (if
                                                                         Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
                                                                      then
                                                                         (Ctx.Cursors (F_Enumeration_Vector).Last - Ctx.Cursors (F_Enumeration_Vector).First + 1) = 16
                                                                         and then Ctx.Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                                         and then Ctx.Cursors (F_Enumeration_Vector).First = (Ctx.Cursors (F_Range_Vector).Last + 1)
                                                                         and then (if
                                                                                      Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
                                                                                   then
                                                                                      (Ctx.Cursors (F_AV_Enumeration_Vector).Last - Ctx.Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                                                                                      and then Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                                      and then Ctx.Cursors (F_AV_Enumeration_Vector).First = (Ctx.Cursors (F_Enumeration_Vector).Last + 1)))))));
               if Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Modular_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Range_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Enumeration_Vector then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_AV_Enumeration_Vector then
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
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Modular_Vector);
      Verify (Ctx, F_Range_Vector);
      Verify (Ctx, F_Enumeration_Vector);
      Verify (Ctx, F_AV_Enumeration_Vector);
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
     (Valid (Ctx, F_Length)
      and then Structural_Valid (Ctx, F_Modular_Vector)
      and then Structural_Valid (Ctx, F_Range_Vector)
      and then Structural_Valid (Ctx, F_Enumeration_Vector)
      and then Structural_Valid (Ctx, F_AV_Enumeration_Vector));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Length)
      and then Valid (Ctx, F_Modular_Vector)
      and then Valid (Ctx, F_Range_Vector)
      and then Valid (Ctx, F_Enumeration_Vector)
      and then Valid (Ctx, F_AV_Enumeration_Vector));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Modular_Vector)
      or Incomplete (Ctx, F_Range_Vector)
      or Incomplete (Ctx, F_Enumeration_Vector)
      or Incomplete (Ctx, F_AV_Enumeration_Vector));

   function Get_Length (Ctx : Context) return RFLX.Arrays.Length is
     (To_Actual (Ctx.Cursors (F_Length).Value.Length_Value));

   procedure Get_Modular_Vector (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Modular_Vector).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Modular_Vector).Last);
   begin
      Process_Modular_Vector (Ctx.Buffer.all (First .. Last));
   end Get_Modular_Vector;

   procedure Get_Range_Vector (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Range_Vector).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Range_Vector).Last);
   begin
      Process_Range_Vector (Ctx.Buffer.all (First .. Last));
   end Get_Range_Vector;

   procedure Get_Enumeration_Vector (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Enumeration_Vector).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Enumeration_Vector).Last);
   begin
      Process_Enumeration_Vector (Ctx.Buffer.all (First .. Last));
   end Get_Enumeration_Vector;

   procedure Get_AV_Enumeration_Vector (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_AV_Enumeration_Vector).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_AV_Enumeration_Vector).Last);
   begin
      Process_AV_Enumeration_Vector (Ctx.Buffer.all (First .. Last));
   end Get_AV_Enumeration_Vector;

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
      procedure Insert is new Types.Insert (RFLX.Arrays.Length);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Arrays.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

   procedure Switch_To_Modular_Vector (Ctx : in out Context; Seq_Ctx : out Modular_Vector_Sequence.Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Modular_Vector);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Modular_Vector);
      Buffer : Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Modular_Vector) then
         Reset_Dependent_Fields (Ctx, F_Modular_Vector);
         Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
         pragma Assert ((if
                            Structural_Valid (Ctx.Cursors (F_Length))
                         then
                            (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = RFLX.Arrays.Length'Size
                            and then Ctx.Cursors (F_Length).Predecessor = F_Initial
                            and then Ctx.Cursors (F_Length).First = Ctx.First
                            and then (if
                                         Structural_Valid (Ctx.Cursors (F_Modular_Vector))
                                      then
                                         (Ctx.Cursors (F_Modular_Vector).Last - Ctx.Cursors (F_Modular_Vector).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                                         and then Ctx.Cursors (F_Modular_Vector).Predecessor = F_Length
                                         and then Ctx.Cursors (F_Modular_Vector).First = (Ctx.Cursors (F_Length).Last + 1)
                                         and then (if
                                                      Structural_Valid (Ctx.Cursors (F_Range_Vector))
                                                   then
                                                      (Ctx.Cursors (F_Range_Vector).Last - Ctx.Cursors (F_Range_Vector).First + 1) = 16
                                                      and then Ctx.Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                                      and then Ctx.Cursors (F_Range_Vector).First = (Ctx.Cursors (F_Modular_Vector).Last + 1)
                                                      and then (if
                                                                   Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
                                                                then
                                                                   (Ctx.Cursors (F_Enumeration_Vector).Last - Ctx.Cursors (F_Enumeration_Vector).First + 1) = 16
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).First = (Ctx.Cursors (F_Range_Vector).Last + 1)
                                                                   and then (if
                                                                                Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
                                                                             then
                                                                                (Ctx.Cursors (F_AV_Enumeration_Vector).Last - Ctx.Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).First = (Ctx.Cursors (F_Enumeration_Vector).Last + 1)))))));
         Ctx.Cursors (F_Modular_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Modular_Vector), Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Modular_Vector)) := (State => S_Invalid, Predecessor => F_Modular_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Modular_Vector_Sequence.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Modular_Vector;

   procedure Switch_To_Range_Vector (Ctx : in out Context; Seq_Ctx : out Range_Vector_Sequence.Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Range_Vector);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Range_Vector);
      Buffer : Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Range_Vector) then
         Reset_Dependent_Fields (Ctx, F_Range_Vector);
         Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
         pragma Assert ((if
                            Structural_Valid (Ctx.Cursors (F_Length))
                         then
                            (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = RFLX.Arrays.Length'Size
                            and then Ctx.Cursors (F_Length).Predecessor = F_Initial
                            and then Ctx.Cursors (F_Length).First = Ctx.First
                            and then (if
                                         Structural_Valid (Ctx.Cursors (F_Modular_Vector))
                                      then
                                         (Ctx.Cursors (F_Modular_Vector).Last - Ctx.Cursors (F_Modular_Vector).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                                         and then Ctx.Cursors (F_Modular_Vector).Predecessor = F_Length
                                         and then Ctx.Cursors (F_Modular_Vector).First = (Ctx.Cursors (F_Length).Last + 1)
                                         and then (if
                                                      Structural_Valid (Ctx.Cursors (F_Range_Vector))
                                                   then
                                                      (Ctx.Cursors (F_Range_Vector).Last - Ctx.Cursors (F_Range_Vector).First + 1) = 16
                                                      and then Ctx.Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                                      and then Ctx.Cursors (F_Range_Vector).First = (Ctx.Cursors (F_Modular_Vector).Last + 1)
                                                      and then (if
                                                                   Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
                                                                then
                                                                   (Ctx.Cursors (F_Enumeration_Vector).Last - Ctx.Cursors (F_Enumeration_Vector).First + 1) = 16
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).First = (Ctx.Cursors (F_Range_Vector).Last + 1)
                                                                   and then (if
                                                                                Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
                                                                             then
                                                                                (Ctx.Cursors (F_AV_Enumeration_Vector).Last - Ctx.Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).First = (Ctx.Cursors (F_Enumeration_Vector).Last + 1)))))));
         Ctx.Cursors (F_Range_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Range_Vector), Predecessor => Ctx.Cursors (F_Range_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Range_Vector)) := (State => S_Invalid, Predecessor => F_Range_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Range_Vector_Sequence.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Range_Vector;

   procedure Switch_To_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out Enumeration_Vector_Sequence.Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Enumeration_Vector);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Enumeration_Vector);
      Buffer : Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_Enumeration_Vector) then
         Reset_Dependent_Fields (Ctx, F_Enumeration_Vector);
         Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
         pragma Assert ((if
                            Structural_Valid (Ctx.Cursors (F_Length))
                         then
                            (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = RFLX.Arrays.Length'Size
                            and then Ctx.Cursors (F_Length).Predecessor = F_Initial
                            and then Ctx.Cursors (F_Length).First = Ctx.First
                            and then (if
                                         Structural_Valid (Ctx.Cursors (F_Modular_Vector))
                                      then
                                         (Ctx.Cursors (F_Modular_Vector).Last - Ctx.Cursors (F_Modular_Vector).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                                         and then Ctx.Cursors (F_Modular_Vector).Predecessor = F_Length
                                         and then Ctx.Cursors (F_Modular_Vector).First = (Ctx.Cursors (F_Length).Last + 1)
                                         and then (if
                                                      Structural_Valid (Ctx.Cursors (F_Range_Vector))
                                                   then
                                                      (Ctx.Cursors (F_Range_Vector).Last - Ctx.Cursors (F_Range_Vector).First + 1) = 16
                                                      and then Ctx.Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                                      and then Ctx.Cursors (F_Range_Vector).First = (Ctx.Cursors (F_Modular_Vector).Last + 1)
                                                      and then (if
                                                                   Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
                                                                then
                                                                   (Ctx.Cursors (F_Enumeration_Vector).Last - Ctx.Cursors (F_Enumeration_Vector).First + 1) = 16
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).First = (Ctx.Cursors (F_Range_Vector).Last + 1)
                                                                   and then (if
                                                                                Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
                                                                             then
                                                                                (Ctx.Cursors (F_AV_Enumeration_Vector).Last - Ctx.Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).First = (Ctx.Cursors (F_Enumeration_Vector).Last + 1)))))));
         Ctx.Cursors (F_Enumeration_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Enumeration_Vector), Predecessor => Ctx.Cursors (F_Enumeration_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_Enumeration_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Enumeration_Vector_Sequence.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Enumeration_Vector;

   procedure Switch_To_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out AV_Enumeration_Vector_Sequence.Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_AV_Enumeration_Vector);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_AV_Enumeration_Vector);
      Buffer : Types.Bytes_Ptr;
   begin
      if Invalid (Ctx, F_AV_Enumeration_Vector) then
         Reset_Dependent_Fields (Ctx, F_AV_Enumeration_Vector);
         Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
         pragma Assert ((if
                            Structural_Valid (Ctx.Cursors (F_Length))
                         then
                            (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = RFLX.Arrays.Length'Size
                            and then Ctx.Cursors (F_Length).Predecessor = F_Initial
                            and then Ctx.Cursors (F_Length).First = Ctx.First
                            and then (if
                                         Structural_Valid (Ctx.Cursors (F_Modular_Vector))
                                      then
                                         (Ctx.Cursors (F_Modular_Vector).Last - Ctx.Cursors (F_Modular_Vector).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8
                                         and then Ctx.Cursors (F_Modular_Vector).Predecessor = F_Length
                                         and then Ctx.Cursors (F_Modular_Vector).First = (Ctx.Cursors (F_Length).Last + 1)
                                         and then (if
                                                      Structural_Valid (Ctx.Cursors (F_Range_Vector))
                                                   then
                                                      (Ctx.Cursors (F_Range_Vector).Last - Ctx.Cursors (F_Range_Vector).First + 1) = 16
                                                      and then Ctx.Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                                      and then Ctx.Cursors (F_Range_Vector).First = (Ctx.Cursors (F_Modular_Vector).Last + 1)
                                                      and then (if
                                                                   Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
                                                                then
                                                                   (Ctx.Cursors (F_Enumeration_Vector).Last - Ctx.Cursors (F_Enumeration_Vector).First + 1) = 16
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                                   and then Ctx.Cursors (F_Enumeration_Vector).First = (Ctx.Cursors (F_Range_Vector).Last + 1)
                                                                   and then (if
                                                                                Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
                                                                             then
                                                                                (Ctx.Cursors (F_AV_Enumeration_Vector).Last - Ctx.Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                                and then Ctx.Cursors (F_AV_Enumeration_Vector).First = (Ctx.Cursors (F_Enumeration_Vector).Last + 1)))))));
         Ctx.Cursors (F_AV_Enumeration_Vector) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_AV_Enumeration_Vector), Predecessor => Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
         Ctx.Cursors (Successor (Ctx, F_AV_Enumeration_Vector)) := (State => S_Invalid, Predecessor => F_AV_Enumeration_Vector);
      end if;
      Take_Buffer (Ctx, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      AV_Enumeration_Vector_Sequence.Initialize (Seq_Ctx, Buffer, Ctx.Buffer_First, Ctx.Buffer_Last, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_AV_Enumeration_Vector;

   procedure Update_Modular_Vector (Ctx : in out Context; Seq_Ctx : in out Modular_Vector_Sequence.Context) is
      Valid_Sequence : constant Boolean := Modular_Vector_Sequence.Valid (Seq_Ctx);
      Buffer : Types.Bytes_Ptr;
   begin
      Modular_Vector_Sequence.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Modular_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Modular_Vector).First, Last => Ctx.Cursors (F_Modular_Vector).Last, Value => Ctx.Cursors (F_Modular_Vector).Value, Predecessor => Ctx.Cursors (F_Modular_Vector).Predecessor);
      end if;
   end Update_Modular_Vector;

   procedure Update_Range_Vector (Ctx : in out Context; Seq_Ctx : in out Range_Vector_Sequence.Context) is
      Valid_Sequence : constant Boolean := Range_Vector_Sequence.Valid (Seq_Ctx);
      Buffer : Types.Bytes_Ptr;
   begin
      Range_Vector_Sequence.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Range_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Range_Vector).First, Last => Ctx.Cursors (F_Range_Vector).Last, Value => Ctx.Cursors (F_Range_Vector).Value, Predecessor => Ctx.Cursors (F_Range_Vector).Predecessor);
      end if;
   end Update_Range_Vector;

   procedure Update_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out Enumeration_Vector_Sequence.Context) is
      Valid_Sequence : constant Boolean := Enumeration_Vector_Sequence.Valid (Seq_Ctx);
      Buffer : Types.Bytes_Ptr;
   begin
      Enumeration_Vector_Sequence.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_Enumeration_Vector) := (State => S_Valid, First => Ctx.Cursors (F_Enumeration_Vector).First, Last => Ctx.Cursors (F_Enumeration_Vector).Last, Value => Ctx.Cursors (F_Enumeration_Vector).Value, Predecessor => Ctx.Cursors (F_Enumeration_Vector).Predecessor);
      end if;
   end Update_Enumeration_Vector;

   procedure Update_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out AV_Enumeration_Vector_Sequence.Context) is
      Valid_Sequence : constant Boolean := AV_Enumeration_Vector_Sequence.Valid (Seq_Ctx);
      Buffer : Types.Bytes_Ptr;
   begin
      AV_Enumeration_Vector_Sequence.Take_Buffer (Seq_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Sequence then
         Ctx.Cursors (F_AV_Enumeration_Vector) := (State => S_Valid, First => Ctx.Cursors (F_AV_Enumeration_Vector).First, Last => Ctx.Cursors (F_AV_Enumeration_Vector).Last, Value => Ctx.Cursors (F_AV_Enumeration_Vector).Value, Predecessor => Ctx.Cursors (F_AV_Enumeration_Vector).Predecessor);
      end if;
   end Update_AV_Enumeration_Vector;

end RFLX.Arrays.Generic_Message;
