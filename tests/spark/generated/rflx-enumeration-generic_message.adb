pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Enumeration.Generic_Message with
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
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, Buffer, (F_Priority => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Cursors := (F_Priority => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final));
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
          when F_Priority =>
             F_Final))
    with
     Pre =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, Fld)
       and Valid_Predecessor (Ctx, Fld);

   pragma Warnings (On, "precondition is always False");

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

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
       and Invalid (Ctx.Cursors (Fld))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Ctx.Cursors (Fld).Predecessor = Ctx.Cursors (Fld).Predecessor'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Old
       and (case Fld is
               when F_Priority =>
                  Invalid (Ctx, F_Priority))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Size : constant Types.Bit_Length := Field_Size (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
                     and Field_Size (Ctx, Fld) = Size);
      case Fld is
         when F_Priority =>
            Ctx.Cursors (F_Priority) := (S_Invalid, Ctx.Cursors (F_Priority).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
                           and Field_Size (Ctx, Fld) = Size);
      end case;
   end Reset_Dependent_Fields;

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
      function Extract is new Types.Extract (RFLX.Enumeration.Priority_Base);
   begin
      return ((case Fld is
                  when F_Priority =>
                     (Fld => F_Priority, Priority_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
                                  Fld = F_Priority
                               then
                                  Field_Last (Ctx, Fld) mod Types.Byte'Size = 0));
               Ctx.Message_Last := ((Field_Last (Ctx, Fld) + 7) / 8) * 8;
               Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
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
      Verify (Ctx, F_Priority);
   end Verify_Message;

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
      procedure Insert is new Types.Insert (RFLX.Enumeration.Priority_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Priority =>
            Insert (Val.Priority_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Priority (Ctx : in out Context; Val : RFLX.Enumeration.Priority_Enum) is
      Field_Value : constant Field_Dependent_Value := (F_Priority, To_Base (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Priority);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx.Message_Last := Last;
      Ctx.Cursors (F_Priority) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Priority).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Priority)) := (State => S_Invalid, Predecessor => F_Priority);
   end Set_Priority;

end RFLX.Enumeration.Generic_Message;
