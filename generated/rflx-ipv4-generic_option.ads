with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.IPv4.Generic_Option with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Annotate (GNATprove, Terminating, Generic_Option);

   type Virtual_Field is (F_Initial, F_Copied, F_Option_Class, F_Option_Number, F_Option_Length, F_Option_Data, F_Final);

   subtype Field is Virtual_Field range F_Copied .. F_Option_Data;

   type Field_Cursors is private;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Option_Data | F_Final =>
               null;
            when F_Copied =>
               Copied_Value : Flag_Base;
            when F_Option_Class =>
               Option_Class_Value : Option_Class_Base;
            when F_Option_Number =>
               Option_Number_Value : Option_Number;
            when F_Option_Length =>
               Option_Length_Value : Option_Length_Base;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= RFLX.Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Ctx.First = RFLX.Types.First_Bit_Index (Ctx.Buffer_First)
          and then Available_Space (Ctx, F_Copied) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Copied)
          and then Path_Condition (Ctx, F_Copied)
          and then Buffer = null;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then RFLX.Types.Byte_Index (First) >= Buffer'First
          and then RFLX.Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= RFLX.Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and then Buffer = null
          and then Has_Buffer (Ctx)
          and then Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Ctx.First = First
          and then Ctx.Last = Last
          and then Available_Space (Ctx, F_Copied) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Copied)
          and then Path_Condition (Ctx, F_Copied);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Buffer /= null
          and then Ctx.Buffer_First = Buffer'First
          and then Ctx.Buffer_Last = Buffer'Last
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and then Structural_Valid_Message (Ctx);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, Fld),
     Post =>
       Present (Ctx, Fld)
          and then Ctx.First <= First
          and then Ctx.Last >= Last
          and then First <= Last;

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and then Value.Fld in Field'Range
          and then Valid_Predecessor (Ctx, Value.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and then Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Get_Copied (Ctx : Context) return Flag with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Copied);

   function Get_Option_Class (Ctx : Context) return Option_Class with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Option_Class);

   function Get_Option_Number (Ctx : Context) return Option_Number with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Option_Number);

   function Get_Option_Length (Ctx : Context) return Option_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Option_Length);

   generic
      with procedure Process_Option_Data (Option_Data : RFLX.Types.Bytes);
   procedure Get_Option_Data (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Option_Data);

   function Valid_Context (Ctx : Context) return Boolean;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Copied =>
            Valid (Value.Copied_Value),
         when F_Option_Class =>
            Valid (Value.Option_Class_Value),
         when F_Option_Number =>
            Valid (Value.Option_Number_Value),
         when F_Option_Length =>
            Valid (Value.Option_Length_Value),
         when F_Option_Data =>
            True,
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
               Last : RFLX.Types.Bit_Length := RFLX.Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Value));

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer : RFLX.Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and then Buffer'Last = Buffer_Last)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and then Cursors (F).Last <= Last
           and then Cursors (F).First <= (Cursors (F).Last + 1)
           and then Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Option_Class)) then
           (Valid (Cursors (F_Copied))
               and then Cursors (F_Option_Class).Predecessor = F_Copied))
        and then (if Structural_Valid (Cursors (F_Option_Number)) then
           (Valid (Cursors (F_Option_Class))
               and then Cursors (F_Option_Number).Predecessor = F_Option_Class))
        and then (if Structural_Valid (Cursors (F_Option_Length)) then
           (Valid (Cursors (F_Option_Number))
               and then Cursors (F_Option_Length).Predecessor = F_Option_Number
               and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1))
        and then (if Structural_Valid (Cursors (F_Option_Data)) then
           (Valid (Cursors (F_Option_Length))
               and then Cursors (F_Option_Data).Predecessor = F_Option_Length
               and then ((RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Debugging_And_Measurement))
                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                   and then (RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                     or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                     or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8)))))
      and then ((if Invalid (Cursors (F_Copied)) then
           Invalid (Cursors (F_Option_Class)))
        and then (if Invalid (Cursors (F_Option_Class)) then
           Invalid (Cursors (F_Option_Number)))
        and then (if Invalid (Cursors (F_Option_Number)) then
           Invalid (Cursors (F_Option_Length)))
        and then (if Invalid (Cursors (F_Option_Length)) then
           Invalid (Cursors (F_Option_Data))))
      and then (if Structural_Valid (Cursors (F_Copied)) then
         (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
           and then Cursors (F_Copied).Predecessor = F_Initial
           and then Cursors (F_Copied).First = First
           and then (if Structural_Valid (Cursors (F_Option_Class)) then
              (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size
                and then Cursors (F_Option_Class).Predecessor = F_Copied
                and then Cursors (F_Option_Class).First = (Cursors (F_Copied).Last + 1)
                and then (if Structural_Valid (Cursors (F_Option_Number)) then
                   (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number'Size
                     and then Cursors (F_Option_Number).Predecessor = F_Option_Class
                     and then Cursors (F_Option_Number).First = (Cursors (F_Option_Class).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Option_Length))
                          and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1 then
                        (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = Option_Length_Base'Size
                          and then Cursors (F_Option_Length).Predecessor = F_Option_Number
                          and then Cursors (F_Option_Length).First = (Cursors (F_Option_Number).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Option_Data))
                               and then ((RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Debugging_And_Measurement))
                                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                                   and then (RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                     or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                     or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                                 or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = RFLX.Types.Bit_Length (Convert (Control))
                                   and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8)) then
                             (Cursors (F_Option_Data).Last - Cursors (F_Option_Data).First + 1) = ((RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) - 2)) * 8
                               and then Cursors (F_Option_Data).Predecessor = F_Option_Length
                               and then Cursors (F_Option_Data).First = (Cursors (F_Option_Length).Last + 1)))))));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.IPv4.Generic_Option;
