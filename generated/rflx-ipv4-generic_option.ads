with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.IPv4.Generic_Option with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Virtual_Field is (F_Initial, F_Copied, F_Option_Class, F_Option_Number, F_Option_Length, F_Option_Data, F_Final);

   subtype Field is Virtual_Field range F_Copied .. F_Option_Data;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is private with
     Default_Initial_Condition =>
       False;

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
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)'Old
          and then Ctx.First = First
          and then Ctx.Last = Last;

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
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Present (Ctx, F_Copied) = Present (Ctx, F_Copied)'Old
          and then Present (Ctx, F_Option_Class) = Present (Ctx, F_Option_Class)'Old
          and then Present (Ctx, F_Option_Number) = Present (Ctx, F_Option_Number)'Old
          and then Present (Ctx, F_Option_Length) = Present (Ctx, F_Option_Length)'Old
          and then Present (Ctx, F_Option_Data) = Present (Ctx, F_Option_Data)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, Fld),
     Post =>
       Present (Ctx, Fld)
          and then Ctx.First <= First
          and then Ctx.Last >= Last
          and then First <= Last;

   function Index (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Index'Result >= Ctx.First
          and then Index'Result - Ctx.Last <= 1;

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then (if Fld /= F_Copied then (if Valid (Ctx, F_Copied)'Old then Valid (Ctx, F_Copied)))
          and then (if Fld /= F_Option_Class then (if Valid (Ctx, F_Option_Class)'Old then Valid (Ctx, F_Option_Class)))
          and then (if Fld /= F_Option_Number then (if Valid (Ctx, F_Option_Number)'Old then Valid (Ctx, F_Option_Number)))
          and then (if Fld /= F_Option_Length then (if Valid (Ctx, F_Option_Length)'Old then Valid (Ctx, F_Option_Length)))
          and then (if Fld /= F_Option_Data then (if Valid (Ctx, F_Option_Data)'Old then Valid (Ctx, F_Option_Data)))
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
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
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
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
       (if Valid'Result then Present (Ctx, Fld)
          and then Structural_Valid (Ctx, Fld));

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

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

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
         case State is
            when S_Valid | S_Structural_Valid | S_Preliminary =>
               First : RFLX.Types.Bit_Index;
               Last : RFLX.Types.Bit_Length;
               Value : Field_Dependent_Value;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
          or State = S_Structural_Valid then Valid_Value (Value));

   type Field_Cursors is array (Field) of Field_Cursor;

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer_Address : RFLX.Types.Integer_Address; Buffer : RFLX.Types.Bytes_Ptr; Index : RFLX.Types.Bit_Index; Fld : Virtual_Field; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then RFLX.Types.Bytes_Address (Buffer) = Buffer_Address)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then Index >= First
      and then Index - Last <= 1
      and then (for all F in Field'First .. Field'Last =>
        (if Cursors (F).State = S_Valid
        or Cursors (F).State = S_Structural_Valid then Cursors (F).First >= First
        and then Cursors (F).Last <= Last
        and then Cursors (F).First <= (Cursors (F).Last + 1)
        and then Cursors (F).Value.Fld = F))
      and then (case Fld is
           when F_Initial =>
              True,
           when F_Copied =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size,
           when F_Option_Class =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size,
           when F_Option_Number =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number'Size,
           when F_Option_Length =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Length).State = S_Valid
                   or Cursors (F_Option_Length).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Debugging_And_Measurement)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                   or (Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then (RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                       or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                       or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                   or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                   or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number'Size
                 and then (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = Option_Length_Base'Size,
           when F_Option_Data =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Length).State = S_Valid
                   or Cursors (F_Option_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Data).State = S_Valid
                   or Cursors (F_Option_Data).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Debugging_And_Measurement)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                   or (Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then (RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                       or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                       or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                   or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                   or (RFLX.Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number'Size
                 and then (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = Option_Length_Base'Size,
           when F_Final =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number'Size));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Index : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
         Fld : Virtual_Field := F_Initial;
         Cursors : Field_Cursors := (others => (State => S_Invalid));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, Index, Fld, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer_Address, Ctx.Buffer, Ctx.Index, Ctx.Fld, Ctx.Cursors));

end RFLX.IPv4.Generic_Option;
