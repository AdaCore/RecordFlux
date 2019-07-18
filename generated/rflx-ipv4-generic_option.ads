with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.IPv4.Generic_Option with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type All_Field_Type is (F_Initial, F_Copied, F_Option_Class, F_Option_Number, F_Option_Length, F_Option_Data, F_Final);

   subtype Field_Type is All_Field_Type range F_Copied .. F_Option_Data;

   type Context_Type (Buffer_First, Buffer_Last : RFLX.Types.Index_Type := RFLX.Types.Index_Type'First; First, Last : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is private with
     Default_Initial_Condition =>
       False;

   function Create return Context_Type;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Context'Constrained
          and then Buffer /= null
          and then Buffer'Last <= RFLX.Types.Index_Type'Last / 2,
     Post =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Context.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Context.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Buffer = null;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index_Type) with
     Pre =>
       not Context'Constrained
          and then Buffer /= null
          and then RFLX.Types.Byte_Index (First) >= Buffer'First
          and then RFLX.Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= RFLX.Types.Bit_Index_Type'Last / 2,
     Post =>
       Valid_Context (Context)
          and then Buffer = null
          and then Has_Buffer (Context)
          and then Context.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Context.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Context.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)'Old
          and then Context.First = First
          and then Context.Last = Last;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context),
     Post =>
       Valid_Context (Context)
          and then not Has_Buffer (Context)
          and then Buffer /= null
          and then Context.Buffer_First = Buffer'First
          and then Context.Buffer_Last = Buffer'Last
          and then Context.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old
          and then Present (Context, F_Copied) = Present (Context, F_Copied)'Old
          and then Present (Context, F_Option_Class) = Present (Context, F_Option_Class)'Old
          and then Present (Context, F_Option_Number) = Present (Context, F_Option_Number)'Old
          and then Present (Context, F_Option_Length) = Present (Context, F_Option_Length)'Old
          and then Present (Context, F_Option_Data) = Present (Context, F_Option_Data)'Old;

   function Has_Buffer (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   procedure Field_Range (Context : Context_Type; Field : Field_Type; First : out RFLX.Types.Bit_Index_Type; Last : out RFLX.Types.Bit_Index_Type) with
     Pre =>
       Valid_Context (Context)
          and then Present (Context, Field),
     Post =>
       Present (Context, Field)
          and then Context.First <= First
          and then Context.Last >= Last
          and then First <= Last;

   function Index (Context : Context_Type) return RFLX.Types.Bit_Index_Type with
     Pre =>
       Valid_Context (Context),
     Post =>
       Index'Result >= Context.First
          and then Index'Result - Context.Last <= 1;

   procedure Verify (Context : in out Context_Type; Field : Field_Type) with
     Pre =>
       Valid_Context (Context),
     Post =>
       Valid_Context (Context)
          and then (if Field /= F_Copied then (if Valid (Context, F_Copied)'Old then Valid (Context, F_Copied)))
          and then (if Field /= F_Option_Class then (if Valid (Context, F_Option_Class)'Old then Valid (Context, F_Option_Class)))
          and then (if Field /= F_Option_Number then (if Valid (Context, F_Option_Number)'Old then Valid (Context, F_Option_Number)))
          and then (if Field /= F_Option_Length then (if Valid (Context, F_Option_Length)'Old then Valid (Context, F_Option_Length)))
          and then (if Field /= F_Option_Data then (if Valid (Context, F_Option_Data)'Old then Valid (Context, F_Option_Data)))
          and then Has_Buffer (Context) = Has_Buffer (Context)'Old
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old;

   procedure Verify_Message (Context : in out Context_Type) with
     Pre =>
       Valid_Context (Context),
     Post =>
       Valid_Context (Context)
          and then Has_Buffer (Context) = Has_Buffer (Context)'Old
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old;

   function Present (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Structural_Valid (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Valid (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context),
     Post =>
       (if Valid'Result then Present (Context, Field)
          and then Structural_Valid (Context, Field));

   function Incomplete (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Structural_Valid_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Valid_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Incomplete_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Get_Copied (Context : Context_Type) return Flag_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Copied);

   function Get_Option_Class (Context : Context_Type) return Option_Class_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Option_Class);

   function Get_Option_Number (Context : Context_Type) return Option_Number_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Option_Number);

   function Get_Option_Length (Context : Context_Type) return Option_Length_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Option_Length);

   generic
      with procedure Process_Option_Data (Option_Data : RFLX.Types.Bytes);
   procedure Get_Option_Data (Context : Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Present (Context, F_Option_Data);

   function Valid_Context (Context : Context_Type) return Boolean;

private

   type State_Type is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

   type Result_Type (Field : All_Field_Type := F_Initial) is
      record
         case Field is
            when F_Initial | F_Option_Data | F_Final =>
               null;
            when F_Copied =>
               Copied_Value : Flag_Type_Base;
            when F_Option_Class =>
               Option_Class_Value : Option_Class_Type_Base;
            when F_Option_Number =>
               Option_Number_Value : Option_Number_Type;
            when F_Option_Length =>
               Option_Length_Value : Option_Length_Type_Base;
         end case;
      end record;

   function Valid_Type (Value : Result_Type) return Boolean is
     ((case Value.Field is
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

   type Cursor_Type (State : State_Type := S_Invalid) is
      record
         case State is
            when S_Valid | S_Structural_Valid | S_Preliminary =>
               First : RFLX.Types.Bit_Index_Type;
               Last : RFLX.Types.Bit_Length_Type;
               Value : Result_Type;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
          or State = S_Structural_Valid then Valid_Type (Value));

   type Cursors_Type is array (Field_Type) of Cursor_Type;

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index_Type; First, Last : RFLX.Types.Bit_Index_Type; Buffer_Address : RFLX.Types.Integer_Address; Buffer : RFLX.Types.Bytes_Ptr; Index : RFLX.Types.Bit_Index_Type; Field : All_Field_Type; Cursors : Cursors_Type) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then RFLX.Types.Bytes_Address (Buffer) = Buffer_Address)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Index >= First
      and then Index - Last <= 1
      and then (for all F in Field_Type'First .. Field_Type'Last =>
        (if Cursors (F).State = S_Valid
        or Cursors (F).State = S_Structural_Valid then Cursors (F).First >= First
        and then Cursors (F).Last <= Last
        and then Cursors (F).First <= (Cursors (F).Last + 1)
        and then Cursors (F).Value.Field = F))
      and then (case Field is
           when F_Initial =>
              True,
           when F_Copied =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size,
           when F_Option_Class =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Type_Base'Size,
           when F_Option_Number =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Type_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number_Type'Size,
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
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Debugging_And_Measurement)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                   or (Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                       or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                       or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                   or (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                   or (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Type_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number_Type'Size
                 and then (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = Option_Length_Type_Base'Size,
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
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Debugging_And_Measurement)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                   or (Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                       or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                       or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                   or (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                   or (RFLX.Types.Bit_Length_Type (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                     and then Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 8))
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Type_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number_Type'Size
                 and then (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = Option_Length_Type_Base'Size,
           when F_Final =>
              (Cursors (F_Copied).State = S_Valid
                   or Cursors (F_Copied).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Class).State = S_Valid
                   or Cursors (F_Option_Class).State = S_Structural_Valid)
                 and then (Cursors (F_Option_Number).State = S_Valid
                   or Cursors (F_Option_Number).State = S_Structural_Valid)
                 and then ((Cursors (F_Option_Class).Value.Option_Class_Value = Convert (Control)
                     and then RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) = 1)
                   or RFLX.Types.Bit_Length_Type (Cursors (F_Option_Number).Value.Option_Number_Value) > 1)
                 and then (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = Option_Class_Type_Base'Size
                 and then (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = Option_Number_Type'Size));

   type Context_Type (Buffer_First, Buffer_Last : RFLX.Types.Index_Type := RFLX.Types.Index_Type'First; First, Last : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Index : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First;
         Field : All_Field_Type := F_Initial;
         Cursors : Cursors_Type := (others => (State => S_Invalid));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, Index, Field, Cursors);

   function Valid_Context (Context : Context_Type) return Boolean is
     (Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer_Address, Context.Buffer, Context.Index, Context.Field, Context.Cursors));

end RFLX.IPv4.Generic_Option;
