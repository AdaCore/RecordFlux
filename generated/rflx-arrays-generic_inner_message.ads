with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.Arrays.Generic_Inner_Message with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type All_Field_Type is (F_Initial, F_Length, F_Payload, F_Final);

   subtype Field_Type is All_Field_Type range F_Length .. F_Payload;

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
          and then Present (Context, F_Length) = Present (Context, F_Length)'Old
          and then Present (Context, F_Payload) = Present (Context, F_Payload)'Old;

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
          and then (if Field /= F_Length then (if Valid (Context, F_Length)'Old then Valid (Context, F_Length)))
          and then (if Field /= F_Payload then (if Valid (Context, F_Payload)'Old then Valid (Context, F_Payload)))
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

   function Get_Length (Context : Context_Type) return Length_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Length);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Context : Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Present (Context, F_Payload);

   function Valid_Context (Context : Context_Type) return Boolean;

private

   type State_Type is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

   type Result_Type (Field : All_Field_Type := F_Initial) is
      record
         case Field is
            when F_Initial | F_Payload | F_Final =>
               null;
            when F_Length =>
               Length_Value : Length_Type;
         end case;
      end record;

   function Valid_Type (Value : Result_Type) return Boolean is
     ((case Value.Field is
         when F_Length =>
            Valid (Value.Length_Value),
         when F_Payload =>
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
           when F_Length =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length_Type'Size,
           when F_Payload | F_Final =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Payload).State = S_Valid
                   or Cursors (F_Payload).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length_Type'Size));

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

end RFLX.Arrays.Generic_Inner_Message;
