pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with RFLX.RFLX_Types;

generic
   type Element_Type is private;
   type Element_Base_Type is mod <>;
   with function Valid (Element : Element_Base_Type) return Boolean;
   with function To_Actual (Element : Element_Base_Type) return Element_Type;
   with function To_Base (Element : Element_Type) return Element_Base_Type;
package RFLX.RFLX_Scalar_Sequence with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, RFLX_Scalar_Sequence);

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes_Ptr, RFLX_Types.Index, RFLX_Types.Length, RFLX_Types.Bit_Index;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is private with
     Default_Initial_Condition =>
       RFLX_Types.To_Index (First) >= Buffer_First
       and RFLX_Types.To_Index (Last) <= Buffer_Last
       and Buffer_Last < RFLX_Types.Index'Last
       and First <= Last + 1
       and Last <= RFLX_Types.Bit_Length'Last - 1
       and First mod RFLX_Types.Byte'Size = 1;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'Length > 0
        and then Buffer'Last < RFLX_Types.Index'Last),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Buffer = null
        and Ctx.Buffer_First = Buffer'First'Old
        and Ctx.Buffer_Last = Buffer'Last'Old
        and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
        and Ctx.Last = RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last)
        and Sequence_Last (Ctx) = Ctx.First - 1),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'Length > 0
        and then Buffer'Last < RFLX_Types.Index'Last
        and then RFLX_Types.To_Index (First) >= Buffer'First
        and then RFLX_Types.To_Index (Last) <= Buffer'Last
        and then First <= Last + 1
        and then Last <= RFLX_Types.Bit_Length'Last - 1
        and then First mod RFLX_Types.Byte'Size = 1),
     Post =>
       (Buffer = null
        and Has_Buffer (Ctx)
        and Valid (Ctx)
        and Ctx.Buffer_First = Buffer'First'Old
        and Ctx.Buffer_Last = Buffer'Last'Old
        and Ctx.First = First
        and Ctx.Last = Last
        and Sequence_Last (Ctx) = First - 1),
     Depends =>
       (Ctx => (Buffer, First, Last), Buffer => null);

   procedure Reset (Ctx : in out Context) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Sequence_Last (Ctx) = Ctx.First - 1);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       (not Has_Buffer (Ctx)
        and Buffer /= null
        and Buffer'First = Ctx.Buffer_First
        and Buffer'Last = Ctx.Buffer_Last
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Valid (Ctx) = Valid (Ctx)'Old
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old),
     Depends =>
       (Ctx => Ctx, Buffer => Ctx);

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) with
     Pre =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Byte_Size (Ctx) = Buffer'Length);

   procedure Next (Ctx : in out Context) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Has_Element (Ctx)),
     Post =>
       (Has_Buffer (Ctx)
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old + Element_Base_Type'Size
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   function Has_Element (Ctx : Context) return Boolean;

   function Valid_Element (Ctx : Context) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and Has_Buffer (Ctx),
        not Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and not Has_Buffer (Ctx));

   function Get_Element (Ctx : Context) return Element_Type with
     Pre =>
       Valid_Element (Ctx);

   function Head (Ctx : Context) return Element_Type with
     Pre =>
       (Valid (Ctx)
        and then Sequence_Last (Ctx) >= Ctx.First + Element_Base_Type'Size - 1);

   procedure Append_Element (Ctx : in out Context; Value : Element_Type) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid (Ctx)
        and then Valid (To_Base (Value))
        and then Available_Space (Ctx) >= Element_Base_Type'Size),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old + Element_Base_Type'Size
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

   function Available_Space (Ctx : Context) return RFLX_Types.Bit_Length;

   function Sequence_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

private

   type Context_State is (S_Valid, S_Invalid);

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is
      record
         Sequence_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer        : RFLX_Types.Bytes_Ptr := null;
         State         : Context_State := S_Valid;
         First_Element : Element_Base_Type := Element_Base_Type'First;
         Next_Element  : Element_Base_Type := Element_Base_Type'First;
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last))
        and RFLX_Types.To_Index (First) >= Buffer_First
        and RFLX_Types.To_Index (Last) <= Buffer_Last
        and First mod RFLX_Types.Byte'Size = 1
        and Buffer_Last < RFLX_Types.Index'Last
        and First <= Last + 1
        and Last <= RFLX_Types.Bit_Length'Last - 1
        and Sequence_Last >= First - 1
        and Sequence_Last <= Last
        and (if Sequence_Last > First - 1 and State = S_Valid then Valid (First_Element)));

   function Has_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid and Ctx.Last - Ctx.Sequence_Last >= Element_Base_Type'Size);

   function Valid_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid and Valid (Ctx.Next_Element));

   function Valid (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid);

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Available_Space (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Last - Ctx.Sequence_Last);

   function Sequence_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Sequence_Last);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Sequence_Last - Ctx.First + 1);

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     (if
        Ctx.Sequence_Last = Ctx.First - 1
      then
         0
      else
         RFLX_Types.Length (RFLX_Types.To_Index (Ctx.Sequence_Last) - RFLX_Types.To_Index (Ctx.First)) + 1);

end RFLX.RFLX_Scalar_Sequence;
