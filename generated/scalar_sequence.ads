with Types; use type Types.Length_Type;

generic
   type Element_Type is private;
   Element_Type_Byte_Size : Types.Length_Type;
   with function Valid_Element_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean;
   with function Convert_To_Element_Type (Buffer : Types.Bytes; Offset : Natural) return Element_Type;
package Scalar_Sequence
  with SPARK_Mode
is

   type Cursor_Type is
      record
         First : Types.Index_Type;
         Last : Types.Index_Type;
      end record;

   function Is_Contained (Buffer : Types.Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Label (Buffer : Types.Bytes)
     with
       Ghost,
       Post => Is_Contained (Buffer);

   function First (Buffer : Types.Bytes) return Cursor_Type
     with
       Pre => Is_Contained (Buffer),
       Post => First'Result.First >= Buffer'First and then First'Result.Last <= Buffer'Last;

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type)
     with
       Pre => (Is_Contained (Buffer)
               and then Cursor.First >= Buffer'First
               and then Cursor.Last <= Buffer'Last);

   function Valid_Element (Buffer : Types.Bytes; Cursor : Cursor_Type) return Boolean is
     (Cursor.First >= Buffer'First
      and then Cursor.Last <= Buffer'Last
      and then Cursor.First <= Cursor.Last
      and then Buffer (Cursor.First .. Cursor.Last)'Length = Element_Type_Byte_Size
      and then Valid_Element_Type (Buffer (Cursor.First .. Cursor.Last), 0))
     with
       Pre => Is_Contained (Buffer);

   function Get_Element (Buffer : Types.Bytes; Cursor : Cursor_Type) return Element_Type is
      (Convert_To_Element_Type (Buffer (Cursor.First .. Cursor.Last), 0))
     with
       Pre => Is_Contained (Buffer) and then Valid_Element (Buffer, Cursor);

end Scalar_Sequence;
