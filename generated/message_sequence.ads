with Types; use type Types.Length_Type;

generic
   with procedure Element_Label (Buffer : Types.Bytes);
   with function Element_Is_Contained (Buffer : Types.Bytes) return Boolean;
   with function Element_Is_Valid (Buffer : Types.Bytes) return Boolean;
   with function Element_Length (Buffer : Types.Bytes) return Types.Length_Type;
package Message_Sequence
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
       Post => (First'Result.First >= Buffer'First
                and then First'Result.Last <= Buffer'Last
                and then Element_Is_Contained (Buffer (First'Result.First .. First'Result.Last)));

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type)
     with
       Pre => (Cursor.First >= Buffer'First
               and then Cursor.Last <= Buffer'Last
               and then Element_Is_Contained (Buffer (Cursor.First .. Cursor.Last))
               and then Element_Is_Valid (Buffer (Cursor.First .. Cursor.Last))),
       Post => (Cursor.First >= Buffer'First
                and then Cursor.Last <= Buffer'Last
                and then Element_Is_Contained (Buffer (Cursor.First .. Cursor.Last)));

   function Valid_Element (Buffer : Types.Bytes; Cursor : Cursor_Type) return Boolean is
      (Element_Is_Valid (Buffer (Cursor.First .. Cursor.Last)))
     with
       Pre => (Cursor.First >= Buffer'First
               and then Cursor.Last <= Buffer'Last
               and then Element_Is_Contained (Buffer (Cursor.First .. Cursor.Last)));

end Message_Sequence;
