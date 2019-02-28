with IPv4.Option;

package IPv4.Options
  with SPARK_Mode
is

   type Offset_Type is new Types.Index_Type;

   function Is_Contained (Buffer : Types.Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Label (Buffer : Types.Bytes)
     with
       Ghost,
       Post => Is_Contained (Buffer);

   function Valid_First (Buffer : Types.Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer),
       Post => (if Valid_First'Result then (Valid_Next (Buffer, Offset_Type (Buffer'First)) and then (IPv4.Option.Is_Contained (Buffer (Buffer'First .. Buffer'Last)) and then IPv4.Option.Is_Valid (Buffer (Buffer'First .. Buffer'Last)))));

   procedure Get_First (Buffer : Types.Bytes; Offset : out Offset_Type; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then (Valid_Next (Buffer, Offset_Type (Buffer'First)) and then (IPv4.Option.Is_Contained (Buffer (Buffer'First .. Buffer'Last)) and then IPv4.Option.Is_Valid (Buffer (Buffer'First .. Buffer'Last))))),
       Post => (Offset >= Offset_Type (Buffer'First) and then ((First >= Buffer'First and then Last <= Buffer'Last) and then IPv4.Option.Is_Contained (Buffer (First .. Last))));

   function Valid_Next (Buffer : Types.Bytes; Offset : Offset_Type) return Boolean
     with
       Pre => (Is_Contained (Buffer) and then Offset >= Offset_Type (Buffer'First)),
       Post => (if Valid_Next'Result then (IPv4.Option.Is_Contained (Buffer (Types.Index_Type (Offset) .. Buffer'Last)) and then IPv4.Option.Is_Valid (Buffer (Types.Index_Type (Offset) .. Buffer'Last))));

   procedure Get_Next (Buffer : Types.Bytes; Offset : in out Offset_Type; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then (Offset >= Offset_Type (Buffer'First) and then (Valid_Next (Buffer, Offset) and then (IPv4.Option.Is_Contained (Buffer (Types.Index_Type (Offset) .. Buffer'Last)) and then IPv4.Option.Is_Valid (Buffer (Types.Index_Type (Offset) .. Buffer'Last)))))),
       Post => (Offset >= Offset_Type (Buffer'First) and then ((First >= Buffer'First and then Last <= Buffer'Last) and then IPv4.Option.Is_Contained (Buffer (First .. Last))));

end IPv4.Options;
