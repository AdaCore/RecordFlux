with IPv4.Option;

package IPv4.Options
  with SPARK_Mode
is

   type Offset_Type is new Positive;

   function Is_Contained (Buffer : Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Initialize (Buffer : Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_First (Buffer : Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   procedure First (Buffer : Bytes; Offset : out Offset_Type; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_First (Buffer)),
       Post => ((First >= Buffer'First and then Last <= Buffer'Last) and then IPv4.Option.Is_Contained (Buffer (First .. Last)));

   function Valid_Next (Buffer : Bytes; Offset : Offset_Type) return Boolean
     with
       Pre => Is_Contained (Buffer);

   procedure Next (Buffer : Bytes; Offset : in out Offset_Type; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Next (Buffer, Offset)),
       Post => ((First >= Buffer'First and then Last <= Buffer'Last) and then IPv4.Option.Is_Contained (Buffer (First .. Last)));

end IPv4.Options;
