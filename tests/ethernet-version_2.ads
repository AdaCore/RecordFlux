with Ethernet; use Ethernet;

package Ethernet.Version_2 is

   function Valid_Destination (Buffer : Bytes) return Boolean with
      Pre => Buffer'Length >= 6;

   function Destination (Buffer : Bytes) return U48 with
      Pre => Buffer'Length >= 6;

   function Valid_Source (Buffer : Bytes) return Boolean with
      Pre => Buffer'Length >= 12;

   function Source (Buffer : Bytes) return U48 with
      Pre => Buffer'Length >= 12;

   function Valid_EtherType (Buffer : Bytes) return Boolean with
      Pre => Buffer'Length >= 14;

   function EtherType (Buffer : Bytes) return U16 with
      Pre => Buffer'Length >= 14;

   function Valid_Payload (Buffer : Bytes) return Boolean with
      Pre => (Buffer'Length >= 14 and then ((Buffer'Length >= 60 and then Buffer'Length <= 1514) and then EtherType (Buffer) >= 1536));

   function Payload (Buffer : Bytes) return Payload_Type with
      Pre => (Buffer'Length >= 14 and then ((Buffer'Length >= 60 and then Buffer'Length <= 1514) and then EtherType (Buffer) >= 1536));

   function Is_Valid (Buffer : Bytes) return Boolean;

end Ethernet.Version_2;
