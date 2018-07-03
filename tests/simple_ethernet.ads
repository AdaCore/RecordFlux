with Types; use Types;

package Simple_Ethernet is

    type U48 is mod 2**48;

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
        Pre => (Buffer'Length >= 14 and then ((Buffer'Length >= 60 and then Buffer'Length <= 1514) and then (EtherType (Buffer) <= 1500 or EtherType (Buffer) >= 1536)));

    function Payload (Buffer : Bytes) return Payload_Type with
        Pre => (Buffer'Length >= 14 and then ((Buffer'Length >= 60 and then Buffer'Length <= 1514) and then (EtherType (Buffer) <= 1500 or EtherType (Buffer) >= 1536)));

    function Is_Valid (Buffer : Bytes) return Boolean;

end Simple_Ethernet;
