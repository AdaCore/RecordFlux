with RFLX.RFLX_Generic_Types;
with RFLX.ICMP;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package Generic_Checksum with
   SPARK_Mode
is

   package ICMP renames RFLX.ICMP;

   function Echo_Request_Reply_Checksum (Tag             : ICMP.Tag;
                                         Code            : ICMP.Code_Zero;
                                         Identifier      : ICMP.Identifier;
                                         Sequence_Number : ICMP.Sequence_Number;
                                         Data            : Types.Bytes) return ICMP.Checksum;

private

   function Add (C1 : ICMP.Checksum;
                 C2 : ICMP.Checksum) return ICMP.Checksum;

end Generic_Checksum;
