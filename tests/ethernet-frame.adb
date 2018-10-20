package body Ethernet.Frame is

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      if Valid_Payload_0_0_0_0_0_0 (Buffer) then
         First := Payload_0_0_0_0_0_0_First (Buffer);
         Last := Payload_0_0_0_0_0_0_Last (Buffer);
      elsif Valid_Payload_0_0_0_0_0_1 (Buffer) then
         First := Payload_0_0_0_0_0_1_First (Buffer);
         Last := Payload_0_0_0_0_0_1_Last (Buffer);
      elsif Valid_Payload_0_0_0_1_0 (Buffer) then
         First := Payload_0_0_0_1_0_First (Buffer);
         Last := Payload_0_0_0_1_0_Last (Buffer);
      elsif Valid_Payload_0_0_0_1_1 (Buffer) then
         First := Payload_0_0_0_1_1_First (Buffer);
         Last := Payload_0_0_0_1_1_Last (Buffer);
      else
         First := Buffer'Last;
         Last := Buffer'First;
      end if;
   end Payload;

end Ethernet.Frame;
