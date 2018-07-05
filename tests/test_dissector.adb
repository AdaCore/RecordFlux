with SPARK.File_IO;
with SPARK.Text_IO;

with Simple_Ethernet; use Simple_Ethernet;
with Types; use Types;

procedure Test_Dissector is
begin
   SPARK.Text_IO.Put_Line("-- Test: Ethernet II");
   declare
      Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet.raw");
      Valid : Boolean;
      Destination : Simple_Ethernet.U48;
      Source : Simple_Ethernet.U48;
      EtherType : U16;
   begin
      Valid := Simple_Ethernet.Is_Valid (Buffer);
      SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
      if Buffer'Length >= 6 and then Simple_Ethernet.Valid_Destination (Buffer) then
         Destination := Simple_Ethernet.Destination (Buffer);
         SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
         if Buffer'Length >= 12 and then Simple_Ethernet.Valid_Source (Buffer) then
            Source := Simple_Ethernet.Source (Buffer);
            SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
            if Buffer'Length >= 14 and then Simple_Ethernet.Valid_EtherType (Buffer) then
               EtherType := Simple_Ethernet.EtherType (Buffer);
               SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 16#0800# then "OK" else "FAILED"));
               if Buffer'Length >= 60 and then Buffer'Length <= 1514
                     and then (Simple_Ethernet.EtherType (Buffer) <= 1500 or Simple_Ethernet.EtherType (Buffer) >= 1536)
                     and then Simple_Ethernet.Valid_Payload (Buffer) then
                  declare
                     Payload : Payload_Type := Simple_Ethernet.Payload (Buffer);
                     A : Bytes := Bytes (Payload);
                  begin
                     SPARK.Text_IO.Put("Payload: ");
                     Bytes_Put (Bytes (Payload));
                  end;
               end if;
            end if;
         end if;
      end if;
      SPARK.Text_IO.New_Line;
   end;
end Test_Dissector;
