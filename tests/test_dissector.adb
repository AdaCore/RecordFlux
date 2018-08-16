with SPARK.File_IO;
with SPARK.Text_IO;

with Ethernet; use Ethernet;
with Ethernet.Version_2;
with Ethernet.IEEE_802_3;
with Simple_Ethernet; use Simple_Ethernet;
with Simple_Ethernet.PDU;
with Types; use Types;

package body Test_Dissector is

   procedure Run is
   begin
      SPARK.Text_IO.Put_Line("-- Test: Ethernet II");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet.raw");
         Valid : Boolean;
         Destination : Simple_Ethernet.U48;
         Source : Simple_Ethernet.U48;
         EtherType : Simple_Ethernet.U16;
      begin
         Valid := Simple_Ethernet.PDU.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Buffer'Length >= 6 and then Simple_Ethernet.PDU.Valid_Destination (Buffer) then
            Destination := Simple_Ethernet.PDU.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Buffer'Length >= 12 and then Simple_Ethernet.PDU.Valid_Source (Buffer) then
               Source := Simple_Ethernet.PDU.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Buffer'Length >= 14 and then Simple_Ethernet.PDU.Valid_EtherType (Buffer) then
                  EtherType := Simple_Ethernet.PDU.EtherType (Buffer);
                  SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 16#0800# then "OK" else "FAILED"));
                  if Buffer'Length >= 60 and then Buffer'Length <= 1514
                        and then (Simple_Ethernet.PDU.EtherType (Buffer) <= 1500 or Simple_Ethernet.PDU.EtherType (Buffer) >= 1536)
                        and then Simple_Ethernet.PDU.Valid_Payload (Buffer) then
                     declare
                        Payload : Payload_Type := Simple_Ethernet.PDU.Payload (Buffer);
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

      SPARK.Text_IO.Put_Line("-- Test: Ethernet II");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet.raw");
         Valid : Boolean;
         Destination : Ethernet.U48;
         Source : Ethernet.U48;
         EtherType : Ethernet.U16;
      begin
         Valid := Ethernet.Version_2.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Buffer'Length >= 6 and then Ethernet.Version_2.Valid_Destination (Buffer) then
            Destination := Ethernet.Version_2.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Buffer'Length >= 12 and then Ethernet.Version_2.Valid_Source (Buffer) then
               Source := Ethernet.Version_2.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Buffer'Length >= 14 and then Ethernet.Version_2.Valid_EtherType (Buffer) then
                  EtherType := Ethernet.Version_2.EtherType (Buffer);
                  SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 16#0800# then "OK" else "FAILED"));
                  if Buffer'Length >= 60 and then Buffer'Length <= 1514 and then EtherType = 16#0800#
                        and then Ethernet.Version_2.Valid_Payload (Buffer) then
                     declare
                        Payload : Payload_Type := Ethernet.Version_2.Payload (Buffer);
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

      SPARK.Text_IO.Put_Line("-- Test: IEEE 802.3");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_802.3.raw");
         Valid : Boolean;
         Destination : Ethernet.U48;
         Source : Ethernet.U48;
         EtherType : Ethernet.U16;
      begin
         Valid := Ethernet.IEEE_802_3.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Buffer'Length >= 6 and then Ethernet.IEEE_802_3.Valid_Destination (Buffer) then
            Destination := Ethernet.IEEE_802_3.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Buffer'Length >= 12 and then Ethernet.IEEE_802_3.Valid_Source (Buffer) then
               Source := Ethernet.IEEE_802_3.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Buffer'Length >= 14 and then Ethernet.IEEE_802_3.Valid_EtherType (Buffer) then
                  EtherType := Ethernet.IEEE_802_3.EtherType (Buffer);
                  SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 46 then "OK" else "FAILED"));
                  if Buffer'Length = EtherType + 14 and then Ethernet.IEEE_802_3.Valid_Payload (Buffer) then
                     declare
                        Payload : Payload_Type := Ethernet.IEEE_802_3.Payload (Buffer);
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

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (too short)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_invalid_too_short.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Version_2.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet II: " & (if Valid then "FAILED" else "OK"));
         Valid := Ethernet.IEEE_802_3.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid IEEE 802.3: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (too long)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_invalid_too_long.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Version_2.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet II: " & (if Valid then "FAILED" else "OK"));
         Valid := Ethernet.IEEE_802_3.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid IEEE 802.3: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (undefined type)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_undefined.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Version_2.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet II: " & (if Valid then "FAILED" else "OK"));
         Valid := Ethernet.IEEE_802_3.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid IEEE 802.3: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (invalid length)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_802.3_invalid_length.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Version_2.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet II: " & (if Valid then "FAILED" else "OK"));
         Valid := Ethernet.IEEE_802_3.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid IEEE 802.3: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;
   end Run;

end Test_Dissector;
