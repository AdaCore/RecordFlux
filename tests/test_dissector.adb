with SPARK.File_IO;
with SPARK.Text_IO;

with Ethernet; use Ethernet;
with Types; use Types;

package body Test_Dissector is

   procedure Run is
   begin
      SPARK.Text_IO.Put_Line("-- Test: Ethernet II");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet.raw");
         Valid : Boolean;
         Destination : Ethernet.UINT48;
         Source : Ethernet.UINT48;
         EtherType : Ethernet.UINT16;
         First : Natural;
         Last : Natural;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Ethernet.Valid_Destination (Buffer) then
            Destination := Ethernet.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Ethernet.Valid_Source (Buffer) then
               Source := Ethernet.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Ethernet.Valid_EtherType (Buffer) then
                  EtherType := Ethernet.EtherType (Buffer);
                  SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 16#0800# then "OK" else "FAILED"));
                  if Ethernet.Valid_Payload (Buffer) then
                     Ethernet.Payload (Buffer, First, Last);
                     SPARK.Text_IO.Put("Payload: ");
                     Bytes_Put (Bytes (Buffer (First .. Last)));
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
         Destination : Ethernet.UINT48;
         Source : Ethernet.UINT48;
         Length : Natural;
         First : Natural;
         Last : Natural;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Ethernet.Valid_Destination (Buffer) then
            Destination := Ethernet.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Ethernet.Valid_Source (Buffer) then
               Source := Ethernet.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Ethernet.Valid_EtherType (Buffer) then
                  Length := Natural (Ethernet.EtherType (Buffer));
                  SPARK.Text_IO.Put_Line("EtherType: " & (if Length = 46 then "OK" else "FAILED"));
                  if Ethernet.Valid_Payload (Buffer) then
                     Ethernet.Payload (Buffer, First, Last);
                     SPARK.Text_IO.Put("Payload: ");
                     Bytes_Put (Bytes (Buffer (First .. Last)));
                  end if;
               end if;
            end if;
         end if;
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Ethernet II + VLAN Tag");
      declare
         Buffer      : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_vlan_tag.raw");
         Valid       : Boolean;
         Destination : Ethernet.UINT48;
         Source      : Ethernet.UINT48;
         TPID        : Ethernet.UINT16;
         TCI         : Ethernet.UINT16;
         EtherType   : Ethernet.UINT16;
         First       : Natural;
         Last        : Natural;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Valid: " & (if Valid then "OK" else "FAILED"));
         if Ethernet.Valid_Destination (Buffer) then
            Destination := Ethernet.Destination (Buffer);
            SPARK.Text_IO.Put_Line("Destination: " & (if Destination = 16#FFFFFFFFFFFF# then "OK" else "FAILED"));
            if Ethernet.Valid_Source (Buffer) then
               Source := Ethernet.Source (Buffer);
               SPARK.Text_IO.Put_Line("Source: " & (if Source = 16#000000000000# then "OK" else "FAILED"));
               if Ethernet.Valid_TPID (Buffer) then
                  TPID := Ethernet.TPID (Buffer);
                  SPARK.Text_IO.Put_Line("TPID: " & (if TPID = 16#8100# then "OK" else "FAILED"));
                  if Ethernet.Valid_TCI (Buffer) then
                     TCI := Ethernet.TCI (Buffer);
                     SPARK.Text_IO.Put_Line("TCI: " & (if TCI = 1 then "OK" else "FAILED"));
                     if Ethernet.Valid_EtherType (Buffer) then
                        EtherType := Ethernet.EtherType (Buffer);
                        SPARK.Text_IO.Put_Line("EtherType: " & (if EtherType = 16#0800# then "OK" else "FAILED"));
                        if Ethernet.Valid_Payload (Buffer) then
                           Ethernet.Payload (Buffer, First, Last);
                           SPARK.Text_IO.Put("Payload: ");
                           Bytes_Put (Bytes (Buffer (First .. Last)));
                        end if;
                     end if;
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
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (too long)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_invalid_too_long.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (undefined type)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_undefined.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;

      SPARK.Text_IO.Put_Line("-- Test: Invalid Ethernet (invalid length)");
      declare
         Buffer : Bytes := SPARK.File_IO.Read_File ("tests/ethernet_802.3_invalid_length.raw");
         Valid : Boolean;
      begin
         Valid := Ethernet.Is_Valid (Buffer);
         SPARK.Text_IO.Put_Line("Invalid Ethernet: " & (if Valid then "FAILED" else "OK"));
         SPARK.Text_IO.New_Line;
      end;
   end Run;

end Test_Dissector;
