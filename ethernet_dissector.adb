with Ada.Text_IO;

package body Ethernet_Dissector is

    type Two_Octets is mod 2**16;
    type Four_Octets is mod 2**32;
    type Six_Octets is mod 2**48;

    function Convert_To_Two_Octets is new Convert_To (Two_Octets);
    function Convert_To_Four_Octets is new Convert_To (Four_Octets);
    function Convert_To_Six_Octets is new Convert_To (Six_Octets);

    function Match (Buffer : in Bytes) return Natural is
        Matched_Bytes : Natural := 0;
        Offset : Natural := 0;
        Destination : Bytes (1 .. 6);
        Source : Bytes (1 .. 6);
        EtherType_Length_1 : Bytes (1 .. 2);
        EtherType_Length_2 : Bytes (1 .. 2);
        EtherType_Length_3 : Bytes (1 .. 2);
        Dot1q_1 : Bytes (1 .. 4);
        Dot1q_2 : Bytes (1 .. 4);
        Length : Bytes (1 .. 2);
        EtherType : Bytes (1 .. 2);
        L : Integer := Buffer'Length;
    begin
        if Buffer'Length < 60 or Buffer'Length > 1520 then
            return 0;
        end if;

        Ada.Text_IO.Put_Line("Buffer Length: " & L'Image);
        Destination := Buffer (1 .. 6);
        Ada.Text_IO.Put ("Destination: ");
        Bytes_Put (Destination);

        Source := Buffer (7 .. 12);
        Ada.Text_IO.Put ("Source: ");
        Bytes_Put (Source);

        EtherType_Length_1 := Buffer (13 .. 14);
        case Convert_To_Two_Octets (EtherType_Length_1) is
            when 16#8100# =>
                Dot1q_1 := Buffer (13 .. 16);
                Ada.Text_IO.Put ("Dot1q_1: ");
                Bytes_Put (Dot1q_1);
                Offset := Offset + 4;
            when 16#88A8# =>
                Dot1q_1 := Buffer (13 .. 16);
                Ada.Text_IO.Put ("Dot1q_1: ");
                Bytes_Put (Dot1q_1);
                Offset := Offset + 4;
                EtherType_Length_2 := Buffer (17 .. 18);
                case Convert_To_Two_Octets (EtherType_Length_2) is
                    when 16#8100# =>
                        Dot1q_2 := Buffer (17 .. 20);
                        Ada.Text_IO.Put ("Dot1q_2: ");
                        Bytes_Put (Dot1q_2);
                        Offset := Offset + 4;
                    when others =>
                        return 0;
                end case;
            when others =>
                null;
        end case;

        EtherType_Length_3 := Buffer (13 + Offset .. 14 + Offset);
        case Convert_To_Two_Octets (EtherType_Length_3) is
            when 0 .. 1500 =>
                Length := Buffer (13 + Offset .. 14 + Offset);
                Ada.Text_IO.Put ("Length: ");
                Bytes_Put (Length);
                if 15 + Offset + Natural (Convert_To_Two_Octets (Length)) - 1 /= Buffer'Length then
                    return 0;
                end if;
                declare
                    Payload : Bytes (1 .. Natural (Convert_To_Two_Octets (Length)));
                begin
                    Payload := Buffer (15 + Offset .. 15 + Offset + Natural (Convert_To_Two_Octets (Length)) - 1);
                    Ada.Text_IO.Put ("Payload: ");
                    Bytes_Put (Payload);
                end;
            when 1501 .. 1535 =>
                return 0;
            when 1536 .. 65535 =>
                EtherType := Buffer (13 + Offset .. 14 + Offset);
                Ada.Text_IO.Put ("EtherType: ");
                Bytes_Put (EtherType);
                declare
                    Payload : Bytes (1 .. Buffer'Length - 14 - Offset);
                begin
                    Payload := Buffer (15 + Offset .. Buffer'Length);
                    Ada.Text_IO.Put ("Payload: ");
                    Bytes_Put (Payload);
                end;
        end case;
        return Buffer'Length;
    end Match;

end Ethernet_Dissector;
