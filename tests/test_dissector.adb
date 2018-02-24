with Ada.Directories;
with Ada.Text_IO;
with Ada.Sequential_IO;

with Types; use Types;
with Ethernet_Dissector;

procedure Test_Dissector is

    function Read_File (Name : in String) return Bytes is
        package Byte_IO is new Ada.Sequential_IO (Byte);
        Input_File : Byte_IO.File_Type;
        Buffer : Bytes (1 .. Integer(Ada.Directories.Size(Name)));
        Value : Byte;
        I : Natural := 0;
    begin
        Byte_IO.Open (Input_File, Byte_IO.In_File, Name);
        while not Byte_IO.End_Of_File (Input_File) loop
            I := I + 1;
            Byte_IO.Read (Input_File, Value);
            Buffer (I) := Value;
        end loop;
        Byte_IO.Close (Input_File);
        return Buffer;
    end Read_File;

    Matched : Natural;

begin

    Ada.Text_IO.Put_Line("-- Test: Ethernet IEEE 802.3");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_802.3.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 60 then " /= 60 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet II");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 60 then " /= 60 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet II, VLAN tag");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_vlan_tag.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 64 then " /= 64 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet II, double VLAN tag");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_double_vlan_tag.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 68 then " /= 68 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet, undefined");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_undefined.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 0 then " /= 0 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet, invalid, too long");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_invalid_too_long.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 0 then " /= 0 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet, invalid, too short");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_invalid_too_short.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 0 then " /= 0 FAILED" else ""));
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("-- Test: Ethernet IEEE 802.3, invalid length");
    Matched := Ethernet_Dissector.Match (Read_File ("tests/ethernet_802.3_invalid_length.raw"));
    Ada.Text_IO.Put_Line("-- Matched: " & Matched'Image & (if Matched /= 0 then " /= 0 FAILED" else ""));
    Ada.Text_IO.New_Line;

end Test_Dissector;
