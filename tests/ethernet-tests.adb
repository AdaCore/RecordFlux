with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with Ethernet.Frame;

package body Ethernet.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Ethernet");
   end Name;

   procedure Test_Ethernet_II (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer      : Bytes := Read_File ("tests/ethernet_ipv4_udp.raw");
      Destination : Ethernet.UINT48;
      Source      : Ethernet.UINT48;
      EtherType   : Ethernet.UINT16;
      First       : Natural;
      Last        : Natural;
   begin
      if Ethernet.Frame.Valid_Destination (Buffer) then
         Destination := Ethernet.Frame.Destination (Buffer);
         Assert (Destination'Image, Ethernet.UINT48'Image (16#FFFFFFFFFFFF#), "Invalid Destination");
         if Ethernet.Frame.Valid_Source (Buffer) then
            Source := Ethernet.Frame.Source (Buffer);
            Assert (Source'Image, Ethernet.UINT48'Image (16#000000000000#), "Invalid Source");
            if Ethernet.Frame.Valid_EtherType (Buffer) then
               EtherType := Ethernet.Frame.EtherType (Buffer);
               Assert (EtherType'Image, Ethernet.UINT16'Image (16#0800#), "Invalid EtherType");
               if Ethernet.Frame.Valid_Payload (Buffer) then
                  Ethernet.Frame.Payload (Buffer, First, Last);
                  Assert (First'Image, Natural'Image (15), "Invalid Payload'First");
                  Assert (Last'Image, Natural'Image (60), "Invalid Payload'Last");
               end if;
            end if;
         end if;
      end if;
      Assert (Ethernet.Frame.Is_Valid (Buffer), "Invalid frame");
   end Test_Ethernet_II;

   procedure Test_IEEE_802_3 (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer      : Bytes := Read_File ("tests/ethernet_802.3.raw");
      Destination : Ethernet.UINT48;
      Source      : Ethernet.UINT48;
      EtherType   : Ethernet.UINT16;
      First       : Natural;
      Last        : Natural;
   begin
      if Ethernet.Frame.Valid_Destination (Buffer) then
         Destination := Ethernet.Frame.Destination (Buffer);
         Assert (Destination'Image, Ethernet.UINT48'Image (16#FFFFFFFFFFFF#), "Invalid Destination");
         if Ethernet.Frame.Valid_Source (Buffer) then
            Source := Ethernet.Frame.Source (Buffer);
            Assert (Source'Image, Ethernet.UINT48'Image (16#000000000000#), "Invalid Source");
            if Ethernet.Frame.Valid_EtherType (Buffer) then
               EtherType := Ethernet.Frame.EtherType (Buffer);
               Assert (EtherType'Image, Ethernet.UINT16'Image (46), "Invalid EtherType");
               if Ethernet.Frame.Valid_Payload (Buffer) then
                  Ethernet.Frame.Payload (Buffer, First, Last);
                  Assert (First'Image, Natural'Image (15), "Invalid Payload'First");
                  Assert (Last'Image, Natural'Image (60), "Invalid Payload'Last");
               end if;
            end if;
         end if;
      end if;
      Assert (Ethernet.Frame.Is_Valid (Buffer), "Invalid frame");
   end Test_IEEE_802_3;

   procedure Test_Ethernet_II_VLAN (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer      : Bytes := Read_File ("tests/ethernet_vlan_tag.raw");
      Destination : Ethernet.UINT48;
      Source      : Ethernet.UINT48;
      TPID        : Ethernet.UINT16;
      TCI         : Ethernet.UINT16;
      EtherType   : Ethernet.UINT16;
      First       : Natural;
      Last        : Natural;
   begin
      if Ethernet.Frame.Valid_Destination (Buffer) then
         Destination := Ethernet.Frame.Destination (Buffer);
         Assert (Destination'Image, Ethernet.UINT48'Image (16#FFFFFFFFFFFF#), "Invalid Destination");
         if Ethernet.Frame.Valid_Source (Buffer) then
            Source := Ethernet.Frame.Source (Buffer);
            Assert (Source'Image, Ethernet.UINT48'Image (16#000000000000#), "Invalid Source");
            if Ethernet.Frame.Valid_TPID (Buffer) then
               TPID := Ethernet.Frame.TPID (Buffer);
               Assert (TPID'Image, Ethernet.UINT16'Image (16#8100#), "Invalid TPID");
               if Ethernet.Frame.Valid_TCI (Buffer) then
                  TCI := Ethernet.Frame.TCI (Buffer);
                  Assert (TCI'Image, Ethernet.UINT16'Image (1), "Invalid TCI");
                  if Ethernet.Frame.Valid_EtherType (Buffer) then
                     EtherType := Ethernet.Frame.EtherType (Buffer);
                     Assert (EtherType'Image, Ethernet.UINT16'Image (16#0800#), "Invalid EtherType");
                     if Ethernet.Frame.Valid_Payload (Buffer) then
                        Ethernet.Frame.Payload (Buffer, First, Last);
                        Assert (First'Image, Natural'Image (19), "Invalid Payload'First");
                        Assert (Last'Image, Natural'Image (65), "Invalid Payload'Last");
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      Assert (Ethernet.Frame.Is_Valid (Buffer), "Invalid frame");
   end Test_Ethernet_II_VLAN;

   procedure Test_Invalid_Ethernet_II_Too_Short (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Bytes := Read_File ("tests/ethernet_invalid_too_short.raw");
   begin
      Assert (Not Ethernet.Frame.Is_Valid (Buffer), "False positive");
   end Test_Invalid_Ethernet_II_Too_Short;

   procedure Test_Invalid_Ethernet_II_Too_Long (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Bytes := Read_File ("tests/ethernet_invalid_too_long.raw");
   begin
      Assert (Not Ethernet.Frame.Is_Valid (Buffer), "False positive");
   end Test_Invalid_Ethernet_II_Too_Long;

   procedure Test_Invalid_Ethernet_II_Undefined_Type (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Bytes := Read_File ("tests/ethernet_undefined.raw");
   begin
      Assert (Not Ethernet.Frame.Is_Valid (Buffer), "False positive");
   end Test_Invalid_Ethernet_II_Undefined_Type;

   procedure Test_Invalid_IEEE_802_3_Invalid_Length (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Bytes := Read_File ("tests/ethernet_802.3_invalid_length.raw");
   begin
      Assert (Not Ethernet.Frame.Is_Valid (Buffer), "False positive");
   end Test_Invalid_IEEE_802_3_Invalid_Length;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Ethernet_II'Access, "Ethernet II");
      Register_Routine (T, Test_IEEE_802_3'Access, "IEEE 802.3");
      Register_Routine (T, Test_Ethernet_II_VLAN'Access, "Ethernet II + VLAN Tag");
      Register_Routine (T, Test_Invalid_Ethernet_II_Too_Short'Access, "Invalid Ethernet II (too short)");
      Register_Routine (T, Test_Invalid_Ethernet_II_Too_Long'Access, "Invalid Ethernet II (too long)");
      Register_Routine (T, Test_Invalid_Ethernet_II_Undefined_Type'Access, "Invalid Ethernet II (undefined type)");
      Register_Routine (T, Test_Invalid_IEEE_802_3_Invalid_Length'Access, "Invalid IEEE 802.3 (invalid length)");
   end Register_Tests;

end Ethernet.Tests;
