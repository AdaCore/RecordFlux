with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Ethernet.Frame;

package body RFLX.Ethernet.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Ethernet");
   end Name;

   Payload_Length : Types.Length;

   procedure Store_Payload_Length (Buffer : Types.Bytes) is
   begin
      Payload_Length := Buffer'Length;
   end Store_Payload_Length;

   procedure Get_Payload_Length is new Ethernet.Frame.Get_Payload (Store_Payload_Length);

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_Ethernet_II (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer         : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_ipv4_udp.raw");
      Context        : Ethernet.Frame.Context := Ethernet.Frame.Create;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Destination);
      if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination) then
         Destination := Ethernet.Frame.Get_Destination (Context);
         Assert (Destination'Image, Ethernet.Address'Image (16#FFFFFFFFFFFF#), "Invalid Destination");

         Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Source);
         if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source) then
            Source := Ethernet.Frame.Get_Source (Context);
            Assert (Source'Image, Ethernet.Address'Image (16#000000000000#), "Invalid Source");

            Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Type_Length_TPID);
            Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Type_Length);
            if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length) then
               Type_Length := Ethernet.Frame.Get_Type_Length (Context);
               Assert (Type_Length'Image, Ethernet.Type_Length'Image (16#0800#), "Invalid Type_Length");

               Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Payload);
               if Ethernet.Frame.Present (Context, Ethernet.Frame.F_Payload) then
                  Get_Payload_Length (Context);
                  Assert (Payload_Length'Image, Types.Length'Image (46), "Invalid Payload length");
               else
                  Assert (False, "Structural invalid Payload");
               end if;
            else
               Assert (False, "Invalid Type_Length");
            end if;
         else
            Assert (False, "Invalid Source");
         end if;
      else
         Assert (False, "Invalid Destination");
      end if;

      Assert (Ethernet.Frame.Structural_Valid_Message (Context), "Structural invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
   end Test_Ethernet_II;

   procedure Test_IEEE_802_3 (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer         : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_802.3.raw");
      Context        : Ethernet.Frame.Context := Ethernet.Frame.Create;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Destination);
      if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination) then
         Destination := Ethernet.Frame.Get_Destination (Context);
         Assert (Destination'Image, Ethernet.Address'Image (16#FFFFFFFFFFFF#), "Invalid Destination");

         Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Source);
         if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source) then
            Source := Ethernet.Frame.Get_Source (Context);
            Assert (Source'Image, Ethernet.Address'Image (16#000000000000#), "Invalid Source");

            Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Type_Length_TPID);
            Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Type_Length);
            if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length) then
               Type_Length := Ethernet.Frame.Get_Type_Length (Context);
               Assert (Type_Length'Image, Ethernet.Type_Length'Image (46), "Invalid Type_Length");

               Ethernet.Frame.Verify (Context, Ethernet.Frame.F_Payload);
               if Ethernet.Frame.Present (Context, Ethernet.Frame.F_Payload) then
                  Get_Payload_Length (Context);
                  Assert (Payload_Length'Image, Types.Length'Image (46), "Invalid Payload length");
               else
                  Assert (False, "Invalid Payload");
               end if;
            else
               Assert (False, "Invalid Type_Length");
            end if;
         else
            Assert (False, "Invalid Source");
         end if;
      else
         Assert (False, "Invalid Destination");
      end if;

      Assert (Ethernet.Frame.Structural_Valid_Message (Context), "Structural invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
   end Test_IEEE_802_3;

   procedure Test_Ethernet_II_VLAN (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer         : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_vlan_tag.raw");
      Context        : Ethernet.Frame.Context := Ethernet.Frame.Create;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      TPID           : Ethernet.TPID;
      TCI            : Ethernet.TCI;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination) then
         Destination := Ethernet.Frame.Get_Destination (Context);
         Assert (Destination'Image, Ethernet.Address'Image (16#FFFFFFFFFFFF#), "Invalid Destination");
         if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source) then
            Source := Ethernet.Frame.Get_Source (Context);
            Assert (Source'Image, Ethernet.Address'Image (16#000000000000#), "Invalid Source");
            if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TPID) then
               TPID := Ethernet.Frame.Get_Tpid (Context);
               Assert (TPID'Image, Ethernet.TPID'Image (16#8100#), "Invalid TPID");
               if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TCI) then
                  TCI := Ethernet.Frame.Get_Tci (Context);
                  Assert (TCI'Image, Ethernet.TCI'Image (1), "Invalid TCI");
                  if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length) then
                     Type_Length := Ethernet.Frame.Get_Type_Length (Context);
                     Assert (Type_Length'Image, Ethernet.Type_Length'Image (16#0800#), "Invalid Type_Length");
                     if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Payload) then
                        Get_Payload_Length (Context);
                        Assert (Payload_Length'Image, Types.Length'Image (47), "Invalid Payload length");
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;

      Assert (Ethernet.Frame.Structural_Valid_Message (Context), "Structural invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
   end Test_Ethernet_II_VLAN;

   procedure Test_Invalid_Ethernet_II_Too_Short (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_invalid_too_short.raw");
      Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Structural_Valid_Message (Context), "Structural valid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");
   end Test_Invalid_Ethernet_II_Too_Short;

   procedure Test_Invalid_Ethernet_II_Too_Long (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_invalid_too_long.raw");
      Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Structural_Valid_Message (Context), "Structural valid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");
   end Test_Invalid_Ethernet_II_Too_Long;

   procedure Test_Invalid_Ethernet_II_Undefined_Type (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_undefined.raw");
      Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Structural_Valid_Message (Context), "Structural valid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");
   end Test_Invalid_Ethernet_II_Undefined_Type;

   procedure Test_Invalid_IEEE_802_3_Invalid_Length (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_802.3_invalid_length.raw");
      Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Structural_Valid_Message (Context), "Structural valid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Incomplete_Message (Context), "Not incomplete frame");
   end Test_Invalid_IEEE_802_3_Invalid_Length;

   procedure Test_Incomplete (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 2);
      Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);

      Ethernet.Frame.Verify_Message (Context);

      Assert (Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source), "Invalid Source");
      Assert (Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination), "Invalid Destination");
      Assert (Ethernet.Frame.Incomplete (Context, Ethernet.Frame.F_Type_Length_TPID), "Not incomplete Type_Length_TPID");
      Assert (not Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length), "Valid Type_Length");
      Assert (not Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TPID), "Valid TPID");
      Assert (not Ethernet.Frame.Structural_Valid_Message (Context), "Structural valid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Incomplete_Message (Context), "Not incomplete frame");
   end Test_Incomplete;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Ethernet_II'Access, "Ethernet II");
      Register_Routine (T, Test_IEEE_802_3'Access, "IEEE 802.3");
      Register_Routine (T, Test_Ethernet_II_VLAN'Access, "Ethernet II + VLAN Tag");
      Register_Routine (T, Test_Invalid_Ethernet_II_Too_Short'Access, "Invalid Ethernet II (length value too short)");
      Register_Routine (T, Test_Invalid_Ethernet_II_Too_Long'Access, "Invalid Ethernet II (length value too long)");
      Register_Routine (T, Test_Invalid_Ethernet_II_Undefined_Type'Access, "Invalid Ethernet II (undefined type)");
      Register_Routine (T, Test_Invalid_IEEE_802_3_Invalid_Length'Access, "Invalid IEEE 802.3 (invalid length)");
      Register_Routine (T, Test_Incomplete'Access, "Incomplete");
   end Register_Tests;

end RFLX.Ethernet.Tests;
