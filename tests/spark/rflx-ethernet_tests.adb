with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.Ethernet.Frame;

package body RFLX.Ethernet_Tests is

   use type RFLX.RFLX_Builtin_Types.Index;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Ethernet");
   end Name;

   Payload_Length : RFLX_Builtin_Types.Length;

   procedure Store_Payload_Length (Buffer : RFLX_Builtin_Types.Bytes)
   with Annotate => (GNATprove, Always_Return);

   procedure Store_Payload_Length (Buffer : RFLX_Builtin_Types.Bytes)
   is
   begin
      Payload_Length := Buffer'Length;
   end Store_Payload_Length;

   procedure Test_Parsing_Ethernet_II (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Get_Payload_Length is new Ethernet.Frame.Generic_Get_Payload (Store_Payload_Length);
      Buffer         : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Context        : Ethernet.Frame.Context;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

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
                  Assert (Payload_Length'Image, RFLX_Builtin_Types.Length'Image (46), "Invalid Payload length");
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

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image, "Invalid Context.Last");
   end Test_Parsing_Ethernet_II;

   procedure Test_Parsing_IEEE_802_3 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Get_Payload_Length is new Ethernet.Frame.Generic_Get_Payload (Store_Payload_Length);
      Buffer         : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_802.3.raw");
      Context        : Ethernet.Frame.Context;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

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
                  Assert (Payload_Length'Image, RFLX_Builtin_Types.Length'Image (46), "Invalid Payload length");
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

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image, "Invalid Context.Last");
   end Test_Parsing_IEEE_802_3;

   procedure Test_Parsing_Ethernet_II_VLAN (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Get_Payload_Length is new Ethernet.Frame.Generic_Get_Payload (Store_Payload_Length);
      Buffer         : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_vlan_tag.raw");
      Context        : Ethernet.Frame.Context;
      Destination    : Ethernet.Address;
      Source         : Ethernet.Address;
      TPID           : Ethernet.TPID;
      TCI            : Ethernet.TCI;
      Type_Length    : Ethernet.Type_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination) then
         Destination := Ethernet.Frame.Get_Destination (Context);
         Assert (Destination'Image, Ethernet.Address'Image (16#FFFFFFFFFFFF#), "Invalid Destination");
         if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source) then
            Source := Ethernet.Frame.Get_Source (Context);
            Assert (Source'Image, Ethernet.Address'Image (16#000000000000#), "Invalid Source");
            if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TPID) then
               TPID := Ethernet.Frame.Get_TPID (Context);
               Assert (TPID'Image, Ethernet.TPID'Image (16#8100#), "Invalid TPID");
               if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TCI) then
                  TCI := Ethernet.Frame.Get_TCI (Context);
                  Assert (TCI'Image, Ethernet.TCI'Image (1), "Invalid TCI");
                  if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length) then
                     Type_Length := Ethernet.Frame.Get_Type_Length (Context);
                     Assert (Type_Length'Image, Ethernet.Type_Length'Image (16#0800#), "Invalid Type_Length");
                     if Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Payload) then
                        Get_Payload_Length (Context);
                        Assert (Payload_Length'Image, RFLX_Builtin_Types.Length'Image (47), "Invalid Payload length");
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (520)'Image, "Invalid Context.Last");
   end Test_Parsing_Ethernet_II_VLAN;

   procedure Test_Parsing_Invalid_Ethernet_II_Too_Short (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_invalid_too_short.raw");
      Context : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Well_Formed_Message (Context), "Well formed frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (456)'Image, "Invalid Context.Last");
   end Test_Parsing_Invalid_Ethernet_II_Too_Short;

   procedure Test_Parsing_Invalid_Ethernet_II_Too_Long (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_invalid_too_long.raw");
      Context : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Well_Formed_Message (Context), "Well formed frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (12168)'Image, "Invalid Context.Last");
   end Test_Parsing_Invalid_Ethernet_II_Too_Long;

   procedure Test_Parsing_Invalid_Ethernet_II_Undefined_Type (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_undefined.raw");
      Context : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Well_Formed_Message (Context), "Well formed frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (not Ethernet.Frame.Incomplete_Message (Context), "Incomplete frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (272)'Image, "Invalid Context.Last");
   end Test_Parsing_Invalid_Ethernet_II_Undefined_Type;

   procedure Test_Parsing_Invalid_IEEE_802_3_Invalid_Length (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_802.3_invalid_length.raw");
      Context : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      Assert (not Ethernet.Frame.Well_Formed_Message (Context), "Well formed frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Incomplete_Message (Context), "Not incomplete frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image, "Invalid Context.Last");
   end Test_Parsing_Invalid_IEEE_802_3_Invalid_Length;

   procedure Test_Parsing_Incomplete (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 2);
      Context : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Ethernet.Frame.Verify_Message (Context);

      Assert (Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Source), "Invalid Source");
      Assert (Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Destination), "Invalid Destination");
      Assert (Ethernet.Frame.Incomplete (Context, Ethernet.Frame.F_Type_Length_TPID),
              "Not incomplete Type_Length_TPID");
      Assert (not Ethernet.Frame.Valid (Context, Ethernet.Frame.F_Type_Length), "Valid Type_Length");
      Assert (not Ethernet.Frame.Valid (Context, Ethernet.Frame.F_TPID), "Valid TPID");
      Assert (not Ethernet.Frame.Well_Formed_Message (Context), "Well formed frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Incomplete_Message (Context), "Not incomplete frame");

      Ethernet.Frame.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (96)'Image, "Invalid Context.Last");
   end Test_Parsing_Incomplete;

   procedure Test_Generating_Ethernet_II (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      use type RFLX_Types.Length;
      Expected : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 2000 - 1 => 0);
      Context  : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, 801, 2000);
      Ethernet.Frame.Set_Destination (Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Context, 16#0800#);
      Ethernet.Frame.Set_Type_Length (Context, 16#0800#);
      Ethernet.Frame.Set_Payload (Context, (69, 0, 0, 46, 0, 1, 0, 0, 64, 17, 124, 188, 127, 0, 0, 1, 127, 0, 0, 1, 0,
                                            53, 0, 53, 0, 26, 1, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                            0));

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Byte_Size (Context)'Img, Expected'Length'Img,
              "Invalid buffer length");

      declare
         Data : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 2000 - 1) := (others => 0);
         Length : constant RFLX_Types.Length := Ethernet.Frame.Byte_Size (Context);
         First  : constant RFLX_Types.Index := Data'First;
         Last   : constant RFLX_Types.Index := Data'First + RFLX_Types.Index (Length + 1) - 2;
         function Read_Pre (Buffer : RFLX_Types.Bytes) return Boolean is
            (Data'Length >= Length and Buffer'Length = Length);
         procedure Read (Buffer : RFLX_Types.Bytes) with
            Pre =>
               Read_Pre (Buffer)
         is
         begin
            Data (First .. Last) := Buffer;
         end Read;
         procedure Ethernet_Frame_Read is new Ethernet.Frame.Generic_Read (Read, Read_Pre);
      begin
         Ethernet_Frame_Read (Context);
         Assert (Data (First .. Last), Expected.all, "Invalid binary representation");
      end;

      -- Eng/RecordFlux/Workarounds#32
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Ethernet.Frame.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_Ethernet_II;

   procedure Test_Generating_IEEE_802_3 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      use type RFLX_Types.Length;
      Expected : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_802.3.raw");
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 2000 - 1 => 0);
      Context  : Ethernet.Frame.Context;
   begin
      Ethernet.Frame.Initialize (Context, Buffer);
      Ethernet.Frame.Set_Destination (Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Context, 46);
      Ethernet.Frame.Set_Type_Length (Context, 46);
      Ethernet.Frame.Set_Payload (Context, (69, 0, 0, 20, 0, 1, 0, 0, 64, 0, 124, 231, 127, 0, 0, 1, 127, 0, 0, 1, 0, 0,
                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");
      Assert (Ethernet.Frame.Byte_Size (Context)'Img, Expected'Length'Img,
              "Invalid buffer length");

      declare
         Data : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 2000 - 1) := (others => 0);
         Length : constant RFLX_Types.Length := Ethernet.Frame.Byte_Size (Context);
         First  : constant RFLX_Types.Index := Data'First;
         Last   : constant RFLX_Types.Index := Data'First + RFLX_Types.Index (Length + 1) - 2;
         function Read_Pre (Buffer : RFLX_Types.Bytes) return Boolean is
            (Data'Length >= Length and Buffer'Length = Length);
         procedure Read (Buffer : RFLX_Types.Bytes) with
            Pre =>
               Read_Pre (Buffer)
         is
         begin
            Data (First .. Last) := Buffer;
         end Read;
         procedure Ethernet_Frame_Read is new Ethernet.Frame.Generic_Read (Read, Read_Pre);
      begin
         Ethernet_Frame_Read (Context);
         Assert (Data (First .. Last), Expected.all, "Invalid binary representation");
      end;

      -- Eng/RecordFlux/Workarounds#32
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Ethernet.Frame.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_IEEE_802_3;

   procedure Test_Generating_Ethernet_II_VLAN (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_vlan_tag.raw");
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 2000 - 1 => 0);
      Context  : Ethernet.Frame.Context;
      Message_Last : RFLX_Builtin_Types.Bit_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, 801, 2000);
      Ethernet.Frame.Set_Destination (Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Context, 16#8100#);
      Ethernet.Frame.Set_TPID (Context, 16#8100#);
      Ethernet.Frame.Set_TCI (Context, 1);
      Ethernet.Frame.Set_Type_Length (Context, 16#0800#);
      Ethernet.Frame.Set_Payload (Context, (69, 0, 0, 47, 0, 1, 0, 0, 64, 0, 124, 231, 127, 0, 0, 1, 127, 0, 0, 1, 0, 0,
                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                            10));

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");

      Message_Last := Ethernet.Frame.Message_Last (Context);
      Ethernet.Frame.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Message_Last)),
              Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_Ethernet_II_VLAN;

   procedure Test_Generating_Ethernet_II_VLAN_Dynamic (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);

      generic
         type T is (<>);
      function Identity (X : T) return T;

      function Identity (X : T) return T is
        (X);

      -- Simulate a type/length/TPID value that is determined at runtime
      function Dynamic_Type_Length is new Identity (Ethernet.Type_Length);

      Expected : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ethernet_vlan_tag.raw");
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 2000 - 1 => 0);
      Context  : Ethernet.Frame.Context;
      Message_Last : RFLX_Builtin_Types.Bit_Length;
   begin
      Ethernet.Frame.Initialize (Context, Buffer, 801, 2000);
      Ethernet.Frame.Set_Destination (Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Context, Dynamic_Type_Length (16#8100#));
      if Ethernet.Frame.Valid_Next (Context, Ethernet.Frame.F_TPID) then
         Ethernet.Frame.Set_TPID (Context, 16#8100#);
         Ethernet.Frame.Set_TCI (Context, 1);
         Ethernet.Frame.Set_Type_Length (Context, 16#0800#);
         Ethernet.Frame.Set_Payload (Context, (69, 0, 0, 47, 0, 1, 0, 0, 64, 0, 124, 231, 127, 0, 0, 1, 127, 0, 0, 1, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 10));
      end if;

      Assert (Ethernet.Frame.Well_Formed_Message (Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Context), "Valid frame");

      Message_Last := Ethernet.Frame.Message_Last (Context);
      Ethernet.Frame.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Message_Last)),
              Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_Ethernet_II_VLAN_Dynamic;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Ethernet_II'Access, "Parsing Ethernet II");
      Register_Routine (T, Test_Parsing_IEEE_802_3'Access, "Parsing IEEE 802.3");
      Register_Routine (T, Test_Parsing_Ethernet_II_VLAN'Access, "Parsing Ethernet II + VLAN Tag");
      Register_Routine (T, Test_Parsing_Invalid_Ethernet_II_Too_Short'Access,
                        "Parsing Invalid Ethernet II (length value too short)");
      Register_Routine (T, Test_Parsing_Invalid_Ethernet_II_Too_Long'Access,
                        "Parsing Invalid Ethernet II (length value too long)");
      Register_Routine (T, Test_Parsing_Invalid_Ethernet_II_Undefined_Type'Access,
                        "Parsing Invalid Ethernet II (undefined type)");
      Register_Routine (T, Test_Parsing_Invalid_IEEE_802_3_Invalid_Length'Access,
                        "Parsing Invalid IEEE 802.3 (invalid length)");
      Register_Routine (T, Test_Parsing_Incomplete'Access, "Parsing Incomplete");
      Register_Routine (T, Test_Generating_Ethernet_II'Access, "Generating Ethernet II");
      Register_Routine (T, Test_Generating_IEEE_802_3'Access, "Generating IEEE 802.3");
      Register_Routine (T, Test_Generating_Ethernet_II_VLAN'Access, "Generating Ethernet II + VLAN Tag");
      Register_Routine (T, Test_Generating_Ethernet_II_VLAN_Dynamic'Access,
                        "Generating Ethernet II + VLAN Tag (dynamic)");
   end Register_Tests;

end RFLX.Ethernet_Tests;
