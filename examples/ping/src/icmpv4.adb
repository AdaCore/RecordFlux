with Basalt.Strings_Generic;
with Ada.Text_IO;
with Interfaces;

package body ICMPv4 with
   SPARK_Mode
is

   ----------------
   -- To_Address --
   ----------------

   procedure Get_Address (Str   :     String;
                          Addr  : out RFLX.IPv4.Address;
                          Valid : out Boolean)
   is
      use type RFLX.IPv4.Address;
      package Int renames Interfaces;
      subtype Octet is RFLX.IPv4.Address range 0 .. 255;
      type Octet_Address is array (1 .. 4) of Octet;
      package Val is new Basalt.Strings_Generic.Value_Option_Modular (Octet);
      Address : Octet_Address := (others => 0);
      Oct_First : Natural;
      Oct_Last  : Natural;
      Oct_Index : Positive := Address'First;
      V         : Val.Optional;
   begin
      Addr  := 0;
      Valid := False;
      if Str'First not in Positive'Range then
         return;
      end if;
      Oct_First := Str'First;
      Oct_Last  := Str'First - 1;
      for I in Str'Range loop
         pragma Loop_Invariant (Oct_First in Str'Range);
         pragma Loop_Invariant (Oct_Last <= Str'Last);
         pragma Loop_Invariant (Oct_Index in Address'Range);
         case Str (I) is
            when '0' .. '9' =>
               Oct_Last := I;
               if I = Str'Last then
                  V := Val.Value (Str (Oct_First .. Oct_Last));
                  if not V.Valid or Oct_Index /= Address'Last then
                     return;
                  end if;
                  Address (Oct_Index) := V.Value;
               end if;
            when '.' =>
               V := Val.Value (Str (Oct_First .. Oct_Last));
               if not V.Valid then
                  return;
               end if;
               Address (Oct_Index) := V.Value;
               if I = Str'Last then
                  return;
               end if;
               Oct_First := I + 1;
               Oct_Last  := I;
               if Oct_Index = Address'Last then
                  return;
               end if;
               Oct_Index := Oct_Index + 1;
            when others =>
               return;
         end case;
      end loop;
      for I in Address'Range loop
         Addr := Addr + RFLX.IPv4.Address (Int.Shift_Left (Int.Unsigned_32 (Address (I)), (I - 1) * 8));
      end loop;
      Valid := True;
   end Get_Address;

   --------------
   -- Generate --
   --------------

   procedure Generate (Buf  : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr;
                       Addr :        RFLX.IPv4.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Generate unimplemented");
      raise Program_Error with "Unimplemented procedure Generate";
   end Generate;

   -----------
   -- Print --
   -----------

   procedure Print (Buf : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Print unimplemented");
      raise Program_Error with "Unimplemented procedure Print";
   end Print;

end ICMPv4;
