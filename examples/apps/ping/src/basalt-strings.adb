--
--  @summary String operations for common types
--  @author  Johannes Kliemann
--  @date    2019-11-19
--
--  Copyright (C) 2019 Componolit GmbH
--
--  This file is part of Basalt, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package body Basalt.Strings with
   SPARK_Mode
is

   -----------
   -- Image --
   -----------

   function Image (V : Boolean) return String
   is
   begin
      if V then
         return "True";
      else
         return "False";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (V : Duration) return String
   is
      Seconds : Long_Integer;
      Frac    : Duration;
      V2      : Duration;
   begin
      V2 := V;
      if V > 9223372036.0 then
         V2 := 9223372036.0;
      end if;
      if V < -9223372036.0 then
         V2 := -9223372036.0;
      end if;
      if V2 = 0.0 then
         Seconds := 0;
      elsif V2 < 0.0 then
         Seconds := Long_Integer (V2 + 0.5);
      else --  V2 > 0.0
         Seconds := Long_Integer (V2 - 0.5);
      end if;
      Frac := abs (V2 - Duration (Seconds));
      Frac := Frac * 1000000 - 0.5;
      --  FIXME: Calls of Image with explicit default parameters
      --  Eng/RecordFlux/Workarounds#2
      declare
         F_Image : constant String          := Image (Long_Integer (Frac), 10, True);
         Pad     : constant String (1 .. 6) := (others => '0');
         S_Image : constant String :=
            (if Seconds = 0 then (if V2 < 0.0 then "-0" else "0") else Image (Seconds, 10, True));
      begin
         if Frac = -0.5 then
            return S_Image & "." & Pad;
         end if;
         if F_Image'Length >= 6 then
            return S_Image & "." & F_Image (1 .. 6);
         else --  F_Image'Length < 6
            return S_Image & "." & Pad (1 .. 6 - F_Image'Length) & F_Image;
         end if;
      end;
   end Image;

end Basalt.Strings;
