with Types; use Types;

package Simple_Ethernet is

   type U16 is mod 2**16;
   function Convert_To_U16 is new Convert_To_Mod (U16);

   type U48 is mod 2**48;
   function Convert_To_U48 is new Convert_To_Mod (U48);

end Simple_Ethernet;
