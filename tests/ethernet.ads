with Types; use Types;

package Ethernet is

   type U48 is mod 2**48;
   function Convert_To_U48 is new Convert_To (U48);

end Ethernet;
