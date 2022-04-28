--
--  @summary Generic string operations
--  @author  Johannes Kliemann
--  @date    2019-11-19
--
--  Copyright (C) 2019 Componolit GmbH
--
--  This file is part of Basalt, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package Basalt.Strings_Generic with
   SPARK_Mode,
   Pure,
   Annotate => (GNATprove, Always_Return)
is

   type Residue_Class_Ring is new Integer range 0 .. 16;
   subtype Base is Residue_Class_Ring range 2 .. 16;
   subtype Base_Value is Positive range 16 .. 64;
   type Base_Size is array (Base'Range) of Base_Value;
   Base_Length : constant Base_Size := (2  => 64,
                                        3  => 41,
                                        4  => 32,
                                        5  => 28,
                                        6  => 25,
                                        7  => 23,
                                        8  => 22,
                                        9  => 21,
                                        10 => 20,
                                        11 => 19,
                                        12 => 18,
                                        13 => 18,
                                        14 => 17,
                                        15 => 17,
                                        16 => 16);

   subtype Residue is Residue_Class_Ring range 0 .. 15;
   type Charset is array (Residue) of Character;
   Lowercase : constant Charset := ('0', '1', '2', '3', '4', '5',
                                    '6', '7', '8', '9',
                                    'a', 'b', 'c', 'd', 'e', 'f');
   Uppercase : constant Charset := ('0', '1', '2', '3', '4', '5',
                                    '6', '7', '8', '9',
                                    'A', 'B', 'C', 'D', 'E', 'F');

   --  Image function for ranged types
   --
   --  @param V  Ranged type value
   --  @param B  Image base, default is base 10
   --  @param C  Use capital letters if True
   --  @return   Image string
   generic
      type I is range <>;
   function Image_Ranged (V : I;
                          B : Base    := 10;
                          C : Boolean := True) return String with
      Post => Image_Ranged'Result'Length <= Base_Length (B) + 1 and Image_Ranged'Result'First = 1;

   --  Image function for modular types
   --
   --  @param V  Modular type value
   --  @param B  Image base, default is base 10
   --  @param C  Use capital letters if True
   --  @return   Image string
   generic
      type U is mod <>;
   function Image_Modular (V : U;
                           B : Base    := 10;
                           C : Boolean := True) return String with
      Post => Image_Modular'Result'Length <= Base_Length (B) and Image_Modular'Result'First = 1;

   generic
      type T is private;
   package Generic_Optional
   is

      type Optional (Valid : Boolean := False) is record
         case Valid is
            when True =>
               Value : T;
            when False =>
               null;
         end case;
      end record;

   end Generic_Optional;

   generic
      type T is mod <>;
   package Value_Option_Modular
   is

      package Optional_Pac is new Generic_Optional (T);
      subtype Optional is Optional_Pac.Optional;

      function Value (S : String;
                      B : Base) return Optional;

      function Value (S : String) return Optional;

   end Value_Option_Modular;

   generic
      type T is range <>;
   package Value_Option_Ranged
   is

      package Optional_Pac is new Generic_Optional (T);
      subtype Optional is Optional_Pac.Optional;

      function Value (S : String;
                      B : Base) return Optional;

      function Value (S : String) return Optional;

   end Value_Option_Ranged;

private

   type Base_Number (Valid : Boolean := False) is record
      case Valid is
         when True =>
            N_Base : Base;
            First  : Positive;
            Last   : Positive;
         when False =>
            null;
      end case;
   end record;

   function Value_Base (S : String) return Base_Number with
      Pre  => S'Length > 2,
      Post => (if Value_Base'Result.Valid then Value_Base'Result.First in S'Range
                                               and then Value_Base'Result.Last in S'Range);

end Basalt.Strings_Generic;
