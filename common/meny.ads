with Tja.Keyboard, Tja.Window.Elementary, Tja.Keyboard.Keys, Tja.Window.Text, Ada.Text_Io, Ada.Integer_Text_IO;
use Tja.Keyboard, Tja.Keyboard.Keys, Tja.Window.Elementary, Tja.Window.Text, Ada.Text_Io, Ada.Integer_Text_IO;

generic

   Array_Size  : Integer;
   String_Size : Integer;
   Meny_Size   : Integer;

package Meny is

   type List_type is array(1..Array_Size) of String(1..String_Size);
   type Select_Type is record
      X,Y      : Positive;
      Caption  : String(1..String_Size);
      List     : List_Type;
   end record;


      type Meny_type is array(1..Meny_Size) of Select_type;
      type Meny_Result_Type is array(1..Meny_Size) of Integer;

      Up_Error,Down_Error : Exception;

      function Select_Box(Box    : in Select_Type;
                          Colour : in Colour_Type )return Integer;

      function Init_Meny(Meny           : in Meny_Type;
                         Colour,Scolour : in Colour_Type;
                         End_Message    : in String)return Meny_Result_Type;

end Meny;
