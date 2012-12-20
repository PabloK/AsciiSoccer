with Meny, Tja.Window.Elementary,Tja.Window.Text,Tja.keyboard;
use Tja.Window.Elementary,Tja.Window.text,Tja.keyboard;

procedure Meny_Test is

   package Game_Meny_package is
      new Meny(4,1,1);
   use Game_Meny_package;

   Game_Meny : Meny_Type := ((10,2,"Antal spelare: ",("2","4","6","8");

begin

   Meny_Res := Init_Meny(Game_Meny,Green,Magenta,"Klar");

end Meny_Test;
